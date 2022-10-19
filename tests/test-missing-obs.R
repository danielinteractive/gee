library(gee)
data(warpbreaks)

# Compare with SAS.
library(r2stream)
sascode <- list(
  "test" = "
    proc genmod data=ana.dat; 
    class wool time tension; 
    model breaks =; 
    repeated subject=wool /corrw within=time type=AR; 
  run; 
  "
)

# AR-1 ----
test <- warpbreaks[c(1, 2, 28, 29), ] # gives 2 obervations for each of the two `wool` levels
test$time <- factor(c(1, 2, 1, 2))
test$time_int <- as.integer(test$time)
works <- gee(breaks ~ 1, id=wool, data=test, index = time, corstr="AR-M", Mv=1)

sas_results <- bee_sas(data = list("dat" = test), sascode = sascode)
writeLines(sas_results$test$sas_out, "sas_ar1_test.txt")

summary(works)
# this gives the same coefficient estimates, 
# but the working correlation matrix is different - maybe ok

library(geeM)
geem_result <- geem(breaks ~ 1, id = wool, waves = time_int, data = test, corstr = "ar1")
summary(geem_result)

# AR-1 with one obs missing ----
test2 <- test[-1, ]
fails <- gee(breaks ~ 1, id=wool, data=test2, index = time, corstr="AR-M", Mv=1)

sas_results <- bee_sas(data = list("dat" = test2), sascode = sascode)
writeLines(sas_results$test$sas_out, "sas_ar1_test2.txt")

summary(fails)
# this gives different coefficient estimate - but very small obs

geem_result2 <- geem(breaks ~ 1, id = wool, waves = time_int, data = test2, corstr = "ar1")
# does not work at all

# Toeplitz works ----
test3 <- warpbreaks[c(1, 2, 3, 4, 28, 29, 30, 31), ]
test3$time <- factor(c(1, 2, 3, 4, 1, 2, 3, 4))
test3$time_int <- as.integer(test3$time)
works <- gee(breaks ~ 1, id=wool, data=test3, 
             # index=time, 
             corstr="stat_M_dep", Mv=3)

sascode2 <- list(
  "test" = "
    proc genmod data=ana.dat; 
    class wool time tension; 
    model breaks =; 
    repeated subject=wool /corrw within=time type=MDEP(3); 
  run; 
  "
)

sas_results <- bee_sas(data = list("dat" = test3), sascode = sascode2)
writeLines(sas_results$test$sas_out, "sas_mdep3_test.txt")

summary(works)
# this looks slightly different... even with the CRAN version of gee.

geem_result3 <- geem(breaks ~ 1, id = wool, waves = time_int, data = test3, corstr = "m-dependent", Mv = 3)
summary(geem_result3)
# so this looks pretty good!!

library(geepack)
# geep_result3 <- geeglm(
#   breaks ~ 1,
#   data = test3,
#   id = wool, # this is taken from `data`
#   family = gaussian(),
#   corstr = "ar1"
# )
# geepack does not even support Toeplitz at all!

test4 <- test3[-1, ]
fails <- gee(breaks ~ 1, id=wool, data=test4, index=time, corstr="stat_M_dep", Mv=3)

geem_result4 <- geem(breaks ~ 1, id = wool, waves = time_int, data = test4, corstr = "m-dependent", Mv = 3)
summary(geem_result4)

sas_results <- bee_sas(data = list("dat" = test4), sascode = sascode2)
writeLines(sas_results$test$sas_out, "sas_mdep3_test4.txt")

summary(fails)
# here more differences. but also very small sample size.

# problem is that geem does not have interface with emmeans:
library(emmeans)
emmeans(geem_result3, ~wool)
# but we could add that... not so difficult. see https://github.com/rvlenth/emmeans/issues/341

