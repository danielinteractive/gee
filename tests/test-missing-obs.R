library(gee)
data(warpbreaks)

# AR-1 works.
test <- warpbreaks[c(1, 2, 28, 29), ] # gives 2 obervations for each of the two `wool` levels
test$time <- factor(c(1, 3, 1, 3))
works <- gee(breaks ~ 1, id=wool, data=test, index = time, corstr="AR-M", Mv=1)

test2 <- test[-1, ]
fails <- gee(breaks ~ 1, id=wool, data=test2, index = time, corstr="AR-M", Mv=1)

# Compare with SAS.
library(r2stream)
sascode <- list(
  "test" = "
    proc genmod data=ana.dat; 
    class wool time tension; 
    model breaks =; 
    repeated subject=wool /within=time type=AR; 
  run; 
  "
)
sas_results <- bee_sas(data = list("dat" = test2), sascode = sascode)
sas_results$test$sas_out


# Toeplitz works.
test3 <- warpbreaks[c(1, 2, 3, 4, 28, 29, 30, 31), ]
test3$time <- factor(c(1, 2, 3, 4, 1, 2, 3, 4))
works <- gee(breaks ~ 1, id=wool, data=test3, corstr="stat_M_dep", Mv=3)

test4 <- test3[-1, ]
fails <- gee(breaks ~ 1, id=wool, data=test4, index=time, corstr="stat_M_dep", Mv=3)

