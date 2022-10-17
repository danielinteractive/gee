library(gee)
data(warpbreaks)

# AR-1 works.
# test <- warpbreaks[c(1, 2, 28, 29), ] # gives 2 obervations for each of the two `wool` levels
# works <- gee(breaks ~ 1, id=wool, data=test, corstr="AR-M", Mv=1)
# fails <- gee(breaks ~ 1, id=wool, data=test[-1, ], corstr="AR-M", Mv=1)

# Toeplitz works.
test2 <- warpbreaks[c(1, 2, 3, 4, 28, 29, 30, 31), ]
# works <- gee(breaks ~ 1, id=wool, data=test2, corstr="stat_M_dep", Mv=3)
fails <- gee(breaks ~ 1, id=wool, data=test2[-1, ], corstr="stat_M_dep", Mv=3)
