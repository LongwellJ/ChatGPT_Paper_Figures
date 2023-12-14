
## weighted kappa
## https://www.datanovia.com/en/lessons/weighted-kappa-in-r-for-two-ordinal-variables/
## Most recent update with all of ASCO and ESMO
## upload excel data and use table() command 
concordance <- as.table(
  rbind(
    c(16, 29, 0, 0), c(9, 15, 0, 0),
    c(0, 0, 1, 4), c(0, 0, 2, 178)
  )
)
dimnames(concordance) <- list(
  Doctor1 = c("1", "2", "3", "4"),
  Doctor2 = c("1", "2", "3", "4")
)
concordance
library("vcd")
res.k <- Kappa(concordance)
res.k
confint(res.k,level = 0.95,)
summary(res.k)