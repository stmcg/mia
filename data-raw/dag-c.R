# Code based on dag_name = "1C" from simulations, which corresponds to DAG (c) in paper

expit = function(p) exp(p) / (1 + exp(p))
N <- 10000

set.seed(1234)

du <- data.frame(C1 = rbinom(n = N, size = 1,  prob = 0.5))
du$A1 <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$C1))
du$D1 <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$A1))
du$B1 <- rnorm(n = N, mean = 2.6*du$C1 + 2*du$A1 + du$A1*du$C1)

du$RA <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$D1))
du$RD <- rbinom(n = N, size = 1, prob = 0.5)
du$RC <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$D1))
du$RB <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$D1))

du$A <- ifelse(du$RA == 1, du$A1, NA)
du$B <- ifelse(du$RB == 1, du$B1, NA)
du$C <- ifelse(du$RC == 1, du$C1, NA)
du$D <- ifelse(du$RD == 1, du$D1, NA)

dat.sim <- du[, c('A', 'B', 'C', 'D')]
colnames(dat.sim) <- c('X2', 'Y', 'X1', 'W')

dat.sim <- dat.sim[!apply(is.na(dat.sim), 1, all), ]
dat.sim <- dat.sim[, c('Y', 'X1', 'X2', 'W')]
rownames(dat.sim) <- 1:nrow(dat.sim)

# usethis::use_data(dat.sim, overwrite = TRUE)
