
library(matrixcalc)
set.seed(3)

n <- 100
k <- 15


theta <- rnorm(n)

theta_k <- matrix(data = rep(theta, k), ncol = k)

beta <- matrix(data = rep(c(-2, -1, 0, 1, 2), k), ncol = k, nrow = n,
               byrow = TRUE)
  
alpha <- matrix(data = 0.7, ncol = k, nrow = n)

gamma <- 0.25

Z <- exp(hadamard.prod(alpha, theta_k) + beta) / (1 + exp(hadamard.prod(alpha, theta_k) + beta))


data <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), ncol = k,
               nrow = n)

summary(data)
prob <- gamma + (1 - gamma) * (exp((alpha * theta) + beta) / (1 + exp((alpha + theta) + beta)))

alpha <- matrix(data = rep(c(0.7, 0.85, 1, 1.15, 1.3), k), ncol = k,
                nrow = n, byrow = TRUE)
