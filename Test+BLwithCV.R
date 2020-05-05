# Test | Bayesian Learning with Confirmation Bias
library(Bolstad)

# Our true parameters are as follows
mu_true <- -19
sd_true <- 10


# Yielding the following distribution of data 
n <- 100000
x <- rnorm(n, mean = mu_true, sd = sd_true)

# Our priors are ass follows:
# Mean µ ~ N(µ_0, τn_0)
mu_0 <- 1
tau <- 1
n_0 <- 1

# Precision τ ∼ Ga(α, β)
alpha <- 10 ^ -1
beta <- 10 ^ -1
tau_0 <- alpha / beta
var_0 <- tau_0 ^ -(1 / 2)

# Likelihood x_i | µ, τ ∼ N (µ, τ ) i.i.d.
x_bar <- mean(x)
x_var <- var(x)

# These yield the following posterior distiributions
# Precision τ | x ∼ Ga (α + n / 2, β + Σ (x_i − x_bar)^2 + (x_bar − µ_0)^2 * nn0 / 2(n + n0))
alpha_post <- alpha + n / 2
beta_post <-
  beta + (1 / 2) * sum((x - x_bar) ^ 2) + (x_bar - mu_0) ^ 2 * (n * n_0) / (2 * (n + n_0))
tau_post <- alpha_post / beta_post
var_post <- tau_post ^ -(1 / 2)

# Mean µ | x, τ ∼ N(x_bar * nτ / (nτ + n0τ) x_bar +  µ_0 * n_0τ / (nτ + n0τ) , τ(nτ + n_0) )
mean_post <-
  (x_bar * n * tau_post) / (n * tau_post + n_0 * tau_post) + (mu_0 * n_0 * tau_post) / (n * tau_post + n_0 * tau_post)

# Print out results
print(paste("mu_true = ", round(mu_true, digits = 2), "; sd_true = ", round(sd_true, digits = 2), sep = ""))
print(paste("mu_0 = ", round(mu_0, digits = 2), "; var_0 = ", round(var_0, digits = 2), sep = ""))
print(paste("x_bar = ", round(x_bar, digits = 2), "; x_var = ", round(x_var, digits = 2), sep = ""))
print(paste("mu_post =", round(mean_post, digits = 2), "; var_post =", round(var_post, digits = 2), sep = ""))

# Try this piecemeal
for(i in 1:length(x)) {
  
}

