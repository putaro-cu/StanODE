library(tidyverse)
library(rstan)
library(bayesplot)
library(ggplot2)
library(cmdstanr)
library(tidybayes)

raw_data <- read.csv("all_data.csv")
data <- raw_data %>% filter(series == 10) %>% select(time, x_obs, n_total)

input <- list(
  N = nrow(data),
  ts = data$time,
  x = data$x_obs,
  n_total = data$n_total[1]
)

stan <- cmdstan_model('model1.stan')
fit <- stan$sample(data = input, iter_warmup = 1000, iter_sampling = 1000, parallel_chains = 4, chains = 4, save_warmup = TRUE)


color_scheme_set("brewer-RdYlBu")
plt_dens <- mcmc_dens_overlay(fit$draws(c("a", "b", "s", "sigma"),inc_warmup = F)) + geom_density(linewidth = 1) + theme_classic()
color_scheme_set("blue")
plt_trace <- mcmc_trace(fit$draws(c("a", "b", "s", "sigma"),inc_warmup = T),n_warmup = 1000) + theme_classic()
plt_pairs <- mcmc_pairs(fit$draws(c("a", "b", "s", "sigma"),inc_warmup = F), off_diag_args = list(size = 0.5, alpha = 0.5))
ggsave("trace_plot.png", plot = plt_trace, width = 1000, height = 800, units = "px", dpi=180)
ggsave("dens_plot.png", plot = plt_dens, width = 1000, height = 800, units = "px", dpi=180)
ggsave("pairs_plot.png", plot = plt_pairs, width = 1000, height = 800, units = "px", dpi=180)
