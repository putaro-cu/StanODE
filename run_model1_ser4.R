library(tidyverse)
library(rstan)
library(bayesplot)
library(ggplot2)
library(cmdstanr)
library(tidybayes)

raw_data <- read.csv("all_data.csv")
data <- raw_data %>% filter(series == 7) %>% select(time, x_obs, n_total)

input <- list(
  N = nrow(data),
  ts = data$time,
  x = data$x_obs,
  n_total = data$n_total[1]
)

stan <- cmdstan_model('model1.stan')
fit_alt <- stan$sample(data = input, iter_warmup = 2000, iter_sampling = 2000, parallel_chains = 4, chains = 4, save_warmup = TRUE, adapt_delta = 0.95)


color_scheme_set("brewer-RdYlBu")
plt_dens <- mcmc_dens_overlay(fit_alt$draws(c("a", "b", "s", "sigma"),inc_warmup = F)) + geom_density(linewidth = 1) + theme_classic()
color_scheme_set("blue")
plt_trace <- mcmc_trace(fit_alt$draws(c("a", "b", "s", "sigma"),inc_warmup = T),n_warmup = 2000) + theme_classic()
plt_pairs <- mcmc_pairs(fit_alt$draws(c("a", "b", "s", "sigma"),inc_warmup = F), off_diag_args = list(size = 0.5, alpha = 0.5))
ggsave("trace_plot_series_4.png", plot = plt_trace, width = 1000, height = 800, units = "px", dpi=180)
ggsave("dens_plot_series_4.png", plot = plt_dens, width = 1000, height = 800, units = "px", dpi=180)
ggsave("pairs_plot_series_4.png", plot = plt_pairs, width = 1000, height = 800, units = "px", dpi=180)

df_xpred <- fit_alt$draws(format = "df") %>%
  spread_draws(mu_pred[time]) %>%
  median_hdi(.width = 0.95)  # 予測値の抽出と95%CIの計算

plt_fitting <- ggplot(df_xpred, aes(x = time)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = '#179407', alpha = 0.4) +
  geom_line(aes(y = mu_pred), linewidth = 1, col = "#179407") +
  geom_line(data = data.frame(input), aes(x = ts, y = x), col = "#179407") +
  geom_point(data = data.frame(input), aes(x = ts, y = x), col = "#179407") +
  ## ylim(c(0, 600)) +
  theme_classic() +
  labs(x = "時間", y = "観測個体数")

ggsave("fitting_plot_series_4.png", plot = plt_fitting, width = 1000, height = 800, units = "px", dpi=180)
