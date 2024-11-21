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

stan <- cmdstan_model('model1_2.stan')
fit <- stan$sample(data = input, iter_warmup = 5000, iter_sampling = 5000, parallel_chains = 4, chains = 4, save_warmup = TRUE, adapt_delta = 0.9)


color_scheme_set("brewer-RdYlBu")
plt_dens <- mcmc_dens_overlay(fit$draws(c("b", "s", "sigma"),inc_warmup = F)) + geom_density(linewidth = 1) + theme_classic()
color_scheme_set("blue")
plt_trace <- mcmc_trace(fit$draws(c("b", "s", "sigma"),inc_warmup = T),n_warmup = 2000) + theme_classic()
plt_pairs <- mcmc_pairs(fit$draws(c("b", "s", "sigma"),inc_warmup = F), off_diag_args = list(size = 0.5, alpha = 0.5))
ggsave("trace_plot1_2.png", plot = plt_trace, width = 1000, height = 400, units = "px", dpi=180)
ggsave("dens_plot1_2.png", plot = plt_dens, width = 1000, height = 400, units = "px", dpi=180)
ggsave("pairs_plot1_2.png", plot = plt_pairs, width = 1000, height = 800, units = "px", dpi=180)

df_xpred <- fit$draws(format = "df") %>%
  spread_draws(mu_pred[time]) %>%
  median_hdi(.width = 0.95)  # 予測値の抽出と95%CIの計算

plt_fitting <- ggplot(df_xpred, aes(x = time)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = '#D53E4F', alpha = 0.4) +
  geom_line(aes(y = mu_pred), linewidth = 1, col = "#D53E4F") +
  geom_line(data = data.frame(input), aes(x = ts, y = x), col = "#D53E4F") +
  geom_point(data = data.frame(input), aes(x = ts, y = x), col = "#D53E4F") +
  ylim(c(0, 600)) +
  theme_classic() +
  labs(x = "時間", y = "観測個体数")

ggsave("fitting_plot1_2.png", plot = plt_fitting, width = 1000, height = 800, units = "px", dpi=180)

fit$draws(format = "df") %>%
    spread_draws(b,s,sigma) %>%
    mean_qi() # EAPと95%ETI
fit$draws(format = "df") %>%
    spread_draws(b,s,sigma) %>%
    mode_hdi() # MAPと95%HDI
fit$draws(format = "df") %>%
    spread_draws(b,s,sigma) %>%
    median_hdi() # MEDと95%HDI