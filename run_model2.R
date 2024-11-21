library(tidyverse)
library(rstan)
library(bayesplot)
library(ggplot2)
library(cmdstanr)
library(tidybayes)

raw_data <- read.csv("all_data_2.csv")
data <- raw_data %>% select(series, time, x_obs, n_total)
series_ids <- unique(data$series)
x_list <- map(series_ids, ~ data %>% filter(series == .x) %>% pull(x_obs))
ts_list <- map(series_ids, ~ data %>% filter(series == .x) %>% pull(time))
n_list <- map(series_ids, ~ data %>% filter(series == .x) %>% pull(n_total))
ts_lengths <- map(ts_list, length)  # 各系列の長さ　このモデルでは全て同じ長さにする必要があるがこれを使えば拡張はできるはず

input <- list(
  Series = length(series_ids),
  N = ts_lengths[1],
  ts = ts_list,
  x = x_list,
  n_total = n_list
)

stan <- cmdstan_model('model2.stan')
fit <- stan$sample(data = input, iter_warmup = 2000, iter_sampling = 2000, parallel_chains = 4, chains = 4, save_warmup = TRUE, )


color_scheme_set("brewer-RdYlBu")
plt_dens <- mcmc_dens_overlay(fit$draws(c("a", "b", "s"),inc_warmup = F)) + geom_density(linewidth = 1) + theme_classic()
color_scheme_set("blue")
plt_trace <- mcmc_trace(fit$draws(c("a", "b", "s"),inc_warmup = T),n_warmup = 2000) + theme_classic()
plt_pairs <- mcmc_pairs(fit$draws(c("a", "b", "s"),inc_warmup = F), off_diag_args = list(size = 0.5, alpha = 0.5))
ggsave("trace_plot2.png", plot = plt_trace, width = 1000, height = 400, units = "px", dpi=180)
ggsave("dens_plot2.png", plot = plt_dens, width = 1000, height = 400, units = "px", dpi=180)
ggsave("pairs_plot2.png", plot = plt_pairs, width = 1000, height = 800, units = "px", dpi=180)

df_xpred <- fit$draws(format = "df") %>%
  spread_draws(mu_pred[series,time,vector]) %>%
  mode_hdi(.width = 0.95)  # MAP推定値と95%CIの計算

df_list <- split(df_xpred, df_xpred$series)# series列で分割してリストに格納


list2env(setNames(df_list, paste0("df", 1:3)), envir = .GlobalEnv) # 各リスト要素を df1, df2, df3 に代入

plt_fitting <- ggplot(df1, aes(x = time)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = '#D53E4F', alpha = 0.4) +
  geom_line(aes(y = mu_pred), linewidth = 1, col = "#D53E4F") +
  geom_ribbon(data = df2, aes(ymin = .lower, ymax = .upper), fill = '#32CD32', alpha = 0.4) +
  geom_line(data = df2, aes(y = mu_pred), linewidth=1, col="#32CD32") +
  geom_ribbon(data = df3, aes(ymin = .lower, ymax = .upper), fill = '#4169E2', alpha = 0.4) +
  geom_line(data = df3, aes(y = mu_pred), linewidth=1, col="#4169E2") +
  geom_line(data=data, aes(x = time, y = x_obs, group = as.factor(series), col = as.factor(series))) +
  geom_point(data=data, aes(x = time, y = x_obs, group = as.factor(series), col = as.factor(series))) +
  theme_classic() +
  labs(x = "時間", y = "観測個体数")

ggsave("fitting_plot2.png", plot = plt_fitting, width = 1000, height = 800, units = "px", dpi=180)
