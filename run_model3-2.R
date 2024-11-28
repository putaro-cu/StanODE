library(tidyverse)
library(rstan)
library(bayesplot)
library(ggplot2)
library(cmdstanr)
library(tidybayes)

raw_data <- read.csv("all_data_3.csv")
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
fit_alt <- stan$sample(data = input, iter_warmup = 3000, iter_sampling = 3000, parallel_chains = 4, chains = 4, save_warmup = TRUE)


color_scheme_set("brewer-RdYlBu")
plt_dens <- mcmc_dens_overlay(fit_alt$draws(c("a", "b", "s"),inc_warmup = F)) + geom_density(linewidth = 1) + theme_classic()
color_scheme_set("blue")
plt_trace <- mcmc_trace(fit_alt$draws(c("a", "b", "s"),inc_warmup = T),n_warmup = 3000) + theme_classic()
plt_pairs <- mcmc_pairs(fit_alt$draws(c("a", "b", "s"),inc_warmup = F), off_diag_args = list(size = 0.5, alpha = 0.5))
ggsave("trace_plot4.png", plot = plt_trace, width = 1500, height = 400, units = "px", dpi=180)
ggsave("dens_plot4.png", plot = plt_dens, width = 1000, height = 400, units = "px", dpi=180)
ggsave("pairs_plot4.png", plot = plt_pairs, width = 1000, height = 800, units = "px", dpi=180)

df_xpred <- fit_alt$draws(format = "df") %>%
  spread_draws(mu_pred[series,time,vector]) %>%
  mode_hdi(.width = 0.95)  # MAP推定値と95%CIの計算

list2env(setNames(df_list, paste0("df", 1:6)), envir = .GlobalEnv) # 各リスト要素を df1, df2, df3 に代入

plt_fitting <- ggplot(df1, aes(x = time)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "#D53E4F", alpha = 0.4) +
    geom_line(aes(y = mu_pred), linewidth = 1, col = "#D53E4F") +
    geom_ribbon(data = df2, aes(ymin = .lower, ymax = .upper), fill = "#cd9c32", alpha = 0.4) +
    geom_line(data = df2, aes(y = mu_pred), linewidth = 1, col = "#cd9c32") +
    geom_ribbon(data = df3, aes(ymin = .lower, ymax = .upper), fill = "#32CD32", alpha = 0.4) +
    geom_line(data = df3, aes(y = mu_pred), linewidth = 1, col = "#32CD32") +
    geom_ribbon(data = df4, aes(ymin = .lower, ymax = .upper), fill = "#41b4e2", alpha = 0.4) +
    geom_line(data = df4, aes(y = mu_pred), linewidth = 1, col = "#41b4e2") +
    geom_ribbon(data = df5, aes(ymin = .lower, ymax = .upper), fill = "#4169E2", alpha = 0.4) +
    geom_line(data = df5, aes(y = mu_pred), linewidth = 1, col = "#4169E2") +
    geom_ribbon(data = df6, aes(ymin = .lower, ymax = .upper), fill = "#a70de4", alpha = 0.4) +
    geom_line(data = df6, aes(y = mu_pred), linewidth = 1, col = "#a70de4") +
    geom_line(data = data, aes(x = time, y = x_obs, group = as.factor(series), col = as.factor(series))) +
    geom_point(data = data, aes(x = time, y = x_obs, group = as.factor(series), col = as.factor(series))) +
    labs(x = "時間", y = "観測個体数", color = "系列") +
    theme_classic() +
    labs(x = "時間", y = "観測個体数")

ggsave("fitting_plot4.png", plot = plt_fitting, width = 1000, height = 800, units = "px", dpi=180)
