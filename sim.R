library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)

# 微分方程式の定義
Ppun_aggr <- function(t, x, params) {
  alpha <- params["alpha"]
  k <- params["k"]
  theta <- params["theta"]
  n_total <- params["n_total"]
  
  dxdt <- alpha * x[1]^k / (theta^k + x[1]^k) * (n_total - x[1]) -
    alpha * (n_total - x[1])^k / (theta^k + (n_total - x[1])^k) * x[1]
  
  list(c(dxdt))
}

# パラメータの固定値設定
alpha <- 16.22615177 / 100
theta <- 51.05357662
n_total <- 100
x0 <- 59.58915479  # 初期個体数
times <- seq(0, 50, by = 0.1)  # 時間範囲

# kの値を0.1刻みで変化させて結果を保存
k_values <- seq(23.51199165, 50, by = 0.5)  # kの範囲と刻み
results <- list()

for (k in k_values) {
  params <- c(alpha = alpha, k = k, theta = theta, n_total = n_total)
  output <- ode(y = c(x = x0), times = times, func = Ppun_aggr, parms = params)
  df <- as.data.frame(output)
  df$k <- k  # kの値を記録
  results[[as.character(k)]] <- df
}

# データを結合
all_results <- bind_rows(results, .id = "k")
all_results$k <- as.numeric(all_results$k)  # kを数値型に変換

# ggplotでプロット
plt <- ggplot(all_results, aes(x = time, y = x, color = factor(k)))
plt <- plt + geom_line(size = 1)
plt <- plt + scale_color_viridis_d(name = "k value")
plt <- plt + labs(title = "Dynamics of Population with Varying k", x = "Time", y = "Population")
plt <- plt + coord_cartesian(ylim = c(0, 100))  # y軸を0から100に制限
plt <- plt + theme_classic()  # クラシックテーマを適用
plt <- plt + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
plt <- plt + guides(color = guide_legend(override.aes = list(size = 2)))

# プロット
plot(plt)