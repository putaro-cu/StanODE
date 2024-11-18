library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)

# 微分方程式の定義
Ppun_aggr <- function(t, x, params) {
  alpha <- params["alpha"]
  beta <- params["beta"]
  s <- params["s"]
  N <- params["N"]
  
  dxdt <- (alpha + beta * x[1]) * (N - x[1]) - (s * x[1]) / (s + x[1])
  
  list(c(dxdt))
}

# パラメータの設定
alpha <- 0.0045
beta <- 0.00015
s <- 10
N <- 600
times <- seq(0, 100, by = 1)  # 時間範囲

# 初期値をランダムにして10個のデータを生成
set.seed(100) 
initial_values <- runif(10, min = 0, max = N)
results <- list()

for (i in 1:10) {
  x0 <- initial_values[i]
  params <- c(alpha = alpha, beta = beta, s = s, N = N)
  output <- ode(y = c(x = x0), times = times, func = Ppun_aggr, parms = params)
  df <- as.data.frame(output)
  df$initial_value <- x0
  
  # 二項分布に従ってばらつかせる
  df$x_obs <- rbinom(n = nrow(df), size = N, prob = df$x / N)
  df$n_total <- N
  results[[i]] <- df
}

# データを結合
all_data <- bind_rows(results, .id = "series")

# ggplotで散布図を作成
plt <- ggplot(all_data, aes(x = time, y = x_obs, color = as.factor(series))) +
  geom_point(size=0.8) +
  geom_line(aes(y = x), linetype = "solid") +
  labs(x = "時間", y = "観測個体数", color = "系列") +
  theme_classic() # +theme(legend.position = "none") 

ggsave("plot.png", plot = plt, width = 1000, height = 800, units = "px")
write.csv(all_data[all_data$time>0,], "all_data.csv", row.names = FALSE)