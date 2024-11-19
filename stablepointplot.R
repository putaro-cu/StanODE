library(ggplot2)
library(rootSolve)

# パラメータの設定
alpha <- 0.0045
beta <- 0.00015
N <- 600
s <- 10

# 三次方程式の定義
cubic_function <- function(x) {
  beta * x^3 - (-alpha + beta * N - beta * s) * x^2 - (alpha * N + beta * N * s - alpha * s - s) * x - alpha * N * s
}

# xの範囲を設定
x_values <- seq(-20, 600, by = 1)

# yの値を計算
y_values <- cubic_function(x_values)

# データフレームを作成
data <- data.frame(x = x_values, y = y_values)

# 交点を計算
roots <- uniroot.all(cubic_function, c(-100, 700))

# 交点のデータフレームを作成
intersection_points <- data.frame(x = roots, y = rep(0, length(roots)))

# ggplotでグラフを描画
plt <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(data = intersection_points, aes(x = x, y = y), color = "blue", size = 3) +
  xlim(c(-20, 700)) +
  labs(x = "x", y = "y") +
  theme_classic()

# グラフを表示
print(plt)
ggsave("plot_stablepoint.png", plot = plt, width = 1000, height = 800, units = "px")