library(ggplot2)
data = read.csv('./data/data.csv', header = TRUE)

# plot 그리기
plot_fit = ggplot(data = data, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm, formula = y~splines::bs(x, 3), se = FALSE)

plot_fit

# plot 저장
ggsave('plots/plot_fit.pdf', plot_fit) # 그림의 크기나 dpi 등의 옵션을 스스로 지정할 수 O
