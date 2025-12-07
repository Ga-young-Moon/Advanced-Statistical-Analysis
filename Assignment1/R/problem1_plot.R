# 1-(b)
library(ggplot2)

df = read.csv('data/problem1.csv')
head(df)

lp = ggplot(df, aes(x, y)) +
  geom_point(color= 'blue', size= 2.5) +
  geom_smooth(method = 'lm', color= 'red')

print(lp)

# 1-(c)
ggsave(filename = 'plots/problem1_plot.png', plot = lp, width= 8, height= 7.5)
