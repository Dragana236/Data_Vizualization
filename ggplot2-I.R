library(MASS)
library(ggplot2)
library(grid)

head(mammals)

# Create general scatter plot
ggplot(mammals, aes(x = body, y = brain)) +
  geom_point() 

# Add regression line, also add some transparency
ggplot(mammals, aes(x = body, y = brain)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = 'lm', col = 'red', se = FALSE)

# Log transformation
ggplot(mammals, aes(x = body, y = brain)) +
  geom_point(alpha = 0.6) +
  coord_fixed() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = 'lm', col = '#C42126', se = FALSE, size = 1)


head(mtcars)
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()

# Change the command below so that cyl is treated as factor
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, col = disp)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

head(diamonds)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_point(alpha = 0.4)


head(iris)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6)

table(iris$Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~Species)

iris_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6)

iris_plot + facet_grid(. ~Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~Species) +
  stat_smooth(method = 'lm', se = FALSE, col = 'red')

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~Species) +
  stat_smooth(method = 'lm', se = FALSE, col = 'red') +
  scale_y_continuous('Sepal Width (cm)',
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous('Sepal Lengt (cm)',
                     limits = c(4,8),
                     expand = c(0,0)) +
  coord_equal()

levels(iris$Species) <- c("Setosa", "Versicolor", "Virginica")


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~Species) +
  stat_smooth(method = 'lm', se = FALSE, col = 'red') +
  scale_y_continuous('Sepal Width (cm)',
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous('Sepal Lengt (cm)',
                     limits = c(4,8),
                     expand = c(0,0)) +
  coord_equal() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.margin = unit(1, "lines"))

