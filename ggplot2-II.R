library(ggplot2)
library(tidyr)

head(iris)

# Base package
plot(iris$Sepal.Length, iris$Sepal.Width)
points(iris$Petal.Length, iris$Petal.Width, col = 'red')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
p + geom_point()
p + geom_jitter()

mtcars
head(mtcars)

# Plot the three variables of mtcars
plot(mtcars$mpg, mtcars$wt, col = as.factor(mtcars$cyl))

# Calculate a linear model 
carModel <- lm(mpg ~ wt, data = mtcars)
plot(mtcars$wt, mtcars$mpg)

# Plot linear model
abline(carModel, lty = 2)

plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})

# Plot linear model with ggplot2

ggplot(mtcars, aes(x = wt, y = mpg, col = as.factor(cyl))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

ggplot(mtcars, aes(x = wt, y = mpg, col = as.factor(cyl))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(group = 1), method = 'lm', se = FALSE, linetype = 2)


head(iris.wide)

ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) +
  geom_point()

head(iris)

iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c('Part', 'Measure', '\\.'))

ggplot(iris.tidy, aes(x = Measure, y = Value, col = Part)) + 
  geom_jitter()


ggplot(iris.tidy, aes(x = Measure, y = Value, col = Part)) + 
  geom_jitter() + 
  facet_grid(. ~ Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

ggplot(iris) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species))


ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, col = as.factor(cyl))) +
  geom_point(shape = 1, size = 4)

ggplot(mtcars, aes(x = wt, y = mpg, fill = as.factor(cyl))) +
  geom_point(shape = 1, size = 4)

ggplot(mtcars, aes(x = wt, y = mpg, fill = as.factor(cyl))) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

ggplot(mtcars, aes(x = wt, y = mpg, fill = as.factor(cyl), col = as.factor(am))) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

ggplot(iris, aes(col = Sepal.Length, y = Sepal.Width, 
                 x = Species)) +
  geom_point()

ggplot(iris, aes(size = Sepal.Length, y = Sepal.Width, 
                 x = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 shape = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 shape = Species, col = Species)) +
  geom_point()

# Map Species to size
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 size = Species)) +
  geom_point()

# Map Species to alpha

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 alpha = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 label = Species)) +
  geom_text()

# Set the color aesthetic 

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point()

# Set the color aesthetic and attribute

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(color = "#4ABEFF")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 fill = Species)) +
  geom_point(shape = 23, color = "#4ABEFF", size = 10)



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 fill = Species)) +
  geom_point(alpha = 0.5)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 fill = Species)) +
  geom_point(shape = 24, col = 'yellow')

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 fill = Species)) +
  geom_text(label = rownames(iris), color = 'red')

ggplot(mtcars, aes(x = mpg, y = qsec, 
                   col = factor(cyl), shape = factor(am))) +
  geom_point()

# Add mapping: (hp/wt) onto size (now 5 aesthetics)
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), 
                   shape = factor(am), size = (hp/wt))) +
  geom_point()


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'identity')


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'jitter')


posn.j <- position_jitter(width = 0.1)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = posn.j)



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'jitter') +
  scale_x_continuous('Sepal Lenght') +
  scale_color_discrete('Species')


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'jitter') +
  scale_x_continuous('Sepal Lenght', limits = c(2,8)) +
  scale_color_discrete('Species')



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'jitter') +
  scale_x_continuous('Sepal Lenght', limits = c(2,8),
                     breaks = seq(2, 8, 3)) +
  scale_color_discrete('Species')

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'jitter') +
  scale_x_continuous('Sepal Lenght', limits = c(2,8),
                     breaks = seq(2, 8, 3), expand = c(0,0)) +
  scale_color_discrete('Species',
                       labels = c('Setosa', 'Versicolor', 'Virginica'))



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 col = Species)) +
  geom_point(position = 'jitter') +
  labs(x = 'Sepal Length', y = 'Sepal Width', col = 'Species')


cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

cyl.am + geom_bar()

cyl.am + geom_bar(position = 'fill')

cyl.am + geom_bar(position = 'dodge')

cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission", 
                    values = c("#E41A1C", "#377EB8"),
                    labels = c("Manual", "Automatic"))





