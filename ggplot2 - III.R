library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library('RColorBrewer')

# Scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = Species))


# Summary statistics 
iris_summary <- iris %>% group_by(Species) %>%
  summarise(Sepal.Length = mean(Sepal.Length), 
            Sepal.Width = mean(Sepal.Width),
            Petal.Length = mean(Petal.Length), 
            Petal.Width = mean(Petal.Width))

# Alternatively
head(iris)
iris_summary <- aggregate(iris[1:4], list(iris$Species), mean)
iris_summary
names(iris_summary)[1] <- "Species"

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() + 
  geom_point(data = iris_summary, shape = 15, size = 5)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() + 
  geom_point(data = iris_summary, shape = 21, size = 5, fill = "#00000080")

# crosshairs
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() + 
  geom_vline(data = iris_summary, linetype = 2,
             aes(xintercept = Sepal.Length, col = Species)) +
  geom_hline(data = iris_summary, linetype = 2,
             aes(yintercept = Sepal.Width, col = Species))

# Jittering
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = 'jitter')


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter(alpha = 0.6)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter(shape = 1)

# Large dataset - diamonds
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1, size = 4)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(shape = '.')



ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_point()

# Adjust for overplotting
ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_point(alpha = 0.5)


ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +
  geom_point(alpha = 0.5, position = 'jitter')

ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +
  geom_jitter()

ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +
  geom_jitter(width = 0.1)

vocab <- read_csv('Vocab.csv')
vocab <- vocab %>%
  select(-1)
vocab

ggplot(vocab, aes(x = education, y = vocabulary)) +
  geom_point()

# Use geom_jitter() instead of geom_point()
ggplot(vocab, aes(x = education, y = vocabulary)) +
  geom_jitter()

ggplot(vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(alpha = 0.2)


ggplot(vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(shape = 1)

# Bar plots
ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram()

diff(range(iris$Sepal.Width)) / 30

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(binwidth = 0.1)

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1)

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, 
                 fill = '#377EB8')

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1)

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = 'stack')


ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = 'dodge')

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = 'fill')

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = 'identity')

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = 'identity', alpha = 0.4)

ggplot(iris, aes(x = Sepal.Width, col = Species)) +
  geom_freqpoly()

sleep <- (ggplot2::msleep) %>%
  select(c(3, 6, 7)) 

sleep <- sleep %>%
  mutate(positive = ifelse(extra > 0, 'TRUE', 'FALSE'))


ggplot(sleep, aes(vore)) +
  geom_bar()

sleep_summary <- sleep %>%
  group_by(vore) %>%
  summarise(avg = mean(sleep_total),
            stdev = sd(sleep_total)) %>%
  filter(!is.na(vore)) %>%
  mutate(vore = as.factor(vore))


ggplot(sleep_summary, aes(x = vore, y = avg)) +
  geom_bar(stat = 'identity')

ggplot(sleep, aes(x = group, fill = positive)) +
  geom_bar()

ggplot(sleep, aes(x = group, fill = positive)) +
  geom_bar(position = 'fill')

ggplot(sleep, aes(x = group, fill = positive)) +
  geom_bar(position = 'dodge')


# Overlapping bar plots
posn_d <- position_dodge(0.2)

# Change the position argument to posn_d
ggplot(sleep, aes(x = group, fill = positive)) +
  geom_bar(position = posn_d)

ggplot(sleep, aes(x = group, fill = positive)) +
  geom_bar(position = posn_d, alpha = 0.6)


View(economics)
recess <- read_csv('recess.csv')

# Plot unemploy as a function of date using a line plot
economics %>%
  ggplot(aes(x = date, y = unemploy)) +
  geom_line()

# Fraction of total population that is unemployed as a function of time
economics %>%
  ggplot(aes(x = date, y = unemploy/pop)) +
  geom_line()

# Draw the recess periods on basic line plot
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
            aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()


ggplot(mtcars, aes(x = cyl , fill = am)) +
  geom_bar() +
  scale_fill_brewer(palette = 'Set1')

vocab <- vocab %>%
  mutate_each(funs(as.factor), c(3, 4))

ggplot(vocab, aes(x = education, fill = vocabulary)) +
  geom_bar() + 
  scale_fill_brewer()




# Make a color range using colorRampPalette() and the set of blues
blues <- brewer.pal(9, 'Blues')
blue_range <- colorRampPalette(blues)

# Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(vocab, aes(x = education, fill = vocabulary)) +
  geom_bar() + 
  scale_fill_manual(values = blue_range(11))


ggplot(vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = 'fill') + 
  scale_fill_manual(values = blue_range(11))

glimpse(vocab)

# Multiple Time Series
fish.species <- tbl_df(fish.species)
fish.species

# Use gather to go from fish.species to fish.tidy
fish.species <- fish.species %>%
  gather(Species, Capture, -Year)


ggplot(fish.species, aes(x = Year, y = Capture, col = Species)) + 
  geom_line()

fish.species

ggplot(fish.species, aes(x = Year, y = Capture, linetype = Species)) + 
  geom_line()


ggplot(fish.species, aes(x = Year, y = Capture, size = Species)) + 
  geom_line()

# fill
ggplot(fish.species, aes(x = Year, y = Capture, fill = Species)) + 
  geom_area()

ggplot(fish.species, aes(x = Year, y = Capture, fill = Species)) + 
  geom_area(position = 'fill')

ggplot(fish.species, aes(x = Year, y = Capture, fill = Species)) + 
  geom_ribbon(aes(ymax = Capture, ymin = 0), alpha = 0.3)

