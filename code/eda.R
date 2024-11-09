library(skimr)
library(rio)
library(tidyverse)
library(patchwork)

# load data
data <- rio::import("data/happiness.csv")
# run data_manipulation.R
source("code/data_manipulation.R")

# view data
skim(data)

plot_theme <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  strip.text = element_text(size = 15),
  axis.text = element_text(size = 15),
  axis.title = element_text(size = 15),
  legend.title = element_text(size = 15),
  legend.text = element_text(size = 12)
)

# univariate on the response
# response
ggplot(data = data, aes(x = score)) +
  geom_histogram() +
  theme_bw() +
  plot_theme

# economy
ggplot(data = data, aes(x = economy)) +
  geom_histogram() +
  theme_bw() +
  plot_theme

# perceived_corruption
ggplot(data = data, aes(x = perceived_corruption)) +
  geom_histogram() +
  theme_bw() +
  plot_theme

# turn country into continent?
# or turn country into longitude and latitude?
# or something else?

# most have 3 years of data some have two and some have 1
ggplot(data = data, aes(x = country)) +
  geom_bar()


# correlation between predictors...
data %>% ggplot(aes(x = perceived_corruption, y = economy)) +
  geom_point() +
  geom_smooth()

# very slight negative relatiohsip all good
cor(data$perceived_corruption, data$economy)

# relationship between response and predictors
# response ~ economy
data %>% ggplot(aes(x = economy, y = score)) +
  geom_point() +
  geom_smooth()

# response ~ perceived_corruption
data %>% ggplot(aes(x = perceived_corruption, y = score)) +
  geom_point() +
  geom_smooth()

# response ~ latitude
data %>% ggplot(aes(x = latitude, y = score)) +
  geom_point() +
  geom_smooth()

# response ~ longitude
data %>% ggplot(aes(x = longitude, y = score)) +
  geom_point() +
  geom_smooth()

# response ~ continent
data %>% ggplot(aes(x = continent, y = score)) +
  geom_boxplot(aes(fill = continent)) +
  theme_bw() +
  plot_theme

# response ~ economy by continent
p_econ_cont <- data %>% ggplot(aes(x = economy, y = score, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  plot_theme

# compute correlation between economy and score by continent
data %>%
  group_by(continent) %>%
  summarise(correlation = cor(economy, score))

# response ~ perceived corruption by continent
p_corrup_cont <- data %>% ggplot(aes(x = perceived_corruption, y = score, color = continent)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  theme_bw() +
  plot_theme

p_corrup_cont + p_econ_cont

# relationship between predictors

# perceived_corruption ~ economy
data %>% ggplot(aes(x = economy, y = perceived_corruption)) +
  geom_point() +
  geom_smooth()

# perceived_corruption ~ continent
data %>% ggplot(aes(x = continent, y = perceived_corruption)) +
  geom_boxplot(aes(fill = continent)) +
  theme_bw() +
  plot_theme

# economy ~ continent
data %>% ggplot(aes(x = continent, y = economy)) +
  geom_boxplot(aes(fill = continent)) +
  theme_bw() +
  plot_theme
