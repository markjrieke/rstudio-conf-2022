# ----------------------------------setup---------------------------------------
library(tidyverse)
library(workboots)
library(riekelib)

# setup themes
extrafont::loadfonts(device = "win")
theme_set(
  theme_minimal(base_family = "Source Sans Pro Light",
                base_size = 24) +
    theme(plot.title.position = "plot",
          plot.title = ggtext::element_markdown(),
          plot.subtitle = ggtext::element_markdown())
)

# --------------------------------prep-data-------------------------------------

# load data
car_prices <- modeldata::car_prices
car_preds <- readr::read_rds("https://github.com/markjrieke/workboots_support/raw/main/data/car_preds.rds")

# apply global transfomations
car_prices <-
  car_prices %>%
  mutate(Price = log10(Price),
         Cylinder = as.character(Cylinder),
         Doors = as.character(Doors))

# split into testing and training
set.seed(999)
car_split <- rsample::initial_split(car_prices)
car_train <- rsample::training(car_split)
car_test <- rsample::testing(car_split)

# ----------------------------------plot----------------------------------------

plot_color <- "#107D92"

# plot without prediction interval
car_preds %>%
  summarise_predictions() %>%
  bind_cols(car_test) %>%
  filter(Price < 4.7) %>%
  ggplot(aes(x = Price, 
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_point(size = 2.5,
             alpha = 0.25,
             color = plot_color) +
  geom_abline(size = 1, 
              linetype = "dashed",
              color = "gray") +
  # geom_errorbar(alpha = 0.25,
  #               width = 0.0125,
  #               size = 0.75,
  #               color = plot_color) +
  labs(title = glue::glue("A model without {color_text(\"**{workboots}**\", plot_color)}"),
       subtitle = "On its own, XGBoost can only generate point predictions",
       x = "Actual",
       y = "Predicted") +
  expand_limits(x = c(3.9, 4.7),
                y = c(3.9, 4.7))

ggquicksave("plots/cars_point.png")

# plot with prediction interval
car_preds %>%
  summarise_predictions() %>%
  bind_cols(car_test) %>%
  filter(Price < 4.7) %>%
  ggplot(aes(x = Price, 
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_point(size = 2.5,
             alpha = 0.25,
             color = plot_color) +
  geom_abline(size = 1, 
              linetype = "dashed",
              color = "gray") +
  geom_errorbar(alpha = 0.25,
                width = 0.0125,
                size = 0.75,
                color = plot_color) +
  labs(title = glue::glue("A model with {color_text(\"**{workboots}**\", plot_color)}"),
       subtitle = glue::glue("With {color_text(\"**workboots**\", plot_color)}, we can generate prediction intervals!"),
       x = "Actual",
       y = "Predicted") +
  expand_limits(x = c(3.9, 4.7),
                y = c(3.9, 4.7))

ggquicksave("plots/cars_interval.png")

