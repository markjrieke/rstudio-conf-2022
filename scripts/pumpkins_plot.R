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

# ----------------------------------plot----------------------------------------
pumpkins_pred_int <- read_rds("data/pumpkins_preds_int.rds")
pumpkins_test <- read_csv("data/pumpkins_test.csv")

pumpkin_color <- "#107D92"

set.seed(99)
pumpkins_plot <-
  pumpkins_pred_int %>%
  summarise_predictions() %>%
  bind_cols(pumpkins_test) %>%
  slice_sample(n = 500)

# plot without prediction interval
pumpkins_plot %>%
  ggplot(aes(x = weight_lbs,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_point(size = 2.5,
             alpha = 0.15,
             color = pumpkin_color) +
  geom_abline(size = 1,
              linetype = "dashed",
              color = "gray") +
  labs(title = glue::glue("A model without {color_text(\"**{workboots}**\", pumpkin_color)}"),
       subtitle = "On its own, XGBoost can only generate point predictions",
       x = "Actual",
       y = "Predicted")

ggquicksave("plots/pumpkins_point.png")

pumpkins_plot %>%
  ggplot(aes(x = weight_lbs,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_point(size = 2.5,
             alpha = 0.15,
             color = pumpkin_color) +
  geom_errorbar(alpha = 0.15,
                width = 0.75,
                size = 0.75,
                color = pumpkin_color) +
  geom_abline(size = 1,
              linetype = "dashed",
              color = "gray") +
  labs(title = glue::glue("A model with {color_text(\"**{workboots}**\", pumpkin_color)}"),
       subtitle = glue::glue("With {color_text(\"**{workboots}**\", pumpkin_color)}, we can generate prediction intervals!"),
       x = "Actual",
       y = "Predicted")

ggquicksave("plots/pumpkins_interval.png")

