# ----------------------------------setup---------------------------------------
library(tidyverse)
library(workboots)
library(riekelib)
library(patchwork)

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
ames_pred_int <- read_rds("https://github.com/markjrieke/workboots_support/raw/main/data/ames_boot_pred_int.rds")
ames_conf_int <- read_rds("https://github.com/markjrieke/workboots_support/raw/main/data/ames_boot_conf_int.rds")
ames_test <- read_csv("https://github.com/markjrieke/workboots_support/raw/main/data/ames_test.csv")

plot_color <- "#107D92"
interval_color <- "#23A0A4"

prediction_text <- "\"prediction\""
confidence_text <- "\"confidence\""

# prediction interval
pred_plot <- 
  ames_pred_int %>%
  summarise_predictions() %>%
  bind_cols(ames_test) %>%
  ggplot(aes(x = First_Flr_SF)) +
  geom_point(aes(y = Sale_Price),
             size = 1.5,
             alpha = 0.15) +
  geom_line(aes(y = .pred),
            size = 1,
            color = plot_color) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper),
              alpha = 0.15,
              fill = plot_color) +
  expand_limits(y = c(4.5, 6)) +
  theme(plot.subtitle = ggtext::element_markdown(family = "Lucida Console", size = 18)) +
  labs(subtitle = glue::glue("interval = {color_text(prediction_text, interval_color)}"),
       x = NULL,
       y = NULL)

# confidence interval
conf_plot <- 
  ames_conf_int %>%
  summarise_predictions() %>%
  bind_cols(ames_test) %>%
  ggplot(aes(x = First_Flr_SF)) +
  geom_point(aes(y = Sale_Price),
             size = 1.5,
             alpha = 0.15) +
  geom_line(aes(y = .pred),
            size = 1,
            color = plot_color) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper),
              alpha = 0.15,
              fill = plot_color) +
  expand_limits(y = c(4.5, 6)) +
  theme(plot.subtitle = ggtext::element_markdown(family = "Lucida Console", size = 18)) +
  labs(subtitle = glue::glue("interval = {color_text(confidence_text, interval_color)}"),
       x = NULL,
       y = NULL)

# patchwork !
pred_plot + conf_plot +
  plot_annotation(title = glue::glue("{color_text(\"**{workboots}**\", plot_color)} can generate multiple interval types"))

ggquicksave("plots/pred_conf_int.png")
