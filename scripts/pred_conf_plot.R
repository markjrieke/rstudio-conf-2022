# ----------------------------------setup---------------------------------------
library(tidymodels)
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

# ----------------------------------workbootin'---------------------------------

# setup data
data("ames")
set.seed(999)
ames <- 
  ames %>%
  select(First_Flr_SF, Sale_Price) %>%
  mutate(across(everything(), log10)) %>%
  slice_sample(n = 200)

# split into test/train
set.seed(888)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

# setup workflow
ames_wf <-
  workflow() %>%
  add_recipe(recipe(Sale_Price ~ First_Flr_SF, data = ames_train)) %>%
  add_model(linear_reg())

# generate pred interval
set.seed(777)
ames_pred_int <-
  ames_wf %>%
  predict_boots(
    n = 2000,
    training_data = ames_train,
    new_data = ames_test,
    verbose = TRUE
  )

# generate conf interval
# ~ fewer because I'm lazy & this is just for the plot ~
set.seed(666)
ames_conf_int <-
  ames_wf %>%
  predict_boots(
    n = 200,
    training_data = ames_train,
    new_data = ames_test,
    interval = "confidence",
    verbose = TRUE
  )

# ----------------------------------plot----------------------------------------
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
             size = 2.5,
             alpha = 0.15) +
  geom_line(aes(y = .pred),
            size = 1,
            color = plot_color) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper),
              alpha = 0.25,
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
             size = 2.5,
             alpha = 0.15) +
  geom_line(aes(y = .pred),
            size = 1,
            color = plot_color) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper),
              alpha = 0.25,
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
