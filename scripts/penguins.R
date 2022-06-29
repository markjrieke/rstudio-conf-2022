# ----------------------------------setup---------------------------------------
library(tidymodels)
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

# ----------------------------------workbootin'---------------------------------

# load our dataset
data("penguins")
penguins <- penguins %>% drop_na()

# split data into testing & training sets
set.seed(123)
penguins_split <- initial_split(penguins)
penguins_test <- testing(penguins_split)
penguins_train <- training(penguins_split)

# create a workflow for vi
penguins_wf <-
  workflow() %>%
  add_recipe(recipe(body_mass_g ~ ., data = penguins_train) %>% step_dummy(all_nominal())) %>%
  add_model(boost_tree("regression"))

# get bootstrap variable importances
set.seed(987)
penguins_vi <-
  penguins_wf %>%
  vi_boots(
    n = 2000,
    training_data = penguins_train,
    verbose = TRUE
  )

# ----------------------------------plot----------------------------------------

plot_color <- "#107D92"

penguins_vi %>%
  summarise_importance() %>%
  mutate(variable = forcats::fct_reorder(variable, .importance)) %>%
  ggplot(aes(x = variable,
             y = .importance,
             ymin = .importance_lower,
             ymax = .importance_upper)) +
  geom_point(size = 2.5) +
  geom_errorbar() +
  coord_flip() +
  labs(title = "Bootstrap estimations of variable importance",
       subtitle = "Uses vip::vi() under the hood",
       x = NULL,
       y = NULL)

ggquicksave("plots/penguins_vip.png")
       