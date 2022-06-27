# --------------------------------libraries-------------------------------------
library(tidymodels)
library(workboots)

# -----------------------------read-in-data-------------------------------------
pumpkins <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv")

pumpkins <- 
  pumpkins %>% 
  tidyr::separate(id, into = c("year", "type")) %>%
  mutate(across(c(year, place, weight_lbs, ott), readr::parse_number),
         weight_lbs = sqrt(weight_lbs)) %>%
  filter(type == "P",
         ott > 20, 
         ott < 1000) %>%
  select(weight_lbs,
         year,
         place,
         ott,
         country) %>%
  drop_na()

# ------------------------------tidymodeling------------------------------------

# split into testing/training
set.seed(999)
pumpkins_split <- initial_split(pumpkins)
pumpkins_train <- training(pumpkins_split)
pumpkins_test <- testing(pumpkins_split)

# preprocessing & model spec
pumpkins_rec <- 
  recipe(weight_lbs ~ ., data = pumpkins_train) %>%
  step_other(country) %>%
  step_mutate(year = as.factor(year)) %>%
  step_dummy(year, country)

pumpkins_spec <-
  boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")

pumpkins_wf <-
  workflow() %>%
  add_recipe(pumpkins_rec) %>%
  add_model(pumpkins_spec)

# -----------------------------workbootin'--------------------------------------

# generate prediction interval
set.seed(666)
pumpkins_preds_int <-
  pumpkins_wf %>%
  predict_boots(
    n = 200,
    training_data = pumpkins_train,
    new_data = pumpkins_test,
    verbose = TRUE
  )

# ----------------------------save----------------------------------------------
pumpkins_test %>% readr::write_csv("data/pumpkins_test.csv")
pumpkins_preds_int %>% readr::write_rds("data/pumpkins_preds_int.rds")
