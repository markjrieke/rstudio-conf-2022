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
  boost_tree(trees = tune(),
             mtry = tune(),
             sample_size = tune(),
             tree_depth = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

pumpkins_wf <-
  workflow() %>%
  add_recipe(pumpkins_rec) %>%
  add_model(pumpkins_spec)

# tuning
set.seed(888)
pumpkins_grid <-
  grid_latin_hypercube(
    trees(), 
    finalize(mtry(), pumpkins_rec %>% prep() %>% bake(new_data = NULL)),
    sample_size = sample_prop(),
    tree_depth(),
    size = 7
  )

set.seed(777)
pumpkins_rs <- vfold_cv(pumpkins_train)

set.seed(666)
pumpkins_tuned <-
  pumpkins_wf %>%
  tune_grid(pumpkins_rs,
            grid = pumpkins_grid)

pumpkins_tuned %>%
  collect_metrics()

pumpkins_final <- 
  pumpkins_wf %>%
  finalize_workflow(pumpkins_tuned %>% select_best("rmse"))

# -----------------------------workbootin'--------------------------------------

# generate prediction interval
pumpkins_preds_int <-
  pumpkins_final %>%
  predict_boots(
    n = 200,
    training_data = pumpkins_train,
    new_data = pumpkins_test,
    verbose = TRUE
  )

# ----------------------------save----------------------------------------------
pumpkins_test %>% readr::write_csv("data/pumpkins_test.csv")
pumpkins_preds_int %>% readr::write_rds("data/pumpkins_preds_int.rds")
