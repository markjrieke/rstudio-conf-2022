# ----------------------------------setup---------------------------------------
library(tidyverse)
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

# ----------------------------no-context-plots----------------------------------

# patient satisfaction over time
set.seed(999)
p1 <-
  tibble(date = seq.Date(as.Date("2021-07-01"), as.Date("2022-07-01"), "months")) %>%
  rowid_to_column() %>%
  rowwise() %>%
  mutate(n = round(rnorm(1, 150, 25)),
         n = if_else(date == as.Date("2022-04-01"), 5, n),
         true_topbox = 0.6 + 2 * rowid/100,
         topbox = rbinom(1, n, true_topbox),
         topbox = if_else(date == as.Date("2022-04-01"), as.integer(2), topbox)) %>%
  ungroup() %>%
  mutate(not_topbox = n - topbox,
         score = topbox/n) %>%
  riekelib::beta_interval(topbox, not_topbox) %>%
  ggplot(aes(x = date,
             y = score,
             ymin = ci_lower,
             ymax = ci_upper)) +
  geom_line(size = 1) +
#  geom_ribbon(alpha = 0.5) +
  expand_limits(y = c(0, 1)) +
  scale_x_date(labels = scales::label_date("%b-%y")) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Monthly Patient Satisfaction Scores",
       subtitle = "Why the large drop in April?",
       x = NULL,
       y = NULL)

# save !
p1
riekelib::ggquicksave("plots/analyst_no_context_p1.png")


# response rate comparison
p2 <- 
  tibble(distribution = c("Email", "Text"),
         response_rate = c(0.25, 0.15),
         n = c(1460, 330)) %>%
  mutate(responded = response_rate * n,
         no_response = n - responded) %>%
  riekelib::beta_interval(responded, no_response) %>%
  mutate(distribution = fct_reorder(distribution, response_rate)) %>%
  ggplot(aes(x = distribution,
             y = response_rate,
             ymin = ci_lower,
             ymax = ci_upper)) +
  geom_point(size = 2.5) +
  geom_text(aes(label = as.character(response_rate) %>% str_remove("0.") %>% paste0("%")),
            size = 8,
            family = "Source Sans Pro Light",
            fontface = "bold",
            vjust = -0.7) +
  #geom_errorbar(size = 1, width = 0.5) +
  coord_flip() +
  expand_limits(y = c(0, 0.3)) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Response Rates by Distribution Method",
       subtitle = "Which method gets more responses?",
       x = NULL,
       y = NULL)

# save!
p2
riekelib::ggquicksave("plots/analyst_no_context_p2.png")

# score comparison
set.seed(877)
p3 <- 
  tibble(hospital = c("Big Bird Emergency",
                      "Grouchytown Hospital",
                      "Cookie Monster Outpatient", 
                      "The Count Regional Hospital",
                      "Bert Bay Hospital",
                      "Elmoville Hospital",
                      "Ernie Shore Hospital")) %>%
  rowwise() %>%
  mutate(n = round(rnorm(1, 150, 25)),
         score = rbeta(1, 175, 75)) %>%
  ungroup() %>%
  mutate(topbox = round(n * score),
         not_topbox = n - topbox) %>%
  riekelib::beta_interval(topbox, not_topbox) %>%
  mutate(hospital = fct_reorder(hospital, score)) %>%
  ggplot(aes(x = hospital,
             y = score,
             ymin = ci_lower,
             ymax = ci_upper)) +
  geom_point(size = 2.5) +
  geom_text(aes(label = as.character(round(score, 2)) %>% str_remove("0.") %>% paste0("%")),
            size = 8,
            family = "Source Sans Pro Light",
            fontface = "bold",
            vjust = -0.7) +
  #geom_errorbar(size = 1, width = 0.5) +
  expand_limits(y = c(0.6, 0.8)) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Patient Satisfaction Scores by Hosptial",
       subtitle = "Are these real differences across the hospitals?",
       x = NULL,
       y = NULL)

# save!
p3
riekelib::ggquicksave("plots/analyst_no_context_p3.png")

# ----------------------------plots-with-context--------------------------------

p1 + geom_ribbon(alpha = 0.25)
riekelib::ggquicksave("plots/analyst_context_p1.png")

p2 + geom_errorbar(size = 1, width = 0.5)
riekelib::ggquicksave("plots/analyst_context_p2.png")

p3 + geom_errorbar(size = 1, width = 0.5)
riekelib::ggquicksave("plots/analyst_context_p3.png")








