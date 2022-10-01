library(tidymodels) # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr) # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker) # for visualizing regression results

urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") |>
    setNames(c("food_regime", "initial_volume", "width")) |>
    mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
