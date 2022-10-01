library(tidymodels) # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr) # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker) # for visualizing regression results

urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") |>
    setNames(c("food_regime", "initial_volume", "width")) |>
    mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

ggplot(
    urchins,
    aes(
        x = initial_volume,
        y = width,
        group = food_regime,
        col = food_regime
    )
) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    scale_color_viridis_d(option = "plasma", end = .7)


lm_mod <- linear_reg()

lm_fit <- lm_mod |>
    fit(width ~ initial_volume * food_regime, data = urchins)

tidy(lm_fit) |>
    dwplot(
        dot_args = list(size = 2, color = "black"),
        whisker_args = list(color = "black"),
        vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
    )

new_points <- expand.grid(
    initial_volume = 20,
    food_regime = c("Initial", "Low", "High")
)

conf_int_pred <- predict(lm_fit, new_data = new_points, type = "conf_int")
mean_pred <- predict(lm_fit, new_data = new_points)

plot_data <- bind_cols(new_points, conf_int_pred) |>
    bind_cols(mean_pred)

ggplot(plot_data, aes(x = food_regime)) +
    geom_point(aes(y = .pred)) +
    geom_errorbar(aes(
        ymin = .pred_lower,
        ymax = .pred_upper
    ),
    width = .2
    ) +
    labs(y = "urchin size")
