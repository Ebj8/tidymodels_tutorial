library(tidymodels) # for the recipes package, along with the rest of tidymodels

# Helper packages
library(nycflights13) # for flight data
library(skimr)

set.seed(123)

flight_data <- flights |>
    mutate(
        # Convert the arrival delay to a factor
        arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
        arr_delay = factor(arr_delay),
        # We will use the date (not date-time) in the recipe below
        date = lubridate::as_date(time_hour)
    ) |>
    # Include the weather data
    inner_join(weather, by = c("origin", "time_hour")) |>
    # Only retain the specific columns we will use
    select(
        dep_time, flight, origin, dest, air_time, distance,
        carrier, date, arr_delay, time_hour
    ) |>
    # Exclude missing data
    na.omit() |>
    # For creating models, it is better to have qualitative columns
    # encoded as factors (instead of character strings)
    mutate_if(is.character, as.factor)

data_split <- initial_split(flight_data, prop = 3 / 4)

train_data <- training(data_split)
test_data <- training(data_split)

flights_rec <- recipe(arr_delay ~ ., data = train_data) |>
    update_role(flight, time_hour, new_role = "ID") |>
    # step_ functions all come from the recipes package and are used to
    # mutate data in a recipe. They're adding new "steps" to the recipe.
    step_date(date, features = c("dow", "month")) |>
    step_holiday(date,
        holidays = timeDate::listHolidays("US"),
        keep_original_cols = FALSE
    ) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors())

# The summary function lists all the current variables and their roles.
summary(flights_rec)

lr_mod <- logistic_reg() |>
    set_engine("glm")

# Workflows are a bundle of models and recipes so that you don't have to call
# your recipe every time you want to do something with your model.
flights_wflow <- workflow() |>
    add_model(lr_mod) |>
    add_recipe(flights_rec)

flights_fit <- flights_wflow |>
    fit(data = train_data)

# predict will normally just predict one of the binary outcomes
# ("on time" or "late") by default but by makinge type = "prob", the output is
# now a probability of that outcome.
predict(flights_fit, test_data, type = "prob")

# augment() does the same thing as predict but adds the predictions onto the
# original data set.
flights_aug <- augment(flights_fit, test_data)

# I actually don;t know what this plot is supposed to do
flights_aug |>
    roc_curve(truth = arr_delay, .pred_late) |>
    autoplot()

# roc_auth produces a metric for how accurate our predictions were; about 76%
# accuracy
flights_aug |>
    roc_auc(truth = arr_delay, .pred_late)
