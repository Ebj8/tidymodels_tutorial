library(tidymodels)

# Helper packages
library(readr) # for importing data
library(vip) # for variable importance plots

hotels <- read_csv("https://tidymodels.org/start/case-study/hotels.csv") |>
    mutate(across(where(is.character), as.factor))

set.seed(123)
splits <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test <- testing(splits)

folds <- vfold_cv(hotel_other)

hotels_rec <- recipe(children ~ ., data = hotel_other) |>
    step_date(arrival_date, features = c("month", "dow")) |>
    step_holiday(arrival_date,
        holidays = timeDate::listHolidays("US"),
        keep_original_cols = FALSE
    ) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors())

lr_mod <- logistic_reg(
    penalty = tune()
) |>
    set_engine("glmnet")

lr_grid <- grid_regular(
    penalty(),
    levels = 30
)

hotel_wflow <- workflow() |>
    add_model(lr_mod) |>
    add_recipe(hotels_rec)

hotel_fit_rs <- hotel_wflow %>%
    tune_grid(
        resamples = folds,
        grid = lr_grid,
        control = control_grid(save_pred = TRUE),
        metrics = metric_set(roc_auc)
    )

collect_metrics(hotel_fit_rs)

best_model <- hotel_fit_rs |>
    select_best("roc_auc")

final_wf <- hotel_wflow |>
    finalize_workflow(best_model)

final_fit <- final_wf |>
    last_fit(splits)

final_flow <- extract_workflow(final_fit)

hotel_aug <- augment(final_flow, hotel_test) |>
    mutate(accurate = if_else(children == .pred_class, 1, 0))
