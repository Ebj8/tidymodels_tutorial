library(tidymodels) # for the rsample package, along with the rest of tidymodels

# Helper packages
library(modeldata) # for the cells data

data(cells, package = "modeldata")
cells

set.seed(123)

cell_split <- initial_split(cells |> select(-case), strata = class)

cell_train <- training(cell_split)
cell_test <- testing(cell_split)

# rand_forest is a decision tree model using 1000 trees
rf_mod <- rand_forest(trees = 1000) |>
    set_engine("ranger") |>
    set_mode("classification")

set.seed(234)
rf_fit <- rf_mod |>
    fit(class ~ ., data = cell_train)

rf_training_pred <-
    predict(rf_fit, cell_train) %>%
    bind_cols(predict(rf_fit, cell_train, type = "prob")) %>%
    # Add the true outcome data back in
    bind_cols(cell_train %>%
        select(class))

rf_training_pred %>% # training set predictions
    roc_auc(truth = class, .pred_PS)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary          1.00
rf_training_pred %>% # training set predictions
    accuracy(truth = class, .pred_class)

rf_testing_pred <-
    predict(rf_fit, cell_test) %>%
    bind_cols(predict(rf_fit, cell_test, type = "prob")) %>%
    bind_cols(cell_test %>% select(class))

rf_testing_pred %>% # test set predictions
    roc_auc(truth = class, .pred_PS)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.891
rf_testing_pred %>% # test set predictions
    accuracy(truth = class, .pred_class)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy binary         0.816

set.seed(345)
folds <- vfold_cv(cell_train, v = 10)

rf_wflow <- workflow() |>
    add_model(rf_mod) |>
    add_formula(class ~ .)

set.seed(456)
# This resamples on the formula and model from our workflow on the 10 folds we
# created earlier.
rf_fit_rs <- rf_wflow |>
    fit_resamples(folds)

# This function pulls out the nested metrics data to produce final results.
collect_metrics(rf_fit_rs)
