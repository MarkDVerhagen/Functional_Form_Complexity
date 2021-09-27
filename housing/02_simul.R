### Model Complexity housing example
## Use: Evaluate OOS performance for the housing example
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(doParallel)
library(xgboost)

X <- rbind(
  read.csv("housing/data/edit/X_train_temp.csv"),
  read.csv("housing/data/edit/X_test_temp.csv")
) %>%
  select(-X)

y <- rbind(
  read.csv("housing/data/edit/y_train_temp.csv"),
  read.csv("housing/data/edit/y_test_temp.csv")
) %>%
  select(-X) %>%
  pull(outcome)

data <- X
data$outcome <- y
data$id <- 1:dim(data)[1]

oos_r2 <- function(y_hat, y) {
  return(1 - sum((y - y_hat)^2) / sum((y - mean(y))^2))
}

n_simul <- 100

cores <- detectCores()
cl <- makeCluster(2)
registerDoParallel(cl)

results <- foreach(
  i = 1:n_simul, .combine = rbind,
  .packages = c("xgboost", "tidyverse")
) %dopar% {
  set.seed(i)
  # make train set
  train_ids <- data %>%
    sample_frac(0.8) %>%
    pull(id)

  train_set <- data %>%
    filter(id %in% train_ids)

  test_set <- data %>%
    filter(!(id %in% train_ids))

  y_train <- train_set %>%
    pull(outcome)
  y_test <- test_set %>%
    pull(outcome)

  area <- c("tfarea", "numberrooms")

  temp <- c("year", "month")
  housing_features <- names(data)[grepl("propertytype|BUILT_|oldnew", names(data))]
  transaction_features <- names(data)[grepl("TRANSACT", names(data))]
  imd_feature <- c("imd")
  travel_feature <- c("travel_to_centre")
  crime_feature <- c("crime_disorder")

  ## Split data into train- and test-set

  lm_train_full <- train_set %>%
    select(-id)
  lm_test_full <- test_set %>%
    select(-id)

  X_train_full <- lm_train_full %>%
    select(-outcome)
  X_test_full <- lm_test_full %>%
    select(-outcome)

  y_train <- lm_train_full %>%
    pull(outcome)
  y_test <- lm_test_full %>%
    pull(outcome)

  ## Fit linear models
  ## Include squared versions of age, income and education in lin data only

  lm0 <- lm(outcome ~ .,
    data = lm_train_full %>% select(outcome, all_of(area))
  )

  lm1 <- lm(outcome ~ .,
    data = lm_train_full %>% select(outcome, all_of(area), all_of(temp))
  )

  lm2 <- lm(outcome ~ .,
    data = lm_train_full %>% select(outcome, all_of(temp))
  )

  lm3 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area),
      all_of(temp), all_of(housing_features)
    )
  )

  lm4 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features)
    )
  )

  lm5 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      imd_feature
    )
  )

  lm6 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature
    )
  )

  lm7 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature
    )
  )

  lm8 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature, crime_feature
    )
  )

  lm9 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature, imd_feature
    )
  )

  lm10 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature, imd_feature
    )
  )

  lm11 <- lm(outcome ~ .,
    data = lm_train_full %>% select(
      outcome, all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature, imd_feature, travel_feature
    )
  )

  ## Fit optimal XG model
  xg0 <- xgboost(
    data = as.matrix(X_train_full %>% select(all_of(area))),
    label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg1 <- xgboost(
    data = as.matrix(X_train_full %>% select(all_of(temp))),
    label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg2 <- xgboost(
    data = as.matrix(X_train_full %>% select(all_of(area), all_of(temp))),
    label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg3 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features)
    )),
    label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg4 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features)
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg5 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      imd_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg6 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg7 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg8 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature, crime_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg9 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature, imd_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg10 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature, imd_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  xg11 <- xgboost(
    data = as.matrix(X_train_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature, imd_feature, travel_feature
    )), label = y_train, nrounds = 1250,
    max_depth = 7, shrinkage = 0.3, verbose = 0
  )

  linear_models <- list(
    lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8,
    lm9, lm10, lm11
  )
  gb_models <- list(
    xg0, xg1, xg2, xg3, xg4, xg5,
    xg6, xg7, xg8, xg9, xg10, xg11
  )
  results_lin <- lapply(linear_models, FUN = function(x) {
    predict(x, lm_test_full)
  })

  results_gb <- list(
    predict(xg0, X_test_full %>% select(all_of(area)) %>%
      as.matrix()),
    predict(xg1, X_test_full %>% select(all_of(temp)) %>%
      as.matrix()),
    predict(xg2, X_test_full %>% select(
      all_of(area),
      all_of(temp)
    ) %>% as.matrix()),
    predict(xg3, X_test_full %>% select(
      all_of(area),
      all_of(temp), all_of(housing_features)
    ) %>% as.matrix()),
    predict(xg4, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features)
    ) %>% as.matrix()),
    predict(xg5, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      imd_feature
    ) %>% as.matrix()),
    predict(xg6, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature
    ) %>% as.matrix()),
    predict(xg7, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature
    ) %>% as.matrix()),
    predict(xg8, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature, crime_feature
    ) %>% as.matrix()),
    predict(xg9, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      travel_feature, imd_feature
    ) %>% as.matrix()),
    predict(xg10, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature, imd_feature
    ) %>% as.matrix()),
    predict(xg11, X_test_full %>% select(
      all_of(area), all_of(temp),
      all_of(housing_features), all_of(transaction_features),
      crime_feature, imd_feature, travel_feature
    ) %>% as.matrix())
  )

  lin_rmse <- lapply(results_lin, FUN = function(x) {
    caret::RMSE(as.numeric(x), y_test)
  })

  lin_r2 <- lapply(results_lin, FUN = function(x) {
    oos_r2(as.numeric(x), y_test)
  })

  gb_rmse <- lapply(results_gb, FUN = function(x) {
    caret::RMSE(as.numeric(x), y_test)
  })

  gb_r2 <- lapply(results_gb, FUN = function(x) {
    oos_r2(as.numeric(x), y_test)
  })

  list(lin_rmse, lin_r2, gb_rmse, gb_r2)
}

saveRDS(results, "housing/data/edit/oos_results.rds")

results_df <- data.frame()

first <- T
for (j in 1:100) {
  temp_df <- c(
    unlist(results[, 1][j]), unlist(results[, 2][j]),
    unlist(results[, 3][j]), unlist(results[, 4][j])
  )
  names(temp_df) <- c(
    "lm0_rmse", "lm1_rmse", "lm2_rmse", "lm3_rmse",
    "lm4_rmse", "lm5_rmse", "lm6_rmse", "lm7_rmse",
    "lm8_rmse", "lm9_rmse", "lm10_rmse", "lm11_rmse",
    "lm0_r2", "lm1_r2", "lm2_r2", "lm3_r2", "lm4_r2",
    "lm5_r2", "lm6_r2", "lm7_r2",
    "lm8_r2", "lm9_r2", "lm10_r2", "lm11_r2",
    "gb0_rmse", "gb1_rmse", "gb2_rmse", "gb3_rmse",
    "gb4_rmse", "gb5_rmse", "gb6_rmse", "gb7_rmse",
    "gb8_rmse", "gb9_rmse", "gb10_rmse", "gb11_rmse",
    "gb0_r2", "gb1_r2", "gb2_r2", "gb3_r2", "gb4_r2",
    "gb5_r2", "gb6_r2", "gb7_r2", "gb8_r2", "gb9_r2", "gb10_r2",
    "gb11_r2"
  )
  if (first) {
    results_df <- temp_df
    first <- F
  } else {
    results_df <- rbind(results_df, temp_df)
  }
}

saveRDS(results_df, "housing/data/edit/simul_oos_1_100.rds")