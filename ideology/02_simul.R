### Model Complexity ideology example
## Use: Evaluate OOS performance
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(xgboost)

data_raw <- readRDS("data/raw/GSS7218_R3.rds")

data <- data_raw %>%
    filter(
        !is.na(wordsum),
        !is.na(polviews),
        !is.na(partyid)
    ) %>%
    filter(partyid != 7) %>%
    mutate(id = 1:n())

oos_r2 <- function(y_hat, y) {
    return(1 - sum((y - y_hat)^2) / sum((y - mean(y))^2))
}

n_simul <- 1000

results_df <- data.frame()

for (i in 1:n_simul) {
    set.seed(i)
    train_ids <- data %>%
        sample_frac(0.8) %>%
        pull(id)

    train_set <- data %>%
        filter(id %in% train_ids) %>%
        mutate(partyid = as.numeric(partyid))

    test_set <- data %>%
        filter(!(id %in% train_ids)) %>%
        mutate(partyid = as.numeric(partyid))

    train_weights <- train_set$wtss
    test_weights <- test_set$wtss

    y_train <- train_set %>%
        pull(partyid)

    y_test <- test_set %>%
        pull(partyid)

    X_train <- train_set %>%
        select(age, educ, income, sex, race, year)
    X_test <- test_set %>%
        select(age, educ, income, sex, race, year)

    train_weights <- train_weights[complete.cases(X_train)]
    test_weights <- test_weights[complete.cases(X_test)]

    y_train <- y_train[complete.cases(X_train)]
    X_train <- X_train[complete.cases(X_train), ]

    y_test <- y_test[complete.cases(X_test)]
    X_test <- X_test[complete.cases(X_test), ]

    ## Fit linear models
    ## Include squared versions of age, income and education in lin data only
    train_set_lm <- X_train
    train_set_lm$partyid <- y_train

    test_set_lm <- X_test
    test_set_lm$partyid <- y_test

    lm0 <- lm(partyid ~ 1 + year,
        data = train_set_lm, weights = train_weights
    )

    lm1 <- lm(partyid ~ 1 + year + age,
        data = train_set_lm, weights = train_weights
    )

    lm2 <- lm(partyid ~ 1 + year + age + sex,
        data = train_set_lm, weights = train_weights
    )

    lm3 <- lm(partyid ~ 1 + year + age + race,
        data = train_set_lm, weights = train_weights
    )

    lm4 <- lm(partyid ~ 1 + year + age + sex + race,
        data = train_set_lm, weights = train_weights
    )

    lm5 <- lm(partyid ~ 1 + year + age + sex + race + income,
        data = train_set_lm, weights = train_weights
    )

    lm6 <- lm(partyid ~ 1 + year + age + sex + race + educ,
        data = train_set_lm, weights = train_weights
    )

    lm7 <- lm(partyid ~ 1 + year + age + sex + race + income + educ,
        data = train_set_lm, weights = train_weights
    )

    ## Fit optimal XG model
    xg0 <- xgboost(
        data = as.matrix(X_train %>% select(year)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg1 <- xgboost(
        data = as.matrix(X_train %>% select(year, age)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg2 <- xgboost(
        data = as.matrix(X_train %>% select(year, age, sex)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg3 <- xgboost(
        data = as.matrix(X_train %>% select(year, age, race)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg4 <- xgboost(
        data = as.matrix(X_train %>% select(year, age, sex, race)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg5 <- xgboost(
        data = as.matrix(X_train %>% select(year, age, sex, race, income)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg6 <- xgboost(
        data = as.matrix(X_train %>% select(year, age, sex, race, educ)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    xg7 <- xgboost(
        data = as.matrix(X_train %>%
            select(year, age, sex, race, income, educ)),
        label = y_train, nrounds = 75,
        max_depth = 3, shrinkage = 0.15
    )

    linear_models <- list(lm0, lm1, lm2, lm3, lm4, lm5, lm6, lm7)
    gb_models <- list(xg0, xg1, xg2, xg3, xg4, xg5, xg6, xg7)
    results_lin <- lapply(linear_models, FUN = function(x) {
        predict(x, test_set_lm)
    })

    results_gb <- list(
        predict(xg0, X_test %>% select(year) %>% as.matrix()),
        predict(xg1, X_test %>% select(year, age) %>% as.matrix()),
        predict(xg2, X_test %>% select(year, age, sex) %>% as.matrix()),
        predict(xg3, X_test %>% select(year, age, race) %>% as.matrix()),
        predict(xg4, X_test %>% select(year, age, sex, race) %>% as.matrix()),
        predict(xg5, X_test %>% select(year, age, sex, race, income) %>%
            as.matrix()),
        predict(xg6, X_test %>% select(year, age, sex, race, educ) %>%
            as.matrix()),
        predict(xg7, X_test %>% select(year, age, sex, race, income, educ) %>%
            as.matrix())
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

    temp_df <- c(lin_rmse, lin_r2, gb_rmse, gb_r2) %>%
        as.data.frame()
    names(temp_df) <- c(
        "lm0_rmse", "lm1_rmse", "lm2_rmse", "lm3_rmse",
        "lm4_rmse", "lm5_rmse", "lm6_rmse", "lm7_rmse",
        "lm0_r2", "lm1_r2", "lm2_r2", "lm3_r2", "lm4_r2",
        "lm5_r2", "lm6_r2", "lm7_r2",
        "gb0_rmse", "gb1_rmse", "gb2_rmse", "gb3_rmse",
        "gb4_rmse", "gb5_rmse", "gb6_rmse", "gb7_rmse",
        "gb0_r2", "gb1_r2", "gb2_r2", "gb3_r2", "gb4_r2",
        "gb5_r2", "gb6_r2", "gb7_r2"
    )

    results_df <- rbind(
        results_df, temp_df
    )
}

saveRDS(results_df, "data/edit/simul_oos.rds")