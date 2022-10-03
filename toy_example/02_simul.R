### Model Complexity Toy Example
## Use: Evaluate OOS performance of flexible and hypothesized models
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(xgboost)
library(Metrics)

## Load data
load("toy_example/data/edit/toy_data.rda")

## Set number of simulations
n_simul <- 1000

## Generate empty vectors to store OOS results
gb_rmse_c <- lm1_rmse_c <- lm2_rmse_c <- lm3_rmse_c <- c()
gb_r2_c <- lm1_r2_c <- lm2_r2_c <- lm3_r2_c <- c()

## Generate empty vectors to store differences in OOS results
gb_lm1_r2 <- rf_lm1_r2 <- lm2_lm1_r2 <- lm3_lm1_r2 <- c()
gb_lm2_r2 <- rf_lm2_r2 <- lm3_lm2_r2 <- c()
gb_lm3_r2 <- rf_lm3_r2 <- c()

## Generate empty vectors to store differences in OOS results
gb_lm1_rmse <- rf_lm1_rmse <- lm2_lm1_rmse <- lm3_lm1_rmse <- c()
gb_lm2_rmse <- rf_lm2_rmse <- lm3_lm2_rmse <- c()
gb_lm3_rmse <- rf_lm3_rmse <- c()

lm1_beta <- lm2_beta <- lm3_beta <- c()

oos_r2 <- function(model, X, y) {
    y_pred <- predict(model, X)
    return(1 - sum((y - y_pred)^2) / sum((y - mean(y))^2))
}

for (i in 1:n_simul) {
    set.seed(i)
    df_train <- df %>%
        mutate(id = 1:n()) %>%
        sample_frac(0.8)

    df_test <- df %>%
        mutate(id = 1:n()) %>%
        filter(!(id %in% df_train$id))

    train_outcome <- df_train$fall
    test_outcome <- df_test$fall

    train_set <- df_train %>%
        select(-id, -fall) %>%
        select(age, assist, sex, med_use)
    test_set <- df_test %>%
        select(-id, -fall) %>%
        select(age, assist, sex, med_use)

    x_train <- as.matrix(train_set)
    x_test <- as.matrix(test_set)

    train_data <- xgb.DMatrix(data = x_train, label = train_outcome)
    test_data <- xgb.DMatrix(data = x_test, label = test_outcome)

    xg_model <- xgb.train(
        data = train_data, nrounds = 200,
        max_depth = 1, eta = 0.1
    )

    lm1_train <- lm(fall ~ 1 + age + assist
        + sex + med_use, data = df_train)
    lm2_train <- lm(fall ~ 1 + age + age2 + assist
        + sex + med_use, data = df_train)
    lm3_train <- lm(fall ~ 1 + age_50p + age_65p + age_75p + age_85p + assist
        + sex + med_use, data = df_train)

    gb_rmse <- Metrics::rmse(predict(xg_model, x_test), test_outcome)
    lm1_rmse <- Metrics::rmse(predict(lm1_train, df_test), test_outcome)
    lm2_rmse <- Metrics::rmse(predict(lm2_train, df_test), test_outcome)
    lm3_rmse <- Metrics::rmse(predict(lm3_train, df_test), test_outcome)

    gb_r2 <- oos_r2(xg_model, x_test, test_outcome)
    lm1_r2 <- oos_r2(lm1_train, df_test, test_outcome)
    lm2_r2 <- oos_r2(lm2_train, df_test, test_outcome)
    lm3_r2 <- oos_r2(lm3_train, df_test, test_outcome)

    gb_rmse_c <- c(gb_rmse_c, gb_rmse)
    lm1_rmse_c <- c(lm1_rmse_c, lm1_rmse)
    lm2_rmse_c <- c(lm2_rmse_c, lm2_rmse)
    lm3_rmse_c <- c(lm3_rmse_c, lm3_rmse)

    gb_r2_c <- c(gb_r2_c, gb_r2)
    lm1_r2_c <- c(lm1_r2_c, lm1_r2)
    lm2_r2_c <- c(lm2_r2_c, lm2_r2)
    lm3_r2_c <- c(lm3_r2_c, lm3_r2)

    gb_lm1_r2 <- c(gb_lm1_r2, gb_r2 - lm1_r2)
    gb_lm2_r2 <- c(gb_lm2_r2, gb_r2 - lm2_r2)
    gb_lm3_r2 <- c(gb_lm3_r2, gb_r2 - lm3_r2)

    lm2_lm1_r2 <- c(lm2_lm1_r2, lm2_r2 - lm1_r2)
    lm3_lm1_r2 <- c(lm3_lm1_r2, lm3_r2 - lm1_r2)
    lm3_lm2_r2 <- c(lm3_lm2_r2, lm3_r2 - lm2_r2)

    gb_lm1_rmse <- c(gb_lm1_rmse, gb_rmse - lm1_rmse)
    gb_lm2_rmse <- c(gb_lm2_rmse, gb_rmse - lm2_rmse)
    gb_lm3_rmse <- c(gb_lm3_rmse, gb_rmse - lm3_rmse)

    lm2_lm1_rmse <- c(lm2_lm1_rmse, lm2_rmse - lm1_rmse)
    lm3_lm1_rmse <- c(lm3_lm1_rmse, lm3_rmse - lm1_rmse)
    lm3_lm2_rmse <- c(lm3_lm2_rmse, lm3_rmse - lm2_rmse)
    
    lm1_beta <- c(lm1_beta, lm1_train$coefficients["assist"])
    lm2_beta <- c(lm2_beta, lm2_train$coefficients["assist"])
    lm3_beta <- c(lm3_beta, lm3_train$coefficients["assist"])
}

beta_df <- data.frame(
    "lm1_beta" = lm1_beta,
    "lm2_beta" = lm2_beta,
    "lm3_beta" = lm3_beta
)

write.csv(beta_df, "toy_example/data/edit/beta_toy.csv")

simul_df <- data.frame(
    "gb_rmse" = gb_rmse_c, "gb_r2" = gb_r2_c,
    "lm1_rmse" = lm1_rmse_c, "lm1_r2" = lm1_r2_c,
    "lm2_rmse" = lm2_rmse_c, "lm2_r2" = lm2_r2_c,
    "lm3_rmse" = lm3_rmse_c, "lm3_r2" = lm3_r2_c,
    "gb_lm1_r2" = gb_lm1_r2,
    "gb_lm2_r2" = gb_lm2_r2,
    "gb_lm3_r2" = gb_lm3_r2,
    "lm2_lm1_r2" = lm2_lm1_r2,
    "lm3_lm1_r2" = lm3_lm1_r2,
    "lm3_lm2_r2" = lm3_lm2_r2,
    "gb_lm1_rmse" = gb_lm1_rmse,
    "gb_lm2_rmse" = gb_lm2_rmse,
    "gb_lm3_rmse" = gb_lm3_rmse,
    "lm2_lm1_rmse" = lm2_lm1_rmse,
    "lm3_lm1_rmse" = lm3_lm1_rmse,
    "lm3_lm2_rmse" = lm3_lm2_rmse
)

write.csv(simul_df, "toy_example/data/edit/simul_toy.csv")
