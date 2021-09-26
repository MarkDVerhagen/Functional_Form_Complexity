### Model Complexity Mincerian wage example
## Use: Generate data for the Mincerian wage example in the paper
## Author: Mark Verhagen

## Load libraries
library(foreign)
library(xgboost)
library(tidyverse)
library(caret)
library(randomForest)
library(doParallel)

source("mincerian/functions.r")

n_obs <- 50000
set.seed(1704)

## Read data from 2018 GSS
data <- read.dta("mincerian/data/raw/GSS2018.dta")

## Transform data
df <- data %>%
  filter(age >= 18,
         age <= 65,
         !is.na(educ)) %>%
  mutate(age_cat = ifelse(age < 25, "18_24",
      ifelse(age < 35, "25_34",
          ifelse(age < 50, "35_49", "50_65")
      )
  ))

## Generate a sample
educ_age_props <- count(df, educ, age_cat, wt = wtss) %>%
    mutate(
        prop = n / sum(n),
        cum_prop = cumsum(prop)
    )

## Sample actual ages
new_sample <- sample_n(educ_age_props, size = n_obs, weight = n, replace = T)

new_sample$age <- ifelse(new_sample$age_cat == "18_24",
    round(runif(dim(new_sample)[1], 18, 24)),
    ifelse(new_sample$age_cat == "25_34",
        round(runif(dim(new_sample)[1], 25, 34)),
        ifelse(new_sample$age_cat == "35_49",
            round(runif(dim(new_sample)[1], 35, 49)),
            round(runif(dim(new_sample)[1], 50, 65))
        )
    )
)

## Generate independent variables
analysis_df <- new_sample %>%
    mutate(
        S = as.numeric(educ),
        X = ifelse(educ >= 10, age - educ - 4, age - 16),
        X = ifelse(X < 0 , 0, X),
        X_sq = X^2
    ) %>%
  rowwise() %>%
  mutate(S_0_8 = min(S, 8),
         S_9_10 = min(2, max(S - 8, 0)),
         S_11_12 = min(2, max(S - 10, 0)),
         S_13_14 = min(2, max(S - 12, 0)),
         S_15p = max(S - 14, 0)) %>%
  ungroup()

## Randomly assign rows to male or female for gender illustration
analysis_df$gender <- ifelse(runif(dim(analysis_df)[1], 0, 1) > 0.5, 1, 0)

## Simulate log wage based on various linear additive models
simul_df <- analysis_df %>%
    mutate(
        epsilon = rnorm(dim(analysis_df)[1], 0, 0.12),
        ln_y_I = 4.5 + 0.125 * S + 0.09 * X +
            rnorm(dim(analysis_df)[1], 0, 0.12),
        ln_y_II = 4.5 + 0.125 * S + 0.09 * X - 0.001 * X^2  +
            rnorm(dim(analysis_df)[1], 0, 0.07),
        ln_y_III = 4.5 + 0.02 * S_0_8 + 0.03 * S_9_10 + 0.30 * S_11_12 +
            0.06 * S_13_14 + 0.06 * S_15p + 0.09 * X - 0.001 * X^2 +
            rnorm(dim(analysis_df)[1], 0, 0.07),
        ln_y_IV = ifelse(gender == 0,
            5.5 + 0.015 * S_0_8 + 0.02 * S_9_10 + 0.25 * S_11_12 +
            0.04 * S_13_14 + 0.04 * S_15p + 0.06 * X - 0.001 * X^2 +
            rnorm(dim(analysis_df)[1], 0, 0.09),
            3.5 + 0.025 * S_0_8 + 0.06 * S_9_10 + 0.35 * S_11_12 +
                0.06 * S_13_14 + 0.08 * S_15p + 0.1 * X - 0.0005 * X^2 +
                rnorm(dim(analysis_df)[1], 0, 0.08)
        )
    ) %>%
    mutate(id = 1 : dim(analysis_df))
