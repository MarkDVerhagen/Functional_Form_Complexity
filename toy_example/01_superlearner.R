library(tidyverse)
library(xgboost)
library(SuperLearner)
library(parallel)
library(xtable)

load("toy_example/data/edit/toy_data.rda")

## -- Linear superlearners

## Generate SuperLearner for linear model with linear age
tune <- list(
    ntrees = c(100, 200, 500),
    max_depth = 1:6,
    shrinkage = c(0.001, 0.01, 0.1)
)

learners <- create.Learner("SL.xgboost",
    tune = tune,
    detailed_names = TRUE, name_prefix = "xgb"
)

x_train <- df %>%
    select(age, med_use, sex, assist)

y_train <- df %>%
    pull(fall)

cluster <- parallel::makeCluster(6)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners$names)
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl_lm1 <- CV.SuperLearner(
        Y = y_train, X = x_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})
parallel::stopCluster(cluster)

saveRDS(cv_sl_lm1, "toy_example/data/final/cv_sl_lm1.rds")

## Generate SuperLearner for linear model with quadratic age
x_train <- df %>%
    select(age, age2, med_use, sex, assist)

y_train <- df %>%
    pull(fall)

cluster <- parallel::makeCluster(6)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners$names)
parallel::clusterSetRNGStream(cluster, 1704)


system.time({
    cv_sl_lm2 <- CV.SuperLearner(
        Y = y_train, X = x_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})
parallel::stopCluster(cluster)

saveRDS(cv_sl_lm2, "toy_example/data/final/cv_sl_lm2.rds")

## Generate SuperLearner for correctly specified linear model

x_train <- df %>%
    select(age_50p, age_65p, age_75p, age_85p, med_use, sex, assist)
y_train <- df %>%
    pull(fall)

cluster <- parallel::makeCluster(6)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners$names)
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl_lm3 <- CV.SuperLearner(
        Y = y_train, X = x_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})
parallel::stopCluster(cluster)

saveRDS(cv_sl_lm3, "toy_example/data/final/cv_sl_lm3.rds")

## Flexible superlearners

## Generate train sets (feature and outcome)
x_train <- df %>%
    select(age, med_use, sex, assist)

y_train <- df %>%
    pull(fall)

## GB grid
tune_gb <- list(
    ntrees = c(100, 200, 500),
    max_depth = 1:7,
    shrinkage = c(0.001, 0.01, 0.1)
)

learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

## Setup clusters
cluster <- parallel::makeCluster(6)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners_gb$names)
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl1 <- CV.SuperLearner(
        Y = y_train, X = x_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm", learners_gb$names)
    )
})

parallel::stopCluster(cluster)

saveRDS(cv_sl1, "toy_example/data/final/cv_sl1.rds")

## Vanilla RF and GB

cluster <- parallel::makeCluster(6)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners$names)
parallel::clusterSetRNGStream(cluster, 1704)

x_train <- df %>%
    select(age, med_use, sex, assist)

y_train <- df %>%
    pull(fall)

system.time({
    cv_sl2 <- CV.SuperLearner(
        Y = y_train, X = x_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm", "SL.xgboost", "SL.ranger")
    )
})
parallel::stopCluster(cluster)

saveRDS(cv_sl2, "toy_example/data/final/cv_sl2.rds")

## RF grid
mtry_seq <- floor(sqrt(ncol(x_train)) * c(0.5, 1, 2))
ntree_seq <- c(100, 200, 500)

learners_rf <- create.Learner("SL.ranger", tune = list(
    num.trees = ntree_seq,
    mtry = mtry_seq
))

x_train <- df %>%
    select(age, med_use, sex, assist)

y_train <- df %>%
    pull(fall)

cluster <- parallel::makeCluster(6)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners_rf$names)
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl3 <- CV.SuperLearner(
        Y = y_train, X = x_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm", "SL.xgboost", learners_rf$names)
    )
})

parallel::stopCluster(cluster)

saveRDS(cv_sl3, "toy_example/data/final/cv_sl3.rds")

## Combine all output into single SuperLearner table and transform to RMSE
cv_sl_lm1 <- readRDS("toy_example/data/final/cv_sl_lm1.rds")
cv_sl_lm2 <- readRDS("toy_example/data/final/cv_sl_lm2.rds")
cv_sl_lm3 <- readRDS("toy_example/data/final/cv_sl_lm3.rds")

cv_sl1 <- readRDS("toy_example/data/final/cv_sl1.rds")
cv_sl2 <- readRDS("toy_example/data/final/cv_sl2.rds")
cv_sl3 <- readRDS("toy_example/data/final/cv_sl3.rds")

comb_sl <- rbind(
    summary(cv_sl1)$Table %>%
        filter(!grepl("Super|Discrete|mean|ranger", Algorithm)),
    summary(cv_sl2)$Table %>%
        filter(!grepl("Discrete|mean|ranger|glm|xgboost", Algorithm)),
    summary(cv_sl3)$Table %>%
        filter(!grepl("Super|Discrete|mean|glm|xgboost", Algorithm)),
    summary(cv_sl_lm1)$Table %>% filter(grepl("glm", Algorithm)),
    summary(cv_sl_lm2)$Table %>% filter(grepl("glm", Algorithm)),
    summary(cv_sl_lm3)$Table %>% filter(grepl("glm", Algorithm))
) %>%
    mutate(Ave = sqrt(Ave), se = se / Ave, Min = sqrt(Min), Max = sqrt(Max)) %>%
    mutate(
        Algorithm = gsub("SL.ranger", "SL.RF", Algorithm),
        Algorithm = gsub("xgb", "SL.GB", Algorithm),
        Algorithm = gsub("_All", "", Algorithm),
        Algorithm = gsub("RF_1", "RF_100_1", Algorithm),
        Algorithm = gsub("RF_2", "RF_200_1", Algorithm),
        Algorithm = gsub("RF_3", "RF_500_1", Algorithm),
        Algorithm = gsub("RF_4", "RF_100_2", Algorithm),
        Algorithm = gsub("RF_5", "RF_200_2", Algorithm),
        Algorithm = gsub("RF_6", "RF_500_2", Algorithm),
        Algorithm = gsub("RF_7", "RF_100_4", Algorithm),
        Algorithm = gsub("RF_8", "RF_200_4", Algorithm),
        Algorithm = gsub("RF_9", "RF_500_4", Algorithm),
    )

print(xtable(comb_sl[order(comb_sl$Ave), ],
    type = "latex",
    digits = c(0, 0, 3, 3, 3, 3)
), file = "tex/tables/SL_toy.tex")
