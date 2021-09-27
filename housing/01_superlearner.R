### Model Complexity housing example
## Use: Generate superlearner output for the housing example
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(SuperLearner)
library(xtable)

## Load data
X_train <- read.csv("housing/data/edit/X_train_full.csv") %>%
    dplyr::select(-X)
X_test <- read.csv("housing/data/edit/X_test_full.csv") %>%
    dplyr::select(-X)

y_train <- read.csv("housing/data/edit/y_train_full.csv") %>%
    dplyr::select(-X)
y_test <- read.csv("housing/data/edit/y_test_full.csv") %>%
    dplyr::select(-X)

## Generate grid 1

tune <- list(
    ntrees = c(200, 500),
    max_depth = 1:3,
    shrinkage = c(0.001, 0.01)
)

learners <- create.Learner("SL.xgboost",
    tune = tune, detailed_names = TRUE,
    name_prefix = "xgb"
)

cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners$names)
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl1 <- CV.SuperLearner(
        Y = y_train$outcome, X = X_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm", learners$names, "SL.ranger")
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_sl1, "cv_sl1.rds")

## Generate grid 2
tune2 <- list(
    max_depth = 4:6,
    shrinkage = c(0.1, 0.3)
)

learners2 <- create.Learner("SL.xgboost",
    tune = tune2, detailed_names = TRUE,
    name_prefix = "xgb"
)

cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, learners2$names)
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl2 <- CV.SuperLearner(
        Y = y_train$outcome, X = X_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm", learners2$names,
            "SL.ranger", "SL.xgboost"
        )
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_sl2, "cv_sl2.rds")

## -- Full Learner Routine

tune_gb <- list(
    ntrees = c(250, 500, 750),
    max_depth = 4:8,
    shrinkage = c(0.1, 0.3, 0.5)
)

learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

tune_rf <- list(
    mtry_seq = floor(sqrt(ncol(X_train)) * c(1, 2)),
    ntree_seq = c(200, 500, 1000)
)

learners_rf <- create.Learner("SL.ranger",
    tune = tune_rf,
    detailed_names = TRUE, name_prefix = "SL.RF"
)

cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, c(learners_gb$names, learners_rf$names))
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl3 <- CV.SuperLearner(
        Y = y_train$outcome, X = X_train,
        cvControl = list(V = 5),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm", learners_gb$names, learners_rf$names
        )
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_sl3, "cv_sl3.rds")


## -- Extra GB Routine
tune_gb <- list(
    ntrees = c(600, 800, 1000),
    max_depth = 5:7,
    shrinkage = c(0.3, 0.4, 0.5)
)

learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, c(learners_gb$names))
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl4 <- CV.SuperLearner(
        Y = y_train$outcome, X = X_train,
        cvControl = list(V = 5),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm", learners_gb$names
        )
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_sl4, "cv_sl4.rds")

total_df <- rbind(summary(cv_sl2)$Table, summary(cv_sl3)$Table) %>%
    filter(!grepl("_4_|_5_|_600_|_800_|_0.1|Super|Discrete", Algorithm)) %>%
    filter(!duplicated(Algorithm)) %>%
    mutate(Ave = sqrt(Ave), se = se / Ave, Min = sqrt(Min), Max = sqrt(Max))

print(xtable(total_df, type = "latex"), file = "../tex/tables/SL_housing.tex")

## Including year and month

X_train <- read.csv("src/X_train_temp.csv") %>%
    dplyr::select(-X)

X_test <- read.csv("src/X_test_temp.csv") %>%
    dplyr::select(-X)

y_train <- read.csv("src/y_train_temp.csv") %>%
    dplyr::select(-X)

y_test <- read.csv("src/y_test_temp.csv") %>%
    dplyr::select(-X)

full_train <- X_train
full_train$outcome <- y_train

full_train <- full_train %>%
    sample_n(250000)

## -- Extra GB ROutine on 0.25 million rows

tune_gb <- list(
    ntrees = c(1500, 1750),
    max_depth = 7:8,
    shrinkage = c(0.3, 0.4)
)

learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, c(learners_gb$names))
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_temp <- CV.SuperLearner(
        Y = as.numeric(unlist(full_train$outcome)),
        X = full_train %>% select(-outcome),
        cvControl = list(V = 3),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm", learners_gb$names
        )
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_temp, "cv_temp.rds")

full_cv_extra <- cv_temp %>%
    mutate(Ave = sqrt(Ave), se = se / Ave, Min = sqrt(Min), Max = sqrt(Max))

full_cv_extra <- full_cv_extra[order(full_cv_extra$Ave), ]

print(xtable(full_cv_extra,
    type = "latex",
    digits = c(0, 0, 3, 3, 3, 3)
), file = "../tex/tables/SL_housing_temp.tex")