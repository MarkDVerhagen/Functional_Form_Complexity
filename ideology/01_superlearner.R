# Model Complexity ideology example
# Use: Estimate SuperLearner output
# Author: Mark Verhagen

# Load libraries
library(tidyverse)
library(SuperLearner)
library(xtable)

X_train <- read.csv("ideology/data/edit/x_train.csv") %>% select(-X)
X_test <- read.csv("ideology/data/edit/x_test.csv") %>% select(-X)
y_train <- read.csv("ideology/data/edit/y_train.csv") %>% select(-X)
y_test <- read.csv("ideology/data/edit/y_test.csv") %>% select(-X)

## Grid 1

tune_gb <- list(
    ntrees = c(100, 200, 500),
    max_depth = 1:6,
    shrinkage = c(0.01, 0.1, 0.2)
)

learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

tune_rf <- list(
    mtry_seq = floor(sqrt(ncol(X_train)) * c(1, 2)),
    ntree_seq = c(100, 200, 500)
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
    cv_sl1 <- CV.SuperLearner(
        Y = y_train$x, X = X_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm",
            learners_gb$names, learners_rf$names
        )
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_sl1, "cv_sl1.rds")

## Grid 2

tune_gb <- list(
    ntrees = c(50, 75, 100, 600),
    max_depth = 3:5,
    shrinkage = c(0.05, 0.1, 0.15)
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
    cv_sl2 <- CV.SuperLearner(
        Y = y_train$x, X = X_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm",
            learners_gb$names
        )
    )
})

parallel::stopCluster(cluster)
saveRDS(cv_sl2, "cv_sl2.rds")


total_df <- rbind(
    summary(cv_sl1)$Table,
    summary(cv_sl2)$Table
) %>%
    as.data.frame() %>%
    mutate(Ave = sqrt(Ave), se = se / Ave, Min = sqrt(Min), Max = sqrt(Max))

print(xtable(total_df[order(total_df$Ave, decreasing = F), ], type = "latex"),
    file = "tex/tables/SL_GSS.tex"
)

## Estimate SuperLearner to various covariate sets
set_1 <- c("year")
set_2 <- c("year", "age")
set_3 <- c("year", "age", "sex")
set_4 <- c("year", "age", "race")
set_5 <- c("year", "age", "sex", "race")
set_6 <- c("year", "age", "sex", "race", "income")
set_7 <- c("year", "age", "sex", "race", "educ")
set_8 <- c("year", "age", "sex", "race", "income", "educ")

set_list <- list(
    set_1, set_2, set_3, set_4, set_5, set_6, set_7, set_8
)

tune_gb <- list(
    ntrees = c(50, 75, 100, 200, 500, 600),
    max_depth = 2:6,
    shrinkage = c(0.01, 0.05, 0.1, 0.15, 0.2)
)

learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

tune_rf <- list(
    mtry = floor(sqrt(ncol(X_train)) * c(1, 2)),
    ntree = c(100, 200, 500)
)

learners_rf <- create.Learner("SL.randomForest",
    tune = tune_rf,
    detailed_names = TRUE, name_prefix = "SL.randomForest"
)

## Full SuperLearner
cluster <- parallel::makeCluster(5)

# Load package to clusters
parallel::clusterEvalQ(cluster, library(SuperLearner))

# Export learner functions
parallel::clusterExport(cluster, c(learners_gb$names))

# Set seeds across clusters
parallel::clusterSetRNGStream(cluster, 1704)

cv_results_list <- lapply(1:8, function(x) {
    print(x)
    print(Sys.time())
    set.seed(1704)
    CV.SuperLearner(
        Y = as.numeric(y_train$x), X =
            X_train_list[[x]],
        cvControl = list(V = 3),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm", learners_gb$names, learners_rf$names)
    ) %>% return()
})

parallel::stopCluster(cluster)
saveRDS(cv_results_list, "./cv_gss_subsets.rds")