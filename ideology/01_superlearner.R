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