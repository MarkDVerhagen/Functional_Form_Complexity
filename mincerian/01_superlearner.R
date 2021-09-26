### Model Complexity Mincerian wage example
## Use: Generate data for the Mincerian wage example in the paper
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(SuperLearner)
library(xtable)

simul_df <- read.csv("data/edit/analysis_df.csv") %>%
    dplyr::select(-X.1)

## Split in train and test sets
train_df <- simul_df %>%
    sample_frac(0.7)

test_df <- simul_df %>%
    filter(!(id %in% train_df$id))

## Setup grids
tune_gb <- list(
    ntrees = c(100, 200, 500),
    max_depth = 1:5,
    shrinkage = c(0.01, 0.1, 0.2, 0.3)
)
learners_gb <- create.Learner("SL.xgboost",
    tune = tune_gb,
    detailed_names = TRUE, name_prefix = "SL.GB"
)

tune_rf <- list(
    mtry_seq = floor(sqrt(ncol(x_train)) * c(1, 2)),
    ntree_seq = c(100, 200, 500)
)
learners_rf <- create.Learner("SL.ranger",
    tune = tune_rf,
    detailed_names = TRUE, name_prefix = "SL.RF"
)

## Make specific train sets including specific features
x_train <- train_df %>%
    select(S, X, gender)
x1_train <- train_df %>%
    select(S, X)
x2_train <- train_df %>%
    select(S, X, X_sq)
x3_train <- train_df %>%
    select(
        S, X, X_sq, S_0_8,
        S_9_10, S_11_12, S_13_14,
        S_15p
    )
x4_train <- train_df %>%
    select(
        S, X, X_sq, S_0_8,
        S_9_10, S_11_12, S_13_14,
        S_15p, gender
    )

## Extract relevant outcomes
y1_train <- train_df %>%
    pull(ln_y_I)
y2_train <- train_df %>%
    pull(ln_y_II)
y3_train <- train_df %>%
    pull(ln_y_III)
y4_train <- train_df %>%
    pull(ln_y_IV)

## Setup clusters
cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, c(learners_gb$names, learners_rf$names))
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl1 <- CV.SuperLearner(
        Y = y1_train, X = x1_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm",
            learners_gb$names, learners_rf$names
        )
    )
})

system.time({
    cv_sl2 <- CV.SuperLearner(
        Y = y2_train, X = x2_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm",
            learners_gb$names, learners_rf$names
        )
    )
})

system.time({
    cv_sl3 <- CV.SuperLearner(
        Y = y3_train, X = x3_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm",
            learners_gb$names, learners_rf$names
        )
    )
})

system.time({
    cv_sl4 <- CV.SuperLearner(
        Y = y4_train, X = x4_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c(
            "SL.mean", "SL.glm",
            learners_gb$names, learners_rf$names
        )
    )
})
parallel::stopCluster(cluster)

saveRDS(cv_sl1, "cv_y1_rf_gb.rds")
saveRDS(cv_sl2, "cv_y2_rf_gb.rds")
saveRDS(cv_sl3, "cv_y3_rf_gb.rds")
saveRDS(cv_sl4, "cv_y4_rf_gb.rds")

## Estimate linear performances

cluster <- parallel::makeCluster(5)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, c(learners_gb$names, learners_rf$names))
parallel::clusterSetRNGStream(cluster, 1704)

system.time({
    cv_sl1_lm <- CV.SuperLearner(
        Y = y1_train, X = x1_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})

system.time({
    cv_sl2_lm <- CV.SuperLearner(
        Y = y2_train, X = x2_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})

system.time({
    cv_sl3_lm <- CV.SuperLearner(
        Y = y3_train, X = x3_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})

system.time({
    cv_sl4_lm <- CV.SuperLearner(
        Y = y4_train, X = x4_train,
        cvControl = list(V = 10),
        parallel = cluster,
        SL.library = c("SL.mean", "SL.glm")
    )
})
parallel::stopCluster(cluster)

saveRDS(cv_sl1_lm, "cv_y1_lm.rds")
saveRDS(cv_sl2_lm, "cv_y2_lm.rds")
saveRDS(cv_sl3_lm, "cv_y3_lm.rds")
saveRDS(cv_sl4_lm, "cv_y4_lm.rds")

## Combine outputs and transform to RMSE
cv_total <- rbind(
    summary(cv_sl1_lm)$Table %>% filter(grepl("SL.glm", Algorithm)),
    summary(cv_sl1)$Table %>% filter(grepl("GB|RF", Algorithm)),
    summary(cv_sl2_lm)$Table %>% filter(grepl("SL.glm", Algorithm)),
    summary(cv_sl2)$Table %>% filter(grepl("GB|RF", Algorithm)),
    summary(cv_sl3_lm)$Table %>% filter(grepl("SL.glm", Algorithm)),
    summary(cv_sl3)$Table %>% filter(grepl("GB|RF", Algorithm)),
    summary(cv_sl4_lm)$Table %>% filter(grepl("SL.glm", Algorithm)),
    summary(cv_sl4)$Table %>% filter(grepl("GB|RF", Algorithm))
) %>%
    mutate(Ave = sqrt(Ave), se = se / Ave, Min = sqrt(Min), Max = sqrt(Max))

print(xtable(cv_total, type = "latex"),
    file = "mincerian/tex/tables/SL_mincerian.tex"
)