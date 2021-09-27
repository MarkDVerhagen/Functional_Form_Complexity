# Model Complexity ideology example
# Use: Generate data for the ideology example in the paper
# Author: Mark Verhagen

# Load libraries
library(tidyverse)
library(haven)

haven::read_dta("ideology/data/raw/GSS7218_R3.DTA") %>%
  saveRDS("ideology/data/raw/GSS7218_R3.Rds")

data_raw <- readRDS("ideology/data/raw/GSS7218_R3.rds")

set.seed(1704)
data <- data_raw %>%
  filter(
    !is.na(wordsum),
    !is.na(polviews),
    !is.na(partyid)
  ) %>%
  filter(partyid != 7) %>%
  mutate(id = 1:n())


## Split into train and test set
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

## Save train and test sets
write.csv(X_train, "ideology/data/edit/x_train.csv")
write.csv(X_test, "ideology/data/edit/x_test.csv")
write.csv(y_train, "ideology/data/edit/y_train.csv")
write.csv(y_test, "ideology/data/edit/y_test.csv")

save(data, train_set, train_set_lm, X_train, X_test, y_train, y_test,
  file = "ideology/data/edit/lm_data.rda"
)

# ## Check model improvements
# train_set_lm <- train_set %>%
#   mutate(
#     age2 = age^2, income2 = income^2, educ2 = educ^2,
#     age3 = age^3, income3 = income^3, educ3 = educ^3
#   )

# lm1 <- lm(partyid ~ 1 + age + educ + as.factor(year) + income + sex + race,
#   data = train_set,
#   weights = wtss
# )

# lm2 <- lm(partyid ~ 1 + age + age2 + as.factor(year) + income + income2 + sex + race + educ + educ2,
#   data = train_set_lm, weights = wtss
# )

# lm2b <- lm(partyid ~ 1 + age + age2 + as.factor(year) + income + income2 + sex + race + educ + educ2,
#   data = train_set_lm, weights = wtss
# )

# lm3 <- lm(partyid ~ 1 + age * race + age2 * race + as.factor(year) + income + income2 + sex + race + educ + educ2,
#   data = train_set_lm, weights = wtss
# )


# lm4 <- lm(partyid ~ 1 + age + age2 + as.factor(year) + income + income2 + sex + race + educ + educ2 +
# sex * race,
#   data = train_set_lm, weights = wtss
# )

# lm5 <- lm(partyid ~ 1 + age * race + age2 * race + as.factor(year) + income + income2 + sex + race + educ + educ2 +
# sex * race,
#   data = train_set_lm, weights = wtss
# )

# texreg::screenreg(list(lm1, lm2, lm3, lm4, lm5))
# summary(lm5)
# lrtest(lm1, lm2)
# lrtest(lm2, lm2b)
# lrtest(lm2, lm4)
# lrtest(lm2, lm5)

# rf1 <- randomForest::randomForest(x = X_train, y = y_train)

# xg1 <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 200)

# ## Check superlearner performance

# tune2 = list(
#   max_depth = 1:6,
#   shrinkage = c(0.001, 0.01, 0.1, 0.3)
# )

# learners2 = create.Learner("SL.xgboost", tune = tune2, detailed_names = TRUE, name_prefix = "xgb")

# cv_sl2 = CV.SuperLearner(
#   Y = y_train, X = X_train,
#   cvControl = list(V = 10),
#   SL.library = c(
#     "SL.mean", "SL.glm", learners2$names,
#     "SL.ranger", "SL.xgboost"
#   )
# )

# saveRDS(cv_sl2, "cv_sl.rds")
# cv_sl2 <- readRDS("cv_sl.rds")
# summary(cv_sl2)

# cluster = parallel::makeCluster(5)

# # Load package to clusters
# parallel::clusterEvalQ(cluster, library(SuperLearner))

# # Set seeds across clusters
# parallel::clusterSetRNGStream(cluster, 1704)
# head(X_train)

# cv_sl3 = CV.SuperLearner(
#   Y = y_train$x, X = X_train,
#   cvControl = list(V = 10),
#   SL.library = c(
#     "SL.mean", "SL.glm",
#     "SL.ranger", "SL.xgboost"
#   )
# )

# saveRDS(cv_sl3, "cv_sl3.rds")
# cv_sl3 <- readRDS("cv_sl3.rds")
# summary(cv_sl3)

# X_train_dummy <- X_train %>%
#   fastDummies::dummy_cols(select_columns = c("sex", "race", "year"), remove_selected_columns = T)

# cv_sl4 = CV.SuperLearner(
#   Y = y_train$x, X = X_train_dummy,
#   cvControl = list(V = 10),
#   SL.library = c(
#     "SL.mean", "SL.glm",
#     "SL.ranger", "SL.xgboost"
#   )
# )

# saveRDS(cv_sl4, "cv_sl4.rds")
# cv_sl4 <- readRDS("cv_sl4.rds")

# ## Include observation weights, year, factors


# cv_sl5 = CV.SuperLearner(
#   Y = y_train$x, X = X_train,
#   obsWeights = train_weights,
#   cvControl = list(V = 10),
#   SL.library = c(
#     "SL.mean", "SL.glm",
#     "SL.ranger", "SL.xgboost"
#   )
# )

# saveRDS(cv_sl5, "cv_sl5.rds")
# cv_sl5 <- readRDS("cv_sl5.rds")

# cv_sl6 = CV.SuperLearner(
#   Y = c(y_train$x, y_test$x), X = rbind(X_train, X_test),
#   obsWeights = c(train_weights, test_weights),
#   cvControl = list(V = 10),
#   SL.library = c(
#     "SL.mean", "SL.glm",
#     "SL.ranger", "SL.xgboost"
#   )
# )

# saveRDS(cv_sl6, "cv_sl6.rds")
# cv_sl6 <- readRDS("cv_sl6.rds")

# X_train_sq <- X_train %>%
#   mutate(age2 = age^2, income2 = income^2, educ2 = educ^2)
# X_test_sq <- X_test %>%
#   mutate(age2 = age^2, income2 = income^2, educ2 = educ^2)

# cv_sl7 = CV.SuperLearner(
#   Y = c(y_train$x, y_test$x), X = rbind(X_train_sq, X_test_sq),
#   obsWeights = c(train_weights, test_weights),
#   cvControl = list(V = 10),
#   SL.library = c(
#     "SL.mean", "SL.glm",
#     "SL.ranger", "SL.xgboost"
#   )
# )

# saveRDS(cv_sl7, "cv_sl7.rds")
# cv_sl7 <- readRDS("cv_sl7.rds")

# summary(cv_sl2)


# xg_predict <- predict(xg1, as.matrix(X_test))
# lm_predict <- predict(lm1, X_test)
# rf_predict <- predict(rf1, X_test)

# xg_is_predict <- predict(xg1, as.matrix(X_train))
# lm_is_predict <- predict(lm1, X_train)
# rf_is_predict <- predict(rf1, X_train)

# caret::RMSE(as.numeric(y_test), xg_predict, na.rm = T)
# caret::RMSE(as.numeric(y_test), lm_predict, na.rm = T)
# caret::RMSE(as.numeric(y_test), rf_predict, na.rm = T)

# caret::RMSE(as.numeric(y_train), xg_is_predict, na.rm = T)
# caret::RMSE(as.numeric(y_train), lm_is_predict, na.rm = T)
# caret::RMSE(as.numeric(y_train), rf_is_predict, na.rm = T)

# train_data <- xgb.DMatrix(
#   data = as.matrix(X_train),
#   label = y_train
# )

# test_data <- xgb.DMatrix(
#   data = as.matrix(X_test),
#   label = y_test
# )

# nround    <- 50 # number of XGBoost rounds
# xg_model <- xgb.train(data = train_data,
#                       nrounds = nround)


# fit_xgb <- function(train_set, test_set, y_var = "ln_y_I",
#                     features = c("S", "X")) {
#   train_outcome <- train_set[[y_var]]
#   train_data <- xgb.DMatrix(
#     data = as.matrix(train_set %>% dplyr::select(all_of(features))),
#     label = train_outcome
#   )
#   test_outcome <- test_set[[y_var]]
#   test_data <- xgb.DMatrix(
#     data = as.matrix(test_set %>% dplyr::select(all_of(features))),
#     label = test_outcome
#   )
#   nround    <- 50 # number of XGBoost rounds
#   xg_model <- xgb.train(data = train_data,
#                         nrounds = nround)
#   return(caret::RMSE(test_set[[y_var]], predict(xg_model, test_data)))
# }