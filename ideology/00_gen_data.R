# Model Complexity ideology example
# Use: Generate data for the ideology example in the paper
# Author: Mark Verhagen

# Load libraries
library(tidyverse)
library(haven)

haven::read_dta("ideology/data/raw/GSS7218_R3.DTA") %>%
  saveRDS("ideology/data/raw/GSS7218_R3.Rds")

data_raw <- readRDS("ideology/data/raw/GSS7218_R3.Rds")

set.seed(1704)
data <- data_raw %>%
  filter(
    !is.na(wordsum),
    !is.na(polviews),
    !is.na(partyid),
    !is.na(age),
    !is.na(income),
    !is.na(race),
    !is.na(educ),
    !is.na(sex),
    !is.na(year),
  ) %>%
  filter(partyid != 7) %>%
  mutate(id = 1:n())


print_mean_and_sd <- function(data, var = "age") {
  print(mean(data[[var]]))
  print(sd(data[[var]]))
  print(summary(data[[var]]))
}

print_mean_and_sd(data, "year")
mean(data$race == 3)
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