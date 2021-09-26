### Model Complexity Toy Example
## Use: Generate data for the toy example in the paper
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(xgboost)
library(texreg)
library(lmtest)

## Set n and seed
n <- 40000
set.seed(1704)

## Simulate data for toy example
age <- rnorm(n, 0, 7.5) + 70
age[age <= 50] <- runif(length(age[age <= 50]), 50, 51)
age[age >= 90] <- runif(length(age[age >= 90]), 89, 90)
high_school <- ifelse((age - 20) / 100 + runif(n, 0, 0.2) >
    runif(n, 0, 1), 1, 0)
gender <- ifelse((runif(n, 0, 1) + (age / 1000)) > 0.5, 1, 0)
has_gchildren <- ifelse(runif(n, 0, 0.08) + (age / 180) > 0.5, 1, 0)
error <- rnorm(n, 0, 0.3)

fixed_part <- round(10 * (ifelse(age > 50, (age - 50) * 0.025, 0.2) +
    ifelse(age > 65, (age - 65) * 0.05, 0) -
    ifelse(age > 75, (age - 75) * 0.04, 0) -
    ifelse(age > 85, (age - 85) * 0.03, 0) + 0.05 * has_gchildren +
    0.05 * high_school -
    0.1 * gender + error))

y <- ifelse(fixed_part < 0, 0, fixed_part)

df <- data.frame(
    fall = y, age = age, age2 = age^2, med_use = high_school,
    sex = gender, assist = has_gchildren
) %>%
    mutate(
        age_50p = ifelse(age > 50, age - 50, 0),
        age_65p = ifelse(age > 65, age - 65, 0),
        age_75p = ifelse(age > 75, age - 75, 0),
        age_85p = ifelse(age > 85, age - 85, 0)
    )

## Save datasets for other scripts
save(df, file = "toy_example/data/edit/toy_data.rda")
write.csv(df, "toy_example/data/edit/toy_df.csv")

## Estimate three functional forms
lm1 <- lm(fall ~ 1 + age + assist + sex + med_use, data = df)

lm2 <- lm(fall ~ 1 + age + age2 + assist + sex + med_use, data = df)

lm3 <- lm(fall ~ 1 + age_50p + age_65p + age_75p + age_85p + assist + sex +
    med_use, data = df)

## Evaluate LR-likelihood test
lrtest(lm1, lm2)
lrtest(lm2, lm3)

## Output regression results
texreg(list(summary(lm1), summary(lm2), summary(lm3)),
    file = "tex/tables/results_toy.tex"
)
screenreg(list(summary(lm1), summary(lm2), summary(lm3)))

## Generate dataset including the implied age effect from the linear models
df <- df %>%
    mutate(
        true_effect = 10 * (age_50p * 0.025 + age_65p * 0.05 -
            age_75p * 0.04 - age_85p * 0.03),
        lm1_implied = -27.64 + 0.51 * age,
        lm2_implied = -5.03 - 0.158 * age + 0.0048 * age2,
        lm3_implied = 1.22 + 0.17 * age_50p + 0.57 * age_65p -
            0.38 * age_75p - 0.32 * age_85p
    )

df_effects <- df %>%
    sample_frac(0.1) %>%
    dplyr::select(age, true_effect, lm1_implied, lm2_implied, lm3_implied) %>%
    reshape2::melt(id.vars = c("age")) %>%
    mutate(` ` = ifelse(grepl("true", variable), "True effect", NA))

saveRDS(df_effects, "toy_example/data/edit/df_effects.rds")
