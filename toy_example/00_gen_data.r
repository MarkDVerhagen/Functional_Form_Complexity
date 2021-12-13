### Model Complexity Toy Example
## Use: Generate data for the toy example in the paper
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(texreg)
library(lmtest)

## Set n and seed
n <- 40000
set.seed(1704)


n <- 40000
set.seed(1704)

## Simulate data for toy example

age <- rnorm(n, 0, 7.5) + 70
age[age <= 50] <- runif(length(age[age <= 50]), 50, 51)
age[age >= 90] <- runif(length(age[age >= 90]), 89, 90)
medication_use <- ifelse((age - 20) / 100 + runif(n, 0, 0.2) >
    runif(n, 0, 1), 1, 0)
gender <- ifelse((runif(n, 0, 1) + (age / 1000)) > 0.5, 1, 0)
assistance <- ifelse(runif(n, 0, 0.08) + (age / 180) > 0.5, 1, 0)
error <- rnorm(n, 0, 0.3)

fixed_part <- (10 * (ifelse(age >= 50, (age - 50) * 0.025, -0.8) +
    ifelse(age > 65, (age - 65) * 0.05, 0) -
    ifelse(age > 75, (age - 75) * 0.04, 0) -
    ifelse(age > 85, (age - 85) * 0.025, 0) + 0.05 * assistance +
    0.05 * medication_use -
    0.1 * gender + error)) + 10

y <- ifelse(fixed_part < 0, 0, fixed_part)

df <- data.frame(
    fall = y, age = age, age2 = age^2, med_use = medication_use,
    sex = gender, assist = assistance
) %>%
    mutate(
        age_50p = ifelse(age >= 50, age - 50, 0),
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
lm1_df <- summary(lm1)$coefficients %>% as.data.frame()
lm2_df <- summary(lm2)$coefficients %>% as.data.frame()
lm3_df <- summary(lm3)$coefficients %>% as.data.frame()

df <- df %>%
    mutate(
        true_effect = 10 * (age_50p * 0.025 + age_65p * 0.05 -
            age_75p * 0.04 - age_85p * 0.025),
        lm1_implied = lm1_df$Estimate[1] + lm1_df$Estimate[2] * age - 10,
        lm2_implied = lm2_df$Estimate[1] + lm2_df$Estimate[2] * age + lm2_df$Estimate[3] * age2 - 10,
        lm3_implied = lm3_df$Estimate[1] + lm3_df$Estimate[2] * age_50p + lm3_df$Estimate[3] * age_65p +
            lm3_df$Estimate[4] * age_75p + lm3_df$Estimate[5] * age_85p - 10
    )

df_effects <- df %>%
    sample_frac(0.1) %>%
    dplyr::select(age, true_effect, lm1_implied, lm2_implied, lm3_implied) %>%
    reshape2::melt(id.vars = c("age")) %>%
    mutate(` ` = ifelse(grepl("true", variable), "True effect", NA))

saveRDS(df_effects, "toy_example/data/edit/df_effects.rds")