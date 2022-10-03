### Model Complexity housing example
## Use: Plot results for housing example
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(texreg)
library(cowplot)
library(ggsci)
library(lmtest)

## Load plot style
source("./styles.R")

## Descriptive table
labs <- data.frame(
  outcome = "Log House Price",
  tfarea = "House size (square meter)",
  numberrooms = "Number of rooms",
  travel_to_centre = "Distance to local centre (minutes)",
  crime_disorder = "Crime and disorder index",
  imd = "Deprivation index",
  propertytype_D = "Property type: Detached",
  propertytype_S = "Property type: Semi-Detached",
  propertytype_T = "Property type: Terraced",
  propertytype_F = "Property type: Flats/Maisonettes",
  oldnew_Y = "New property"
)

## Read Data

df_test <- read.csv("housing/data/edit/X_test_temp.csv") %>%
  select(-X)
df_test$outcome <- read.csv("housing/data/edit/y_test_temp.csv") %>%
  select(-X) %>%
  pull(outcome)

df_train <- read.csv("housing/data/edit/X_train_temp.csv") %>%
  select(-X)
df_train$outcome <- read.csv("housing/data/edit/y_train_temp.csv") %>%
  select(-X) %>%
  pull(outcome)

df_total <- rbind(df_train, df_test)

df_total %>%
  dplyr::select(
    outcome, tfarea, numberrooms, travel_to_centre, crime_disorder,
    imd, starts_with("property"), oldnew_Y
  ) %>%
  vtable::sumtable(
    labels = labs, out = "latex",
    file = "tex/tables/summary_tab.tex"
  )

## Plot OOS performance of models

df_oos <- readRDS("housing/data/edit/simul_oos_1_100.rds")

df_oos_melt <- df_oos %>%
  reshape2::melt() %>%
  group_by(variable) %>%
  summarise(mean = mean(value), sd = sd(value)) %>%
  ungroup() %>%
  mutate(
    model = ifelse(grepl("lm", variable), "Linear", "Flexible"),
    outcome_type = ifelse(grepl("r2", variable), "R-Squared", "RMSE"),
    vars = ifelse(grepl("lm0_|gb0_", variable), "Area",
      ifelse(grepl("lm1_|gb2_", variable), "Area + Temp",
        ifelse(grepl("lm2|gb1_", variable), "Temp",
          ifelse(grepl("lm3|gb3", variable), "Area + Temp + Housing",
            ifelse(grepl("lm4|gb4", variable),
              "Area + Temp + Housing +\nTransaction",
              ifelse(grepl("lm5|gb5", variable),
                "Area + Temp + Housing +\nTransaction + Deprivation",
                ifelse(grepl("lm6|gb6", variable),
                  "Area + Temp + Housing +\nTransaction + Crime",
                  ifelse(grepl("lm7|gb7", variable),
                    "Area + Temp + Housing +\nTransaction + Travel time",
                    ifelse(grepl("lm8|gb8", variable),
                      "Area + Temp + Housing +\nTransaction + Crime +\nTravel time",
                      ifelse(grepl("lm9|gb9", variable),
                        "Area + Temp + Housing +\nTransaction + IMD + Travel time",
                        ifelse(grepl("lm10|gb10", variable),
                          "Area + Temp + Housing +\nTransaction + IMD + Crime",
                          ifelse(grepl("lm11|gb11", variable),
                            "Area + Temp + Housing +\nTransaction + IMD + Crime +\nTravel time", NA
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

ggplot(
  df_oos_melt %>% filter(outcome_type == "R-Squared"),
  aes(x = mean, y = reorder(vars, mean), fill = model)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = paste0(round(mean, 3) * 100, "%")),
    hjust = -0.4, size = 6,
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(aes(xmin = mean - 2 * sd, xmax = mean + 2 * sd),
    position = position_dodge(width = 0.9), width = 0.2
  ) +
  ggsci::scale_fill_aaas(name = "Model", labels = c("GB", "Hypothesized")) +
    xlim(0, 1) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 20, ver = T) +
    theme(axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 24),
          axis.title.x = element_text(size = 24)) +
  ylab("Explanatory variables") +
  xlab("Out-of-sample R-squared")

ggsave("tex/figs/fig1_housing.pdf", last_plot(), width = 13, height = 9)

## Shapley plots

df_shap <- read.csv("housing/data/edit/housing_shap_temp.csv") %>%
  select(-X)

df_train <- read.csv("housing/data/edit/X_train_temp.csv") %>%
  select(-X)
df_train$outcome <- read.csv("housing/data/edit/y_train_temp.csv") %>%
  select(-X) %>%
  pull(outcome)

lr <- lm(outcome ~ ., data = df_train)

## Add squares
df_train_v2 <- df_train %>%
  mutate(
    imd2 = imd^2,
    travel_to_centre2 = travel_to_centre^2,
    tfarea2 = tfarea^2,
    crime_disorder2 = crime_disorder^2,
    numberrooms2 = numberrooms^2,
    year2 = year^2,
    month2 = month^2
  )

## Add cubes
df_train_v3 <- df_train %>%
  mutate(
    imd2 = imd^2,
    travel_to_centre2 = travel_to_centre^2,
    tfarea2 = tfarea^2,
    crime_disorder2 = crime_disorder^2,
    numberrooms2 = numberrooms^2,
    imd3 = imd^3,
    travel_to_centre3 = travel_to_centre^3,
    tfarea3 = tfarea^3,
    crime_disorder3 = crime_disorder^3,
    numberrooms3 = numberrooms^3
  )

df_train_v4 <- df_train %>%
  mutate(
    tfarea2 = tfarea^2,
    # numerrooms = as.factor(numberrooms),
    # travel_to_centre_0_10 = ifelse(travel_to_centre <= 10, travel_to_centre, 0),
    # travel_to_centre_10_20 = ifelse((travel_to_centre > 10) & (travel_to_centre <= 20), travel_to_centre, 0),
    # travel_to_centre_20_plus = ifelse((travel_to_centre > 20), travel_to_centre, 0)
  )

## Add stepwise
df_train_v5 <- df_train %>%
  mutate(
    tfarea2 = tfarea^2,
    # numerrooms = as.factor(numberrooms),
    travel_to_centre_0_5 = ifelse(travel_to_centre <= 5, travel_to_centre, 0),
    travel_to_centre_5_10 = ifelse((travel_to_centre > 5) & (travel_to_centre <= 10), travel_to_centre, 0),
    travel_to_centre_10_15 = ifelse((travel_to_centre > 10) & (travel_to_centre <= 15), travel_to_centre, 0),
    travel_to_centre_15_20 = ifelse((travel_to_centre > 15) & (travel_to_centre <= 20), travel_to_centre, 0),
    travel_to_centre_20_plus = ifelse((travel_to_centre > 20), travel_to_centre, 0)
  ) %>%
  dplyr::select(-travel_to_centre)

lr1 <- lm(outcome ~ ., data = df_train)
lr2 <- lm(outcome ~ ., data = df_train_v2)
lr3 <- lm(outcome ~ ., data = df_train_v3)
lr4 <- lm(outcome ~ ., data = df_train_v4)
lr5 <- lm(outcome ~ ., data = df_train_v5)

## Stepwise travel to centre

lr2_travel <- lm(outcome ~ 1 + tfarea + tfarea2 + travel_to_centre, data = df_train_v2)
lr3_travel <- lm(outcome ~ 1 + tfarea + tfarea2 + travel_to_centre + travel_to_centre2, data = df_train_v3)
lr4_travel <- lm(outcome ~ 1 + tfarea + tfarea2 + travel_to_centre_0_5 + travel_to_centre_5_10 +
  travel_to_centre_10_15 + travel_to_centre_15_20 + travel_to_centre_20_plus, data = df_train_v5)
lr5_travel <- lm(outcome ~ 1 + tfarea + tfarea2 + travel_to_centre,
  data = df_train_v2 %>% mutate(travel_to_centre = as.factor(round(travel_to_centre)))
)

## Squared tfarea
lr1_area <- lm(outcome ~ ., data = df_train)
lr2_area <- lm(outcome ~ ., data = df_train %>% mutate(tfarea2 = tfarea^2))
lr3_area <- lm(outcome ~ ., data = df_train %>% mutate(tfarea2 = tfarea^2, tfarea3 = tfarea^3))

## Square crime
lr1_crime <- lm(outcome ~ ., data = df_train)
lr2_crime <- lm(outcome ~ ., data = df_train %>% mutate(crime_disorder2 = crime_disorder^2))
lr3_crime <- lm(outcome ~ ., data = df_train %>% mutate(crime_disorder2 = crime_disorder^2, crime_disorder3 = crime_disorder^3))

## Square crime
lr1_year <- lm(outcome ~ ., data = df_train)
lr2_year <- lm(outcome ~ ., data = df_train %>% mutate(year2 = year^2))
lr3_year <- lm(outcome ~ ., data = df_train %>% mutate(year = as.factor(year)))

lr_coef <- as.data.frame(summary(lr)$coefficients)
names(lr_coef) <- c("estimate", "std.error", "t.value", "p.value")

lr_coef$var <- rownames(lr_coef)
rownames(lr_coef) <- NULL

lr_coef2 <- as.data.frame(summary(lr2)$coefficients)
names(lr_coef2) <- c("estimate", "std.error", "t.value", "p.value")

lr_coef2$var <- rownames(lr_coef2)
rownames(lr_coef2) <- NULL

predict_linear <- function(df, total_df, coefficient = "X", scale = F) {
  if (scale) {
    df[, coefficient] <- scale(df[, coefficient])
  }

  coef <- total_df$estimate[total_df$var == coefficient]
  return(coef * df[, coefficient])
}

predict_quadratic <- function(df, total_df, coefficient = "X", scale = F) {
  coefficient2 <- paste0(coefficient, "2")

  if (scale) {
    df[, coefficient] <- scale(df[, coefficient])
  }

  coef <- total_df$estimate[total_df$var == coefficient]
  coef2 <- total_df$estimate[total_df$var == coefficient2]
  return(coef * df[, coefficient] + coef2 * df[, coefficient2])
}

make_df <- function(df_test, df_shap, lr_coef, lr_coef2, coefficient = "X", var = "X") {
  return(data.frame(
    predicted = scale(predict_linear(df_test, lr_coef, coefficient, scale = F)),
    predicted2 = scale(predict_quadratic(df_test, lr_coef2, coefficient, scale = F)),
    shapley = scale(df_shap[, paste0(coefficient, "_shap")]),
    data = df_shap[, coefficient]
  ) %>%
    reshape2::melt(id.vars = c("data")) %>%
    mutate(var = var))
}


## Add squares to test data
df_test <- df_test %>%
  mutate(
    imd2 = imd^2,
    travel_to_centre2 = travel_to_centre^2,
    tfarea2 = tfarea^2,
    crime_disorder2 = crime_disorder^2,
    numberrooms2 = numberrooms^2,
    year2 = year^2,
    month2 = month^2,
  )

tfarea <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "tfarea",
  var = "House size (square meter)"
)

travel <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "travel_to_centre",
  var = "Travel time to hub (minutes)"
)

crime <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "crime_disorder",
  var = "Crime index"
)

imd <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "imd",
  var = "Deprivation index"
)

rooms <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "numberrooms",
  var = "Number of rooms"
)

year <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "year",
  var = "Year of transaction"
)

month <- make_df(df_test, df_shap, lr_coef, lr_coef2,
  coefficient = "month",
  var = "Month of transaction"
)

total_df <- rbind(tfarea, travel, crime, rooms, year, month)
total_df <- total_df %>%
  mutate(var = gsub(" \\(square meter\\)| \\(minutes\\)", "", var))
shap_df <- total_df %>% filter(grepl("shap", variable))

shap_df <- shap_df %>%
  filter(!((var == "Travel time to hub") & (data > 40))) %>%
  filter(!((var == "House size") & (data > 500))) %>%
  filter(!((var == "Number of rooms") & (data > 12)))
total_df <- total_df %>%
  filter(!((var == "Travel time to hub") & (data > 40))) %>%
  filter(!((var == "House size") & (data > 500))) %>%
  filter(!((var == "Number of rooms") & (data > 12)))

set.seed(1704)

shap_plot <- ggplot(shap_df, aes(y = value, x = data)) +
  geom_jitter(
    data = shap_df %>% sample_frac(0.05) %>% filter(variable == "shapley"),
    aes(color = "Shapley"), alpha = 0.1, size = 3
  ) +
  geom_smooth(
    data = total_df %>% filter(variable == "shapley"),
    formula = y ~ splines::bs(x, knots = 9),
    color = "darkblue", se = FALSE, size = 0.4
  ) +
  geom_line(
    data = total_df %>% filter(variable == "predicted"),
    aes(y = value, x = data, linetype = "Linear"), color = "black", size = 0.6
  ) +
  geom_line(
    data = total_df %>% filter(variable == "predicted2"),
    aes(y = value, x = data, linetype = "Quadratic"), color = "black", size = 0.6
  ) +
  facet_wrap(~var, scales = "free") +
  ylab("Implied effect on log house price") +
  scale_colour_manual(
    name = "Model Type",
    values = c("Shapley" = ggsci::pal_aaas("default")(2)[1])
  ) +
  scale_linetype_manual(name = "", values = c("Linear" = "dashed", "Quadratic" = "dotted")) +
  theme_cowplot() +
  custom_theme(text_size = 20, hor = T) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(labels = round)

shap_plot + plot_annotation(tag_levels = "A")

ggsave("tex/figs/fig2_housing.pdf", shap_plot, width = 14, height = 9)
