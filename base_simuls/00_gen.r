### Generate data base simulations
## Use: Generate data for four functional forms
## Author: Mark Verhagen

library(tidyverse)
library(randomForest)
library(xgboost)

set.seed(1704)

## Set n and X variable
n <- 100
X <- runif(n, -4, 4)

## Generate y outcome based on four functional forms
y0 <- -1 + .9 * X
y1 <- -2 * ifelse(X < -3, 1, 0) + 2.55 * ifelse(X > -2, 1, 0) -
    2 * ifelse(X > 0, 1, 0) + 4 * ifelse(X > 2, 1, 0) - 1 * ifelse(X > 3, 1, 0)
y2 <- 6 + 0.4 * X - 0.36 * X^2 + 0.005 * X^3
y3 <- 2.83 * sin(pi / 2 * X)

## Generate white noise such that overall variance is fixed
var0 <- 5 - var(y0)
var1 <- 5 - var(y1)
var2 <- 5 - var(y2)
var3 <- 5 - var(y3)

epsilon0 <- rnorm(n, 0, sqrt(var0))
epsilon1 <- rnorm(n, 0, sqrt(var1))
epsilon2 <- rnorm(n, 0, sqrt(var2))
epsilon3 <- rnorm(n, 0, sqrt(var3))

## Generate outcome plus noise and store in dataframe
df <- data.frame(
    X = X,
    y_0 = y0 + epsilon0,
    y_1 = y1 + epsilon1,
    y_2 = y2 + epsilon2,
    y_3 = y3 + epsilon3
)

return_oos_r2 <- function(actual, predict) {
    return(1 - sum((actual - predict)^2) / sum((actual - mean(actual))^2))
}

## Generate ids for every row
df_analysis <- df %>%
    mutate(id = 1:n())

train_set <- df_analysis

obs_vars <- c("y_0", "y_1", "y_2", "y_3")

## Obtian outcome value per functional form for trainset
train_ys <- lapply(obs_vars, FUN = function(x) {
    train_set[, x]
})

## Obtain train and test version of feature set
X_train <- train_set %>%
    select(X)

## Estimate true linear effects
true_vars <- list(y0, y1, y2, y3)
lms <- lapply(true_vars, FUN = function(x) {
    lm(y ~ 1 + X, data = df_analysis %>% mutate(y = x))
})

## Estimate random forest models on train set
rfs <- lapply(train_ys, FUN = function(x) {
    randomForest(x = X_train, y = x)
})

## Estimate gradient boosting models
gbs <- lapply(train_ys, FUN = function(x) {
    xgboost(data = as.matrix(X_train), label = x, nrounds = 200)
})

## Plotting
pred_df <- data.frame(X = seq(-4, 4, 8 / 100))

## True association
pred_df$lm_0 <- predict(lms[[1]], pred_df)
pred_df$lm_1 <- predict(lms[[2]], pred_df)
pred_df$lm_2 <- predict(lms[[3]], pred_df)
pred_df$lm_3 <- predict(lms[[4]], pred_df)

## Implied association
pred_df$rf_0 <- predict(gbs[[1]], as.matrix(pred_df$X))
pred_df$rf_1 <- predict(rfs[[2]], as.matrix(pred_df$X))
pred_df$rf_2 <- predict(rfs[[3]], as.matrix(pred_df$X))
pred_df$rf_3 <- predict(rfs[[4]], as.matrix(pred_df$X))

pred_df$gb_0 <- predict(gbs[[1]], as.matrix(pred_df$X))
pred_df$gb_1 <- predict(gbs[[2]], as.matrix(pred_df$X))
pred_df$gb_2 <- predict(gbs[[3]], as.matrix(pred_df$X))
pred_df$gb_3 <- predict(gbs[[4]], as.matrix(pred_df$X))

melt_pred_df <- reshape2::melt(pred_df, id.vars = c("X")) %>%
    mutate(model = ifelse(grepl("rf", variable), "RF",
        ifelse(grepl("gb", variable), "GB",
            ifelse(grepl("lm", variable), "LM", NA)
        )
    ), data = gsub("rf_|gb_|lm_", "", variable))

melt_observed <- df %>%
    reshape2::melt(id.vars = ("X")) %>%
    mutate(model = "Simulated", data = gsub("y_", "", variable))

df_true <- data.frame(
    X = X, y_0 = y0, y_1 = y1,
    y_2 = y2, y_3 = y3
)

melt_true <- df_true %>%
    reshape2::melt(id.vars = ("X")) %>%
    mutate(model = "True", data = gsub("y_", "", variable))

total_melt <- rbind(melt_pred_df, melt_observed, melt_true) %>%
    mutate(data = ifelse(data == "0", "Linear",
        ifelse(data == "1", "Step",
            ifelse(data == "2", "Quadratic",
                ifelse(data == "3", "Sine", NA)
            )
        )
    ))

    
ggplot(total_melt, aes(y = value, x = X)) +
    geom_point(data = total_melt %>% filter(!(model %in% c("True", "LM", "GB", "RF"))), alpha = 0.3, aes(color = model)) +
    geom_line(data = total_melt %>% filter(model == "True"), aes(linetype = model)) +
    geom_line(data = total_melt %>% filter(model == "LM"), aes(linetype = model)) +
    facet_wrap(~data) +
    xlab("X") +
    ylab("Y") +
    scale_color_manual(
        name = "", values = c(ggsci::pal_aaas("default")(3)[3:1], ggsci::pal_aaas("default")(10)[10]),
        labels = c("Simulated data")
    ) +
    scale_linetype_manual(name = "", values = c("dashed", "solid"), labels = c("Linear Model", "True effect")) +
    cowplot::theme_cowplot() +
    theme(
        # panel.grid.minor.y = element_line(size = 0.25, linetype = "dotted"),
        # panel.grid.major.y = element_line(size = 0.25, linetype = "dotted"),
        strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
        strip.background = element_rect(fill = NA, color = "black", size = 1.5),
        legend.position = "top",
        panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
        text = element_text(size = 20)
    ) +
    guides(color = guide_legend(override.aes = list(fill = NA)))

ggsave("../tex/figs/fig1_base.pdf", last_plot(), width = 10, height = 8)

ggplot(total_melt, aes(y = value, x = X)) +
    geom_point(data = total_melt %>% filter(!(model %in% c("True", "LM"))), alpha = 0.3, aes(color = model)) +
    geom_line(data = total_melt %>% filter(model == "True"), aes(linetype = model)) +
    facet_wrap(~data) +
    xlab("X") +
    ylab("Y") +
    scale_color_manual(
        name = "", values = c(ggsci::pal_aaas("default")(3)[1:3], ggsci::pal_aaas("default")(10)[10]),
        labels = c("Gradient Boosting", "Random Forest", "Simulated data")
    ) +
    scale_linetype_manual(name = "", values = c("solid"), labels = c("True effect")) +
    cowplot::theme_cowplot() +
    theme(
        # panel.grid.minor.y = element_line(size = 0.25, linetype = "dotted"),
        # panel.grid.major.y = element_line(size = 0.25, linetype = "dotted"),
        strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
        strip.background = element_rect(fill = NA, color = "black", size = 1.5),
        legend.position = "top",
        panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
        text = element_text(size = 20)
    ) +
    guides(color = guide_legend(override.aes = list(fill = NA)))

ggsave("../tex/figs/fig1_base2.pdf", last_plot(), width = 10, height = 8)
