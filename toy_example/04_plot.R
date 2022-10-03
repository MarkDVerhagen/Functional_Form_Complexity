### Model Complexity Toy Example
## Use: Generate plots and tables for paper
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(vtable)

## Load plotting style
source("./styles.R")

## Load data
df_effects <- readRDS("toy_example/data/edit/df_effects.rds")
beta_df <- read_csv("toy_example/data/edit/beta_toy.csv")

## Pot implied effects of the age variable

ggplot(df_effects, aes(y = value, x = age, color = variable)) +
  geom_line(data = df_effects %>%
    filter(!grepl("true", variable)), size = 1.5) +
  geom_line(
    data = df_effects %>% filter(grepl("true", variable)) %>% mutate(linetype = "True"), aes(linetype = linetype), color = "black", size = 1
  ) +
  scale_linetype_manual(values = c("dashed")) +
  xlim(50, 85) +
  cowplot::theme_cowplot() +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_colour_manual(name = "",
                      labels = c("Model 1", "Model 2", "Model 3"),
                            values = MetBrewer::met.brewer("Egypt")[1:3]) +
  custom_theme(text_size = 20, hor = T) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  xlab("Age") +
  scale_y_continuous(name = "Implied age effect", limits = c(-3, 17))

ggsave("tex/figs/fig1_toy.pdf", last_plot(), width = 7, height = 7)

## Generate table with simulation results
simul_results <- read.csv("toy_example/data/edit/simul_toy.csv")

simul_results %>%
  dplyr::select(-X) %>%
  vtable::sumtable(
    file = "tex/tables/simul_toy.tex", out = "latex",
    summ = c(
      "notNA(x)", "mean(x)", "sd(x)", "min(x)",
      "pctile(x)[5]", "pctile(x)[95]", "max(x)"
    )
  )

## Plot shapley results
df <- read.csv("toy_example/data/edit/toy_shap.csv")

true_effects <- readRDS("toy_example/data/edit/effects.rds") %>%
  group_by(variable) %>%
  mutate(value = scale(value))
true_effects$` ` <- NULL

df_melt_X <- df %>%
  dplyr::select(
    age, age_shap
  ) %>%
  rename(value = age_shap) %>%
  mutate(
    variable = "Shapley",
    value = scale(value)
  )

comb_df <- rbind(df_melt_X, true_effects) %>%
  filter(variable %in%
    c("lm1_implied", "lm2_implied", "lm3_implied", "Shapley"))

comb_df$variable <- factor(comb_df$variable,
  levels = c("lm1_implied", "lm2_implied", "lm3_implied", "Shapley")
)

ggplot(comb_df, aes(y = value, x = age)) +
  geom_point(
    data = comb_df %>% filter(variable == "Shapley"), alpha = alpha,
    aes(color = variable), size = scatter_size
  ) +
  geom_line(
    data = comb_df %>% filter(variable == "lm3_implied"),
    aes(linetype = "True effect"), size = 1
  ) +
  xlab("Age") +
    ylab("Implied effect") +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 24, hor = T) +
  xlim(50, 90) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  scale_color_manual(
    name = "", labels = c("Shapley"),
    values = c(ggsci::pal_aaas("default")(2)[1])
  ) +
  scale_linetype_manual(
    name = " ", labels = c("Correct model"), values = c("dashed")
  )

ggsave("tex/figs/fig2_toy.pdf", last_plot(), width = 12, height = 9)

## Beta-hat distribution

beta_df_melt <- beta_df %>%
  reshape2::melt()

unique(beta_df_melt$variable)

beta_df_melt$variable <- case_when(
  beta_df_melt$variable == "lm1_beta" ~ "Linear",
  beta_df_melt$variable == "lm2_beta" ~ "Quadratic",
  beta_df_melt$variable == "lm3_beta" ~ "PieceLinear"
)

beta_df_melt <- beta_df_melt[!is.na(beta_df_melt$variable), ]

ggplot(beta_df_melt, aes(x = value, fill = variable)) +
  geom_density(aes(y = ..count..), color = "black", alpha = 0.4
  ) +
  xlab("Estimated coefficient on Grandchildren") +
    ylab("Frequency") +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 24, hor = T) +
  geom_vline(data = data.frame(xintercept = c(-0.02, -0.68, 0.57),
                               variable = c("Linear", "Quadratic", "PieceLinear")),
             aes(xintercept = xintercept, color = variable), linetype = "dashed") +
  scale_fill_manual(name = "",
                    values = MetBrewer::met.brewer("Juarez")[1:3]) +
  scale_color_manual(name = "",
                    values = MetBrewer::met.brewer("Juarez")[1:3])

ggsave("tex/figs/fig_toy_beta.pdf", last_plot(), width = 12, height = 9)

