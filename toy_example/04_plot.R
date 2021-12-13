### Model Complexity Toy Example
## Use: Generate plots and tables for paper
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(vtable)

## Load data
df_effects <- readRDS("toy_example/data/edit/df_effects.rds")

## Pot implied effects of the age variable

ggplot(df_effects, aes(y = value, x = age, color = variable, linetype = ` `)) +
  geom_point(data = df_effects %>%
    filter(!grepl("true", variable)), alpha = 0.3) +
  geom_line(
    data = df_effects %>% filter(grepl("true", variable)), color = "black"
  ) +
  scale_linetype_manual(values = c("dashed")) +
  ggsci::scale_color_aaas(
    name = "",
    labels = c("Model 1", "Model 2", "Model 3")
  ) +
  xlim(50, 90) +
  cowplot::theme_cowplot() +
  theme(
    panel.grid.minor.y = element_line(size = 0.25, linetype = "dotted"),
    panel.grid.major.y = element_line(size = 0.25, linetype = "dotted"),
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.5),
    legend.position = "top",
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    text = element_text(size = 20)
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  ylab("Implied age effect") +
  xlab("Age")

ggsave("tex/figs/fig1_toy.pdf", last_plot(), width = 7, height = 9)

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
    data = comb_df %>% filter(variable == "Shapley"), alpha = 0.4,
    aes(color = variable)
  ) +
  geom_line(
    data = comb_df %>% filter(variable == "lm3_implied"),
    aes(linetype = "Correct model")
  ) +
  xlab("Age") +
  ylab("Implied effect") +
  cowplot::theme_cowplot() +
  theme(
    panel.grid.minor.y = element_line(size = 0.25, linetype = "dotted"),
    panel.grid.major.y = element_line(size = 0.25, linetype = "dotted"),
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.5),
    legend.position = "top",
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    text = element_text(size = 30)
  ) +
  xlim(50, 90) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  scale_color_manual(
    name = "", labels = c("Shapley"),
    values = c(ggsci::pal_aaas("default")(2)[2])
  ) +
  scale_linetype(
    name = " ", labels = c("Correct model")
  )

ggsave("tex/figs/fig2_toy.pdf", last_plot(), width = 12, height = 9)
