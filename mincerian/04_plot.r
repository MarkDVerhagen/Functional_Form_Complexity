## Plot simulations for Mincerian Wage equation
## Load libraries
library(tidyverse)
library(texreg)
library(cowplot)
library(ggsci)
library(ggpubr)
library(vtable)

## Make descriptipve table
df <- read.csv("data/edit/analysis_df.csv") %>%
  dplyr::select(-X.1)

labs <- data.frame(
  age = "Age",
  S = "Schooling (years)",
  X = "Work experience (years)",
  ln_y_I = "Log wages (linear-I)",
  ln_y_II = "Log wages (linear-II)",
  ln_y_III = "Log wages (linear-III)",
  ln_y_IV = "Log wages (linear-IV)",
  gender = "Sex (1 = female, 0 = male)"
)

df %>%
  select(age, S, X, gender, ln_y_I, ln_y_II, ln_y_III, ln_y_IV) %>%
  mutate(gender = ifelse(gender == 1, "Female", "Male")) %>%
  vtable::sumtable(
    labels = labs, out = "latex",
    file = "tex/tables/desc_mincerian.tex"
  )

## Plot OOS performance
source("mincerian/functions.R")
data <- readRDS("data/final/simul_4models_100.rds")

df <- data %>%
  mutate(
    lm_1_1 = 1 - (lm_1_1 / lm_1_bench),
    lm_1_2 = 1 - (lm_1_2 / lm_1_bench),
    lm_1_3 = 1 - (lm_1_3 / lm_1_bench),
    lm_1_4 = 1 - (lm_1_4 / lm_1_bench),
    lm_2_1 = 1 - (lm_2_1 / lm_2_bench),
    lm_2_2 = 1 - (lm_2_2 / lm_2_bench),
    lm_2_3 = 1 - (lm_2_3 / lm_2_bench),
    lm_2_4 = 1 - (lm_2_4 / lm_2_bench),
    lm_3_1 = 1 - (lm_3_1 / lm_3_bench),
    lm_3_2 = 1 - (lm_3_2 / lm_3_bench),
    lm_3_3 = 1 - (lm_3_3 / lm_3_bench),
    lm_3_4 = 1 - (lm_3_4 / lm_3_bench),
    lm_4_1 = 1 - (lm_4_1 / lm_4_bench),
    lm_4_2 = 1 - (lm_4_2 / lm_4_bench),
    lm_4_3 = 1 - (lm_4_3 / lm_4_bench),
    lm_4_4 = 1 - (lm_4_4 / lm_4_bench),
    gb_1 = 1 - (gb_1 / lm_1_bench),
    gb_2 = 1 - (gb_2 / lm_2_bench),
    gb_3 = 1 - (gb_3 / lm_3_bench),
    gb_4 = 1 - (gb_4 / lm_4_bench)
  ) %>%
  dplyr::select(-lm_1_bench, -lm_2_bench, -lm_3_bench, -lm_4_bench)

df_melt <- df %>%
  reshape2::melt() %>%
  mutate(
    dataset = ifelse(grepl("lm_1|gb_1", variable), "Dataset I",
      ifelse(grepl("lm_2|gb_2", variable), "Dataset II",
        ifelse(grepl("lm_3|gb_3", variable), "Dataset III",
          ifelse(grepl("lm_4|gb_4", variable), "Dataset IV",
            "Else"
          )
        )
      )
    ),
    model = ifelse(grepl("\\d{1}_1", variable), "Linear I",
      ifelse(grepl("\\d{1}_2", variable), "Linear II",
        ifelse(grepl("\\d{1}_3", variable), "Linear III",
          ifelse(grepl("\\d{1}_4", variable), "Linear IV",
            ifelse(grepl("gb", variable), "XGBoost",
              "Other"
            )
          )
        )
      )
    )
  )

df_summ <- df_melt %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    model = unique(model),
    dataset = unique(dataset)
  )

df_plot <- df_summ %>%
  filter(!grepl("rf", variable))

custom_pal <- c(
  ggsci::pal_aaas()(6)[6],
  RColorBrewer::brewer.pal(4, "Blues")
)

df_plot$model <- factor(df_plot$model, levels = c(
  "XGBoost", "Linear I", "Linear II",
  "Linear III", "Linear IV"
))
text_size <- 20


ggplot(df_plot, aes(y = mean, x = model, group = dataset, fill = model)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(as.character(round(mean, 2) * 100), "%")),
    vjust = -0.5
  ) +
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  scale_fill_manual(values = custom_pal, name = "") +
  ylim(0, 1) +
  scale_y_continuous(
    limits = c(0.5, 1.05), oob = scales::rescale_none,
    labels = scales::percent
  ) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
  facet_wrap(~dataset, nrow = 1) +
  cowplot::theme_cowplot() +
  xlab("Model") +
  ylab("Out-of-sample R-squared") +
  theme(
    panel.grid.minor.x = element_line(size = 0.25), # linetype = 'dotted'),
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.5),
    legend.position = "top",
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    text = element_text(size = 20)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
    axis.text.y = element_text(size = (text_size - 1)),
    axis.title.y = element_text(size = 16.3)
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

ggsave("tex/figs/fig1_mincerian.pdf", last_plot(), width = 12, height = 6)

## Plot in-sample regression results

analysis_df <- read.csv("data/edit/analysis_df.csv")

form_0 <- as.formula("ln_y ~ 1")
form_1 <- as.formula("ln_y ~ 1 + S + X")
form_2 <- as.formula("ln_y ~ 1 + S + X + X_sq")
form_3 <- as.formula(
  "ln_y ~ 1 + S_0_8 + S_9_10 + S_11_12 + S_13_14 + S_15p + X + X_sq"
)
form_4 <- as.formula(
  "ln_y ~ 1 + S_0_8 + S_9_10 + S_11_12 + S_13_14 + S_15p + X + X_sq + gender"
)

formulas <- list(form_0, form_1, form_2, form_3, form_4)

vars <- list("ln_y_I", "ln_y_II", "ln_y_III", "ln_y_IV")

coef_dfs <- lapply(vars, FUN = function(x) {
  make_coef_df(formulas, analysis_df, var = x)
})

total_df <- data.frame()

for (i in coef_dfs) {
  total_df <- rbind(total_df, i)
}

total_df <- total_df[total_df$model_no != "Null", ]
plot_df <- total_df[total_df$coefficient != "(Intercept)", ]
plot_df$dataset <- as.factor(plot_df$dataset)

plot_df <- plot_df %>%
  mutate(var_type = ifelse(grepl("X", coefficient), "Years Work Experience",
    ifelse(grepl("S", coefficient), "Years Schooling", "Sex")
  )) %>%
  mutate(coefficient = gsub("gender", "Female", coefficient)) %>%
  mutate(
    coefficient = gsub("S_|S", "School years: ", coefficient),
    coefficient = gsub("_", " to ", coefficient),
    coefficient = gsub("p$", "+", coefficient),
    coefficient = gsub("X to sq", "Work years (squared)", coefficient),
    coefficient = gsub("X", "Work years", coefficient),
    dataset = gsub("ln_y_", "Dataset ", dataset)
  ) %>%
  mutate(
    sig = ifelse(`p-value` < 0.01, "*", ""),
    sig_pos = estimate + 0.05
  )

ggplot(
  plot_df,
  aes(x = estimate, y = coefficient, fill = model_no)
) +
  geom_point(
    size = 2.5, position = position_dodge(width = 0.5),
    color = "black", pch = 21
  ) +
  facet_wrap(~dataset, nrow = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_cowplot() +
  scale_fill_manual(
    values = custom_pal[2:5],
    name = "Model type"
  ) +
  theme(legend.position = "top") +
  xlab("Coefficient Estimate") +
  ylab("Coefficient") +
  geom_text(
    data = plot_df, aes(x = sig_pos, y = coefficient, label = sig),
    color = "black",
    position = position_dodge(width = 0.5),
    vjust = 0.7
  ) +
  theme(
    panel.grid.major.x = element_line(
      size = 0.5, linetype = "dotted",
      colour = "lightgrey"
    ),
    panel.grid.minor.x = element_line(
      size = 0.25, linetype = "dotted",
      colour = "lightgrey"
    ),
    strip.placement = "outside",
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.2),
    legend.position = "top",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    plot.margin = grid::unit(c(0.5, 0.5, 0.1, 0.1), "mm")
  )

ggsave("tex/figs/fig2_mincerian.pdf", last_plot(), width = 12, height = 8)

## -- Shapley plots

df <- read.csv("data/edit/mincerian_shap.csv") %>%
  rowwise() %>%
  mutate(
    S1 = as.numeric(S1),
    S_0_8 = min(S1, 8),
    S_9_10 = min(max(S1 - 8, 0), 2),
    S_11_12 = min(max(S1 - 10, 0), 2),
    S_13_14 = min(max(S1 - 12, 0), 2),
    S_15p = max(S1 - 14, 0),
  ) %>%
  ungroup()

df$X1_1_predict <- predict_XSQ(df, total_df, "Linear I", "ln_y_I")
df$X1_2_predict <- predict_XSQ(df, total_df, "Linear II", "ln_y_I")
df$X1_3_predict <- predict_XSQ(df, total_df, "Linear III", "ln_y_I")
df$X1_4_predict <- predict_XSQ(df, total_df, "Linear IV", "ln_y_I")

df$X2_1_predict <- predict_XSQ(df, total_df, "Linear I", "ln_y_II")
df$X2_2_predict <- predict_XSQ(df, total_df, "Linear II", "ln_y_II")
df$X2_3_predict <- predict_XSQ(df, total_df, "Linear III", "ln_y_II")
df$X2_4_predict <- predict_XSQ(df, total_df, "Linear IV", "ln_y_II")

df$X3_1_predict <- predict_XSQ(df, total_df, "Linear I", "ln_y_III")
df$X3_2_predict <- predict_XSQ(df, total_df, "Linear II", "ln_y_III")
df$X3_3_predict <- predict_XSQ(df, total_df, "Linear III", "ln_y_III")
df$X3_4_predict <- predict_XSQ(df, total_df, "Linear IV", "ln_y_III")

df$X4_1_predict <- predict_XSQ(df, total_df, "Linear I", "ln_y_IV")
df$X4_2_predict <- predict_XSQ(df, total_df, "Linear II", "ln_y_IV")
df$X4_3_predict <- predict_XSQ(df, total_df, "Linear III", "ln_y_IV")
df$X4_4_predict <- predict_XSQ(df, total_df, "Linear IV", "ln_y_IV")

df$S1_1_predict <- predict_S(df, total_df, "Linear I", "ln_y_I")
df$S1_2_predict <- predict_S(df, total_df, "Linear II", "ln_y_I")
df$S1_3_predict <- predict_S(df, total_df, "Linear III", "ln_y_I")
df$S1_4_predict <- predict_S(df, total_df, "Linear IV", "ln_y_I")

df$S2_1_predict <- predict_S(df, total_df, "Linear I", "ln_y_II")
df$S2_2_predict <- predict_S(df, total_df, "Linear II", "ln_y_II")
df$S2_3_predict <- predict_S(df, total_df, "Linear III", "ln_y_II")
df$S2_4_predict <- predict_S(df, total_df, "Linear IV", "ln_y_II")

df$S3_1_predict <- predict_S(df, total_df, "Linear I", "ln_y_III")
df$S3_2_predict <- predict_S(df, total_df, "Linear II", "ln_y_III")
df$S3_3_predict <- predict_S(df, total_df, "Linear III", "ln_y_III")
df$S3_4_predict <- predict_S(df, total_df, "Linear IV", "ln_y_III")

df$S4_1_predict <- predict_S(df, total_df, "Linear I", "ln_y_IV")
df$S4_2_predict <- predict_S(df, total_df, "Linear II", "ln_y_IV")
df$S4_3_predict <- predict_S(df, total_df, "Linear III", "ln_y_IV")
df$S4_4_predict <- predict_S(df, total_df, "Linear IV", "ln_y_IV")

df_melt_X <- df %>%
  sample_n(250) %>%
  dplyr::select(
    X1, X1_shap, X2_shap, X3_shap, X4_shap,
    contains("_predict"),
    -starts_with("S"), sex
  ) %>%
  reshape2::melt(id.vars = c("X1", "sex")) %>%
  mutate(
    model = ifelse(grepl("shap", variable), "Shapley Value",
      ifelse(grepl("_1", variable), "Linear I",
        ifelse(grepl("_2", variable), "Linear II",
          ifelse(grepl("_3", variable), "Linear III",
            ifelse(grepl("_4", variable), "Linear IV", "None")
          )
        )
      )
    ),
    dataset = gsub("_.*", "", variable),
    dataset = gsub("X", "Dataset ", dataset),
    sex = as.factor(ifelse(sex == 1, "Female", "Male"))
  ) %>%
  rename(Sex = sex) %>%
  group_by(variable) %>%
  mutate(value = scale(value)) %>%
  ungroup()

df_melt_S <- df %>%
  sample_n(250) %>%
  dplyr::select(
    S1, S1_shap, S2_shap, S3_shap, S4_shap,
    contains("_predict"), sex
  ) %>%
  dplyr::select(-contains("X"), sex) %>%
  reshape2::melt(id.vars = c("S1", "sex")) %>%
  mutate(
    model = ifelse(grepl("shap", variable), "Shapley Value",
      ifelse(grepl("_1", variable), "Linear I",
        ifelse(grepl("_2", variable), "Linear II",
          ifelse(grepl("_3", variable), "Linear III",
            ifelse(grepl("_4", variable), "Linear IV", "None")
          )
        )
      )
    ),
    dataset = gsub("_.*", "", variable),
    dataset = gsub("S", "Dataset ", dataset),
    S1 = as.numeric(S1),
    sex = as.factor(ifelse(sex == 1, "Female", "Male"))
  ) %>%
  rename(Sex = sex) %>%
  group_by(variable) %>%
  mutate(value = scale(value)) %>%
  ungroup()

X_shap_plot <- ggplot(
  df_melt_X %>% filter(!grepl("shap", variable)),
  aes(y = value, x = X1, color = Sex)
) +
  geom_point(
    data = df_melt_X %>% filter(grepl("shap", variable)),
    alpha = 0.5, size = 3.5
  ) +
  geom_line(aes(linetype = model), color = "black", size = 0.8) +
  ggsci::scale_color_aaas() +
  xlab("Work experience (years)") +
  ylab("Implied effect on log wages") +
  scale_linetype_manual(values = c("solid", "dashed", "twodash", "dotted")) +
  theme_cowplot() +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  facet_wrap(~dataset, nrow = 1) +
  theme(
    panel.grid.minor.y = element_line(size = 0.25, linetype = "dotted"),
    panel.grid.major.y = element_line(size = 0.25, linetype = "dotted"),
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.5),
    legend.position = "top",
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    text = element_text(size = 20),
    legend.title = element_blank()
  )

S_shap_plot <- ggplot(
  df_melt_S %>% filter(!grepl("shap", variable)),
  aes(y = value, x = S1, color = Sex)
) +
  geom_point(
    data = df_melt_S %>% filter(grepl("shap", variable)),
    size = 3.5, alpha = 0.5
    #  color = ggsci::pal_aaas("default")(4)[1], size = 3.5
  ) +
  ggsci::scale_color_aaas() +
  geom_line(aes(linetype = model), color = "black", size = 0.8) +
  xlab("Schooling (years)") +
  ylab("Implied effect on log wages") +
  scale_linetype_manual(values = c("solid", "dashed", "twodash", "dotted")) +
  theme_cowplot() +
  theme(
    panel.grid.minor.y = element_line(size = 0.25, linetype = "dotted"),
    panel.grid.major.y = element_line(size = 0.25, linetype = "dotted"),
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.5),
    legend.position = "top",
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    text = element_text(size = 20),
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  facet_wrap(~dataset, nrow = 1)

ggarrange(X_shap_plot + theme(legend.position = "none"),
  S_shap_plot + theme(legend.position = "none"),
  nrow = 2, common.legend = TRUE, legend = "top"
)

ggsave("tex/figs/fig3_mincerian.pdf", last_plot(), width = 12, height = 8)