### Model Complexity ideology example
## Use: Make plots
## Author: Mark Verhagen

## Load libraries
library(tidyverse)
library(ggpubr)
library(cowplot)
library(ggsci)
library(vtable)

## Load plot style
source("./styles.R")

## Descriptive tables
data_raw <- readRDS("./ideology/data/raw/GSS7218_R3.Rds")

data <- data_raw %>%
    filter(
        !is.na(wordsum),
        !is.na(polviews),
        !is.na(partyid)
    ) %>%
    filter(partyid != 7) %>%
    mutate(id = 1:n())

labs <- data.frame(
    partyid = "Party Identification",
    income = "Income",
    age = "Age",
    educ = "Schooling (Years)",
    sex = "Sex",
    race = "Race",
    year = "Survey Year"
)

data %>%
    dplyr::select(
        partyid, age, educ, income, sex, race, year
    ) %>%
    mutate(
        sex = ifelse(sex == 2, "Female", "Male"),
        race = ifelse(race == 1, "White",
            ifelse(race == 2, "Black", "Other")
        ),
        educ = as.numeric(educ),
        partyid = as.factor(ifelse(partyid == 0, "Strong democrat",
            ifelse(partyid == 1, "Not strong democrat",
                ifelse(partyid == 2, "Independent, near democrat",
                    ifelse(partyid == 3, "Independent",
                        ifelse(partyid == 4,
                            "Independent, near republican",
                            ifelse(partyid == 5,
                                "Not strong republican",
                                ifelse(partyid == 6,
                                    "Strong republican", NA
                                )
                            )
                        )
                    )
                )
            )
        )),
        income = as.factor(ifelse(income == 1, "< $1000",
            ifelse(income == 2, "$1000-2999",
                ifelse(income == 3, "$3000-3999",
                    ifelse(income == 4, "$4000-4999",
                        ifelse(income == 5, "$5000-5999",
                            ifelse(income == 6, "$6000-6999",
                                ifelse(income == 7, "$7000-7999",
                                    ifelse(income == 8, "$8000-9999",
                                        ifelse(income == 9, "$10000-14999",
                                            ifelse(income == 10, "$15000-19999",
                                                ifelse(income == 11,
                                                    "$20000-24999",
                                                    ifelse(income == 12,
                                                        "$25000+", NA
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
        )),
        year = as.factor(year)
    ) %>%
    vtable::sumtable(
        labels = labs,
        out = "latex", file = "../tex/tables/gss_summary_tab.tex"
    )


data %>%
    filter(!is.na(age)) %>%
    dplyr::select(
        partyid, age, educ, income, sex, race, year
    ) %>%
    mutate(
        sex = ifelse(sex == 2, "Female", "Male"),
        race = ifelse(race == 1, "White",
            ifelse(race == 2, "Black", "Other")
        ),
        educ = as.numeric(educ),
        partyid = as.numeric(partyid + 1),
        income = as.numeric(income),
        year = as.numeric(year)
    ) %>%
    vtable::sumtable(
        labels = labs,
        out = "latex", file = "../tex/tables/gss_num_summary_tab.tex"
    )

## OOS plot for different base sets
df_oos <- readRDS("ideology/data/edit/simul_oos.rds")

df_oos_melt <- df_oos %>%
    reshape2::melt() %>%
    group_by(variable) %>%
    summarise(mean = mean(value), sd = sd(value)) %>%
    ungroup() %>%
    mutate(
        model = ifelse(grepl("lm", variable), "Linear", "Flexible"),
        outcome_type = ifelse(grepl("r2", variable), "R-Squared", "RMSE"),
        vars = ifelse(grepl("0", variable), "Year",
            ifelse(grepl("lm1|gb1", variable), "Year + Age",
                ifelse(grepl("lm2|gb2", variable), "Year + Age + Sex",
                    ifelse(grepl("lm3|gb3", variable), "Year + Age + Race",
                        ifelse(grepl("lm4|gb4", variable),
                            "Year + Age + Sex +\nRace",
                            ifelse(grepl("lm5|gb5", variable),
                                "Year + Age + Sex +\nRace + Income",
                                ifelse(grepl("lm6|gb6", variable),
                                    "Year + Age + Sex +\nRace + Education",
                                    ifelse(grepl("lm7|gb7", variable),
                                        "Year + Age + Sex +\nRace + Income + Education",
                                        NA
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ) %>%
    filter(vars != "Year")

ggplot(df_oos_melt %>% filter(outcome_type == "R-Squared"), aes(
    x = mean,
    y = reorder(vars, mean), fill = model
)) +
    geom_bar(
        stat = "identity", position = position_dodge(width = 0.9),
        color = "black"
    ) +
    geom_text(aes(
        label = paste0(round(mean, 3) * 100, "%"),
        x = mean + 2 * sd + 0.01
    ),
    position = position_dodge(width = 0.9),
    size = 6
    ) +
    geom_errorbar(aes(
        xmin = ifelse(mean - 2 * sd >= 0, mean - 2 * sd, 0),
        xmax = mean + 2 * sd
    ),
    position = position_dodge(width = 0.9), width = 0.2
    ) +
    scale_fill_manual(
    name = "Model",
    values = ggsci::pal_aaas("default")(4)[3:4],
    labels = c("GB", "Hypothesized")) +
    # ggsci::scale_fill_aaas(name = "", ) +
    xlim(0, 0.18) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 20, ver = T) +
    ylab("Explanatory variables") +
    xlab("Out-of-sample R-squared") +
    theme(axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 21),
          axis.title.x = element_text(size = 21))

ggsave("tex/figs/fig1_gss.pdf", last_plot(), width = 12, height = 7)

## (Interaction) shap plots
int_shap_df <- read.csv("ideology/data/edit/int_shap_values.csv")
kern_shap_df <- read.csv("ideology/data/edit/gss_shap_gb.csv")

load("ideology/data/edit/lm_data.rda")

lm1 <- lm(partyid ~ 1 + age + educ + as.factor(year) + income + sex + race,
    data = train_set,
    weights = wtss
)

lm2 <- lm(partyid ~ 1 + age + age2 + as.factor(year) + income + income2 +
    sex + race + educ + educ2, data = train_set_lm, weights = wtss)

coefs_df <- as.data.frame(summary(lm1)$coefficients)
coefs_df$var <- rownames(coefs_df)

coefs_df2 <- as.data.frame(summary(lm2)$coefficients)
coefs_df2$var <- rownames(coefs_df2)

kern_shap_df <- kern_shap_df %>%
    mutate(
        implied_age = scale(age * coefs_df$Estimate[coefs_df$var == "age"]),
        implied_educ = scale(educ * coefs_df$Estimate[coefs_df$var == "educ"]),
        implied_income = scale(income * coefs_df$Estimate[coefs_df$var ==
            "income"]),
        implied_age2 = scale(age * coefs_df2$Estimate[coefs_df2$var ==
            "age"] +
            age^2 * coefs_df2$Estimate[coefs_df2$var == "age2"]),
        implied_educ2 = scale(educ * coefs_df2$Estimate[coefs_df2$var ==
            "educ"] +
            educ^2 * coefs_df2$Estimate[coefs_df2$var == "educ2"]),
        implied_income2 = scale(income * coefs_df2$Estimate[coefs_df2$var ==
            "income"] +
            income^2 * coefs_df2$Estimate[coefs_df2$var == "income2"])
    ) %>%
    mutate(
        age_shap = scale(age_shap),
        educ_shap = scale(educ_shap),
        income_shap = scale(income_shap)
    )

plot_a1_df <- kern_shap_df %>%
    select(age_shap, income_shap, educ_shap) %>%
    reshape2::melt()

plot_a2_df <- kern_shap_df %>%
    select(age, income, educ) %>%
    reshape2::melt()

plot_a3_df <- kern_shap_df %>%
    select(implied_age, implied_income, implied_educ) %>%
    reshape2::melt()

plot_a4_df <- kern_shap_df %>%
    select(implied_age2, implied_income2, implied_educ2) %>%
    reshape2::melt()

plot_a1_df$data <- plot_a2_df$value
plot_a1_df$linear <- plot_a3_df$value
plot_a1_df$quadratic <- plot_a4_df$value

plot_a1_df <- plot_a1_df %>%
    mutate(variable = ifelse(variable == "age_shap", "Age",
        ifelse(variable == "income_shap", "Income",
            ifelse(variable == "educ_shap", "Education", NA)
        )
    ))

panel_A <- ggplot(plot_a1_df, aes(y = value, x = data, color = variable)) +
    geom_point(alpha = (alpha - 0.1), size = (scatter_size - 1)) +
    # geom_smooth(se = F) +
    geom_line(aes(y = linear, x = data, linetype = "Linear"), color = "black") +
    geom_line(aes(
        y = quadratic, x = data,
        linetype = "Quadratic"
    ), color = "black") +
    facet_wrap(~variable, scales = "free", ncol = 1) +
        scale_colour_manual(name = "Variable",
                            values = MetBrewer::met.brewer("Egypt")[1:3]) +
    scale_linetype_manual(name = "", values = c("solid", "dashed")) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 20, hor = T) +
    ylab("Implied effect") +
    xlab("Observed variable value") +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 14)
    )

ggsave("tex/figs/fig2_gss.pdf", last_plot())

# Panel B
panel_B <- ggplot(int_shap_df, aes(y = age_year_shap, x = age, color = year)) +
    geom_point(alpha = alpha, size = (scatter_size - 2)) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 19, hor = T) +
    xlab("Age") +
    ylab("Difference in age effect by survey year") +
    scale_colour_distiller(
        name = "Survey Year",
        guide = guide_legend(direction = "vertical")
    ) +
    theme(legend.position = "right") +
    theme(legend.title = element_blank())

# Panel C
panel_C <- ggplot(int_shap_df, aes(
    y = age_race_shap, x = age,
    color = as.factor(race)
)) +
    geom_point(alpha = alpha, size = (scatter_size - 1)) +
        cowplot::theme_cowplot() +
        scale_color_manual(name = "Race", labels = c("White", "Black", "Other"),
                           values = MetBrewer::met.brewer("Hokusai1")[c(1, 5, 7)]) +
    # scale_color_aaas() +
    custom_theme(hor = T, text_size = 19) +
    xlab("Age") +
    ylab("Difference in age effect by race") +
    theme(legend.position = "right") +
    theme(legend.title = element_blank())

# Panel D
panel_D <- ggplot(int_shap_df, aes(
    y = race_sex_shap, x = race,
    color = as.factor(sex)
)) +
    geom_point(alpha = alpha, size = (scatter_size - 1)) +
    cowplot::theme_cowplot() +
    scale_color_manual(
        name = "Race", labels = c("Male", "Female"),
        values = MetBrewer::met.brewer("Signac")[c(3, 13)]
    ) +
    custom_theme(text_size = 19, hor = T) +
    xlab("Race") +
    ylab("Difference in race effect by sex") +
    scale_x_continuous(
        labels = c("White", "Black", "Other"),
        breaks = c(1, 2, 3)
    ) +
    theme(legend.position = "right") +
    theme(legend.title = element_blank())

# Panel E
panel_E <- ggplot(int_shap_df, aes(
    y = income_race_shap, x = income,
    color = as.factor(race)
)) +
    geom_point(alpha = alpha, size = (scatter_size - 1)) +
    cowplot::theme_cowplot() +
    scale_color_manual(name = "Race", labels = c("White", "Black", "Other"),
                       values = MetBrewer::met.brewer("Hokusai1")[c(1, 5, 7)]) +
    custom_theme(text_size = 19, hor = T) +
    xlab("Income") +
    ylab("Difference in income effect by race") +
    theme(legend.position = "right") +
    theme(legend.title = element_blank())

ggdraw() +
    draw_plot(panel_A, x = 0, y = 0, width = .31, height = 0.95) +
    draw_plot(panel_B, x = .34, y = .5, width = .33, height = .45) +
    draw_plot(panel_C, x = .34, y = 0, width = .33, height = .45) +
    draw_plot(panel_D, x = .67, y = .5, width = .33, height = .45) +
    draw_plot(panel_E, x = .67, y = 0, width = .33, height = .45) +
    draw_plot_label(
        label = c(
            "A. Overall effect by variables",
            "B. Age and Survey Year interaction",
            "C. Age and Race interaction", "D. Race and Sex interaction",
            "E. Income and Race interaction"
        ), size = 20,
        x = c(-0.05, 0.28, 0.28, 0.62, 0.62), y = c(1, 1, 0.5, 1, 0.5)
    )


ggsave("tex/figs/fig3_gss.pdf", last_plot(), width = 20, height = 12)

