### Model Complexity Mincerian wage example
## Use: Functions for Mincerian wage example
## Author: Mark Verhagen

## Load libraries
library(caret)
library(texreg)

fit_xgb <- function(train_set, test_set, y_var = "ln_y_I",
                    features = c("S", "X")) {
    train_outcome <- train_set[[y_var]]
    train_data <- xgb.DMatrix(
        data = as.matrix(train_set %>% dplyr::select(all_of(features))),
        label = train_outcome
    )
    test_outcome <- test_set[[y_var]]
    test_data <- xgb.DMatrix(
        data = as.matrix(test_set %>% dplyr::select(all_of(features))),
        label = test_outcome
    )
    nround <- 50 # number of XGBoost rounds
    xg_model <- xgb.train(
        data = train_data,
        nrounds = nround
    )
    return(caret::RMSE(test_set[[y_var]], predict(xg_model, test_data)))
}

lm_fits <- function(formulas, train, test, var = "ln_y_I") {
    test$ln_y <- test[[var]]
    train$ln_y <- train[[var]]
    fits <- lapply(formulas, FUN = function(x) {
        lm(x, data = train)
    })
    perfs <- lapply(fits, FUN = function(x) {
        caret::RMSE(test$ln_y, predict(x, test))
    })
    return(list(fits, perfs))
}

make_coef_df <- function(formulas, analysis_df, var = "ln_y_I") {
    fits_1 <- lm_fits(formulas,
        train = analysis_df,
        test = analysis_df, var = var
    )[1]

    coefs_1 <- lapply(fits_1[[1]], FUN = function(x) {
        summary(x)$coefficients
    })

    model_names <- c("Null", "Linear I", "Linear II", "Linear III", "Linear IV")
    res_1 <- data.frame()
    count <- 1
    for (i in coefs_1) {
        temp <- as.data.frame(i)
        temp$model_no <- model_names[[count]]
        temp$coefficient <- rownames(temp)
        temp$dataset <- var
        rownames(temp) <- NULL
        names(temp) <- c(
            "estimate", "std.error", "t-value", "p-value",
            "model_no", "coefficient", "dataset"
        )
        res_1 <- rbind(res_1, temp)
        count <- count + 1
    }
    return(res_1)
}

recover_coef <- function(total_df, coefficient = "X", model = "Linear I",
                         dataset = "ln_y_I") {
    coef <- total_df$estimate[(total_df$dataset == dataset) &
        (total_df$coefficient == coefficient) &
        (total_df$model_no == model)]
    coef <- ifelse(length(coef) == 0, 0, coef)
    return(coef)
}

predict_X <- function(df, total_df, model = "Linear I", dataset = "ln_y_I") {
    return(df$X1 * total_df$estimate[(total_df$dataset == dataset) &
        (total_df$coefficient == "X") & (total_df$model_no == model)])
}

predict_XSQ <- function(df, total_df, model = "Linear I", dataset = "ln_y_I") {
    return(df$X1 * recover_coef(total_df,
        coefficient = "X",
        model = model, dataset = dataset
    ) +
        df$X1^2 * recover_coef(total_df,
            coefficient = "X_sq",
            model = model, dataset = dataset
        ))
}

predict_S <- function(df, total_df, model = "Linear I", dataset = "ln_y_I") {
    if (model %in% c("Linear III", "Linear IV")) {
        return(df$S_0_8 * recover_coef(total_df,
            coefficient = "S_0_8",
            model = model, dataset = dataset
        ) +
            df$S_9_10 * recover_coef(total_df,
                coefficient = "S_9_10",
                model = model, dataset = dataset
            ) +
            df$S_11_12 * recover_coef(total_df,
                coefficient = "S_11_12",
                model = model, dataset = dataset
            ) +
            df$S_13_14 * recover_coef(total_df,
                coefficient = "S_13_14",
                model = model, dataset = dataset
            ) +
            df$S_15p * recover_coef(total_df,
                coefficient = "S_15p",
                model = model, dataset = dataset
            ))
    } else {
        return(df$S1 * recover_coef(total_df,
            coefficient = "S",
            model = model, dataset = dataset
        ))
    }
}