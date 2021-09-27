### Functional Form Benchmarking: Mincerian Wage Example

In this example I show how flexible form models can be used to diagnose a possible lack of complexity in a functional form. I start from an a-priori hypothesised model and evaluate its fit to a dataset vis-a-vis the fit of a flexible form model. Evaluation is done out-of-sample to ensure the flexible form model is not overfitted to the data.

The case study presented here concerns a typical Mincerian Wage Equation relating years of education to (log) yearly wages. The typical functional form estimated is as follows:

log(y) = a_0 + b_1 S + b_2 X + epsilon

where `S` reflects the number of years of education enjoyed by person `i`, and `X` reflects the number of years of work experience enjoyed by person `i` (Carter, ).

This functional form has been innovated upon over the past half century to include higher order terms on the effect of `X`, as well as a step-wide effect for parameter `b1` as a function of `S` (Heckman). This example is meant to illustrate how flexible form models could've assisted in identifying such additional model complexity without having to specify alternative functional forms a-priori. In other words, flexible form models function serve as a benchmark of model complexity.

To this effect, I simulate data on wages, years of education, and years of work experience in line with four DGPs:

1. DGP 1: 4.5 + 0.125 S + 0.09 X + epsilon
2. DGP 2: 4.5 + 0.125 S + 0.09 X - 0.001 \* X^2 + epsilon
3. DGP 3: 4.5 + 0.02 _ S_0_8 + 0.03 _ S*9_10 + 0.30 * S*11_12 + 0.06 * S*13_14 + 0.06 * S*15p + 0.09 * X - 0.001 \* X^2 + epsilon
4. DGP 4: _for females_ 3.5 + 0.025 _ S_0_8 + 0.06 _ S*9_10 + 0.35 * S*11_12 + 0.06 * S*13_14 + 0.08 * S*15p + 0.1 * X - 0.0005 * X^2 + epsilon, *for males*: 5.5 + 0.015 * S*0_8 + 0.02 * S*9_10 + 0.25 * S*11_12 + 0.04 * S*13_14 + 0.04 * S*15p + 0.06 * X - 0.001 \_ X^2 + epsilon

```
# simulate log wage based on four linear additive DGPs
simul_df <- analysis_df %>%
    mutate(
        epsilon = rnorm(dim(analysis_df)[1], 0, 0.1),
        ln_y_I = 4.5 + 0.125 * S + 0.09 * X + epsilon,
        ln_y_II = 4.5 + 0.125 * S + 0.09 * X - 0.001 * X^2 + epsilon,
        ln_y_III = 4.5 + 0.02 * S_0_8 + 0.03 * S_9_10 + 0.30 * S_11_12 +
            0.06 * S_13_14 + 0.06 * S_15p + 0.09 * X - 0.001 * X^2 + epsilon,
        ln_y_IV = ifelse(gender == 0,
            5.5 + 0.015 * S_0_8 + 0.02 * S_9_10 + 0.25 * S_11_12 +
            0.04 * S_13_14 + 0.04 * S_15p + 0.06 * X - 0.001 * X^2 +
            epsilon,
            3.5 + 0.025 * S_0_8 + 0.06 * S_9_10 + 0.35 * S_11_12 +
                0.06 * S_13_14 + 0.08 * S_15p + 0.1 * X - 0.0005 * X^2 + epsilon
        )
    ) %>%
    mutate(id = 1 : dim(analysis_df))
```

In other words, the first dataset is generated using a simple linear additive model without any higher order terms on either `S` or `X`. The second dataset is generated with a second order polynomial for the association between log wages and `X`. The third dataset is a piece-wise linear model with respect to the effect of `S`. The fourth dataset includes an interaction with an individual's sex for all the coefficients in the piece-wise model.

I then estimate four pre-specified functional forms through OLS in line with the four DGPs defined above. In other words: each dataset is fitted to four models, of which one corresponds to the 'correct' DGP, Note, however, that additional complexity in this case is not problematic, thus all four DGPs defined earlier should be equally able to fit the first dataset. As the DGP complexity increases, however, the simpler functional forms will fit the datasets with high complexity less well. I also fit all four datasets to a vanilla XGBoost model, which is a tree-based functional form-free model.

To generate a holistic evaluation metric for both the pre-specified models as well as the XGBoost model, I split the dataset into a train and test set. I use the train set to fit each of the models to all four datasets, and use the test set to make predictions using the fitted models. I then calculate an evaluation metric of model fit by generating an out-of-sample R2 measure which is simply 1 minus the ratio of the model predictions' RMSE versus that of a null model including the mean of the train set only.

The results can be found below:
