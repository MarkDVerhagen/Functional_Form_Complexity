## Code for Functional Form Complexity paper

This repository contains all code to generate the results for the paper 'Incorporating Machine Learning into Sociological Model-building'.

The repository contains code in five separate folders. Each folder contains reproduction material for an example used in the paper. The first folder `base_simuls` contains a script to generate a couple of non-linear functional forms. Each of the other four folders has the same script structure. First a `00_gen_data.R` script used to wrangle the data. `01_superlearner.R` estimates typical functoinal forms together with a SuperLearner containing various flexible models. `02_simul.R` evaluates the out-of-sample fit metric of the best performing flexible model from the SuperLearner and the hypothesized model at different feature sets. `03_shap.py` calculates Shapley values for the flexible model. `04_plot.R` plots the results.

More detailed information for the `housing`, `mincerian` and `ideology` examples is enclosed in a folder-specific `README.md`.

```bash
├── base_simuls
│   ├── 00_gen.R
├── toy_example
│   ├── 00_gen_data.R
│   ├── 01_superlearner.R
│   ├── 02_simul.R
│   ├── 03_shap.py
│   ├── 04_plot.R
├── mincerian
│   ├── README.md
│   ├── 00_gen_data.R
│   ├── 01_superlearner.R
│   ├── 02_simul.R
│   ├── 03_shap.py
│   ├── 04_plot.R
├── housing
│   ├── README.md
│   ├── 00_gen_data.R
│   ├── 01_superlearner.R
│   ├── 02_simul.R
│   ├── 03_shap.py
│   ├── 04_plot.R
├── ideology
│   ├── README.md
│   ├── 00_gen_data.R
│   ├── 01_superlearner.R
│   ├── 02_simul.R
│   ├── 03_shap.py
│   ├── 04_plot.R
```
