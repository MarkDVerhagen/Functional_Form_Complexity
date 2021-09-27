### Predicting House Prices using Machine Learning

This repository documents the analysis of hous prices in the UK using variuous ML methods

#### Data

Data can be downloaded from https://reshare.ukdataservice.ac.uk/854240/ and should be put into the `data/` folder. A description of the dataset can be found on Bin Chi's [github page](https://github.com/Bin-Chi/Link-LR-PPD-and-Domestic-EPCs).

Added to this dataset are a couple of select columns for the London region. Specifically:

1. _travel_to_centre_: the average travel time to the city centre (Department for Transport, 2011)
2. _imd_: deprivation index (2010 CLG)
3. _crime_: crime index (2010 CLG)

#### Models

Repository supports three types of models at the moment:

1. Linear Regression
2. Random Forest
3. Gradient Boosting

#### Functionality

The `Predict` can be used to estimate models and generate predictions.
