# modelDown

[![Travis-CI Build Status](https://travis-ci.org/MI2DataLab/modelDown.svg?branch=master)](https://travis-ci.org/MI2DataLab/modelDown)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/modelDown)](https://cran.r-project.org/package=modelDown)
[![Build Status](https://api.travis-ci.org/MI2DataLab/modelDown.png)](https://travis-ci.org/MI2DataLab/modelDown)
[![Coverage
Status](https://img.shields.io/codecov/c/github/MI2DataLab/modelDown/master.svg)](https://codecov.io/github/MI2DataLab/modelDown?branch=master)


`modelDown` generates a website with HTML summaries for predictive models.
Is uses [DALEX](https://github.com/pbiecek/DALEX) explainers to compute and plot summaries of how given models behave. We can see how exactly scores for predictions were calculated (Prediction BreakDown), how much each variable contributes to predictions (Variable Response), which variables are the most important for a given model (Variable Importance) and how well out models behave (Model Performance).

`pkgdown` documentation: https://mi2datalab.github.io/modelDown/

An example website for regression models: https://mi2datalab.github.io/modelDown_example/

## Getting started

Do you want to start right now ? Check out our [getting started](https://mi2datalab.github.io/modelDown/getting-started) guide.

## Index page

<center><img width="600" src="https://raw.githubusercontent.com/MI2DataLab/modelDown/master/misc/index.PNG"></center>

Index page presents basic information about data provided in explainers. You can also see types of all explainers given as parameters. Additionally, summary statistics are available for numerical variables. For categorical variables, tables with frequencies of factor levels are presented.

## Model Performance

<center><img width="600" src="https://raw.githubusercontent.com/MI2DataLab/modelDown/master/misc/performance.PNG"></center>

Module shows result of function `model_performance`. 

## Variable Importance

<center><img width="600" src="https://raw.githubusercontent.com/MI2DataLab/modelDown/master/misc/importance.PNG"></center>

Output of function `variable_importance` is presented in form of a plot as well as a table.

## Variable Response

<center><img width="600" src="https://raw.githubusercontent.com/MI2DataLab/modelDown/master/misc/response.PNG"></center>

For each variable, plot is created by using function `variable_response`. Plots can be easily navigated using links on the left side. One can provide names of variables to include in the module with argument `vr.vars` (if argument is not used, plots for all variables of first explainer are generated).

## Prediction BreakDown

<center><img width="600" src="https://raw.githubusercontent.com/MI2DataLab/modelDown/master/misc/prediction.PNG"></center>

Module presents plot generated with function `prediction_breakdown` for particular observations. Observations to be presented can be provided by user as input parameter (named `pb.observations`), otherwise, for each explainer, observation with highest residual value is presented. You can also see exact values of the observation in the generated table.
