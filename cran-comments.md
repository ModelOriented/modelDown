# modelDown

## Test environments
* local Ubuntu 18.04.2 installs, R 3.6.0
* Ubuntu 14.04.5 (on travis-ci), R 3.6.0
* macOS 10.13.3 (on travis-ci), R 3.6.0
* win-builder (devel)

## R CMD check results
My first cran submission. Here are comments to check NOTEs:

There were no ERRORs or WARNINGs

There were 2 NOTEs:

* Possibly mis-spelled words in DESCRIPTION:
    * NCN (14:59) - It's polish abbreviation for "Narodowe Centrum Nauki"
    * explainers (8:31) - It's proper name introduced in DALEX package. It describes object that contains ML model and additional information for it

* Suggests or Enhances not in mainstream repositories (also suggested but not available for checking):
    * drifter - package is part of our organisation and is not published to cran yet. Repository could be found here: https://github.com/ModelOriented/drifter. It's not required for main functionality of modelDown. 
