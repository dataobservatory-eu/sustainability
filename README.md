
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Demo Material for Green Deal Observatory

<!-- badges: start -->
<!-- badges: end -->

Please, note that the `README.md` is generated from `README.Rmd.` Please
edit the `.Rmd` file.

## Directories

`data`: all finalized, reproducible datasets. `data-raw`: all data that
is work-in-progress. `not_included`: your own scratch directory, not
synchronized to github. `R`: R language scripts `Python`: Python
language scripts. `rsconnect`: rpubs connection files, not synchronized

The main directory has Rmd files, which contain R or Python codes
embedded in the markdown text. Each .Rmd file has a corresponding
subdirectory, such as `Eurobarometer_19_files` for
`Eurobarometer_19.Rmd`. Furthermore, it may have the knitted `.pdf`,
`.docx` or `.html` versions of the Eurobarometer\_19 document. The main
directory contains all `.bib` files for bibliographical references,
please, reference your data sources, too.

Please do not leave scratch, R and Python codes, downloaded data tables
in the main directory.

## Processed Files From 2020

-   [Eurobarometer
    2013](https://github.com/dataobservatory-eu/sustainability/blob/master/Eurobarometer_13.md)
-   [Eurobarometer
    2015](https://github.com/dataobservatory-eu/sustainability/blob/master/Eurobarometer_15.md)
-   [Eurobarometer
    2017](https://github.com/dataobservatory-eu/sustainability/blob/master/Eurobarometer_17.md)
-   [Eurobarometer
    2018](https://github.com/dataobservatory-eu/sustainability/blob/master/Eurobarometer_18.md)
-   [Eurobarometer
    2019](https://github.com/dataobservatory-eu/sustainability/blob/master/Eurobarometer_19.md)

## Processed Files From 2021

-   [Climate-retroharmonize.Rmd](https://github.com/dataobservatory-eu/sustainability/blob/main/Climate-retroharmonize.Rmd)
    is the actual harmonization code

-   [Climate-regionalize.Rmd](https://github.com/dataobservatory-eu/sustainability/blob/main/Climate-Regionalize.Rmd)
    is the regionalization code

-   [data\_files.Rmd](https://github.com/dataobservatory-eu/sustainability/blob/main/data_files.Rmd)
    is an overview of the Eurobarometer data (you find the questionnaire
    documentation links, but they are also downloaded to data-raw.

-   [Climate-join.Rmd](https://github.com/dataobservatory-eu/sustainability/blob/main/Climate-join.Rmd)
    joins your datasets with the newly harmonized, regionalized ones.

They create -
[climate-panel.rds](https://github.com/dataobservatory-eu/sustainability/blob/main/data-raw/climate-panel.rds):
panel data of four surveys.

-   [climate-panel\_recoded.rds](https://github.com/dataobservatory-eu/sustainability/blob/main/data-raw/climate-panel_recoded.rds):
    regional aggregates from `climate-panel.rds`.

## Analysis

This is from 2020, but interesting to read. \* [Transition Policy &
Pollutants
2019](https://github.com/dataobservatory-eu/sustainability/blob/master/Transition_policy_polluntants.md)

## Further files

-   [sustainability.bib](https://github.com/dataobservatory-eu/sustainability/blob/main/sustainability.bib)
    is the project bibliography.

## Code of Conduct

Please note that the regions project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
