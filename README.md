
# DIETCOST

<!-- badges: start -->

<!-- badges: end -->

This package is the R version of the DIETCOST SOFTWARE, providing tools
to easily perform Monte Carlo simulations, aiming to evaluate the cost
and environmental impact of a diet. Also provided are tools to
manipulate the data and to conduct basic statistical analysis of the
results.

## Installation

You can install the development version of DIETCOST from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hbracarense/dietcost")
```

## Example

The main function of this package is monteCarloSimulation(), displayed
bellow:

``` r
library(DIETCOST)
monteCarloSimulation(5, DIETCOST::foods, DIETCOST::nutrient_targets, DIETCOST::food_groups, person = 'woman', 'PF', c(1,2,3), 0.5, allow_discretionary = TRUE, allow_alcohol = TRUE, allow_takeaway = TRUE)
#> [1] "All meals formed will be saved as .csv files in directory C:/Users/hbrac/OneDrive/Documentos/Nutrição/Produtos/Pacote/dietcost/results_20250501154825"
#> [1] "Iteration: 1"
#> [1] "We are too high on fat. Current: 9052.80054495475. Max: 513.1"
#> [1] "FARINHA LACTEA impacts fat and intake must be between 0 and 0. Options: 0. Current: 0"
#> [1] "Changing FARINHA LACTEA intake from 0 to 0"
#> [1] "Iteration: 2"
#> [1] "We are too high on sugars_perc. Current: 0.110025397298352. Max: 0"
#> [1] "ACUCAR DIETCOST impacts sugars_perc and intake must be between 0 and 0. Options: 0. Current: 0"
#> [1] "Changing ACUCAR DIETCOST intake from 0 to 0"
#> [1] "Iteration: 3"
#> [1] "We are too high on CHO. Current: 21691.0158021275. Max: 2887.5"
#> [1] "SALAME impacts CHO and intake must be between 0 and 0. Options: 0. Current: 0"
#> [1] "Changing SALAME intake from 0 to 0"
#> [1] "Iteration: 4"
#> [1] "We are too high on fibre. Current: 2824.541160645. Max: 700"
#> [1] "DOCE DE FRUTAS DIETCOST impacts fibre and intake must be between 0 and 0. Options: 0. Current: 0"
#> [1] "Changing DOCE DE FRUTAS DIETCOST intake from 0 to 0"
#> [1] "Iteration: 5"
#> [1] "We are too high on redmeat. Current: 1950. Max: 196"
#> [1] "LINGUICA NO VAREJO DIETCOST impacts redmeat and intake must be between 0 and 0. Options: 0. Current: 0"
#> [1] "Changing LINGUICA NO VAREJO DIETCOST intake from 0 to 0"
```

This function saves in the top directory a report containing the stats
of the run. It also creates a folder, which will store the resulting
diets, if any is formed, as a .csv.

The development of this package also encompassed the creation of a
‘standard table’, whose use is greatly advised to ensure better results.
Although the usage of this package is possible with different dataset
formats, choosing to adopt the ‘standard table’ makes DIETCOST so much
easier.

This table is made availabe throught the GitHub repo and also as .xlsx
on the external data folder of the package. Said folder also houses two
script examples.

Finally, this package also contains the already preprocessed datased,
which can be accessed and stored into R dataframes as bellow:

``` r
foods_df <- DIETCOST::foods
food_groups_df <- DIETCOST::food_groups
nutrient_targets <- DIETCOST::nutrient_targets
```

Please report any bugs or submit any queries either to this GitHub repo
or to <hbracarense@hotmail.com>.
