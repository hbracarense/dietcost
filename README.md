
# DIETCOST

<!-- badges: start -->

<!-- badges: end -->

This package is the R version of the DIETCOST SOFTWARE, providing tools
to easily perform Monte Carlo simulations, aiming to evaluate the cost
and environmental impact of a diet. Also provided are tools to
manipulate the data and to conduct basic statistical analysis of the
results.

## Installation

You can install the stable version of DIETCOST from CRAN with:

``` r
install.packages("DIETCOST")
```

Also, the development version of DIETCOST is available in
[GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("hbracarense/dietcost")
```

## Example

The main function of this package is monteCarloSimulation(), displayed
bellow:

``` r
library(DIETCOST)
monteCarloSimulation(tempdir(), 5, DIETCOST::foods, DIETCOST::nutrient_targets, DIETCOST::food_groups, person = 'woman', 'PF', c(1,2,3), 0.5, allow_discretionary = TRUE, allow_alcohol = TRUE, allow_takeaway = TRUE)
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
or to <h@bracarense.com>.
