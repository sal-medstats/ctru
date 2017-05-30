# CTRU R Functions

This repository contains R functions to facilitate work at the [Sheffield Clinical Trials Research Unit (CTRU)](https://www.shef.ac.uk/scharr/sections/dts/ctru), part of the [School of Health and Related Research (ScHARR)](http://www.sheffield.ac.uk/scharr) at [The University of Sheffield](http://www.sheffield.ac.uk/).  The intention is to share code between colleagues so that common repetitive tasks become trivial and we do not spend time solving the same problems.

## Installation

If all you want to do is use these functions then its pretty straight-forward to install them thanks to the [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) package.  Install it from CRAN and then install this repository from GitHub...

```
install.packages('devtools')
devtools::install_github('ns-ctru/ctru')
## And of course load the library
library(ctru)
```

You can now use the functions `read_prospect()`, `fields_prospect()` and so forth.

## Collaborating

To collaborate in this work you will need to install [Git](https://git-scm.com) on your computer and have a [GitHub account](https://www.github.com/join).  If you're not familiar with either of these you may find the tutorial [Conversational Git](http://blog.anvard.org/conversational-git/) a useful place to start.  The [GitHub help pages](https://help.github.com/) are also excellent.

### Cloning

Once you've got a GitHub account you need to [clone](https://git-scm.com/docs/git-clone) this repository or make [pull requests](https://git-scm.com/docs/git-pull).

### SSH Keys

I would advocate using [SSH Keys](https://help.github.com/articles/generating-an-ssh-key/) with your GitHub account to make it easy to push updates without having to enter your password.


## ToDo

### Functions

* Function to download 'Fields' and 'Froms' tabs from DM Googlesheets using either [googlesheets](https://cran.r-project.org/web/packages/googlesheets/index.html) or [gsheet](https://cran.r-project.org/web/packages/gsheet/index.html).
* Function for calculating EQ5D-5L (supposedly due to be added to Prospect but not clear when, see [slide 40 and 41](http://www.slideshare.net/OHENews/ohe-seminar-5ll-value-set-oct2014-revised-jun15)).  Could possibly have it summarise and plot scores by user-specified variable (default being the event and the group)

### Features of Functions

#### `regress_ctru()`

* Include options to set the reference level (via `relevel()`)for each factor variable in a model (something akin to the way `texreg()` handles things).
* Include ability to bootstrap regression results, particularly important for mixed models where p-values are unreliable due to uncertainty in the degrees of freedom.  Some leverage to do this via `texreg()` but `stargazer()` is a more flexible tabulating option.
* Include all results from ITT/PP models, coefficients and CIs, p-values as part fo the returned list which can then be parsed for inclusion in text.

#### `table_summary()`

* Function to summarise specified measurements by specified subset.
* N/Mean/SD/Min/Max/Median/IQR reported for specified variables for the specified grouping.

##### ToDo

* Full support for Non-Standard Evaluation when explicitly supplying grouing variables as an argument rather than `...`.

#### `idm_lsoa()`

* Function to combine [Lower Super Output Area (LSOA) level Index of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015) statistics with an arbitrary user specified data frame based on 2011 postcode.  Provides the overall IMD score and each component as absolute numbers and deciles as well as the ranking of all scores and components across England.

##### ToDo

* Add in 2010 data.
* Add in data on LSOAs in Wales.
