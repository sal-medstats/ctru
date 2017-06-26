[![ORCiD](https://img.shields.io/badge/ORCiD-0000--0001--8301--6857-green.svg)](https://orcid.org/0000-0001-8301-6857)

# CTRU R Functions

This repository contains R functions to facilitate work at the [Sheffield Clinical Trials Research Unit (CTRU)](https://www.shef.ac.uk/scharr/sections/dts/ctru), part of the [School of Health and Related Research (ScHARR)](http://www.sheffield.ac.uk/scharr) at [The University of Sheffield](http://www.sheffield.ac.uk/).  The intention is to share code between colleagues so that common repetitive tasks become trivial and we do not spend time solving the same problems.

## Installation and Usage

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

Once you've got a GitHub account you need to [fork](https://guides.github.com/activities/forking/) the [`ns-ctru/ctru` repository](https://github.com/ns-ctru/ctru), [clone](https://git-scm.com/docs/git-clone) your fork to your computer to work on it, make changes/addition and [push](https://help.github.com/articles/pushing-to-a-remote/) them back to your fork then make a make [pull requests](https://git-scm.com/docs/git-pull).

### SSH Keys

I would advocate using [SSH Keys](https://help.github.com/articles/generating-an-ssh-key/) with your GitHub account to make it easy to push updates without having to enter your password every single time.

## Functions

### `read_prospect()`

* Function to facilitate reading and labelling of data exported from as plain text files from the CTRU 'bespoke' database [Prospect](https://www.ctru-prospect.shef.ac.uk/).
* Uses the exported `Lookups.csv` to convert all factor variables to the correct encoding.
* Unfortunately it can't recreate the relational nature of the data that exists within the database from which it has been exported :-/.

#### ToDo

* Add functionality to download 'Fields' and 'Froms' tabs from DM Googlesheets using either [googlesheets](https://cran.r-project.org/web/packages/googlesheets/index.html).

### `recruitment()`

* Function to summarise screening and recruitment(/enrolment) to studies.
* Produces tables and figures overall and by study site.

#### ToDo

* Possibly add option to summarise by treatment arm too.

### `table_summary()`

* Function to summarise specified measurements by specified subset.
* N/Mean/SD/Min/Max/Median/IQR reported for specified variables for the specified grouping.

#### ToDo

* Full support for Non-Standard Evaluation when explicitly supplying grouing variables as an argument rather than `...`.

### `plot_summary()`

* Function to plot specified measurements by specified subset.
* Produces histograms by specified treatment groups for continuous variables.
* Produces bar charts by specified treatment groups for factor variables.
* Pooled plots are produced and optionally individual plots for each variable can be produced.

#### ToDo

* For factor variables need to group responses into surveys and `facet_grid()` them with rows for surveys and columns for the specified groups.
* Extend factor summaries to be performed by specified events.
* Finish off plotting continuous variables by variable (rows) and event (columns).

### `idm_lsoa()`

* Function to combine [Lower Super Output Area (LSOA) level Index of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015) statistics with an arbitrary user specified data frame based on 2011 postcode.  Provides the overall IMD score and each component as absolute numbers and deciles as well as the ranking of all scores and components across England.

#### ToDo

* Add in 2010 data.
* Add in data on LSOAs in Wales.

### `eq5d_score()`

* Function for calculating EQ5D-5L (see [slide 40 and 41](http://www.slideshare.net/OHENews/ohe-seminar-5ll-value-set-oct2014-revised-jun15) for scoring).  Could possibly have it summarise and plot scores by user-specified variable (default being the event and the group)

#### ToDo

* Very much a work in progress, need to fully understand [Non-Standard Evaluation](http://dplyr.tidyverse.org/articles/programming.html) to get the function working and fully flexible.


### `regress_ctru()`

#### ToDo

* Include options to set the reference level (via `relevel()`)for each factor variable in a model (something akin to the way `texreg()` handles things).
* Option (default) to exponentiate model coefficients and CIs when link function is `binomial`.
* Include ability to bootstrap regression results, particularly important for mixed models where p-values are unreliable due to uncertainty in the degrees of freedom.  Some leverage to do this via `texreg()` but `stargazer()` is a more flexible tabulating option.
* Include all results from ITT/PP models, coefficients and CIs, p-values as part fo the returned list which can then be parsed for inclusion in text.
