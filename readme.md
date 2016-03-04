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

You can now use the functions `read_prospect()`, fields_prospect()` and so forth.

## Collaborating

To collaborate in this work you will need to install [Git](https://git-scm.com) on your computer and have a [GitHub account](https://www.github.com/join).  If you're not familiar with either of these you may find the tutorial [Conversational Git](http://blog.anvard.org/conversational-git/) a useful place to start.

### Cloning

Once you've got a GitHub account you need to [clone](https://git-scm.com/docs/git-clone) this repository or make [pull requests](https://git-scm.com/docs/git-pull).

### SSH Keys

I would advocate using [SSH Keys](https://help.github.com/articles/generating-an-ssh-key/) with your GitHub account to make it easy to push updates without having to enter your password.


## ToDo

### Functions

* Function to download 'Fields' and 'Froms' tabs from DM Googlesheets using either [googlesheets](https://cran.r-project.org/web/packages/googlesheets/index.html) or [gsheet](https://cran.r-project.org/web/packages/gsheet/index.html).
