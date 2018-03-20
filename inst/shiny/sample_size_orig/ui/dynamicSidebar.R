## See the following resources...
##
## https://shiny.rstudio.com/articles/dynamic-ui.html
## https://stackoverflow.com/questions/45804944/r-shiny-create-menuitem-after-object-is-created-button-clicked

## Method 1 : conditionalPanel()
## Working  : FALSE
## Comments : This doesn't work, or at least I couldn't get it to, when wrapped with shinydashboard()
sliderInput(inputId = "power",
            label   = "Power (1 - Beta)",
            value   = 0.9,
            min     = 0,
            max     = 1,
            step    = 0.01,
            round   = FALSE,
            ticks   = TRUE),
sliderInput(inputId = "sig.level",
            label   = "Significance Level",
            value   = 0.01,
            min     = 0.00001,
            max     = 0.1,
            step    = 0.00001,
            round   = FALSE,
            ticks   = TRUE),
selectInput(inputId  = "package",
            label    = "Package",
            selected = "pwr",
            choices  = c("pwr" = "pwr",
                         "samplesize"   = "samplesize",
                         "TrialSize"    = "TrialSize",
                         "clusterPower" = "clusterPower",
                         "longpower"    = "longpower",
                         "PowerTOST"    = "PowerTOST")),
## Conditional on the package chosen the options of which functions
## to use from that package are now displayed
conditionalPanel(
    condition = "input.package == pwr",
    selectInput(inputId  = "test",
                label    = "Function from pwr package",
                selected = NULL,
                choices  = c("One-sample Proportion"                            = "pwr.p.test",
                             "Two-sample Proportion (equal size)"               = "pwr.2p.test",
                             "Two-sample Proportion (unequal size)"             = "pwr.2p2n.test",
                             "T-test (One, Two and Paired sample, equal size)"  = "pwr.t.test",
                             "T-test (Two sample, unequal size)"                = "pwr.t2n.test",
                             "ANOVA (One-way balanched)"                        = "pwr.anova.test",
                             "Correlation Test"                                 = "pwr.r.test",
                             "Chi-squared Test"                                 = "pwr.chisq.test",
                             "General Linear Model"                             = "pwr.f2.test"))
),
conditionalPanel(
    condition = "input.package == samplesize",
    selectInput(inputId  = "test",
                label    = "Function from samplesize package",
                selected = NULL,
                choices  = c("T-Test of Means" = "n.ttest",
                             "Wilcoxon-Mann-Whitney for Ordinal Data"))
)

## Method 2 : renderUI
## Working  :
## Comment  :
