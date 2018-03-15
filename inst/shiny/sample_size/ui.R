## Header
header <- dashboardHeader(title = "Sample Size")
## Sidebar
side <- dashboardSidebar(
                sidebarMenu(
                    menuItem("About",        tabName = "about",        icon = icon("dashboard")),
                    menuItem("pwr",          tabName = "pwr",          icon = icon("dashboard")),
                    menuItem("samplesize",   tabName = "samplesize",   icon = icon("dashboard")),
                    menuItem("TrialSize",    tabName = "TrialSize",    icon = icon("dashboard")),
                    menuItem("clusterPower", tabName = "clusterPower", icon = icon("dashboard")),
                    menuItem("longpower",    tabName = "longpower",    icon = icon("dashboard")),
                    menuItem("PowerTOST",    tabName = "PowerTOST",    icon = icon("dashboard"))
                )
)
## Body
body <- dashboardBody(
            tabItems(
                tabItem(tabName = "about",
                        h2("About"),
                        fluidRow(width = 12,
                                 p("This site allows you to compute sample sizes for lots of study designs.  It uses a number of packages that are listed on the",
                                 a(href = "https://cran.r-project.org/web/views/ClinicalTrials.html",
                                   "CRAN Clinical Trials TaskView"),
                                 " and is in essence simply a graphical wrapper for the various packages used.  For now only point estimates of the estimated power based on the supplied parameters are provided but in due course the site will be extended and will perform calculations around the parameters estimated and plot these graphically since there is very often uncertainty around the estimates of effect sizes and the obtained sample size will often differ from that desired due to missing data (whether thats failure to recruit sufficient participants or loss to follow-up)."),
                                 p("Many of these calculations over-simplify the analysis, assuming a direct test of proportions, or comparison of means is made (some of the provided calculators account for study designs such as clustering and/or longitudinal analyses).  Very often analyses are far more sophisticated and include adjustments for covariates and in doing so the power of the test performed is altered.  Users interested in a more complete and rounded approach to sample size calculations that accounts for this may be interested in simulation and are pointed towards the excellent package ",
                                   a(href = "https://cran.r-project.org/web/packages/simglm/index.html",
                                     "simglm"),
                                   " (",
                                   a(href = "https://github.com/lebebr01/simglm",
                                     "GitHub"),
                                   ") which allows a more complete specification of many study designs and performs simulations to determine the power of a given sample size.  It includes a Shiny WebUI application to ease the process of specifying models.")
                                 )
                        ),
                tabItem(tabName = "pwr",
                        h2("pwr"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/pwr/",
                                     "pwr"),
                                   " package.  Users would likely benefit from reading the ",
                                   a(href = "https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html",
                                     "package vignette"),
                                   "."),
                                 selectInput(inputId = "test",
                                             label   = "Test",
                                             choices = c("One-sample Proportion"                            = "pwr.p.test",
                                                         "Two-sample Proportion (equal size)"               = "pwr.2p.test",
                                                         "Two-sample Proportion (unequal size)"             = "pwr.2p2n.test",
                                                         "T-test (One, Two and Paired sample, equal size)"  = "pwr.t.test",
                                                         "T-test (Two sample, unequal size)"                = "pwr.t2n.test",
                                                         "ANOVA (One-way balanched)"                        = "pwr.anova.test",
                                                         "Correlation Test"                                 = "pwr.r.test",
                                                         "Chi-squared Test"                                 = "pwr.chisq.test",
                                                         "General Linear Model"                             = "pwr.f2.test"
                                                         )
                                             ),
                                 selectInput(inputId = "alternative",
                                             label   = "Alternative Hypothesis",
                                             choices = c("Two-Sided"   = "two.sided",
                                                         "Less"        = "less",
                                                         "Greater"     = "greater")),
                                 sliderInput(inputId = "h",
                                             label   = "Effect Size",
                                             value   = 0.1,
                                             min     = 0,
                                             max     = 1,
                                             step    = 0.01,
                                             round   = FALSE,
                                             format  = NULL,
                                             ticks   = TRUE),
                                 sliderInput(inputId = "power",
                                             label   = "Power",
                                             value   = 0.9,
                                             min     = 0,
                                             max     = 1,
                                             step    = 0.01,
                                             round   = FALSE,
                                             format  = NULL,
                                             ticks   = TRUE),
                                 sliderInput(inputId = "sig.level",
                                             label   = "Significance Level",
                                             value   = 0.01,
                                             min     = 0.00001,
                                             max     = 0.1,
                                             step    = 0.00001,
                                             round   = FALSE,
                                             format  = NULL,
                                             ticks   = TRUE)
                                )
                        ),
                tabItem(tabName = "samplesize",
                        h2("samplesize"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/samplesize/",
                                     "samplesize"),
                                   " package.")
                                )
                        ),
                tabItem(tabName = "TrialSize",
                        h2("TrialSize"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/TrialSize/",
                                     "TrialSize"),
                                   " package.")
                                )
                        ),
                tabItem(tabName = "clusterPower",
                        h2("clusterPower"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/clusterPower/",
                                     "clusterPower"),
                                   " package.")
                                )
                        ),
                tabItem(tabName = "longpower",
                        h2("longpower"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/longpower/",
                                     "longpower"),
                                   " package.")
                                )
                        ),
                tabItem(tabName = "PowerTOST",
                        h2("PowerTOST"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/PowerTOST/",
                                     "PowerTOST"),
                                   " package.")
                                )
                        )
            )
)

ui <- dashboardPage(header,
                    side,
                    body)


## Generic sample code to copy and paste into sections above
## sliderInput(inputId = "",
##             label   = "",
##             value   = ,
##             min     = ,
##             max     = ,
##             step    = ,
##             round   = FALSE,
##             format  = NULL,
##             ticks   = TRUE)
## selectInput(inputId  = "",
##             label    = "",
##             selected = NULL,
##             choices  = c("" = "",
##                          "" = "",
##                          "" = ""))
