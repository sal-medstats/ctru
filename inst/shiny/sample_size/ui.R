## Header
header <- dashboardHeader(title = "Fuel Efficency")
## Sidebar
side <- dashboardSidebar(
                sidebarMenu(
                    menuItem("About",        tabName = "about",     icon = icon("dashboard")),
                    menuItem("pwr",          tabName = "pwr",     icon = icon("dashboard")),
                    menuItem("clusterPower", tabName = "clusterPower", icon = icon("dashboard"))
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
                                 " and is in essence simply a graphical wrapper for the various packages used.")
                                 )
                        ),
                tabItem(tabName = "pwr",
                        h2("pwr"),
                        fluidRow(width = 12,
                                 p("Perform sample size calculations using the ",
                                   a(href = "https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html",
                                     "pwr"),
                                   " package."),
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
                                             )
                                )
                        ),
                tabItem(tabName = "clusterPower",
                        h2("clusterPower"),
                        fluidRow(width = 12,
                                 p("")
                                )
                        )
            )
)

ui <- dashboardPage(header,
                    side,
                    body)
