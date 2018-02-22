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
                                 a(href="https://cran.r-project.org/web/views/ClinicalTrials.html",
                                   "CRAN Clinical Trials TaskView"),
                                 " and is in essence simply a graphical wrapper for the various packages used.")
                                 )
                        ),
                tabItem(tabName = "pwr",
                        h2("pwr"),
                        fluidRow(width = 12,
                                 p("")
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
