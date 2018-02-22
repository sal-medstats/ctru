server <- function(input, output, session){
    ## pwr calculations
    output$pwr <- reactive({
        if(input$test == "pwr.p.test"){

        }
        else if(input$test == "pwr.2p.test"){

        }
        else if(input$test == "pwr.2pn.test"){

        }
        else if(input$test == "pwr.t.test"){

        }
        else if(input$test == "pwr.anova.test"){

        }
        else if(input$test == "pwr.r.test"){

        }
        else if(input$test == "pwr.chisq.test"){

        }
        else if(input$test == "pwr.f2.test"){

        }
    })
    ## clusterPower calculations
    output$clusterPower <- reactive({

    })
}
