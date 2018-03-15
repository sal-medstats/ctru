server <- function(input, output, session){
    ## pwr calculations
    output$pwr <- reactive({
        if(input$test == "pwr.p.test"){
            results <- pwr::pwr.p.test(h           = input$h,
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       alternative = input$alternative)
        }
        else if(input$test == "pwr.2p.test"){
            results <- pwr::pwr.2p.test(h           = input$h,
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       alternative = input$alternative)

        }
        else if(input$test == "pwr.2p2n.test"){
            results <- pwr::pwr.2p2n.test(h           = input$h,
                                          power       = input$power,
                                          sig.level   = input$sig.level,
                                          alternative = input$alternative)

        }
        else if(input$test == "pwr.t.test"){
            results <- pwr::pwr.t.test(d           = input$h,
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       type        = input$type,
                                       alternative = input$alternative)
        }
        else if(input$test == "pwr.anova.test"){
            results <- pwr::pwr.p.test(f           = input$h,
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       alternative = input$alternative)
        }
        else if(input$test == "pwr.r.test"){
            results <- pwr::pwr.p.test(r           = input$r,
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       alternative = input$alternative)
        }
        else if(input$test == "pwr.chisq.test"){
            results <- pwr::pwr.chisq.test(w           = input$r,
                                           power       = input$power,
                                           sig.level   = input$sig.level,
                                           alternative = input$alternative)
        }
        else if(input$test == "pwr.f2.test"){
            results <- pwr::pwr.chisq.test(f           = input$r,
                                           power       = input$power,
                                           sig.level   = input$sig.level,
                                           alternative = input$alternative)
        }
        ceiling(results$n)
    })
    ## samplesize calculations
    output$samplesize <- reactive({

    })
    ## TrialSize calculations
    output$TrialSize <- reactive({

    })
    ## clusterPower calculations
    output$clusterPower <- reactive({

    })
    ## longpower calculations
    output$longpower <- reactive({

    })
    ## powerTOST calculations
    output$powerTOST <- reactive({

    })
}
