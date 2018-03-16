server <- function(input, output, session){
    ## pwr calculations
    pwr_results <- reactive({
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
        results
    })
    ## ValueBox : pwr n
    output$pwr_n <- renderValueBox({
        valueBox(value    = formatC(ceiling(pwr_results()$n)),
                 subtitle = "Number of participants (per arm)",
                 icon     = icon("users"),
                 width    = 12,
                 color    = "blue")
    })
    ## ValueBox : pwr alpha
    output$pwr_alpha <- renderValueBox({
        valueBox(value    = formatC(pwr_results()$sig.level,
                                    format = "f",
                                    digits = 4),
                 subtitle = "Significance Level",
                 icon     = icon("users"),
                 width    = 12,
                 color    = "green")
    })
    ## ValueBox : pwr power
    output$pwr_power <- renderValueBox({
        valueBox(value    = formatC(pwr_results()$power,
                                    format = "f",
                                    digits = 4),
                 subtitle = "Power",
                 icon     = icon("users"),
                 color    = "cyan")
    })
    ## samplesize calculations
    output$samplesize <- reactive({
        if(input$test == "n.ttest"){
            results <- samplesize:n.ttest(power     = input$power,
                                          alpha     = input$alpha,
                                          mead.diff = input$mean.diff,
                                          sd1       = input$sd1,
                                          sd2       = input$sd2,
                                          k         = input$k,
                                          design    = input$design,
                                          fraction  = input$fraction,
                                          variance  = input$variance)
        }
        else if(input$test == "n.wilcox.ord"){
            results <- samplesize:n.wilcox.ord()
        }
        results
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
