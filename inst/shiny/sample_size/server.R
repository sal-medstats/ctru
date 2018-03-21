server <- function(input, output, session){
    ## Perform the calculations based on the provided parameters
    results <- reactive({
        tidy_results <- list()
        ## pwr
        if(input$test == "pwr.p.test"){
            results <- pwr::pwr.p.test(h           = ES.h(input$p1, input$p2),
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       alternative = input$alternative)
        }
        else if(input$test == "pwr.2p.test"){
            results <- pwr::pwr.2p.test(h           = ES.h(input$p1, input$p2),
                                        power       = input$power,
                                        sig.level   = input$sig.level,
                                        alternative = input$alternative)
        }
        else if(input$test == "pwr.2p2n.test"){
            results <- pwr::pwr.2p2n.test(h           = ES.h(input$p1, input$p2),
                                          n1          = input$n1,
                                          power       = input$power,
                                          sig.level   = input$sig.level,
                                          alternative = input$alternative)
        }
        else if(input$test == "pwr.t.test"){
            results <- pwr::pwr.t.test(d           = input$d,
                                       power       = input$power,
                                       sig.level   = input$sig.level,
                                       type        = input$type,
                                       alternative = input$alternative)
        }
        else if(input$test == "pwr.t2n.test"){
            results <- pwr::pwr.t.test(d           = input$d,
                                       n1          = input$n1,
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
    ##     ## ToDo - Add in conditional statements for all functions from other packages
    ##     ## samplesize
    ##     else if(input$test == "n.ttest"){
    ##         results <- samplesize::n.ttest()
    ##     }
    ##     else if(input$test == "n.wilcox.ord"){
    ##         results <- samplesize::n.wilcox.ord()
    ##     }
    ##     ## TrialSize
    ##     else if(input$test == "OneWayANOVA.pairwise"){
    ##         results <- TrialSize::OneWayANOVA.pairwise()
    ##     }
    ##     else if(input$test == "OneWayANOVA.pairwiseComparison"){
    ##         results <- TrialSize::OneWayANOVA.pairwiseComparison()
    ##     }
    ##     else if(input$test == "ANOVA.Repeat.Measure"){
    ##         results <- TrialSize::OneWayANOVA.pairwise()
    ##     }
    ##     else if(input$test == "ABE"){
    ##         results <- TrialSize::ABE()
    ##     }
    ##     else if(input$test == "Cox.Equality"){
    ##         results <- TrialSize::Cox.Equality()
    ##     }
    ##     else if(input$test == "Cox.Equivalence"){
    ##         results <- TrialSize::Cox.Equivalence()
    ##     }
    ##     else if(input$test == "Cox.NIS"){
    ##         results <- TrialSize::Cox.NIS()
    ##     }
    ##     else if(input$test == "OneSampleMean.Equality"){
    ##         results <- TrialSize::OneSampleMean.Equality()
    ##     }
    ##     else if(input$test == "OneSampleMean.Equivalence"){
    ##         results <- TrialSize::OneSampleMean.Equivalence()
    ##     }
    ##     else if(input$test == "OneSampleMean.NIS"){
    ##         results <- TrialSize::OneSampleMean.NIS()
    ##     }
    ##     else if(input$test == "OneSampleProportion.Equality"){
    ##         results <- TrialSize::OneSampleProportion.Equality()
    ##     }
    ##     else if(input$test == "OneSampleProportion.Equivalence"){
    ##         results <- TrialSize::OneSampleProportion.Equivalence()
    ##     }
    ##     else if(input$test == "OneSampleProportion.NIS"){
    ##         results <- TrialSize::OneSampleProportion.NIS()
    ##     }
    ##     ## clusterPower
    ##     else if(input$test == "crtpwr.2mean"){
    ##         results <- clusterPower::crtpwr.2mean()
    ##     }
    ##     else if(input$test == "crtpwr.2prop"){
    ##         results <- clusterPower::crtpwr.2prop()
    ##     }
    ##     else if(input$test == "crtpwr.2rate"){
    ##         results <- clusterPower::crtpwr.2rate()
    ##     }
    ##     else if(input$test == "hayes.power.poisson"){
    ##         results <- clusterPower::hayes.power.poisson()
    ##     }
    ##     ## longpower
    ##     else if(input$test == "diggle.linear.power"){
    ##         results <- longpower::diggle.linear.power()
    ##     }
    ##     else if(input$test == "edland.linear.power"){
    ##         results <- longpower::edland.linear.power()
    ##     }
    ##     else if(input$test == "liu.liang.linear.power"){
    ##         results <- longpower::liu.liang.linear.power()
    ##     }
    ##     ## PowerTOST
    ##     ## ToDo - Add functions from the PowerTOST package
    ##     ## else if(input$test == ""){
    ##     ##     ...
    ##     ## }
    ##     ##
        ## Standardise the output conditional on the package that has been used
        ## and what is returned
        if(input$pkg == "pwr"){
            tidy_results$pkg    <- "pwr"
            tidy_results$n      <- results$n
            if(input$test %in% c("pwr.p.test", "pwr.2p.test", "pwr.2p2n.test" )){
                tidy_results$effect <- results$h
            }
            else if(input$test %in% c("pwr.t.test", "pw2.2tb.test")){
                tidy_results$effect <- results$d
            }
            tidy_results$power  <- results$power
            tidy_results$alpha  <- results$sig.level
            tidy_results$method <- results$alterantive
            ## Unequal sample sizes need different handling
            if(input$test %in% c("pwr.2p2n.test", "pwr.t2n.test")){
                tidy_results$n1 <- input$n1
                tidy_results$n2 <- results$n2
            }
        }
    ##     else if(pkg == "samplesize"){
    ##     }
    ##     else if(pkg == "TrialSize"){
    ##     }
    ##     else if(pkg == "clusterPower"){
    ##     }
    ##     else if(pkg == "longpower"){
    ##     }
    ##     ## ToDo - Add functions from the PowerTOST package
    ##     ## else if(pkg == "PowerTOST"){
    ##     ## }
        tidy_results
    })
    ## Render a Table of the results
    output$table <- renderTable({

    })
    ## Render Boxes
    ## ValueBox : n
    output$pkg <- renderValueBox({
        valueBox(value    = results()$pkg,
                 subtitle = "Package",
                 icon     = icon("users"),
                 width    = 12,
                 color    = "blue")
    })
    ## ValueBox : n
    output$n <- renderValueBox({
        valueBox(value    = formatC(ceiling(results()$n)),
                 subtitle = "Number of participants (per arm)",
                 icon     = icon("users"),
                 width    = 12,
                 color    = "blue")
    })
    ## ValueBox : alpha
    output$alpha <- renderValueBox({
        valueBox(value    = formatC(results()$alpha,
                                    format = "f",
                                    digits = 4),
                 subtitle = "Significance Level",
                 icon     = icon("users"),
                 width    = 12,
                 color    = "green")
    })
    ## ValueBox : power
    output$power <- renderValueBox({
        valueBox(value    = formatC(results()$power,
                                    format = "f",
                                    digits = 4),
                 subtitle = "Power",
                 icon     = icon("users"),
                 color    = "purple")
    })

}
