require(imp4p)

source(system.file("ProstarApp/server", "mod_DetQuantImpValues.R", package = 'Prostar'), local = TRUE)$value


callModule(moduleMVPlots, "mvImputationPlots_PeptideLevel",
    data = reactive(rv$current.obj),
    title = reactive("POV distribution"),
    pal = reactive(rv$PlotParams$paletteForConditions),
    pattern = c("Missing", "Missing POV", "Missing MEC")
)


mod_DetQuantImpValues_server("peptide_DetQuantValues_DT",
    obj = reactive({rv$current.obj}),
    quant = reactive({rv$widgets$peptideImput$pepLevel_detQuantile}),
    factor = reactive({rv$widgets$peptideImput$pepLevel_detQuant_factor})
)


popover_for_help_server("modulePopover_HelpImputationPeptide",
    title = "Algorithm",
    content = HTML(paste0("<ul><li><strong>imp4p [Ref. 7]</strong>
        a proteomic-specific multiple imputation method that operates on
        peptide-level datasets and which proposes to impute each missing
        value according to its nature (left-censored  or random). To tune
        the number of iterations, let us keep in mind that, the more iterations,
        the more accurate the results, yet the more time-consuming the
        computation.</li> <li><strong>Dummy censored:</strong> each missing
        value is supposed to be a censored value and is replaced by the XXX
        quantile of the corresponding sample abundance distribution <ul><li>
        <strong>KNN </strong>see [Other ref. 2].</li><li><strong>MLE </strong>
        Imputation with the maximum likelihood estimate of the expected
        intensity (see the norm R package).</li></ul></ul>"))
        )



callModule(moduleProcess, "moduleProcess_PepImputation",
    isDone = reactive({
        rvModProcess$modulePepImputationDone
    }),
    pages = reactive({
        rvModProcess$modulePepImputation
    }),
    rstFunc = resetModulePepImputation,
    forceReset = reactive({
        rvModProcess$modulePepImputationForceReset
    })
)


resetModulePepImputation <- reactive({
    ## update widgets values (reactive values)
    resetModuleProcess("PepImputation")

    rv$widgets$peptideImput$pepLevel_algorithm <- "None"
    rv$widgets$peptideImput$pepLevel_basicAlgorithm <- "None"
    rv$widgets$peptideImput$pepLevel_detQuantile <- 2.5
    rv$widgets$peptideImput$pepLevel_detQuant_factor <- 1
    rv$widgets$peptideImput$pepLevel_imp4p_nbiter <- 10
    rv$widgets$peptideImput$pepLevel_imp4p_withLapala <- FALSE
    rv$widgets$peptideImput$pepLevel_imp4p_qmin <- 2.5
    rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib <- "beta"
    rv$widgets$peptideImput$pepLevel_KNN_n <- 10

    rvModProcess$modulePepImputationDone <- rep(FALSE, 2)

    rv$current.obj <- rv$dataset[[input$datasets]]
})



observeEvent(input$peptideLevel_missing.value.algorithm, {
    .var <- input$peptideLevel_missing.value.algorithm
    rv$widgets$peptideImput$pepLevel_algorithm <- .var
})

observeEvent(input$peptideLevel_missing.value.basic.algorithm, {
    .var <- input$peptideLevel_missing.value.basic.algorithm
    rv$widgets$peptideImput$pepLevel_basicAlgorithm <- .var
})

observeEvent(input$peptideLevel_detQuant_quantile, {
    .var <- input$peptideLevel_detQuant_quantile
    rv$widgets$peptideImput$pepLevel_detQuantile <- .var
})

observeEvent(input$peptideLevel_detQuant_factor, {
    .var <- input$peptideLevel_detQuant_factor
    rv$widgets$peptideImput$pepLevel_detQuant_factor <- .var
})

observeEvent(input$KNN_n, {
    rv$widgets$peptideImput$pepLevel_KNN_n <- input$KNN_n
})

observeEvent(input$peptideLevel_imp4p_nbiter, {
    .var <- input$peptideLevel_imp4p_nbiter
    rv$widgets$peptideImput$pepLevel_imp4p_nbiter <- .var
})


observeEvent(input$peptideLevel_imp4p_withLapala, {
    .var <- input$peptideLevel_imp4p_withLapala
    rv$widgets$peptideImput$pepLevel_imp4p_withLapala <- .var
})

observeEvent(input$peptideLevel_imp4p_qmin, {
    .var <- input$peptideLevel_imp4p_qmin
    rv$widgets$peptideImput$pepLevel_imp4p_qmin <- .var
})

observeEvent(input$peptideLevel_imp4pLAPALA_distrib, {
    .var <- input$peptideLevel_imp4pLAPALA_distrib
    rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib <- .var
})



##########
#####  UI for the PEPTIDE LEVEL Imputation process
##########
output$screenPepImputation1 <- renderUI({
    # req(rv$current.obj)
    # isolate({
    nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv$current.obj))
    if (nbEmptyLines > 0) {
        tags$p("Your dataset contains empty lines (fully filled with missing
    values). In order to use the imputation tool, you must delete them by
      using the filter tool.")
    } else if (sum(is.na(Biobase::exprs(rv$current.obj))) == 0) {
        tags$p("Your dataset does not contains missing values.")
    } else {
        tabPanel("Miss. values imputation",
            id = "tabPanelImputation",
            value = "imputation",
            tags$div(
                tags$div(
                    style = "display:inline-block; vertical-align: top;
                 padding-right: 20px;",
                    popover_for_help_ui("modulePopover_HelpImputationPeptide"),
                    selectInput("peptideLevel_missing.value.algorithm",
                        NULL,
                        choices = imputationAlgorithms,
                        selected = rv$widgets$peptideImput$pepLevel_algorithm,
                        width = "150px"
                    )
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
          padding-right: 20px;",
                    uiOutput("basicAlgoUI")
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
          padding-right: 20px;",
                    uiOutput("detQuantOptsUI"),
                    uiOutput("KNNOptsUI"),
                    uiOutput("imp4pOptsUI")
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
                 padding-right: 20px;",
                    uiOutput("imp4pOpts2UI")
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
                 padding-right: 20px;",
                    uiOutput("peptideLevel_detQuant_impValues")
                )
            ),
            uiOutput("peptideLevel_warningImputationMethod"),
            tags$div(
                tags$div(
                    style = "display:inline-block; vertical-align: top;
                 padding-right: 20px;",
                    actionButton("peptideLevel_perform.imputation.button",
                        "Perform imputation",
                        class = actionBtnClass
                    )
                )
            ),
            tagList(
                tags$hr(),
                withProgress(message = "", detail = "", value = 0, {
                    incProgress(0.5, detail = "Aggregation in progress")
                    moduleMVPlotsUI("mvImputationPlots_PeptideLevel")
                })
            )
        )
    }
})



output$screenPepImputation2 <- renderUI({
    tagList(
        actionButton("peptideLevel_ValidImputation",
            "Save imputation",
            class = actionBtnClass
        )
    )
})



output$basicAlgoUI <- renderUI({
    if (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods") {
        return(NULL)
    }

    selectInput("peptideLevel_missing.value.basic.algorithm",
        "Methods",
        width = "150px",
        choices = basicMethodsImputationAlgos,
        selected = rv$widgets$peptideImput$pepLevel_basicAlgorithm
    )
})


output$detQuantOptsUI <- renderUI({
    req(rv$widgets$peptideImput$pepLevel_basicAlgorithm)
    req(rv$widgets$peptideImput$pepLevel_algorithm)
    if ((rv$widgets$peptideImput$pepLevel_basicAlgorithm != "detQuantile") ||
        (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods")) {
        return(NULL)
    }

    tagList(
        tags$div(
            style = "display:inline-block; vertical-align: top;
      padding-right: 20px;",
            numericInput("peptideLevel_detQuant_quantile", "Quantile",
                value = rv$widgets$peptideImput$pepLevel_detQuantile,
                step = 1, min = 0, max = 100,
                width = "100px"
            )
        ),
        tags$div(
            style = "display:inline-block; vertical-align: top;
      padding-right: 20px;",
            numericInput("peptideLevel_detQuant_factor", "Factor",
                value = rv$widgets$peptideImput$pepLevel_detQuant_factor,
                step = 1, min = 0, max = 10,
                width = "100px"
            )
        )
    )
})


output$KNNOptsUI <- renderUI({
    req(rv$widgets$peptideImput$pepLevel_basicAlgorithm)
    req(rv$widgets$peptideImput$pepLevel_algorithm)
    if ((rv$widgets$peptideImput$pepLevel_basicAlgorithm != "KNN") ||
        (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods")) {
        return(NULL)
    }

    isolate({
        numericInput("KNN_n", "Neighbors",
            value = rv$widgets$peptideImput$pepLevel_KNN_n,
            step = 1, min = 0,
            max = max(rv$widgets$peptideImput$KNN_n, nrow(rv$current.obj)),
            width = "100px"
        )
    })
})


output$imp4pOptsUI <- renderUI({
    if (rv$widgets$peptideImput$pepLevel_algorithm != "imp4p") {
        return(NULL)
    }

    updateSelectInput(session, "peptideLevel_missing.value.basic.algorithm",
        selected = "None"
    )
    tagList(
        tags$div(
            style = "display:inline-block; vertical-align: top;
      padding-right: 40px;",
            numericInput("peptideLevel_imp4p_nbiter", "Iterations",
                value = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                step = 1, min = 1, width = "100px"
            )
        ),
        tags$div(
            style = "display:inline-block; vertical-align: bottom;
      padding-right: 20px;",
            checkboxInput("peptideLevel_imp4p_withLapala", "Impute MEC also",
                value = rv$widgets$peptideImput$pepLevel_imp4p_withLapala
            )
        )
    )
})


output$imp4pOpts2UI <- renderUI({
    if (!isTRUE(rv$widgets$peptideImput$pepLevel_imp4p_withLapala)) {
        return(NULL)
    }


    tagList(
        tags$div(style = "display:inline-block; vertical-align: top;
      padding-right: 20px;",
            numericInput("peptideLevel_imp4p_qmin", "Upper lapala bound",
                value = rv$widgets$peptideImput$pepLevel_imp4p_qmin,
                step = 0.1, min = 0, max = 100,
                width = "100px"
            )
        ),
        tags$div(style = "display:inline-block; vertical-align: top;
      padding-right: 20px;",
            radioButtons("peptideLevel_imp4pLAPALA_distrib",
                "Distribution type",
                choices = G_imp4PDistributionType_Choices,
                selected = rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib
            )
        )
    )
})



output$peptideLevel_detQuant_impValues <- renderUI({
    req(rv$widgets$peptideImput$pepLevel_basicAlgorithm)
    req(rv$widgets$peptideImput$pepLevel_algorithm)
    if ((rv$widgets$peptideImput$pepLevel_basicAlgorithm != "detQuantile") ||
        (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods")) {
        return(NULL)
    }


    mod_DetQuantImpValues_ui("peptide_DetQuantValues_DT")
})

output$peptideLevel_TAB_detQuant_impValues <- DT::renderDataTable(server = TRUE, {
    values <- getQuantile4Imp(
      Biobase::exprs(rv$current.obj),
        rv$widgets$peptideImput$pepLevel_detQuantile / 100,
        rv$widgets$peptideImput$pepLevel_detQuant_factor
    )
    DT::datatable(round(as.data.frame(t(values$shiftedImpVal)),
        digits = rv$settings_nDigits
    ),
    extensions = c("Scroller"),
    options = list(
        initComplete = initComplete(),
        dom = "frtip",
        bLengthChange = FALSE
    )
    )
})



#
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_perform.imputation.button, {
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DAPAR::GetTypeofData(rv$current.obj)
    )
    nbMVBefore <- length(which(m))
    .widget <- rv$widgets$peptideImput

    algo <- rv$widgets$peptideImput$pepLevel_algorithm
    
    
    .tmp <- NULL
    
    .tmp <- try({
    if (algo == "None") {
        .tmp <- rv$dataset[[input$datasets]]
    } else {
        withProgress(message = "", detail = "", value = 0, {
            incProgress(0.5, detail = "Imputation in progress")
            if (algo == "imp4p") {
                if (.widget$pepLevel_imp4p_withLapala) {
                    .distrib <- .widget$pepLevel_imp4pLAPALA_distrib
                    .tmp <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                    nb.iter = .widget$pepLevel_imp4p_nbiter,
                                                    lapala = .widget$pepLevel_imp4p_withLapala,
                                                    q.min = .widget$pepLevel_imp4p_qmin / 100,
                                                    distribution = as.character(.distrib)
                                                    )
                } else {
                    .tmp <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                    nb.iter = .widget$pepLevel_imp4p_nbiter,
                                                    lapala = .widget$pepLevel_imp4p_withLapala
                    )
                    }
            } else if (algo == "BasicMethods") {
                algoBasic <- .widget$pepLevel_basicAlgorithm
                switch(algoBasic,
                    KNN = {
                        .tmp <- wrapper.impute.KNN(rv$dataset[[input$datasets]],
                                                   K = .widget$pepLevel_KNN_n)
                        },
                    MLE = {
                        .tmp <- wrapper.impute.mle(
                            obj = rv$dataset[[input$datasets]])
                        },
                    detQuantile = {
                        .tmp <- wrapper.impute.detQuant(
                            rv$dataset[[input$datasets]],
                            qval = (.widget$pepLevel_detQuantile / 100),
                            factor = .widget$pepLevel_detQuant_factor,
                            na.type = "Missing POV"
                        )
                    }
                )
            }
            incProgress(1, detail = "Finalize imputation")
        })
    }

    .tmp
    })

    if(inherits(.tmp, "try-error")) {
       
        mod_SweetAlert_server(id = 'sweetalert_peptideLevel_perform_imputation_button',
                              text = .tmp[[1]],
                              type = 'error' )
        
        
    } else {
      rv$current.obj <- .tmp
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DAPAR::GetTypeofData(rv$current.obj)
    )
    nbMVAfter <- length(which(m))
    rv$nbMVimputed <- nbMVAfter - nbMVBefore
    rvModProcess$modulePepImputationDone[1] <- TRUE
    }
})






##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_ValidImputation, {
    isolate({
        l.params <- build_ParamsList_PepImputation()

        name <- paste0("Imputed", ".", rv$typeOfDataset)
        rv$current.obj <- saveParameters(
            rv$current.obj, name,
            "peptideImputation", l.params
        )

        rvModProcess$modulePepImputationDone[2] <- TRUE
        UpdateDatasetWidget(rv$current.obj, name)
    })
})





output$peptideLevel_warningImputationMethod <- renderText({
    req(rv$widgets$peptideImput$pepLevel_algorithm != "None")
    rv$widgets$peptideImput$pepLevel_imp4p_withLapala

    algo <- rv$widgets$peptideImput$pepLevel_algorithm
    withMEC <- rv$widgets$peptideImput$pepLevel_imp4p_withLapala

    t <- switch(algo,
        imp4p = {
            if (isFALSE(withMEC)) {
                "<font color='red'>Please note that aggregation of peptides
              won't be possible if MEC (Missing on the Entire Condition)
              data aren't imputed. To do so, check 'Impute MEC also' option.
           </font color='red'>"
            } else {
                "<font color='red'><strong>Warning:</strong> Imputed MEC
               (Missing on the Entire Condition)
                values must be very cautiously interpreted <br>[see the
           User manual, Section 6.3.1].</font color='red'>"
            }
        },
        BasicMethods = "<font color='red'>Please note that none of these
         'Basic methods' impute the MEC (Missing on the Entire Condition) and
         aggregation of peptides won't be possible if MEC data aren't imputed.
          <br>To do so, please choose the 'imp4p' algorithm as imputation
    method and check 'Impute MEC also' option.</font color='red'>"
    )

    HTML(t)
})




###################


popover_for_help_server("modulePopover_helpForImputation",
    title = p(if (is.null(rv$current.obj.name)) {
                "No dataset"
            } else {
                paste0(rv$current.obj.name)
            }),
     content = "Before each processing step, a backup of the
            current dataset is stored. It is possible to reload one of them
          at any time.",
            color = "white"
        )


