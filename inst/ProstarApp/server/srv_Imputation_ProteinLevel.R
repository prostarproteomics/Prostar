require(imp4p)
source(system.file("ProstarApp/server", "mod_DetQuantImpValues.R", package = 'Prostar'), local = TRUE)$value


callModule(moduleMVPlots, "mvImputationPlots_MV",
    data = reactive({rv$imputePlotsSteps[["step0"]]}),
    title = reactive("POV distribution"),
    pal = reactive(rv$PlotParams$paletteForConditions),
    pattern = c("Missing", "Missing POV", "Missing MEC")
)

callModule(moduleMVPlots, "mvImputationPlots_MEC",
    data = reactive({rv$imputePlotsSteps[["step1"]]}),
    title = reactive("Distribution after POV imputation"),
    pal = reactive(rv$PlotParams$paletteForConditions),
    pattern = c("Missing", "Missing POV", "Missing MEC")
)

callModule(moduleMVPlots, "mvImputationPlots_Valid",
    data = reactive({rv$imputePlotsSteps[["step2"]]}),
    title = reactive("Distribution after POV and MEC imputation"),
    pal = reactive(rv$PlotParams$paletteForConditions),
    pattern = c("Missing", "Missing POV", "Missing MEC")
)

mod_DetQuantImpValues_server("POV_DetQuantValues_DT",
    obj = reactive({rv$current.obj}),
    quant = reactive({ rv$widgets$proteinImput$POV_detQuant_quantile}),
    factor = reactive({rv$widgets$proteinImput$POV_detQuant_factor})
)

mod_DetQuantImpValues_server("MEC_DetQuantValues_DT",
                             obj = reactive({rv$current.obj}),
                             quant = reactive({rv$widgets$proteinImput$MEC_detQuant_quantile}),
                             factor = reactive({rv$widgets$proteinImput$MEC_detQuant_factor})
                             )


callModule(moduleProcess, "moduleProcess_ProtImputation",
    isDone = reactive({rvModProcess$moduleProtImputationDone}),
    pages = reactive({rvModProcess$moduleProtImputation }),
    rstFunc = resetModuleProtImputation,
    forceReset = reactive({rvModProcess$moduleProtImputationForceReset})
)



resetModuleProtImputation <- reactive({
    ## update widgets values (reactive values)
    resetModuleProcess("ProtImputation")

    rv$widgets$proteinImput$POV_algorithm <- "None"
    rv$widgets$proteinImput$POV_detQuant_quantile <- 2.5
    rv$widgets$proteinImput$POV_detQuant_factor <- 1
    rv$widgets$proteinImput$POV_KNN_n <- 10
    rv$widgets$proteinImput$MEC_algorithm <- "None"
    rv$widgets$proteinImput$MEC_detQuant_quantile <- 2.5
    rv$widgets$proteinImput$MEC_detQuant_factor <- 1
    rv$widgets$proteinImput$MEC_fixedValue <- 0



    rv$current.obj <- rv$dataset[[input$datasets]]

    rv$imputePlotsSteps[["step0"]] <- rv$current.obj
    rvModProcess$moduleProtImputationDone <- rep(FALSE, 3)
})


########
observeEvent(input$POV_missing.value.algorithm, {
    rv$widgets$proteinImput$POV_algorithm <- input$POV_missing.value.algorithm
})

observeEvent(input$MEC_missing.value.algorithm, {
    rv$widgets$proteinImput$MEC_algorithm <- input$MEC_missing.value.algorithm
})

observeEvent(input$POV_detQuant_quantile, {
    rv$widgets$proteinImput$POV_detQuant_quantile <- input$POV_detQuant_quantile
})

observeEvent(input$POV_detQuant_factor, {
    rv$widgets$proteinImput$POV_detQuant_factor <- input$POV_detQuant_factor
})

observeEvent(input$KNN_nbNeighbors, {
    rv$widgets$proteinImput$POV_KNN_n <- input$KNN_nbNeighbors
})

observeEvent(input$MEC_detQuant_quantile, {
    rv$widgets$proteinImput$MEC_detQuant_quantile <- input$MEC_detQuant_quantile
})

observeEvent(input$MEC_fixedValue, {
    rv$widgets$proteinImput$MEC_fixedValue <- input$MEC_fixedValue
})

observeEvent(input$MEC_detQuant_factor, {
    rv$widgets$proteinImput$MEC_detQuant_factor <- input$MEC_detQuant_factor
})
#########



output$screenProtImput1 <- renderUI({
    if (sum(is.na(Biobase::exprs(rv$current.obj))) == 0) {
        tags$p("Your dataset does not contain missing values")
    } else {
        tagList(
            tags$div(
                tags$div(style = "display:inline-block; vertical-align: top;
                  padding-right: 20px;",
                    uiOutput("sidebar_imputation_step1")
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
                  padding-right: 20px;",
                    uiOutput("POV_Params")
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
                  padding-right: 20px;",
                    uiOutput("POV_showDetQuantValues")
                )
            ),
            tags$div(style = "display:inline-block; vertical-align: top;
              padding-right: 20px;",
                actionButton("perform.POVimputation.button",
                    "Perform POV imputation",
                    class = actionBtnClass
                )
            ),
            tags$div(style = "display:inline-block; vertical-align: top;
              padding-right: 20px;",
                uiOutput("ImputationStep1Done")
            ),
            htmlOutput("helpForImputation"),
            tags$hr(),
            moduleMVPlotsUI("mvImputationPlots_MV")
        )
    }
})



output$sidebar_imputation_step1 <- renderUI({
    # req(rv$current.obj)

    # isolate({
    if (length(grep("Imputed", input$datasets)) == 0) {
        rv$imputePlotsSteps[["step0"]] <- rv$dataset[[input$datasets]]
        shinyjs::enable("perform.POVimputation.button")
    } else {
        shinyjs::disable("perform.POVimputation.button")
    }

    algo <- imputationAlgorithmsProteins_POV

    tags$div(
        style = "display:inline-block; vertical-align: top;
      padding-right: 40px;",
        selectInput("POV_missing.value.algorithm", "Algorithm for POV",
            choices = algo,
            selected = rv$widgets$proteinImput$POV_algorithm,
            width = "150px"
        )
    )

    # })
})



output$POV_showDetQuantValues <- renderUI({
    req(rv$widgets$proteinImput$POV_algorithm)

    if (rv$widgets$proteinImput$POV_algorithm == "detQuantile") {
        tagList(
            h5("The POV will be imputed by the following values :"),
            mod_DetQuantImpValues_ui("POV_DetQuantValues_DT")
        )
    }
})



output$POV_Params <- renderUI({
    req(rv$widgets$proteinImput$POV_algorithm)

    isolate({
        .widget <- rv$widgets$proteinImput
        switch(.widget$POV_algorithm,
            detQuantile = {
                tagList(
                    tags$div(
                        style = "display:inline-block; vertical-align: top;
            padding-right: 40px;",
                        numericInput("POV_detQuant_quantile", "Quantile",
                            value = .widget$POV_detQuant_quantile,
                            step = 0.5, min = 0, max = 100, width = "100px"
                        )
                    ),
                    tags$div(
                        style = "display:inline-block; vertical-align: top;
                 padding-right: 40px;",
                        numericInput("POV_detQuant_factor", "Factor",
                            value = .widget$POV_detQuant_factor,
                            step = 0.1, min = 0, max = 10, width = "100px"
                        )
                    )
                )
            },
            KNN = {
                numericInput("KNN_nbNeighbors", "Neighbors",
                    value = .widget$POV_KNN_n, step = 1, min = 0,
                    max = max(nrow(rv$current.obj), .widget$POV_KNN_n),
                    width = "100px"
                )
            }
        )
    })
})





observeEvent(input$perform.POVimputation.button, {
    rv$current.obj
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                        pattern = "Missing POV",
                        level = DAPAR::GetTypeofData(rv$current.obj)
                        )
    nbPOVBefore <- length(which(m))

    withProgress(message = "", detail = "", value = 0, {
        incProgress(0.25, detail = "Find MEC blocks")

        .tmp <- NULL
        .tmp <- try({
          switch(rv$widgets$proteinImput$POV_algorithm,
            None = .tmp <- rv$dataset[[input$datasets]],
            slsa = {
                incProgress(0.5, detail = "slsa Imputation")
                .tmp <- wrapper.impute.slsa(
                    rv$dataset[[input$datasets]])
            },
            detQuantile = {
                incProgress(0.5, detail = "det quantile Imputation")
                .tmp <- wrapper.impute.detQuant(
                    obj = rv$dataset[[input$datasets]],
                    qval = rv$widgets$proteinImput$POV_detQuant_quantile / 100,
                    factor = rv$widgets$proteinImput$POV_detQuant_factor,
                    na.type = 'Missing POV')
            },
            KNN = {
                incProgress(0.5, detail = "KNN Imputation")
                .tmp <- wrapper.impute.KNN(obj = rv$dataset[[input$datasets]],
                                           K = rv$widgets$proteinImput$POV_KNN_n)
            }
        )
        })
      
        if(inherits(.tmp, "try-error")) {
         mod_SweetAlert_server(id = 'sweetalert_perform_POVimputation_button',
                                  text = .tmp[[1]],
                                  type = 'error' )
        } else {
          # sendSweetAlert(
          #   session = session,
          #   title = "Success",
          #   type = "success"
          # )
          rv$current.obj <- .tmp
        # incProgress(0.75, detail = 'Reintroduce MEC blocks')
        incProgress(1, detail = "Finalize POV imputation")
        m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                            pattern = "Missing POV",
                            level = DAPAR::GetTypeofData(rv$current.obj)
                            )
        nbPOVAfter <- length(which(m))
        rv$nbPOVimputed <- nbPOVBefore - nbPOVAfter

        rv$impute_Step <- 1
        rv$imputePlotsSteps[["step1"]] <- rv$current.obj
        rvModProcess$moduleProtImputationDone[1] <- TRUE
        shinyjs::enable("perform.imputationMEC.button")
        shinyjs::enable("ValidImputation")
        }
    })
    # })
})


output$ImputationStep1Done <- renderUI({
    # isolate({

    if (isTRUE(rvModProcess$moduleProtImputationDone[1])) {
        tagList(
            h5(
                paste0("POV imputation done.", rv$nbPOVimputed, " were imputed")
            ),
            h5("Updated graphs can be seen on step \"2 - Missing on the
        Entire Condition\".")
        )
    }
    # })
})


#------------------------------------------------------------
#                             SCREEN 2
#------------------------------------------------------------
output$screenProtImput2 <- renderUI({
    imputationDone <- isTRUE(rvModProcess$moduleProtImputationDone[1])
    
    if (!imputationDone && sum(is.na(Biobase::exprs(rv$current.obj))) == 0) {
        tags$p("Your dataset does not contain any missing values.")
    } else {
        tagList(
            uiOutput("warningMECImputation"),
            tags$div(
                tags$div(
                    style = "display:inline-block; vertical-align: top;
        padding-right: 20px;",
                    uiOutput("MEC_chooseImputationMethod")
                ),
                tags$div(
                    style = "display:inline-block; vertical-align: top;
        padding-right: 20px;",
                    uiOutput("MEC_Params")
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
        padding-right: 20px;",
                    uiOutput("MEC_showDetQuantValues")
                )
            ),
            tagList(
                tags$div(style = "display:inline-block; vertical-align: top;
        padding-right: 20px;",
                    actionButton("perform.imputationMEC.button",
                        "Perform imputation",
                        class = actionBtnClass
                    )
                ),
                tags$div(style = "display:inline-block; vertical-align: top;
        padding-right: 20px;",
                    uiOutput("ImputationStep2Done")
                )
            ),
            tags$hr(),
            withProgress(message = "", detail = "", value = 0, {
                incProgress(0.5, detail = "Building plots...")

                moduleMVPlotsUI("mvImputationPlots_MEC")
            })
        )
    }
})




output$MEC_showDetQuantValues <- renderUI({
    req(rv$widgets$proteinImput$MEC_algorithm)

    if (rv$widgets$proteinImput$MEC_algorithm == "detQuantile") {
        tagList(
            h5("The MEC will be imputed by the following values :"),
            mod_DetQuantImpValues_ui("MEC_DetQuantValues_DT")
        )
    }
})



observeEvent(rv$widgets$proteinImput$MEC_algorithm, {
    if (rv$widgets$proteinImput$MEC_algorithm == "None") {
        rv$current.obj <- rv$dataset[[input$datasets]]
        return(NULL)
    }
})


output$MEC_chooseImputationMethod <- renderUI({
    algo <- imputationAlgorithmsProteins_MEC

    tags$div(
        style = "display:inline-block; vertical-align: top;
    padding-right: 40px;",
        selectInput("MEC_missing.value.algorithm", "Algorithm for MEC",
            choices = algo,
            selected = rv$widgets$proteinImput$MEC_algorithm, width = "150px"
        )
    )
})



output$MEC_Params <- renderUI({
    req(rv$widgets$proteinImput$MEC_algorithm)
    isolate({
        .widget <- rv$widgets$proteinImput
        switch(.widget$MEC_algorithm,
            detQuantile = {
                tagList(
                    tags$div(style = "display:inline-block; vertical-align: top;
            padding-right: 40px;",
                        numericInput("MEC_detQuant_quantile", "Quantile",
                                     value = .widget$MEC_detQuant_quantile,
                                     step = 0.5,
                                     min = 0,
                                     max = 100,
                                     width = "100px"
                                     )
                    ),
                    tags$div(style = "display:inline-block; vertical-align: top;
            padding-right: 40px;",
                        numericInput("MEC_detQuant_factor", "Factor",
                            value = .widget$MEC_detQuant_factor,
                            step = 0.1, min = 0, max = 10,
                            width = "100px"
                        )
                    )
                )
            },
            fixedValue = {
                numericInput("MEC_fixedValue", "Fixed value",
                             value = .widget$MEC_fixedValue,
                             step = 0.1, min = 0, max = 100,
                             width = "100px"
                             )
            }
        )
    })
})




observeEvent(input$perform.imputationMEC.button, {
    rv$current.obj
    # isolate({
    withProgress(message = "", detail = "", value = 0, {
        incProgress(0.25, detail = "Reintroduce MEC")

        m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
            pattern = "Missing MEC",
            level = DAPAR::GetTypeofData(rv$current.obj)
        )
        nbMECBefore <- length(which(m))
        incProgress(0.75, detail = "MEC Imputation")

        .tmp <- NULL
        .tmp <- try({
          .tmp <- switch(rv$widgets$proteinImput$MEC_algorithm,
            detQuantile = {
                wrapper.impute.detQuant(rv$current.obj,
                    qval = rv$widgets$proteinImput$MEC_detQuant_quantile / 100,
                    factor = rv$widgets$proteinImput$MEC_detQuant_factor,
                    na.type = "Missing MEC"
                )
            },
            fixedValue = {
                wrapper.impute.fixedValue(rv$current.obj,
                    fixVal = rv$widgets$proteinImput$MEC_fixedValue,
                    na.type = "Missing MEC"
                )
            }
        )
          .tmp
        })

        if(inherits(.tmp, "try-error")) {
            mod_SweetAlert_server(id = 'sweetalert_perform_imputationMEC_button',
                                  text = .tmp[[1]],
                                  type = 'error' )
        } else {
          
          rv$current.obj <- .tmp
        m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
            pattern = "Missing MEC",
            level = DAPAR::GetTypeofData(rv$current.obj)
        )
        nbMECAfter <- length(which(m))
        rv$nbMECimputed <- nbMECBefore - nbMECAfter

        incProgress(1, detail = "Finalize MEC imputation")
        rv$impute_Step <- 2
        rv$imputePlotsSteps[["step2"]] <- rv$current.obj
        rvModProcess$moduleProtImputationDone[2] <- TRUE
        }
    })
    # })
})


output$ImputationStep2Done <- renderUI({
    # isolate({
    if (isTRUE(rvModProcess$moduleProtImputationDone[2])) {
        tagList(
            h5("MEC imputation done.", rv$nbMECimputed, " were imputed"),
            h5("Updated graphs cans be seen on step \"3 - Save\".")
        )
    }
    # })
})

output$warningMECImputation <- renderUI({
    tags$p(tags$b("Warning:"), "Imputing MEC in a conservative way
  is a real issue as, in the given condition, there is no observed
  value to rely on.
   Thus, if imputation is not avoidable, imputed MEC must be very
    cautiously interpreted.")
})

#------------------------------------------------------------
#                             SCREEN 3
#------------------------------------------------------------

output$screenProtImput3 <- renderUI({
    tagList(
        tags$div(
            style = "display:inline-block; vertical-align: top;
      padding-right: 20px;",
            actionButton("ValidImputation", "Save imputation",
                class = actionBtnClass
            )
        ),
        tags$div(
            style = "display:inline-block; vertical-align: top;
      padding-right: 20px;",
            uiOutput("ImputationSaved")
        ),
        tags$hr(),
        moduleMVPlotsUI("mvImputationPlots_Valid")
    )
})



##' -- Validate and Save the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation, {
    isolate({
        name <- paste0("Imputed", ".", rv$typeOfDataset)
        rv$current.obj <- saveParameters(
            rv$current.obj, name,
            "proteinImputation", build_ParamsList_ProteinImputation()
        )
        UpdateDatasetWidget(rv$current.obj, name)


        rv$ValidImputationClicked <- TRUE
        rvModProcess$moduleProtImputationDone[3] <- TRUE
    })
})



output$ImputationSaved <- renderUI({
    req(input$datasets)
    if ((length(grep("Imputed", input$datasets)) != 1)) {
        return(NULL)
    } else if (grep("Imputed", input$datasets) == 1) {
        h4("The imputed dataset has been saved.")
    }
})
