

mod_ds_ui <- function(id) {
    ns <- NS(id)

    tabPanel("Descriptive statistics",
             value = "DescriptiveStatisticsTab",
             tabsetPanel(
                 id = "DS_tabSetPanel",
                 tabPanel("Overview",
                          value = "DS_tabGeneral",
                          tagList(
                              br(),
                              format_DT_ui("overview_DS"),
                              uiOutput("versionsUI")
                          )
                 ),
                 tabPanel("Quantification type",
                          value = "DS_tabOverviewMV",
                          #mod_plotsMetacellHistos_ui("MVPlots_DS")
                          uiOutput("plotsMVHistograms")
                 ),
                 tabPanel(
                     title = "Data explorer",
                     value = "DS_DataExplorer",
                     mod_MSnSetExplorer_ui(id = "test")
                 ),
                 tabPanel("Corr. matrix",
                          value = "DS_tabCorrMatrix",
                          checkboxInput("showDataLabels", "Show labels", value = FALSE),
                          uiOutput("plotsCorM")
                 ),
                 tabPanel("Heatmap",
                          value = "DS_tabHeatmap",
                          uiOutput("plotsHeatmap")
                 ),
                 tabPanel("PCA",
                          value = "DS_PCA",
                          uiOutput("plotsPCA")
                 ),
                 tabPanel("Intensity distr.",
                          value = "DS_tabDensityplot",
                          uiOutput("IntensityStatsPlots")
                 ),
                 tabPanel("CV distr.",
                          value = "DS_tabDistVar",
                          uiOutput("plotsDistCV")
                 )
             )
    )
}


mod_ds_server <- function(id, obj, cc) {
    moduleServer(id,function(input, output, session) {
            ns <- session$ns
            
            callModule(moduleDensityplot, "densityPlot_DS",
                       data = reactive({rv$current.obj})
            )
            
            callModule(moduleBoxplot, "boxPlot_DS",
                       data = reactive({rv$current.obj}),
                       pal = reactive({ unique(rv$PlotParams$paletteForConditions)})
            )
            
            format_DT_server("overview_DS",
                             data = reactive({GetDatasetOverview()}),
                             filename = "DescriptiveStats_Overview"
            )
            
            format_DT_server("PCAvarCoord",
                             data = reactive({
                                 if (!is.null(rv$res.pca)) {
                                     round(rv$res.pca$var$coord, digits = 7)
                                 }
                             }),
                             filename = "PCA_Var_Coords"
            )
            
            
            output$versionsUI <- renderUI({
                rv$current.obj
                
                Prostar_msnset_version <- rv$current.obj@experimentData@other$Prostar_Version
                DAPAR_msnset_version <- rv$current.obj@experimentData@other$DAPAR_Version
                
                tagList(
                    br(),
                    h3('This dataset was created with:'),
                    p(paste0('Prostar version:', Prostar_msnset_version)),
                    p(paste0('DAPAR version:', DAPAR_msnset_version))
                )
            })
            
            observeEvent(c(input$pca.axe1, input$pca.axe2), {
                rv$PCA_axes <- c(input$pca.axe1, input$pca.axe2)
            })
            
            observeEvent(input$varScale_PCA, {
                rv$PCA_varScale <- input$varScale_PCA
                rv$res.pca <- wrapper.pca(rv$current.obj, rv$PCA_varScale,
                                          ncp = Compute_PCA_nbDimensions()
                )
            })
            
            observeEvent(rv$current.obj, {
                rv$res.pca <- wrapper.pca(rv$current.obj,
                                          rv$PCA_varScale,
                                          ncp = Compute_PCA_nbDimensions()
                )
            })
            
            
            
            output$plotsCorM <- renderUI({
                tagList(
                    tags$br(), tags$br(),
                    tags$div(
                        tags$div(
                            style = "display:inline-block; vertical-align: middle;",
                            tags$p("Plot options")
                        ),
                        tags$div(style = "display:inline-block; vertical-align: middle;",
                            tags$div(
                                tags$div(style = "display:inline-block; vertical-align: top;",
                                    shinyWidgets::dropdownButton(
                                        tags$div(
                                            tags$div(style = "display:inline-block; vertical-align: bottom;",
                                                sliderInput("expGradientRate",
                                                            "Tune to modify the color gradient",
                                                            min = 0, max = 1, value = defaultGradientRate, step = 0.01
                                                ),
                                                tooltip = "Plots parameters",
                                                icon = icon("gear"), status = optionsBtnClass
                                            )
                                        ),
                                        tooltip = "Plots parameters",
                                        icon = icon("gear"), status = optionsBtnClass
                                    )
                                )
                            )
                        )
                    ),
                    withProgress(message = "", detail = "", value = 1, {
                        highchartOutput("corrMatrix", width = plotWidth, height = plotHeight)
                    })
                )
            })
            
            
            
            output$IntensityStatsPlots <- renderUI({
                tagList(
                    tags$br(), tags$br(),
                    tags$div(
                        tags$div(
                            style = "display:inline-block; vertical-align: middle;",
                            tags$p("Plot options")
                        )
                        
                    ),
                    fluidRow(
                        column(width = 6, moduleDensityplotUI(ns("densityPlot_DS"))),
                        column(width = 6, moduleBoxplotUI(ns("boxPlot_DS")))
                    )
                )
            })
            
            output$plotsMVHistograms <- renderUI({
                plot('tets')
                mod_plotsMetacellHistos_server(id = "MVPlots_DS_2",
                                               obj = reactive({rv$current.obj}),
                                               pal = reactive({rv$PlotParams$paletteForConditions}),
                                               pattern = reactive({NULL}),
                                               showSelect = reactive({TRUE})
                )
                
                mod_plotsMetacellHistos_ui("MVPlots_DS_2")
            })
            
            
            
            output$plotsDistCV <- renderUI({
                tagList(
                    helpText("Display the condition-wise distributions of the log-intensity
    CV (Coefficient of Variation) of the protein/peptides."),
                    helpText("For better visualization, it is possible to zoom in by
      click-and-drag."),
                    withProgress(message = "", detail = "", value = 1, {
                        highchartOutput("viewDistCV", width = plotWidth, height = plotHeight)
                    })
                )
            })
            
            
            output$plotsHeatmap <- renderUI({
                tagList(
                    div(
                        div(
                            style = "display:inline-block; vertical-align: middle;
        padding-right: 20px;",
                            selectInput("distance", "Distance",
                                        choices = G_heatmapDistance_Choices,
                                        selected = rv$PlotParams$heatmap.distance,
                                        width = "150px"
                            )
                        ),
                        div(
                            style = "display:inline-block; vertical-align: middle;",
                            selectInput("linkage", "Linkage",
                                        choices = G_heatmapLinkage_Choices,
                                        selected = rv$PlotParams$heatmap.linkage,
                                        width = "150px"
                            )
                        ),
                        tags$hr(),
                        uiOutput("DS_PlotHeatmap")
                    )
                )
            })
            
            
            output$plotsPCA <- renderUI({
                tagList(
                    uiOutput("WarningNA_PCA"),
                    uiOutput("pcaOptions"),
                    fluidRow(
                        column(width = 6, imageOutput("pcaPlotVar", width = "auto", height = "auto")),
                        column(width = 6, imageOutput("pcaPlotInd", width = "auto", height = "auto"))
                    ),
                    fluidRow(
                        column(width = 6, highchartOutput("pcaPlotEigen")),
                        column(width = 6, format_DT_ui("PCAvarCoord"))
                    )
                )
            })
            
            
            
            
            output$pcaPlotInd <- renderImage(
                {
                    # req(rv$PCA_axes)
                    # req(rv$res.pca)
                    
                    outfile <- tempfile(fileext = ".png")
                    # Generate a png
                    png(outfile)
                    image <- DAPAR::plotPCA_Ind(rv$res.pca, rv$PCA_axes)
                    print(image)
                    dev.off()
                    
                    # Return a list
                    list(
                        src = outfile,
                        alt = "This is alternate text"
                    )
                },
                deleteFile = FALSE
            )
            
            
            output$pcaPlotVar <- renderImage(
                {
                    req(rv$PCA_axes)
                    req(rv$res.pca)
                    
                    outfile <- tempfile(fileext = ".png")
                    # Generate a png
                    png(outfile)
                    image <- DAPAR::plotPCA_Var(rv$res.pca, rv$PCA_axes)
                    print(image)
                    dev.off()
                    
                    # Return a list
                    list(
                        src = outfile,
                        alt = "This is alternate text"
                    )
                },
                deleteFile = FALSE
            )
            
            
            
            output$pcaPlotEigen <- renderHighchart({
                req(rv$res.pca)
                plotPCA_Eigen_hc(rv$res.pca)
            })
            
            output$pcaOptions <- renderUI({
                req(rv$current.obj)
                m <- match.metacell(DAPAR::GetMetacell(rv$current.obj),
                                    pattern = c("Missing", "Missing POV", "Missing MEC"),
                                    level = DAPAR::GetTypeofData(rv$current.obj)
                )
                
                tagList(
                    if (length(which(m)) > 0) {
                        tags$p("Warning: As your dataset contains missing values,
      the PCA cannot be computed.
             Please impute them first")
                    } else {
                        tags$div(
                            tags$div(
                                style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                                numericInput("pca.axe1", "Dimension 1",
                                             min = 1,
                                             max = Compute_PCA_nbDimensions(), value = 1, width = "100px"
                                )
                            ),
                            tags$div(
                                style = "display:inline-block; vertical-align: middle;",
                                numericInput("pca.axe2", "Dimension 2",
                                             min = 1,
                                             max = Compute_PCA_nbDimensions(), value = 2, width = "100px"
                                )
                            ),
                            tags$div(
                                style = "display:inline-block; vertical-align: middle;
          padding-right: 20px;",
                                checkboxInput("varScale_PCA", "Variance scaling",
                                              value = rv$PCA_varScale
                                )
                            )
                        )
                    }
                )
            })
            
            
            
            #######################################
            
            
            output$DS_sidebarPanel_tab <- renderUI({
                req(rv$typeOfDataset)
                
                .choices <- NULL
                switch(rv$typeOfDataset,
                       protein = {
                           .choices <- list("Quantitative data" = "tabExprs",
                                            "Proteins metadata" = "tabfData",
                                            "Experimental design" = "tabpData")
                       },
                       peptide = {
                           .choices <- list("Quantitative data" = "tabExprs",
                                            "Peptides metadata" = "tabfData",
                                            "Experimental design" = "tabpData")
                       },
                       {
                           .choices <- list("Quantitative data" = "tabExprs",
                                            "Analyte metadata" = "tabfData",
                                            "Experimental design" = "tabpData")
                       }
                )
                
                tagList(
                    tags$div(
                        tags$div(
                            style = "display:inline-block; vertical-align: middle;
        padding-right: 40px;",
                            radioButtons("DS_TabsChoice", "Table to display",
                                         choices = .choices,
                                         inline = TRUE,
                                         selected = character(0)
                            )
                        ),
                        tags$div(
                            style = "display:inline-block; vertical-align: middle;",
                            uiOutput("legendForExprsData")
                        )
                    )
                )
            })
            
            
            
            
            
            output$DS_sidebarPanel_heatmap <- renderUI({
                req(rv$current.obj)
                tagList(
                    h3("Clustering Options"),
                    selectInput("distance", "Distance",
                                choices = G_heatmapDistance_Choices,
                                selected = rv$PlotParams$heatmap.distance,
                                width = "150px"
                    ),
                    br(),
                    selectInput("linkage", "Linkage",
                                choices = G_heatmapLinkage_Choices,
                                selected = rv$PlotParams$heatmap.linkage,
                                width = "150px"
                    )
                )
            })
            
            
            mod_MSnSetExplorer_server(
                id = "test",
                data = reactive({rv$current.obj}),
                digits = reactive({rv$settings_nDigits}),
                palette.conds = reactive({rv$PlotParams$paletteForConditions})
            )
            
            
            
            
            
            
            
            viewDistCV <- reactive({
                req(rv$current.obj)
                rv$PlotParams$paletteForConditions
                
                isolate({
                    rv$tempplot$varDist <- wrapper.CVDistD_HC(rv$current.obj,
                                                              pal = rv$PlotParams$paletteForConditions
                    )
                })
                rv$tempplot$varDist
            })
            
            
            
            corrMatrix <- reactive({
                req(rv$current.obj)
                input$expGradientRate
                input$showDataLabels
                
                gradient <- NULL
                if (is.null(input$expGradientRate)) {
                    gradient <- defaultGradientRate
                } else {
                    gradient <- input$expGradientRate
                }
                
                isolate({
                    rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(rv$current.obj,
                                                                     gradient,
                                                                     showValues = input$showDataLabels
                    )
                    rv$tempplot$corrMatrix
                })
            })
            
            
            observeEvent(input$distance, {
                rv$PlotParams$heatmap.distance <- input$distance
            })
            observeEvent(input$linkage, {
                rv$PlotParams$heatmap.linkage <- input$linkage
            })
            
            heatmap <- reactive({
                req(rv$current.obj)
                input$linkage
                input$distance
                
                isolate({
                    wrapper.heatmapD(
                        rv$current.obj,
                        input$distance,
                        input$linkage,
                        TRUE
                    )
                })
            })
            
            
            
            
            
            
            
            
            output$DS_PlotHeatmap <- renderUI({
                req(rv$current.obj)
                if (nrow(rv$current.obj) > limitHeatmap) {
                    tags$p("The dataset is too big to compute the heatmap in a
      reasonable time.")
                } else {
                    tagList(
                        withProgress(message = "Building plot", detail = "", value = 1, {
                            plotOutput("heatmap", width = "900px", height = "600px")
                        })
                    )
                }
            })
            
            
            
            
            
            # options for boxplot
            # #------------------------------------------------------
            output$ChooseLegendForSamples <- renderUI({
                req(rv$current.obj)
                
                .names <- colnames(Biobase::pData(rv$current.obj))
                
                
                checkboxGroupInput("legendForSamples",
                                   label = "Choose data to show in legend",
                                   choices = .names,
                                   selected = .names[2]
                )
            })
            
            observeEvent(input$legendForSamples, {
                rv$PlotParams$legendForSamples <- as.vector(
                    apply(
                        as.data.frame(Biobase::pData(rv$current.obj)[, input$legendForSamples]), 1,
                        function(x) paste(x, collapse = "_")
                    )
                )
            })
            
            
            shinyBS::addPopover(session, "histo_missvalues_per_lines_per_conditions", "Info",
                                content = paste0(
                                    "<p>Test",
                                    "test</p><p>Explanation .</p>"
                                ), trigger = "click"
            )
            
            
            
            
            ##' Draw a heatmap of current data
            ##'
            ##' @author Samuel Wieczorek
            output$heatmap <- renderImage(
                {
                    # A temp file to save the output. It will be deleted after renderImage
                    # sends it, because deleteFile=TRUE.
                    outfile <- tempfile(fileext = ".png")
                    
                    # Generate a png
                    tryCatch({
                        png(outfile, width = 900, height = 600)
                        heatmap()
                        dev.off()
                    },
                    error = function(e) {
                        #if(showErrLog)
                        shinyjs::info(conditionMessage(e))
                        return(NULL)
                        #     mod_errorModal_server("test_error",
                        #         reactive({readLines(logfilename)})
                        # )
                        # return(NULL)
                    })
                    
                    # Return a list
                    list(
                        src = outfile,
                        alt = "This is alternate text"
                    )
                },
                deleteFile = TRUE
            )
            
            
            ##' distribution of the variance in current.obj
            ##'
            ##' @author Samuel Wieczorek
            output$viewDistCV <- renderHighchart({
                viewDistCV()
            })
            
            
            
            ##' Draw a correlation matrix of intensities in current.obj
            ##'
            ##' @author Samuel Wieczorek
            output$corrMatrix <- renderHighchart({
                corrMatrix()
            })
            
            
        })
}




# Example
#
ui <- fluidPage(
    tagList(
        mod_ds_ui('tree_test1')
    )
)

server <- function(input, output) {
    
    utils::data('Exp1_R25_prot')
    tags1 <- mod_ds_server('tree_test1', 
                           obj = reactive({Exp1_R25_prot}))
    
}

shinyApp(ui = ui, server = server)


