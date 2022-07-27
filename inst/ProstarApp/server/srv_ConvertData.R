

callModule(modulePopover, "modulePopover_convertChooseDatafile",
    data = reactive(
        list(
            title = "Data file",
            content = "Select one (.txt, .csv, .tsv, .xls, .xlsx) file."
        )
    )
)

callModule(modulePopover, "modulePopover_convertIdType",
    data = reactive(
        list(
            title = "ID definition",
            content = "If you choose the automatic ID, Prostar will build
          an index."
        )
    )
)



callModule(modulePopover, "modulePopover_convertProteinID",
    data = reactive(list(
        title = "Select protein IDs",
        content = "Select the column containing the parent protein IDs."
    ))
)


callModule(modulePopover, "modulePopover_convertDataQuanti",
    data = reactive(
        list(
            title = "Quantitative data",
            content = "Select the columns that are quantitation values
            by clicking in the field below."
        )
    )
)

mod_staticDT_server("overview_convertData",
    data = reactive({
        GetDatasetOverview()
    })
)




## --------------------------------------------------------------
## Gestion du slideshow
## --------------------------------------------------------------


output$checkConvertPanel <- renderUI({
    rv$tab1
    rv$pageConvert
    color <- rep("lightgrey", NUM_PAGES_CONVERT)

    txt <- c(
        "Select file", "Select ID", "Select quantitative data",
        "Build design", "Convert"
    )
    buildTable(txt, color)
})



########### STEP 1 ############
output$Convert_SelectFile <- renderUI({
    tagList(
        br(), br(),
        radioButtons("choose_software", "Software to import from",
            choices = setNames(nm = c("maxquant", "proline")),
            selected = character(0)
        ),
        uiOutput("choose_file_to_import"),
        uiOutput("ManageXlsFiles"),
        uiOutput("ConvertOptions")
    )
})



output$choose_file_to_import <- renderUI({
    req(input$choose_software)
    fluidRow(
        column(
            width = 2,
            modulePopoverUI("modulePopover_convertChooseDatafile")
        ),
        column(
            width = 10,
            fileInput("file1", "",
                multiple = FALSE,
                accept = c(".txt", ".tsv", ".csv", ".xls", ".xlsx")
            )
        )
    )
})


fileExt.ok <- reactive({
    req(input$file1$name)
    authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
    ext <- GetExtension(input$file1$name)
    !is.na(match(ext, authorizedExts))
})

output$ConvertOptions <- renderUI({
    req(input$choose_software)
    req(input$file1)
    req(fileExt.ok())

    tagList(
        radioButtons("typeOfData",
            "Is it a peptide or protein dataset ?",
            choices = c(
                "peptide dataset" = "peptide",
                "protein dataset" = "protein"
            )
        ),
        radioButtons("checkDataLogged",
            "Are your data already log-transformed ?",
            # width = widthWellPanel,
            choices = c(
                "yes (they stay unchanged)" = "yes",
                "no (they wil be automatically transformed)" = "no"
            ),
            selected = "no"
        ),
        br(),
        checkboxInput("replaceAllZeros",
            "Replace all 0 and NaN by NA",
            value = TRUE
        )
    )
})




############ Read text file to be imported ######################
observeEvent(c(input$file1, input$XLSsheets), {
    input$XLSsheets
    if (((GetExtension(input$file1$name) %in% c("xls", "xlsx"))) &&
        is.null(input$XLSsheets)) {
        return(NULL)
    }

    authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")

    if (!fileExt.ok()) {
        shinyjs::info("Warning : this file is not a text nor an Excel file !
     Please choose another one.")
    } else {
        # result = tryCatch(
        #   {
        ClearUI()
        ClearMemory()
        ext <- GetExtension(input$file1$name)
        shinyjs::disable("file1")
        switch(ext,
            txt = {
                rv$tab1 <- read.csv(input$file1$datapath,
                    header = TRUE, sep = "\t", as.is = TRUE
                )
            },
            csv = {
                rv$tab1 <- read.csv(input$file1$datapath,
                    header = TRUE, sep = ";", as.is = TRUE
                )
            },
            tsv = {
                rv$tab1 <- read.csv(input$file1$datapath,
                    header = TRUE, sep = "\t", as.is = TRUE
                )
            },
            xls = {
                rv$tab1 <- readExcel(input$file1$datapath,
                    ext,
                    sheet = input$XLSsheets
                )
            },
            xlsx = {
                rv$tab1 <- readExcel(input$file1$datapath,
                    ext,
                    sheet = input$XLSsheets
                )
            }
        )

        colnames(rv$tab1) <- gsub(".", "_", colnames(rv$tab1), fixed = TRUE)
        colnames(rv$tab1) <- gsub(" ", "_", colnames(rv$tab1), fixed = TRUE)
    }
})



output$ManageXlsFiles <- renderUI({
    req(input$choose_software)
    req(input$file1)

    .ext <- GetExtension(input$file1$name)
    if ((.ext == "xls") || (.ext == "xlsx")) {
        sheets <- listSheets(input$file1$datapath)
        selectInput("XLSsheets", "sheets",
            choices = as.list(sheets),
            width = "200px"
        )
    }
})




################## STEP 2 ###############################

output$Convert_DataId <- renderUI({
    tagList(
        br(), br(),
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: top;
        padding-right: 100px;",
                uiOutput("id"),
                uiOutput("warningNonUniqueID")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: top;",
                uiOutput("convertChooseProteinID_UI"),
                uiOutput("previewProteinID_UI")
            )
        )
    )
})


output$id <- renderUI({
    req(rv$tab1)

    .choices <- c("AutoID", colnames(rv$tab1))
    names(.choices) <- c("Auto ID", colnames(rv$tab1))

    tagList(
        modulePopoverUI("modulePopover_convertIdType"),
        selectInput("colnameForID", label = "", choices = .choices)
    )
})


output$warningNonUniqueID <- renderUI({
    req(input$colnameForID != "AutoID")
    req(rv$tab1)

    t <- (length(as.data.frame(rv$tab1)[, input$colnameForID])
    == length(unique(as.data.frame(rv$tab1)[, input$colnameForID])))

    if (!t) {
        text <- "<img src=\"images/Problem.png\" height=\"24\"></img>
    <font color=\"red\">
        Warning ! Your ID contains duplicate data.
        Please choose another one."
    } else {
        text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
    }
    HTML(text)
})


output$convertChooseProteinID_UI <- renderUI({
    req(rv$tab1)
    req(input$typeOfData != "protein")

    .choices <- c("", colnames(rv$tab1))
    names(.choices) <- c("", colnames(rv$tab1))
    tagList(
        modulePopoverUI("modulePopover_convertProteinID"),
        selectInput("convert_proteinId",
            "",
            choices = .choices, selected = character(0)
        )
    )
})




output$helpTextDataID <- renderUI({
    input$typeOfData
    if (is.null(input$typeOfData)) {
        return(NULL)
    }
    t <- ""
    switch(input$typeOfData,
        protein = {
            t <- "proteins"
        },
        peptide = {
            t <- "peptides"
        }
    )
    txt <- paste("Please select among the columns of your data the one that
                corresponds to a unique ID of the ", t, ".", sep = " ")
    helpText(txt)
})



datasetID_Ok <- reactive({
    req(input$colnameForID)
    req(rv$tab1)
    if (input$colnameForID == "AutoID") {
        t <- TRUE
    } else {
        t <- (length(as.data.frame(rv$tab1)[, input$colnameForID])
        == length(unique(as.data.frame(rv$tab1)[, input$colnameForID])))
    }
    t
})



output$previewProteinID_UI <- renderUI({
    req(input$convert_proteinId != "")

    tagList(
        p(style = "color: black;", "Preview"),
        tableOutput("previewProtID")
    )
})



output$previewProtID <- renderTable(
    # req(input$convert_proteinId),
    head(rv$tab1[, input$convert_proteinId]),
    colnames = FALSE
)






output$Convert_ExpFeatData <- renderUI({
    tagList(
        shinyjs::useShinyjs(),
        fluidRow(
            column(
                width = 4,
                radioButtons("selectIdent", "Provide identification method",
                    choices = list(
                        "No (default values will be computed)" = FALSE,
                        "Yes" = TRUE
                    ),
                    selected = FALSE
                )
            ),
            column(width = 4, uiOutput("checkIdentificationTab")),
            column(width = 4, shinyjs::hidden(
                div(
                    id = "warning_neg_values",
                    p(
                        "Warning : Your original dataset may contain
                      negative values",
                        "so that they cannot be logged. Please check
                      back the dataset or",
                        "the log option in the first tab."
                    )
                )
            ))
        ),
        fluidRow(
            column(width = 4, uiOutput("eData", width = "400px")),
            column(width = 8, shinyjs::hidden(
                uiOutput("inputGroup", width = "600px")
            ))
        )
    )
})


output$inputGroup <- renderUI({
    # if (is.null(input$choose_quantitative_columns) || is.null(rv$tab1))
    #  return(NULL)

    n <- length(input$choose_quantitative_columns)

    input_list <- lapply(seq_len(n), function(i) {
        inputName <- paste("colForOriginValue_", i, sep = "")
        div(
            div(
                style = "align: center;display:inline-block; vertical-align:
          middle;padding-right: 10px;",
                p(tags$strong(paste0(
                    "Identification col. for ",
                    input$choose_quantitative_columns[i]
                )))
            ),
            div(
                style = "align: center;display:inline-block;
              vertical-align: middle;",
                selectInput(inputName, "",
                    choices = c("None", colnames(rv$tab1))
                )
            )
        )
    })
    do.call(tagList, input_list)
})


observeEvent(input[["colForOriginValue_1"]], 
  ignoreInit = TRUE, ignoreNULL = FALSE, {
    n <- length(input$choose_quantitative_columns)
    lapply(seq(2, n), function(i) {
        inputName <- paste("colForOriginValue_", i, sep = "")
        start <- which(colnames(rv$tab1) == input[["colForOriginValue_1"]])

        if (input[["colForOriginValue_1"]] == "None") {
            .select <- "None"
        } else {
            .select <- colnames(rv$tab1)[(i - 1) + start]
        }
        updateSelectInput(session, inputName, selected = .select)
    })
})

observe({
    shinyjs::toggle("warning_neg_values",
        condition = !is.null(input$choose_quantitative_columns) &&
            length(which(rv$tab1[, input$choose_quantitative_columns] < 0)) > 0
    )
    shinyjs::toggle("selectIdent",
        condition = !is.null(input$choose_quantitative_columns)
    )
    shinyjs::toggle("inputGroup",
        condition = as.logical(input$selectIdent) == TRUE
    )
})

output$eData <- renderUI({
    input$file1
    req(rv$tab1)

    choices <- colnames(rv$tab1)
    names(choices) <- colnames(rv$tab1)

    tagList(
        modulePopoverUI("modulePopover_convertDataQuanti"),
        selectInput("choose_quantitative_columns",
            label = "",
            choices = choices,
            multiple = TRUE, width = "200px",
            size = 20,
            selectize = FALSE
        )
    )
})



output$checkIdentificationTab <- renderUI({
    req(as.logical(input$selectIdent) == TRUE)

    shinyValue("colForOriginValue_", length(input$choose_quantitative_columns))
    temp <- shinyValue(
        "colForOriginValue_",
        length(input$choose_quantitative_columns)
    )

    # if ((length(which(temp == "None")) == length(temp)))
    # {
    #   img <- "images/Ok.png"
    #   txt <- "Correct"
    # }  else {

    if (length(which(temp == "None")) > 0) {
        img <- "images/Problem.png"
        txt <- "The identification method is not appropriately defined for
      each sample."
    } else {
        if (length(temp) != length(unique(temp))) {
            img <- "images/Problem.png"
            txt <- "There are duplicates in identification columns."
        } else {
            img <- "images/Ok.png"
            txt <- "Correct"
        }
    }
    # }
    tags$div(
        tags$div(
            tags$div(
                style = "display:inline-block;",
                tags$img(
                    src = img,
                    height = 25
                )
            ),
            tags$div(style = "display:inline-block;", tags$p(txt))
        )
    )
})






checkIdentificationMethod_Ok <- reactive({
    res <- TRUE
    tmp <- NULL
    if (isTRUE(as.logical(input$selectIdent))) {
        tmp <- shinyValue("colForOriginValue_", nrow(quantiDataTable()))
        if ((length(grep("None", tmp)) > 0) || (sum(is.na(tmp)) > 0)) {
            res <- FALSE
        }
    }
    res
})





############# STEP 4 ######################

output$Convert_BuildDesign <- renderUI({
    req(input$file1)
    tagList(
        tags$p(
            "If you do not know how to fill the experimental design, you can
            click on the '?' next to each design in the list that appear
            once the conditions are checked or got to the ",
            actionLink("linkToFaq1", "FAQ",
                style = "background-color: white"
            ),
            " page."
        ),
        fluidRow(
            column(
                width = 6,
                tags$b("1 - Fill the \"Condition\" column to identify
                the conditions to compare.")
            ),
            column(
                width = 6,
                uiOutput("UI_checkConditions")
            )
        ),
        fluidRow(
            column(width = 6, uiOutput("UI_hierarchicalExp")),
            column(width = 6, uiOutput("checkDesign"))
        ),
        hr(),
        selectInput("convert_reorder", "Order by conditions ?",
            choices = c("No" = "No", "Yes" = "Yes"),
            width = "100px"
        ),
        tags$div(
            tags$div(
                style = "display:inline-block; vertical-align: top;",
                uiOutput("viewDesign", width = "100%")
            ),
            tags$div(
                style = "display:inline-block; vertical-align: top;",
                shinyjs::hidden(div(
                    id = "showExamples",
                    uiOutput("designExamples")
                ))
            )
        )
    )
})



############# STEP 5 ########################


output$Convert_Convert <- renderUI({
    tagList(
        br(), br(),
        uiOutput("convertFinalStep"),
        mod_staticDT_ui("overview_convertData"),
        uiOutput("conversionDone"),
        p("Once the 'Load' button (above) clicked, you will be automatically
    redirected to Prostar home page. The dataset will be accessible within
    Prostar
    interface and processing menus will be enabled. However, all importing
    functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled
    (because successive dataset loading can make Prostar unstable). To work
    on another dataset, use first the 'Reload Prostar' functionality from
    the 'Dataset manager' menu: it will make Prostar restart with a fresh R
      session where import functions are enabled.")
    )
})



output$convertFinalStep <- renderUI({
    req(rv$designChecked)
    if (!(rv$designChecked$valid)) {
        return(NULL)
    }
    tagList(
        uiOutput("checkAll_convert", width = "50"),
        htmlOutput("msgAlertCreateMSnset"),
        hr(),
        textInput("filenameToCreate", "Enter the name of the study"),
        actionButton("createMSnsetButton", "Convert data",
            class = actionBtnClass
        ),
        uiOutput("warningCreateMSnset")
    )
})


output$conversionDone <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {
        return(NULL)
    }

    h4("The conversion is done. Your dataset has been automatically loaded
       in memory. Now, you can switch to the Descriptive statistics panel to
       vizualize your data.")
})



output$warningCreateMSnset <- renderUI({
    if (isTRUE(as.logical(input$selectIdent))) {
        n <- length(input$choose_quantitative_columns)

        colNamesForMetacell <- unlist(lapply(seq_len(n), function(x) {
            input[[paste0("colForOriginValue_", x)]]
        }))

        if (length(which(colNamesForMetacell == "None")) > 0) {
            text <- "<font color=\"red\"> Warning: The MSnset cannot be created
      because the identification
            method are not fully filled.  <br>"
            HTML(text)
        }
    }
})



#######################################
observeEvent(input$createMSnsetButton, ignoreInit = TRUE, {
    colNamesForMetacell <- NULL
    if (isTRUE(as.logical(input$selectIdent))) {
        n <- length(input$choose_quantitative_columns)

        colNamesForMetacell <- unlist(lapply(seq_len(n), function(x) {
            input[[paste0("colForOriginValue_", x)]]
        }))
        if (length(which(colNamesForMetacell == "None")) > 0) {
            return(NULL)
        }
        if (!is.null(rv$newOrder)) {
            colNamesForMetacell <- colNamesForMetacell[rv$newOrder]
        }
    }

    isolate({
        result <- tryCatch(
            {
                ext <- GetExtension(input$file1$name)
                txtTab <- paste("tab1 <- read.csv(\"", input$file1$name,
                    "\",header=TRUE, sep=\"\t\", as.is=T)",
                    sep = ""
                )
                txtXls <- paste("tab1 <- read.xlsx(", input$file1$name,
                    ",sheet=", input$XLSsheets, ")",
                    sep = ""
                )
                switch(ext,
                    txt = writeToCommandLogFile(txtTab),
                    csv = writeToCommandLogFile(txtTab),
                    tsv = writeToCommandLogFile(txtTab),
                    xls = writeToCommandLogFile(txtXls),
                    xlsx = writeToCommandLogFile(txtXls)
                )


                input$filenameToCreate
                rv$tab1
                .chooseCols <- input$choose_quantitative_columns
                indexForEData <- match(
                    .chooseCols,
                    colnames(rv$tab1)
                )
                if (!is.null(rv$newOrder)) {
                    .chooseCols <- .chooseCols[rv$newOrder]
                    indexForEData <- indexForEData[rv$newOrder]
                }

                indexForFData <- seq(1, ncol(rv$tab1))[-indexForEData]



                metadata <- hot_to_r(input$hot)
                logData <- (input$checkDataLogged == "no")


                indexForMetacell <- NULL
                if (!is.null(colNamesForMetacell) &&
                    (length(grep("None", colNamesForMetacell)) == 0) &&
                    (sum(is.na(colNamesForMetacell)) == 0)) {
                    indexForMetacell <- match(
                        colNamesForMetacell,
                        colnames(rv$tab1)
                    )
                }

                options(digits = 15)

                protId <- NULL
                if (input$typeOfData == "protein") {
                    protId <- input$colnameForID
                } else if (input$typeOfData == "peptide") {
                    protId <- input$convert_proteinId
                }

                tmp <- DAPAR::createMSnset(
                    file = rv$tab1,
                    metadata = metadata,
                    indExpData = indexForEData,
                    colnameForID = input$colnameForID,
                    indexForMetacell = indexForMetacell,
                    logData = logData,
                    replaceZeros = input$replaceAllZeros,
                    pep_prot_data = input$typeOfData,
                    proteinId = gsub(".", "_", protId, fixed = TRUE),
                    software = input$choose_software
                )
                ClearUI()
                ClearMemory()
                rv$current.obj <- tmp

                rv$current.obj.name <- input$filenameToCreate
                rv$indexNA <- which(is.na(exprs(rv$current.obj)))

                l.params <- list(filename = input$filenameToCreate)

                loadObjectInMemoryFromConverter()

                updateTabsetPanel(session, "tabImport", selected = "Convert")
            },
            error = function(e) {
                shinyjs::info(paste("Error :", "CreateMSnSet", ":",
                    conditionMessage(e),
                    sep = " "
                ))
            },
            finally = {
                # cleanup-code
            }
        )
    })
})
