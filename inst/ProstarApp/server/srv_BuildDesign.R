color_renderer <- reactive({
  conds <- rv$hot$Condition
  pal <- brewer.pal(length(unique(conds)),"Dark2")
  
  txt <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);"
  c <- 1
  for (i in 1:length(conds)){
    if (conds[i] != "")
      txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
  }
  txt <- paste0(txt,"}")
  
  return (txt)
})



#----------------------------------------------------------
observeEvent(input$btn_checkConds,{
  
  rv$hot
  if (length(grep("Bio.Rep", colnames(rv$hot))) > 0) { return(NULL)}
  
  rv$newOrder <- order(rv$hot["Condition"])
  rv$hot <- rv$hot[rv$newOrder,]
  rv$conditionsChecked <- DAPAR::check.conditions(rv$hot$Condition)
  
})



#----------------------------------------------------------
observeEvent(input$eData.box,{
  rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                        Condition = rep("",length(input$eData.box)),
                        stringsAsFactors = FALSE)
  
 
})

#-------------------------------------------------------------
output$hot <- renderRHandsontable({
  rv$hot
  input$chooseExpDesign
  
  if (is.null(rv$hot)){
    rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                          Condition = rep("",length(input$eData.box)),
                          stringsAsFactors = FALSE)
  }
  
  hot <- rhandsontable(rv$hot,rowHeaders=NULL, 
                       fillHandle = list(direction='vertical', 
                                         autoInsertRow=FALSE,
                                         maxRows=nrow(rv$hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = TRUE, 
                     allowColEdit = FALSE,
                     allowInsertRow = FALSE,
                     allowInsertColumn = FALSE,
                     allowRemoveRow = TRUE,
                     allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(renderer = color_renderer()) %>%
    hot_col(col = "Sample.name", readOnly = TRUE)
  
  if (!is.null(input$chooseExpDesign)) {
    switch(input$chooseExpDesign,
           FlatDesign = {
               if ("Bio.Rep" %in% colnames(rv$hot))
                   hot <- hot %>% hot_col(col = "Bio.Rep", readOnly = TRUE)
               },
           twoLevelsDesign = {
               if ("Tech.Rep" %in% colnames(rv$hot))
                   hot <- hot %>% hot_col(col =  "Tech.Rep", readOnly = TRUE)
           } ,
           threeLevelsDesign = {
               if ("Analyt.Rep" %in% colnames(rv$hot))
                   hot <- hot %>% hot_col(col = "Analyt.Rep", readOnly = TRUE)
           }
    )
  }
  hot
  
})





#----------------------------------------------------------
output$UI_checkConditions  <- renderUI({

  req(rv$hot)
  rv$conditionsChecked
  
  
  if (sum(rv$hot$Condition == "")==0){
    tags$div(
      tags$div(style="display:inline-block;",
               actionButton("btn_checkConds", "Check conditions")
      ),
      
      tags$div(style="display:inline-block;",
               if(!is.null(rv$conditionsChecked)){
                 
                 if (isTRUE(rv$conditionsChecked$valid)){
                   img <- "images/Ok.png"
                   txt <- "Correct conditions"
                 }else {
                   img <- "images/Problem.png"
                   txt <- "Invalid conditions"
                 }
                 tagList(
                   tags$div(
                     tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                     tags$div(style="display:inline-block;",tags$p(txt))
                   ),
                   if(!isTRUE(rv$conditionsChecked$valid)){
                     tags$p(rv$conditionsChecked$warn)
                   }
                 )
               }
      )
    )
  } else {
    tagList(
      br(),br(),
      br(),
      br()
    )
    
  }
})



#------------------------------------------------------------------------------
output$UI_hierarchicalExp <- renderUI({
  req(rv$conditionsChecked)
  if (!isTRUE(rv$conditionsChecked$valid)){return(NULL)
  } else {
    tagList(
      div(
        div(
          # edit1
          style="display:inline-block; vertical-align: middle;",
          tags$b("2 - Choose the type of experimental design and complete it accordingly")
        ),
        div(
          # edit2
          style="display:inline-block; vertical-align: middle;",
          tags$button(id="btn_helpDesign", tags$sup("[?]"), class="Prostar_tooltip")
        )
      ),
      
      radioButtons("chooseExpDesign", "",
                   choices = c("Flat design (automatic)" = "FlatDesign" ,
                               "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                               "3 levels design (complete Bio.Rep a,d Tech.Rep columns)" = "threeLevelsDesign" ))
    )
  }
  
})






#------------------------------------------------------------------------------
output$viewDesign <- renderUI({
  
  rv$designSaved
  if (isTRUE(rv$designSaved)){return(NULL)}
  
  tagList(
    h4("Design"),
    rHandsontableOutput("hot")
  )
})




#------------------------------------------------------------------------------
output$designExamples <- renderUI({
  input$chooseExpDesign
  
  switch(input$chooseExpDesign,
         FlatDesign = {},
         twoLevelsDesign = tagList(
           h4("Example for a 2-levels design"),
           rHandsontableOutput("twolevelsExample")
         ),
         threeLevelsDesign = tagList(
           h4("Example for a 3-levels design"),
           rHandsontableOutput("threelevelsExample")
         ))
})


#------------------------------------------------------------------------------
observe({
  shinyjs::onclick("btn_helpDesign",{
    shinyjs::toggle(id = "exLevels", anim = TRUE)}
  )
})

#------------------------------------------------------------------------------
observeEvent(input$chooseExpDesign, {
  rv$designChecked <- NULL
  switch(input$chooseExpDesign,
         FlatDesign = {
           rv$hot  <- data.frame(rv$hot[,1:2],
                                 Bio.Rep = seq(1:nrow(rv$hot)),
                                 stringsAsFactors = FALSE)
         },
         twoLevelsDesign = {
           rv$hot  <- data.frame(rv$hot[,1:2],Bio.Rep = rep("",nrow(rv$hot)),
                                 Tech.Rep = seq(1:nrow(rv$hot)),
                                 stringsAsFactors = FALSE)
         },
         threeLevelsDesign = {
           #if (length(grep("Tech.Rep", colnames(rv$hot))) > 0) { return(NULL)}
           rv$hot  <- data.frame(rv$hot[,1:2],
                                 Bio.Rep = rep("",nrow(rv$hot)),
                                 Tech.Rep = rep("",nrow(rv$hot)),
                                 Analyt.Rep = seq(1:nrow(rv$hot)),
                                 stringsAsFactors = FALSE)
         }
  )
})



#------------------------------------------------------------------------------
output$twolevelsExample <- renderRHandsontable({
  
  df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                   Condition = c(rep( "A", 4), rep("B", 4), rep("C", 6)),
                   Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                   Tech.Rep = c(1:14),
                   stringsAsFactors = FALSE)
  
  
  pal <- brewer.pal(3,"Dark2")
  
  color_rend <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);
  
  if(col==1 && (row>=0 && row<=3)) {td.style.background = '#1B9E77';}
  if(col==1 && (row>=4 && row<=7)) {td.style.background = '#D95F02';}
  if(col==1 && (row>=8 && row<=14)) {td.style.background = '#7570B3';}
  
  
  if(col==2 && (row==0||row==1||row==4||row==5||row==8||row==9||row==12||row==13)) 
  {td.style.background = 'lightgrey';}
  
  if(col==3 && (row==0||row==2||row==4||row==6||row==8||row==10||row==12)) 
  {td.style.background = 'lightgrey';}
}"

  rhandsontable(df,rowHeaders=NULL, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                      maxRows=nrow(rv$hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,allowInsertColumn = FALSE,
                     allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(readOnly = TRUE,renderer = color_rend)
  
  })



#------------------------------------------------------------------------------
output$threelevelsExample <- renderRHandsontable({
  
  df <- data.frame(Sample.name= paste0("Sample ",as.character(1:16)),
                   Condition = c(rep( "A", 8), rep("B", 8)),
                   Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                   Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                   Analyt.Rep = c(1:16),
                   stringsAsFactors = FALSE)
  
  
  pal <- brewer.pal(2,"Dark2")
  
  color_rend <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);
  
  if(col==1 && (row>=0 && row<=7)) 
  {td.style.background = '#1B9E77';}
  
  if(col==1 && (row>=8 && row<=15)) 
  {td.style.background = '#D95F02';}
  
  if(col==2 && (row==0||row==1||row==2||row==3||row==8||row==9||row==10||row==11)) 
  {td.style.background = 'lightgrey';}
  
  if(col==3 && (row==0||row==1||row==4||row==5|| row==8||row==9||row==12||row==13)) 
  {td.style.background = 'lightgrey';}
  
  
  if(col==4 && (row==0||row==2||row==4||row==6|| row==8||row==10||row==12||row==14)) 
  {td.style.background = 'lightgrey';}
}"
 rhandsontable(df,rowHeaders=NULL,fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                    maxRows=nrow(rv$hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,allowInsertColumn = FALSE,
                     allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(readOnly = TRUE,renderer = color_rend)
  
  })


#------------------------------------------------------------------------------
observeEvent(input$hot,{
  rv$hot <-  hot_to_r(input$hot)
  
})



#------------------------------------------------------------------------------
observeEvent(input$btn_checkDesign,{
  rv$designChecked <- DAPAR::check.design(rv$hot)
})

#------------------------------------------------------------------------------
output$checkDesign <- renderUI({
  req(input$chooseExpDesign)
  rv$designChecked
  switch(isolate({input$chooseExpDesign}),
         FlatDesign = {},
         twoLevelsDesign = { if (sum(rv$hot$Bio.Rep == "") > 0) {return(NULL)}},
         threeLevelsDesign = {if ((sum(rv$hot$Bio.Rep == "")+sum(rv$hot$Tech.Rep == "")) > 0) {return(NULL)}}
  )
  
  
  tags$div(
    tags$div(
      style="display:inline-block;",
      actionButton("btn_checkDesign", "Check design")
    ),
    
    tags$div(
      style="display:inline-block;",
      if(!is.null(rv$designChecked)){
        
        if (isTRUE(rv$designChecked$valid)){
          shinyjs::enable("createMSnsetButton")
          img <- "images/Ok.png"
          txt <- "Correct design"
        }else {
          img <- "images/Problem.png"
          txt <- "Invalid design"}
        tagList(
          tags$div(
            tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
            tags$div(style="display:inline-block;",tags$p(txt))
          ),
          if(!isTRUE(rv$designChecked$valid)){
            shinyjs::disable("createMSnsetButton")
            tags$p(rv$designChecked$warn)
          } else {
            shinyjs::enable("createMSnsetButton")
          }
        )
      } else {
        shinyjs::disable("createMSnsetButton")
      }
    )
    
  )
  
  
})




