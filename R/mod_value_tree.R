#' value_tree UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_value_tree_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinydashboard::box(
      width = NULL,
      status ="primary",
      collapsible = FALSE,
      collapsed = FALSE,
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(2,
            shiny::selectizeInput(
              inputId = ns("node_category"),
              label = HTML('<p style = "color: white;"> Select Top Node Level Category</p>'),
              choices = c("Benefits/Risks", "Benefits/Identified Risks/Potential Risks"),
              selected = c("Benefits/Risks")
            )
          ),
          shiny::column(1,
            colourpicker::colourInput(
              inputId = ns("col1"),
              label = HTML('<p style = "color: white;"> Color Benefits </p>'),
              value = "#66B512",
              returnName = FALSE
            )
          ),
          shiny::column(1,
           uiOutput(ns('col_2'))
          ),
          shiny::column(1,
           shiny::conditionalPanel(
              condition = "input.node_category == 'Benefits/Identified Risks/Potential Risks'",
              colourpicker::colourInput(
                inputId = ns("col3"),
                label = HTML('<p style = "color: white;"> Color Potential Risks </p>'), 
                value = "#de9a12",
                returnName = FALSE
              )
            )
          ),
          shiny::column(2,
            tags$head(
              tags$style(
                HTML("#dropdown-menu-dropdown_add_node {
                background-color: #393D3F !important;}"
                )
              )
            ),
            shinyWidgets::dropdownButton(
              inputId = ns("dropdown_add_node"),
              label = "Add Node: ",
              tooltip = shinyWidgets::tooltipOptions(placement = "right", title = "Add a new node to the plot"),
              icon = icon("plus"),
              status = "custom",
              circle = FALSE,
              uiOutput(ns('add_selection')),
              uiOutput(ns('add_text')),
              shiny::actionButton(
                inputId = ns("add_node"),
                label = "Add Node",
                icon = icon("plus"),
                style="color: #fff; background-color: #999999; border-color: #2e6da4"
              ),
              tags$style(type = 'text/css', "#add_node { width:100%; margin-top: 35px;}")
            )
          ),
          tags$head(
            tags$style(
              HTML("#dropdown-menu-dropdown_remove_node {
                  background-color: #393D3F !important;}"
              )
            )
          ),
          shiny::column(2,
            shinyWidgets::dropdownButton(
              inputId = ns("dropdown_remove_node"),
              label = "Remove Node: ",
              tooltip = shinyWidgets::tooltipOptions(
                placement = "right",
                title = "Remove a node of the plot"
              ),
              icon = icon("minus"),
              status = "custom",
              circle = FALSE,
                shiny::uiOutput(ns('remove_selection')),     
                shiny::actionButton(
                  inputId = ns("remove_node"),
                  label = "Remove Node",
                  icon = icon("minus"),
                  style = "color: #fff;
                    background-color: #999999;
                    border-color: #2e6da4;"
                ),
                tags$style(type = 'text/css', "#remove_node { width:100%; margin-top: 35px;}")
              )
            ),
            tags$head(
              tags$style(
                HTML("#dropdown-menu-dropdown_shift_node {
                      background-color: #393D3F !important;}"
                )
              )
            ),
            shiny::column(2,
              shinyWidgets::dropdownButton(
                inputId = ns("dropdown_shift_node"),
                label = "Shift Node: ",
                tooltip = shinyWidgets::tooltipOptions(placement = "right", title = "Shift a node in the plot"),
                icon = icon("arrows-alt-v"),
                status = "custom",
                circle = FALSE,
                shiny::uiOutput(ns('shift_selection_from')),
                shiny::uiOutput(ns('shift_selection_to')), 
                shiny::actionButton(
                 inputId = ns("shift_node"),
                 label = "Shift Node",
                 icon = icon("arrows-alt-v"),
                 style = "color: #fff; background-color: #999999; border-color: #2e6da4"
                ),
                tags$style(type = 'text/css', "#remove_node { width:100%; margin-top: 35px;}")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(5,
              shiny::helpText("Note: All entered nodes will be deleted if 'Level category' is changed!")
            ),
            shiny::column(7,
              shiny::helpText("Note: Plot is interactive. Use mouse wheel to zoom in/out and mouse click to collapse node/branches.")
            )
          )
        )
      ),   
      collapsibleTree::collapsibleTreeOutput(
        outputId = ns("value_tree"),
        width = "100%",
        height ="400px"
      ),
      shiny::downloadButton(
        outputId = ns('downloadPlot'), 
        label = 'Download Plot as HTML'
      )
    )
  
}
    
#' value_tree Server Function
#'
#' @noRd 
mod_value_tree_server <- function(input, output, session) {
  ns <- session$ns
  ####... Value Tree-tab ####
  #Color Option for Risks/Identified Risks:
  output$col_2 <- shiny::renderUI({
    colourpicker::colourInput(
      inputId = ns("col2"),
      label = HTML(paste0('<p style = "color: white;"> ',ifelse(input$node_category == "Benefits/Risks", 'Color Risks', 'Color Identified Risks'), '</p>')), 
      value = "#D30F4B",
    )
  })
  
  # create reactive object with 4 possible Hierarchy levels 
  
  Benefit_Risk <- shiny::reactiveValues(
    val = NULL
  )
  
  shiny::observeEvent(c(input$node_category), {
    BR <- Benefit_Risk$val
    if (length(unique(unlist(Benefit_Risk$val[, 2:4]))[unique(unlist(Benefit_Risk$val[, 2:4])) != ""]) > 0) {
        Benefit_Risk$val <- data.frame(
          Level1 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("Benefits", "Risks")} else {c("Benefits ","Identified Risks","Potential Risks")},
          Level2 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("", "")} else {c("","","")},
          Level3 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("", "")} else {c("","","")},
          Level4 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("", "")} else {c("","","")}
        )
    } else {
      Benefit_Risk$val <- data.frame(
        Level1 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("Benefits", "Risks")} else {c("Benefits ","Identified Risks","Potential Risks")},
        Level2 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("", "")} else {c("","","")},
        Level3 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("", "")} else {c("","","")},
        Level4 = if(shiny::isolate(input$node_category) == "Benefits/Risks") {c("", "")} else {c("","","")}
      )
    }
  })
  
  Benefit_Risk2 <- shiny::reactiveValues(val = NULL)
   
   shiny::observeEvent(Benefit_Risk$val, {
     Benefit_Risk2$val <- Benefit_Risk$val
   })
   
  #Sorted collapsibleTree values
  shiny::observeEvent(input$sort, {
    Benefit_Risk2$val <- Benefit_Risk2$val %>%
      dplyr::arrange(dplyr::desc(Level1))
  })
  
  plotInput <- function(){
    tree_col <- c(input$col1, input$col2, input$col3)
    col_ <- c("#ebebeb")
    for(i in 1:length(unique(Benefit_Risk2$val$Level1))) { 
      col_ <- c(col_, rep(tree_col[[i]], length(Benefit_Risk2$val[Benefit_Risk2$val$Level1 == unique(Benefit_Risk2$val$Level1)[[i]] & Benefit_Risk2$val$Level2 == "", ]$Level1)))
    }
    for(i in 1:length(unique(Benefit_Risk2$val$Level1))) { 
      col_ <- c(col_, rep(tree_col[[i]], length(Benefit_Risk2$val[Benefit_Risk2$val$Level1 == unique(Benefit_Risk2$val$Level1)[[i]] & Benefit_Risk2$val$Level2 != ""  & Benefit_Risk2$val$Level3 == "", ]$Level2)))
    }
    for(i in 1:length(unique(Benefit_Risk2$val$Level1))) { 
      col_ <- c(col_, rep(tree_col[[i]], length(Benefit_Risk2$val[Benefit_Risk2$val$Level1 == unique(Benefit_Risk2$val$Level1)[[i]] & Benefit_Risk2$val$Level3 != ""  & Benefit_Risk2$val$Level4 == "", ]$Level3)))
    }
    for(i in 1:length(unique(Benefit_Risk2$val$Level1))) { 
      col_ <- c(col_, rep(tree_col[[i]], length(Benefit_Risk2$val[Benefit_Risk2$val$Level1 == unique(Benefit_Risk2$val$Level1)[[i]]  & Benefit_Risk2$val$Level4 != "", ]$Level4)))
    }
    
    collapsibleTree::collapsibleTree(
      df = Benefit_Risk2$val,
      root ="Benefit-Risk",
      hierarchy = c("Level1", "Level2", "Level3","Level4"),
      collapsed = FALSE,
      fill = col_
    )
  }
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.html",
    content = function(file) {
      r2d3::save_d3_html(plotInput(), file = file)
    })    
  
  # Value tree output with package collapsibleTree
  output$value_tree <- collapsibleTree::renderCollapsibleTree(
    plotInput()
  )
  
  # create text Input for adding node names (has to be a unique name!)
  output$add_text <- shiny::renderUI({
    shiny::textInput(
      inputId = ns("add_text"),
      label = HTML('<p style = "color: #337ab7;"> Type new node name: </p>'),
      value = "",
      placeholder = "New node name"
    )
  })
  
  output$add_selection <- shiny::renderUI({
    choices <- unique(unlist(Benefit_Risk$val[, 1:3]))[unique(unlist(Benefit_Risk$val[, 1:3])) != ""]
    selected <- add_selection_selected$val
    input$remove_node
    shiny::selectizeInput(
      inputId = ns("add_selection"),
      label = HTML('<p style = "color: #337ab7;"> Add a new node to node: </p>'),
      choices = choices,
      selected = selected
    )
  })
  
  shiny::observeEvent(input$shift_node, {
    BR <- Benefit_Risk$val  
    shiny::req(input$shift_selection_from)
    
    index1 <- as.integer(which(apply(BR, 1, function(x) {any(x==input$shift_selection_from)})))
    index2 <- as.integer(which(apply(BR, 2, function(x) {any(x==input$shift_selection_from)})))
    index3 <- as.numeric(which(apply(BR, 2, function(x) {any(x==input$shift_selection_to)})))
    
    BR2 <- BR[-index1,]

    BR_tmp <- BR2[which(apply(BR2, 1, function(x) {any(x == input$shift_selection_to)})),]
    index_tmp <- which(BR_tmp[, index3 + 1] == "")
    
    BR_add <- BR_tmp[index_tmp, ]
    BR_add[,index3 + 1] <- input$shift_selection_from
    
    BR_Res <- rbind(BR2, BR_add)

    Benefit_Risk$val <- BR_Res
  })
  
  add_selection_selected <- shiny::reactiveValues(
    val = "Benefits"
  )
  
  shiny::observeEvent(input$add_selection, {
    add_selection_selected$val <- input$add_selection
  })
  
  # update reactive Value data.frame Benefit_Risk$val when add_node button was clicked
  shiny::observeEvent(input$add_node, {
    BR <- Benefit_Risk$val
    index_row <- which(apply(BR, 1, function(x){any(x == input$add_selection)}))[1]
    
    BR2 <- BR[index_row, ]
    index_col <- which(BR2 == input$add_selection)
    BR2[index_col + 1] <- input$add_text
    
    BR_ <- rbind(Benefit_Risk$val, BR2)
    BR_ <- BR_[!duplicated(BR_), ]
    Benefit_Risk$val <- BR_
  })
  
  output$remove_selection <- shiny::renderUI({
    input$add_node
    input$remove_node
    BR <- Benefit_Risk$val
    shiny::selectizeInput(
      inputId = ns("remove_selection"),
      label = HTML('<p style = "color: #337ab7;"> Remove node: </p>'),
      choices = unique(unlist(Benefit_Risk$val[, 2:4]))[unique(unlist(Benefit_Risk$val[, 2:4])) != ""],
      selected = add_selection_selected$val
    )
  })
  
  output$shift_selection_from <- shiny::renderUI({
    input$add_node
    input$shift_node
    BR <- Benefit_Risk$val
    shiny::selectizeInput(
      inputId = ns("shift_selection_from"),
      label = HTML('<p style = "color: #337ab7;"> Shift node: </p>'),
      choices = unique(unlist(Benefit_Risk$val[, 2:4]))[unique(unlist(Benefit_Risk$val[, 2:4])) != ""],
      selected = add_selection_selected$val
    )
  })
  
  output$shift_selection_to <- shiny::renderUI({
    input$add_node
    input$shift_node
    BR <- Benefit_Risk$val
    choices <- unique(unlist(BR[, 1:3]))[unique(unlist(BR[, 1:3])) != ""]
    if( any(choices == input$shift_selection_from)) {
      choices <- choices[-which(choices == input$shift_selection_from)]
    }
    shiny::selectizeInput(
      inputId = ns("shift_selection_to"),
      label = HTML('<p style = "color: #337ab7;"> Shift to node: </p>'),
      choices = choices,
      selected = add_selection_selected$val
    )
  })
  
  shiny::observeEvent(input$remove_node, {
    if(input$remove_selection != "") {
      BR <- Benefit_Risk$val
      
      BR <- BR[!apply(BR == input$remove_selection, 1, any), ]
      
      #refactor to update selectinput
      BR$Level2 <- factor(BR$Level2)
      BR$Level3 <- factor(BR$Level3)
      BR$Level4 <- factor(BR$Level4)
      Benefit_Risk$val <- BR
    }
  })
}
    
## To be copied in the UI
# mod_value_tree_ui("value_tree_ui_1")
    
## To be copied in the server
# callModule(mod_value_tree_server, "value_tree_ui_1")
 
