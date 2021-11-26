#' #' File creation UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd 
#' #'
#' #' @importFrom shiny NS tagList 
#' file_creation_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     shiny::uiOutput({
#'        ns('start_text')
#'     }),
#'     shinyWidgets::prettyRadioButtons(
#'          inputId = ns('adtte_data'),
#'          label = 'Select SAS data', 
#'          shape = 'round', 
#'          animation = 'smooth',
#'          choices = c(
#'            "SAS file (from Disc)" = "sas",
#'            "SAS file (from Server)" = "server"
#'          )
#'        ),
#'         shiny::conditionalPanel(
#'           condition = paste0("input['", ns("adtte_data"), "\'] == \'sas\'"),
#'           #condition = "input.adtte_data == 'sas'",
#'           shiny::fluidRow(
#'             column(6,
#'               shiny::fileInput(
#'                 inputId =  ns("adtte_file"), 
#'                 label = "ADTTE data",
#'                 multiple = FALSE,
#'                 accept = NULL,
#'                 width = NULL
#'               ),
#'               shiny::uiOutput(ns("wrong_adtte_format_text"))
#'             ),
#'             column(6,
#'               shiny::fileInput(
#'                 inputId =  ns("adsl_file"), 
#'                 label = "ADSL data (optional for treatment variable)",
#'                 multiple = FALSE,
#'                 accept = NULL,
#'                 width = NULL
#'               ),
#'               shiny::uiOutput(ns("wrong_adsl_format_text"))
#'             )
#'          )
#'       ),
#'       shiny::conditionalPanel(
#'        condition = paste0("input['", ns("adtte_data"), "\'] == \'server\'"),
#'          shiny::uiOutput(ns("studySelect")),
#'          shiny::textInput(
#'            inputId =  ns("user"),
#'            label = "Username:"
#'            ),
#'          shiny::passwordInput(
#'            inputId =  ns("password"),
#'            label = "Password:"
#'            ),
#'          shiny::tags$br(),
#'          shiny::actionButton(
#'            inputId =  ns("retrieve_files"),
#'            label = "Retrieve files",
#'            icon = icon("download"),
#'            style = paste0("color:#FFFFFF ; background-color: ", breasy_blue, ";")
#'          )
#'        ),
#'        shiny::fluidRow(
#'          shiny::column(4,
#'            shiny::uiOutput(ns("TrtV"))
#'          ),
#'          shiny::column(4,
#'            shiny::uiOutput(ns("verumV"))
#'          ),
#'          shiny::column(4, 
#'            shiny::uiOutput(ns("comparV"))
#'          )
#'        ),
#'        shiny::fluidRow(
#'          shiny::column(4,
#'            shiny::uiOutput(ns("parameter"))
#'          ),
#'          shiny::column(4,
#'            shiny::uiOutput(ns("visit"))
#'          ),
#'          shiny::column(4,
#'           shiny::uiOutput(ns("estimates"))  
#'          )
#'        ),
#'       shinyBS::bsCollapse(
#'         shinyBS::bsCollapsePanel(
#'           shiny::HTML('<p style="color:black; font-size:100%;"> ADTTE (+ADSL) data: </p>'),
#'           shiny::wellPanel(
#'             id = "table_adtte_Panel",
#'             style = "color:black; overflow-y:scroll; max-height: 600px",
#'             shiny::dataTableOutput(ns('table_adtte'))
#'           )
#'         ),
#'         shinyBS::bsCollapsePanel(
#'           shiny::HTML('<p style="color:black; font-size:100%;"> CSV Output file: </p>'),
#'           shiny::wellPanel(
#'             id = "table_csv_Panel",
#'             style = "color:black; overflow-y:scroll; max-height: 600px",
#'             shiny::dataTableOutput(ns('table_csv'))
#'           )
#'         )
#'       )
#'     )
#' }
#'     
#' #' File creation Server Function
#' #'
#' #' @noRd 
#' file_creation_server <- function(input, output, session){
#'   ns <- session$ns
#'   
#'   #Start text
#'   output$start_text <- shiny::renderUI({
#'     list(
#'       HTML(
#'         "
#'           <h1> Create a csv file from your ADTTE SAS file </h1>
#'           <p> Required variables in ADTTE: </p>
#'           
#'         "
#'       )
#'     )
#'   })
#'   
#'   #Reactive object to read ADTTE data set
#'   adtte_data <- shiny::reactive({
#'     if (is.null(input$adtte_file)) {
#'       return(NULL) 
#'     } else {
#'       #check if file format is .sas7bdat or .sas7cdat
#'       #extract the data path ending
#'       inFile <- input$adtte_file$datapath
#'       split_path <- strsplit(x = inFile, split = "[.]")
#'       path_ending <- split_path[[1]][length(split_path[[1]])]
#'       if (path_ending %in% c("sas7bdat", "sas7cdat")) {
#'         
#'         adtte <- haven::read_sas(input$adtte_file$datapath)
#'         if (!is.null(input$adsl_file)) {
#'           inFile2 <- input$adsl_file$datapath
#'           split_path2 <- strsplit(x = inFile2, split = "[.]")
#'           path_ending2 <- split_path2[[1]][length(split_path2[[1]])]
#'           if (path_ending2 %in% c("sas7bdat", "sas7cdat")) {
#'             adsl <- haven::read_sas(input$adsl_file$datapath)
#'             same_colnames <- intersect(colnames(adsl),colnames(adtte))
#'             #remove ADSNAME (dataset name) for merging 
#'             if ("ADSNAME" %in% same_colnames) {
#'               same_colnames <- same_colnames[-which(same_colnames == "ADSNAME")]
#'             }
#'             adtte <- adtte %>% 
#'               dplyr::left_join(adsl, by = same_colnames)
#'           } else {
#'             output$wrong_adsl_format_text <- shiny::renderUI({
#'               HTML(paste0("
#'               <b style = 'color:#E43157'>
#'                 Wrong data format! Please upload SAS data in .sas7bdat or .sas7cdat format!
#'               </b>"))
#'             })
#'           }
#'         }
#'         adtte
#'       } else {
#'         output$wrong_adtte_format_text <- shiny::renderUI({
#'           HTML(paste0("
#'           <b style = 'color:#E43157'>
#'             Wrong data format! Please upload SAS data in .sas7bdat or .sas7cdat format!
#'           </b>"))
#'         })
#'         return(NULL)
#'       }
#'     }
#'     
#'   })
#'   
#'   output$TrtV <- shiny::renderUI({
#'     if (is.null(adtte_data())) {
#'       return()
#'     } else { 
#'       choices <- as.list(names(adtte_data())) 
#'       choices <- c(choices[stringr::str_detect(choices, "TRT01")], choices[!(stringr::str_detect(choices, "TRT01"))])
#'     }
#'     shinyWidgets::pickerInput(
#'       inputId = ns("Sel_Trt"),
#'       label = "Select treatment variable",
#'       choices = choices,
#'       selected = NULL,
#'       multiple = TRUE,
#'       options = shinyWidgets::pickerOptions(maxOptions = 1)
#'     )
#'   })
#'   
#'   output$verumV <- shiny::renderUI({
#'     shiny::req(input$Sel_Trt)
#'     if (is.null(req(input$Sel_Trt))) return()
#'     else { 
#'       if (is.factor(adtte_data()[, which(names(adtte_data()) == input$Sel_Trt)])) {
#'         choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$Sel_Trt)]))
#'       } else {
#'         choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$Sel_Trt)]))
#'       }
#'     }
#'     shinyWidgets::pickerInput(
#'       inputId = ns("verum"),
#'       label = "Select Verum",
#'       choices = choices,
#'       selected = NULL,
#'       multiple = TRUE,
#'       options = list(
#'         `actions-box` = TRUE,
#'         `selected-text-format` = "count > 0",
#'         `count-selected-text` = "{0} selected (of {1})",
#'         `live-search` = TRUE,
#'         `header` = "Select multiple items",
#'         `none-selected-text` = "No selection!"
#'       )
#'     )
#'   })
#'   
#'   output$comparV <- shiny::renderUI({
#'     shiny::req(input$Sel_Trt)
#'     if (is.null(input$Sel_Trt)) return()
#'     else { 
#'       if (is.factor(adtte_data()[, which(names(adtte_data()) == input$Sel_Trt)])) {
#'         choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$Sel_Trt)]))
#'         } else {
#'           choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$Sel_Trt)]))
#'         }
#'     }
#'     
#'     shinyWidgets::pickerInput(
#'       inputId = ns("compa"),
#'       label = "Select Comparator",
#'       choices = choices ,
#'       selected = NULL,
#'       multiple = TRUE,
#'       options = list(
#'         `actions-box` = TRUE,
#'         `selected-text-format` = "count > 0",
#'         `count-selected-text` = "{0} selected (of {1})",
#'         `live-search` = TRUE,
#'         `header` = "Select multiple items",
#'         `none-selected-text` = "No selection!"
#'       )
#'     )
#'   })
#'   
#'    output$parameter <- shiny::renderUI({
#'     if (is.null(adtte_data())) return()
#'     else { 
#'       choices <- as.list(names(adtte_data())) 
#'       choices <- c(choices[stringr::str_detect(choices, "PARA")], choices[!(stringr::str_detect(choices, "PARA"))])
#'     }
#'     
#'     shinyWidgets::pickerInput(
#'       inputId = ns("parameter"),
#'       label = "Select parameter variable",
#'       choices = choices ,
#'       selected = choices[1],
#'       multiple = FALSE,
#'       options = list(
#'         `actions-box` = TRUE,
#'         `selected-text-format` = "count > 0",
#'         `count-selected-text` = "{0} selected (of {1})",
#'         `live-search` = TRUE,
#'         `header` = "Select multiple items",
#'         `none-selected-text` = "No selection!"
#'       )
#'     )
#'   })
#'    
#'     output$visit <- shiny::renderUI({
#'     if (is.null(adtte_data())) return()
#'     else { 
#'       choices <- as.list(names(adtte_data())) 
#'       choices <- c(choices[stringr::str_detect(choices, "AVIS")], choices[!(stringr::str_detect(choices, "AVIS"))])
#'     }
#'     
#'     shinyWidgets::pickerInput(
#'       inputId = ns("visit"),
#'       label = "Select visit variable",
#'       choices = choices ,
#'       selected = choices[1],
#'       multiple = FALSE,
#'       options = list(
#'         `actions-box` = TRUE,
#'         `selected-text-format` = "count > 0",
#'         `count-selected-text` = "{0} selected (of {1})",
#'         `live-search` = TRUE,
#'         `header` = "Select multiple items",
#'         `none-selected-text` = "No selection!"
#'       )
#'     )
#'   })
#'     
#'   output$estimates <- shiny::renderUI({
#'     if (is.null(adtte_data())) return()
#'     else { 
#'       #### add New Functions to Pickerinput here ####
#'       choices <- c("Risk differences", "Relative risks")
#'     }
#'     
#'     shinyWidgets::pickerInput(
#'       inputId = ns("estimates"),
#'       label = "Select estimate(s)",
#'       choices = choices ,
#'       selected = choices[1],
#'       multiple = TRUE,
#'       options = list(
#'         `actions-box` = TRUE,
#'         `selected-text-format` = "count > 0",
#'         `count-selected-text` = "{0} selected (of {1})",
#'         `live-search` = TRUE,
#'         `header` = "Select multiple items",
#'         `none-selected-text` = "No selection!"
#'       )
#'     )
#'   })
#'   
#'   adtte_data2 <- shiny::reactive({
#'     shiny::req(adtte_data())
#'     shiny::req(input$Sel_Trt)
#'     shiny::req(input$compa)
#'     shiny::req(input$verum)
#'     if (is.null(adtte_data())) {
#'       return(NULL) 
#'     } else {
#'       Sel_trt <- shiny::req(input$Sel_Trt)
#'       #SAStmp <- adtte_data()[which(adtte_data()[,which(names(adtte_data())==Sel_trt)] == input$verum | adtte_data()[,which(names(adtte_data())==Sel_trt)] == input$compa),]
#'       if (!is.null(input$verum) & !is.null(input$compa))
#'         tmp <- adtte_data() %>% 
#'           dplyr::filter(!!rlang::sym(Sel_trt) == input$verum | !!rlang::sym(Sel_trt) == input$compa)
#'       #SAStmp[order(SAStmp$PARAM, SAStmp$AVISITN, SAStmp[,which(names(SAStmp)==input$Sel_Trt)]),]
#'     }
#'     tmp
#'   })
#'   
#'   csv_file <- shiny::reactive({
#'     if (is.null(adtte_data2())) {
#'       # print("TRUE")
#'       return(NULL)
#'     } else {
#'       if(dim(adtte_data2())[1] == 0) {
#'         return(NULL)
#'       } else {
#'       
#'       test_parameter <<- input$parameter
#'       test_visit <<- input$visit
#'       test_adtte_data2 <<- adtte_data2()
#'       test_estimates <<- input$estimates
#'       
#'       ## alternative 
#'       test_adtte_data2 %>% dplyr::group_by(!!!rlang::syms(c(test_parameter, test_visit))) %>% summarise(N = n())
#'       #test_adtte_data2 %>% dplyr::group_by(!!!rlang::syms(c(test_parameter, test_visit))) %>% summarise(N = n_distinct(USUBJID))
#'       
#'       
#'       csv1 <- unique(adtte_data2()[,which(names(adtte_data2()) %in% c(input$parameter, input$visit))])
#'       csv1_test <<- csv1
#'       names(csv1)[which(names(csv1) == input$parameter)] <- "OUTCOME"
#'     #   
#'     #   
#'     #   ### CHECK csv1 ### 
#'       csv_1 <- merge(input$estimates, data.frame(csv1))
#'       colnames(csv_1)[1] <- "ESTIMATE"
#'     #   
#'       csv2 <- cbind(
#'         csv_1, NUMBER_EVENTS_VERUM=rep(0, dim(csv_1)[1]),
#'         NUMBER_PATIENTS_VERUM=rep(0, dim(csv_1)[1]), NUMBER_EVENTS_COMP=rep(0, dim(csv_1)[1]),
#'         NUMBER_PATIENTS_COMP=rep(0, dim(csv_1)[1]), EFFECT=rep(0, dim(csv_1)[1]), LOWER95=rep(0, dim(csv_1)[1]), UPPER95=rep(0, dim(csv_1)[1])
#'       )
#'       
#'       Sel_trt <- input$Sel_Trt
#'       
#'       PARAM_levels <- unique(adtte_data2()[[input$parameter]])
#'       AVISITN_levels <- unique(adtte_data2()[[input$visit]])
#' 
#'       for (i in 1:length(PARAM_levels)) {
#'         for (j in 1:length(AVISITN_levels)) {
#'           if (dim(adtte_data2()[which(adtte_data2()[[input$parameter]]==PARAM_levels[i] & adtte_data2()[[input$visit]]==AVISITN_levels[j]),])[1] != 0) {
#'             NR_CNSR_VERUM <- adtte_data2() %>% 
#'               dplyr::filter(Sel_trt == input$verum) %>%
#'               dplyr::select(CNSR) %>%
#'               sum()
#'             #NR_CNSR_VERUM <- sum(adtte_data2()[which(adtte_data2()[[input$parameter]]==PARAM_levels[i] & adtte_data2()[[input$visit]]==AVISITN_levels[j] & adtte_data2()[,which(names(adtte_data2())==Sel_trt)] == input$verum),]$CNSR)
#'             NR_CNSR_COMP <- adtte_data2() %>% 
#'               dplyr::filter(Sel_trt == input$compa) %>%
#'               dplyr::select(CNSR) %>%
#'               sum()
#'             
#'             
#'             NR_CNSR_COMP <- sum(adtte_data2()[which(adtte_data2()[[input$parameter]]==PARAM_levels[i] & adtte_data2()[[input$visit]]==AVISITN_levels[j] & adtte_data2()[,which(names(adtte_data2())==Sel_trt)] == input$compa),]$CNSR)
#'             csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_VERUM <- 
#'               dim(test_adtte_data2[which(test_adtte_data2[[test_parameter]]==PARAM_levels[i] & test_adtte_data2[[test_visit]]==AVISITN_levels[j] & test_adtte_data2[,Sel_trt] == test_verum),])[1]
#'             
#'             csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_COMP <- dim(test_adtte_data2[which(test_adtte_data2[[test_parameter]]==PARAM_levels[i] & test_adtte_data2[[test_visit]]==AVISITN_levels[j] & test_adtte_data2[,which(names(test_adtte_data2)==Sel_trt)] ==test_compa),])[1]
#'             csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_EVENTS_VERUM <- csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_VERUM - NR_CNSR_VERUM
#'             csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_EVENTS_COMP <- csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_COMP - NR_CNSR_COMP
#'           }
#'         }
#'       }
#'       
#'       # for (i in 1:length(PARAM_levels)) {
#'       #   for (j in 1:length(AVISITN_levels)) {
#'       #     if (dim(adtte_data2[which(adtte_data2[[parameter]]==PARAM_levels[i] & adtte_data2[[visit]]==AVISITN_levels[j]),])[1] != 0) {
#'       #       csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NR_CNSR_VERUM <- sum(adtte_data2[which(adtte_data2[[parameter]]==PARAM_levels[i] & adtte_data2[[visit]]==AVISITN_levels[j] & adtte_data2[,which(names(adtte_data2)==Sel_trt)] == verum),]$CNSR)
#'       #       csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NR_CNSR_COMP <- sum(adtte_data2[which(adtte_data2[[parameter]]==PARAM_levels[i] & adtte_data2[[visit]]==AVISITN_levels[j] & adtte_data2[,which(names(adtte_data2)==Sel_trt)] == compa),]$CNSR)
#'       #       csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_VERUM <- dim(adtte_data2[which(adtte_data2[[parameter]]==PARAM_levels[i] & adtte_data2[[visit]]==AVISITN_levels[j] & adtte_data2[,which(names(adtte_data2)==Sel_trt)] == verum),])[1]
#'       #       csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_COMP <- dim(adtte_data2[which(adtte_data2[[parameter]]==PARAM_levels[i] & adtte_data2[[visit]]==AVISITN_levels[j] & adtte_data2[,which(names(adtte_data2)==Sel_trt)] == compa),])[1]
#'       #       csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_EVENTS_VERUM <- csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_VERUM - NR_CNSR_VERUM
#'       #       csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_EVENTS_COMP <- csv2[which(csv2$OUTCOME==PARAM_levels[i] & csv2$AVISITN == AVISITN_levels[j]),]$NUMBER_PATIENTS_COMP - NR_CNSR_COMP
#'       #     }
#'       #   }
#'       # }
#'       
#'      #csv_test <<- csv2
#'     #    
#'       csv2_test <<- csv2
#'       # if ("Risk differences" %in% input$estimates) {
#'       #  for (a in 1:dim(csv2)[1]) {
#'       #    riskCI <- fmsb::riskdifference(csv2[a,]$NUMBER_EVENTS_VERUM, csv2[a,]$NUMBER_EVENTS_COMP, csv2[a,]$NUMBER_PATIENTS_VERUM, csv2[a,]$NUMBER_PATIENTS_COMP)
#'       #    csv2[a,]$EFFECT <- riskCI[["estimate"]]
#'       #    csv2[a,which(names(csv2) %in% c("LOWER95", "UPPER95"))] <- c(riskCI[["conf.int"]])
#'       #  }
#'       # }
#'       
#'     #   return(csv2)
#'         return(csv2)       
#'       }
#'      }
#'   })
#'   
#'   output$table_adtte <- renderDataTable(adtte_data2(), options = list(autoWidth = FALSE))
#'   output$table_csv <- renderDataTable(csv_file(), options = list(autoWidth = FALSE))
#' }
#'     
#' ## To be copied in the UI
#' # file_creation_ui("file_creation")
#'     
#' ## To be copied in the server
#' # callModule(file_creation_server, "file_creation")
#'  
