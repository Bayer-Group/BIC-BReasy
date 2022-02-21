#' File creation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
file_creation_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput({
       ns('start_text')
    }),
    shinyWidgets::prettyRadioButtons(
         inputId = ns('adtte_data'),
         label = 'Select SAS data',
         shape = 'round',
         animation = 'smooth',
         choices = c(
           "SAS file (from Disc)" = "sas",
           "SAS file (from Server)" = "server"
         )
       ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("adtte_data"), "\'] == \'sas\'"),
          #condition = "input.adtte_data == 'sas'",
          shiny::fluidRow(
            column(6,
              shiny::fileInput(
                inputId =  ns("adtte_file"),
                label = "ADTTE data",
                multiple = FALSE,
                accept = NULL,
                width = NULL
              ),
              shiny::uiOutput(ns("wrong_adtte_format_text"))
            ),
            column(6,
              shiny::fileInput(
                inputId =  ns("adsl_file"),
                label = "ADSL data (optional for treatment variable)",
                multiple = FALSE,
                accept = NULL,
                width = NULL
              ),
              shiny::uiOutput(ns("wrong_adsl_format_text"))
            )
         )
      ),
      shiny::conditionalPanel(
       condition = paste0("input['", ns("adtte_data"), "\'] == \'server\'"),
         shiny::uiOutput(ns("studySelect")),
         shiny::textInput(
           inputId =  ns("user"),
           label = "Username:"
           ),
         shiny::passwordInput(
           inputId =  ns("password"),
           label = "Password:"
           ),
         shiny::tags$br(),
         shiny::actionButton(
           inputId =  ns("retrieve_files"),
           label = "Retrieve files",
           icon = icon("download"),
           style = paste0("color:#FFFFFF ; background-color: ", breasy_blue, ";")
         )
       ),
       shiny::fluidRow(
         shiny::column(4,
            shiny::uiOutput(ns("sel_treatment")),
            shiny::uiOutput(ns("sel_treatment_check"))
         ),
         shiny::column(4,
           shiny::uiOutput(ns("sel_verum")),
           shiny::uiOutput(ns("sel_verum_check"))
         ),
         shiny::column(4,
           shiny::uiOutput(ns("sel_comparator")),
           shiny::uiOutput(ns("sel_comparator_check"))
         )
       ),
       shiny::fluidRow(
         shiny::column(4,
           shiny::uiOutput(ns("parameter"))
         ),
         shiny::column(4,
           shiny::uiOutput(ns("sel_parameter"))
         )
       ),
        shiny::fluidRow(
         shiny::column(4,
           shiny::uiOutput(ns("analysis_set"))
          
           #shiny::uiOutput(ns("visit"))
         ),
          shiny::column(4,
          shiny::uiOutput(ns("subject_identifier"))  
          )
        ),
        fluidRow(
         shiny::column(4,
          shiny::uiOutput(ns("data_scope"))
         
         ),
          shiny::column(4,
            shiny::uiOutput(ns("sel_data_scope"))
         
         )
       ),
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> Filter: </p>'),
          "Filter options"
        )
      ),
      fluidRow(
        column(4,
        shiny::uiOutput(ns("event_identifyer"))
        ),
        column(4,
          shiny::uiOutput(ns("sel_event_identifyer"))
        )#,
        # column(4,
        #   shiny::uiOutput(ns("sel_censor_identifyer"))
        # )
       
      ),
      fluidRow(
        column(4,
        shiny::uiOutput(ns("estimates"))
        )
      ),
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> Advanced settings: </p>'),
          "Advanced settings",
          shiny::uiOutput(ns("stratification"))
        )
      ),
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> ADTTE (+ADSL) data: </p>'),
          shiny::wellPanel(
            id = "table_adtte_Panel",
            style = "color:black; overflow-y:scroll; max-height: 600px",
            shiny::dataTableOutput(ns('table_adtte'))
          )
        ),
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> CSV Output file: </p>'),
          shiny::wellPanel(
            id = "table_csv_Panel",
            style = "color:black; overflow-y:scroll; max-height: 600px",
            shiny::dataTableOutput(ns('table_csv'))
          )
        )),
        shiny::fluidRow(
          shiny::actionButton(
          inputId = ns("save_button"),
          label = "Save as .csv",
          icon = icon("save"),
          style = "color: #fff;
            background-color: #999999;
            border-color: #2e6da4;"
          )
        
      )
    )
}

#' File creation Server Function
#'
#' @noRd
file_creation_server <- function(input, output, session){
  ns <- session$ns

  #Start text
  output$start_text <- shiny::renderUI({
    list(
      HTML(
        "
          <h1> Create a csv file from your ADTTE SAS file </h1>
          <p> Required variables in ADTTE: </p>

        "
      )
    )
  })

  #Reactive object to read ADTTE data set
  adtte_data <- shiny::reactive({
    if (is.null(input$adtte_file)) {
      return(NULL)
    } else {
      #check if file format is .sas7bdat or .sas7cdat
      #extract the data path ending
      inFile <- input$adtte_file$datapath
      split_path <- strsplit(x = inFile, split = "[.]")
      path_ending <- split_path[[1]][length(split_path[[1]])]
      if (path_ending %in% c("sas7bdat", "sas7cdat")) {

        adtte <- haven::read_sas(input$adtte_file$datapath)
        if (!is.null(input$adsl_file)) {
          inFile2 <- input$adsl_file$datapath
          split_path2 <- strsplit(x = inFile2, split = "[.]")
          path_ending2 <- split_path2[[1]][length(split_path2[[1]])]
          if (path_ending2 %in% c("sas7bdat", "sas7cdat")) {
            adsl <- haven::read_sas(input$adsl_file$datapath)
            same_colnames <- intersect(colnames(adsl),colnames(adtte))
            #remove ADSNAME (dataset name) for merging
            if ("ADSNAME" %in% same_colnames) {
              same_colnames <- same_colnames[-which(same_colnames == "ADSNAME")]
            }
            adtte <- adtte %>%
              dplyr::left_join(adsl, by = same_colnames)
          } else {
            output$wrong_adsl_format_text <- shiny::renderUI({
              HTML(paste0("
              <b style = 'color:#E43157'>
                Wrong data format! Please upload SAS data in .sas7bdat or .sas7cdat format!
              </b>"))
            })
          }
        }
        adtte
      } else {
        output$wrong_adtte_format_text <- shiny::renderUI({
          HTML(paste0("
          <b style = 'color:#E43157'>
            Wrong data format! Please upload SAS data in .sas7bdat or .sas7cdat format!
          </b>"))
        })
        return(NULL)
      }
    }

  })
  
  #### Select treatment variable ####
  output$sel_treatment <- shiny::renderUI({
    if (is.null(adtte_data())) {
      return()
    } else {
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "TRT")], choices[!(stringr::str_detect(choices, "TRT"))])
      # possible treatment variable names (add more here if nessecary)
      trt_variable_names <- c("TRT01P", "TRT01PN", "TRT01A", "TRT01AN","TRTP", "TRTPN", "TRTA", "TRTAN")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- trt_variable_names[which(trt_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_treatment"),
      label = "Select treatment variable",
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1)
    )
  })
  treatment_check_flag <- shiny::reactiveValues(val = FALSE)
  shiny::observeEvent(c(adtte_data(), input$sel_treatment), {
    shiny::req(adtte_data())
    if (is.null(input$sel_treatment)) {
      output$sel_treatment_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            Please select a treatment variable. </i></span>'
          )
        )
      })
      treatment_check_flag$val <- FALSE
    } else {
      treatment_check_flag$val <- TRUE
      output$sel_treatment_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #16de5f"> <i class="fa fa-check"></i></span>'
          )
        )
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  #### Select verum variable ####
  output$sel_verum <- shiny::renderUI({
    shiny::req(input$sel_treatment)
    if (is.null(req(input$sel_treatment))) return()
    else {
      if (is.factor(adtte_data()[, which(names(adtte_data()) == input$sel_treatment)])) {
        choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$sel_treatment)]))
      } else {
        choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$sel_treatment)]))
      }
    }
    # if only two choices are available and one is placebo, select the non-placebo term 
    # as default selection
    if (length(as.vector(unlist(choices))) == 2 & any(c("PLACEBO","Placebo","placebo") %in% as.vector(unlist(choices)))) {
      selected <- as.vector(unlist(choices))[!as.vector(unlist(choices)) %in% c("PLACEBO","Placebo","placebo")]
    } else {
      selected <- NULL
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_verum"),
      label = "Select Verum",
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  verum_check_flag <- shiny::reactiveValues(val = FALSE)
  shiny::observeEvent(c(adtte_data(), input$sel_treatment, input$sel_verum), {
    shiny::req(adtte_data())
    
    if (!is.null(input$sel_treatment)){
    if (is.null(input$sel_verum)) {
      output$sel_verum_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            Please select verum term. </i></span>'
          )
        )
      })
      verum_check_flag$val <- FALSE
    } else {
      verum_check_flag$val <- TRUE
      output$sel_verum_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #16de5f"> <i class="fa fa-check"></i></span>'
          )
        )
      })
    }
    } else {
     output$sel_verum_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            ''
          )
        )
      })
    } 
     
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  #### Select comparator variable ####
  output$sel_comparator <- shiny::renderUI({
    shiny::req(input$sel_treatment)
    if (is.null(req(input$sel_treatment))) return()
    else {
      if (is.factor(adtte_data()[, which(names(adtte_data()) == input$sel_treatment)])) {
        choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$sel_treatment)]))
      } else {
        choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$sel_treatment)]))
      }
    }
    # if only two choices are available and one is placebo, select the non-placebo term 
    # as default selection
    if (length(as.vector(unlist(choices))) == 2 & any(c("PLACEBO","Placebo","placebo") %in% as.vector(unlist(choices)))) {
      selected <- as.vector(unlist(choices))[as.vector(unlist(choices)) %in% c("PLACEBO","Placebo","placebo")]
    } else {
      selected <- NULL
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_comparator"),
      label = "Select comparator",
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  comparator_check_flag <- shiny::reactiveValues(val = FALSE)
  shiny::observeEvent(c(adtte_data(), input$sel_treatment, input$sel_comparator), {
    shiny::req(adtte_data())
    
    if (!is.null(input$sel_treatment)){
    if (is.null(input$sel_comparator)) {
      output$sel_comparator_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            Please select comparator term. </i></span>'
          )
        )
      })
      comparator_check_flag$val <- FALSE
    } else {
      comparator_check_flag$val <- TRUE
      output$sel_comparator_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #16de5f"> <i class="fa fa-check"></i></span>'
          )
        )
      })
    }
    } else {
     output$sel_comparator_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            ''
          )
        )
      })
    } 
     
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

   output$parameter <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "PARA")], choices[!(stringr::str_detect(choices, "PARA"))])
    }

    shinyWidgets::pickerInput(
      inputId = ns("parameter"),
      label = "Select outcome variable",
      choices = choices ,
      selected = choices[1],
      multiple = FALSE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
   
   output$sel_parameter <- shiny::renderUI({
    shiny::req(input$parameter)
    if (is.null(req(input$parameter))) return()
    else {
      if (is.factor(adtte_data()[, which(names(adtte_data()) == input$parameter)])) {
        choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$parameter)]))
      } else {
        choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$parameter)]))
      }
    }

    shinyWidgets::pickerInput(
      inputId = ns("sel_parameter"),
      label = "Outcome",
      choices = choices,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })

  output$analysis_set <- shiny::renderUI({
    if (is.null(adtte_data())) { return()
      } else {
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "SAFFL")], choices[!(stringr::str_detect(choices, "SAFFL"))])
   
      shinyWidgets::pickerInput(
        inputId = ns("analysis_set"),
        label = "Analysis set",
        choices = choices ,
        selected = choices[1],
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 0",
          `count-selected-text` = "{0} selected (of {1})",
          `live-search` = TRUE,
          `header` = "Select multiple items",
          `none-selected-text` = "No selection!"
        )
      )
    }
  })

  output$estimates <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- c("Incidence Rate (by 100 pat-yrs)")
    }

    shinyWidgets::pickerInput(
      inputId = ns("estimates"),
      label = "Select estimate(s)",
      choices = choices ,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  
  
  
  output$subject_identifier <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "SUBJID")], choices[!(stringr::str_detect(choices, "SUBJID"))])
    }
    
    shinyWidgets::pickerInput(
      inputId = ns("subject_identifier"),
      label = "Subject identifier",
      choices = choices ,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  
  output$event_identifyer <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      #### add New Functions to Pickerinput here ####
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "CNSR")], choices[!(stringr::str_detect(choices, "CNSR"))])
    }
    
    shinyWidgets::pickerInput(
      inputId = ns("event_identifyer"),
      label = "Event identifyer",
      choices = choices ,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  
  output$sel_event_identifyer <- shiny::renderUI({
    shiny::req(input$event_identifyer)
    if (is.null(req(input$event_identifyer))) return()
    else {
      if (is.factor(adtte_data()[, which(names(adtte_data()) == input$event_identifyer)])) {
        choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$event_identifyer)]))
      } else {
        choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$event_identifyer)]))
      }
    }

    shinyWidgets::pickerInput(
      inputId = ns("sel_event_identifyer"),
      label = "Select Event",
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  
  output$data_scope <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "AVISIT")], choices[!(stringr::str_detect(choices, "AVISIT"))])
    }

    shinyWidgets::pickerInput(
      inputId = ns("data_scope"),
      label = "Data scope",
      choices = choices ,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
  
   output$stratification <- shiny::renderUI({
   
    choices <- c("No selection")

    shinyWidgets::pickerInput(
      inputId = ns("stratification"),
      label = "Stratification",
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })

   output$sel_data_scope <- shiny::renderUI({
    shiny::req(input$data_scope)
    if (is.null(req(input$data_scope))) return()
    else {
      if (is.factor(adtte_data()[, which(names(adtte_data()) == input$data_scope)])) {
        choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$data_scope)]))
        choices <- c("No selection", choices)
      } else {
        choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$data_scope)]))
        choices <- c("No selection", choices)
      }
    }

    shinyWidgets::pickerInput(
      inputId = ns("sel_data_scope"),
      label = "Data_scope",
      choices = choices,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 0",
        `count-selected-text` = "{0} selected (of {1})",
        `live-search` = TRUE,
        `header` = "Select multiple items",
        `none-selected-text` = "No selection!"
      )
    )
  })
   
  adtte_data2 <- shiny::reactive({
    shiny::req(adtte_data())
    shiny::req(input$sel_treatment)
    shiny::req(input$sel_comparator)
    shiny::req(input$sel_verum)
    if (is.null(adtte_data())) {
      return(NULL)
    } else {
      Sel_trt <- shiny::req(input$sel_treatment)
      if (!is.null(input$sel_verum) & !is.null(input$sel_comparator))
        tmp <- adtte_data() %>%
          dplyr::filter(!!rlang::sym(Sel_trt) %in% input$sel_verum | !!rlang::sym(Sel_trt) %in% input$sel_comparator)
    }
    tmp
  })
  
  csv_file <- shiny::reactive({
 
      adtte <- adtte_data2()
      
      tmp <- effect_calc(
        data = adtte,
        outcome = input$sel_parameter,
        scope = input$data_scope,
        datascope = input$sel_data_scope ,
        population = input$analysis_set,
        treatment= input$sel_treatment,
        verum = input$sel_verum,
        comparator = input$sel_comparator,
        cnsr = input$event_identifyer,
        param = input$parameter,
        event = input$sel_event_identifyer,
        strat = input$stratification
      )

      csv2 <- tmp
      return(csv2)
  })

  output$table_adtte <- renderDataTable(adtte_data2(), options = list(autoWidth = FALSE))
  output$table_csv <- renderDataTable(csv_file(), options = list(autoWidth = FALSE))
}

## To be copied in the UI
# file_creation_ui("file_creation")

## To be copied in the server
# callModule(file_creation_server, "file_creation")
  

