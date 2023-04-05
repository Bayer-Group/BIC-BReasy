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
    shiny::uiOutput(ns("update_button_panel")),
    shinyWidgets::prettyRadioButtons(
      inputId = ns('adtte_data'),
      label = 'Input type',
      shape = 'round',
      animation = 'smooth',
      choices = c(
        "SAS file (from Disc)" = "sas"#,
        #"SAS file (from Server)" = "server"
      )
    ),
    shiny::conditionalPanel(condition = paste0("input['", ns("adtte_data"), "\'] == \'sas\'"),
      shiny::fluidRow(
        shiny::column(5,
          shiny::fileInput(
            inputId =  ns("adtte_file"),
            label = "ADTTE data (.sas7bdat format)",
            multiple = FALSE,
            accept = NULL,
            width = NULL
          ),
          shiny::uiOutput(ns("wrong_adtte_format_text"))
        ),
        shiny::column(5,
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
    shiny::conditionalPanel(condition = paste0("input['", ns("adtte_data"), "\'] == \'server\'"),
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
      shiny::column(3,
        shiny::uiOutput(ns("sel_treatment")),
        shiny::uiOutput(ns("sel_treatment_check"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_verum")),
        shiny::uiOutput(ns("sel_verum_check"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_comparator")),
        shiny::uiOutput(ns("sel_comparator_check"))
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shiny::uiOutput(ns("parameter"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_parameter")),
        shiny::uiOutput(ns("sel_outcome_check"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_aval"))#,
       # shiny::uiOutput(ns("sel_outcome_check"))
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shiny::uiOutput(ns("analysis_set")),
        shiny::uiOutput(ns("analysis_set_check"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("subject_identifier"))  
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shiny::uiOutput(ns("data_scope"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_data_scope")),
        shiny::uiOutput(ns("sel_datascope_check"))
      )
    ),
    shiny::fluidRow(
      shiny::column(2,
        shiny::uiOutput(ns("subgroups_1"))
      ),
      shiny::column(3,
        
        shiny::uiOutput(ns("subgroups_2"))
      )
    ),
    shiny::column(10,
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> Filter: (click to open) </p>'),
          "Filter options",
          shiny::uiOutput(ns("filter_percentage")),
          shiny::uiOutput(ns("pickerinput_adtte")),
          shiny::fluidRow(
            shiny::column(4,
              shiny::actionButton(
                inputId = ns("insertBtn"),
                label = "Add",
                icon = icon("plus")
              )
            ),
            shiny::column(4,
              shiny::actionButton(
                inputId = ns("removeBtn"),
                label = "Delete",
                icon = icon("minus")
              )
            )
          ), 
          shiny::tags$div(id = "placeholder"),
          shiny::conditionalPanel(condition = "output.condition_filter == true",
            shiny::actionButton(
              inputId = ns("apply"),
              label = "Apply Filter Selection!",
              icon = icon("redo"),
              style = "color: #fff; background-color: #61a337; border-color: #fff"
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shiny::uiOutput(ns("event_identifyer"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_event_identifyer")),
        shiny::uiOutput(ns("sel_event_identifyer_check"))
      )
    ),
    shiny::fluidRow(
      # shiny::column(4,
      #   shiny::uiOutput(ns("estimates"))
      # ),
      shiny::column(3,
        shiny::uiOutput(ns("effect"))
      ),
      shiny::conditionalPanel(condition = paste0("input['", ns("effect"), "\'] == \'CID\' | input['", ns("effect"), "\'] == \'EXCESS_CID\'" ),
        shiny::column(3,
          shiny::uiOutput(ns("day_variable")),
          shiny::uiOutput(ns("day_variable_check"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(2,
        shiny::uiOutput(ns("stratification_1"))
      ),
    # shinyBS::bsCollapse(
    #   shinyBS::bsCollapsePanel(
    #     shiny::HTML('<p style="color:black; font-size:100%;"> Advanced settings: </p>'),
    #     "Advanced settings",
        shiny::column(3,
          shiny::uiOutput(ns("stratification_2"))
        )
    ),
      
    #   )
    # ),
      shiny::column(10,
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
            ),
            shiny::uiOutput(ns('required_variables_text'))
          )
        )
      )
    )
}


#' File creation Server Function
#'
#' @noRd
file_creation_server <- function(input, output, session) {
  ns <- session$ns

  #Start text
  output$start_text <- shiny::renderUI({
    list(
      HTML(
        "
          <h1> Create a BReasy input file (as .csv) from your ADTTE SAS file </h1>
          <h5> Please upload your adtte file and complete all required settings.
               If necessary also upload the appropriate adsl data set.</h5>
          <h5> After completing all settings, press the <span style='color: white;background-color: #61a337;height: 26px;border-radius:25px;'> <i class='fa-solid fa-spinner'></i> Calculate! </span>-button.
               The results of the calculation appear in the 'CSV Output file' box.</h5>
          <h5> If this dataset has the appropriate form, please press the <span style='color: white; background-color: #61a337;height: 26px;border-radius:25px'><i class='fa-solid fa-download'></i>Save as .csv </span>-button.
               The saved data can be uploaded via <b><i class='fa-solid fa-upload'></i> Data Upload </b>-tab.</h5>
          
        "
      )
    )
  })
  
  output$update_button_panel <- shiny::renderUI({
    shiny::req(adtte_data())
    shiny::absolutePanel(
      id = "update_button_panel",
      class = "modal-content",
      fixed = TRUE,
      draggable = FALSE,
      HTML(paste0("<div style='background-color: white'>")),
      top = 200,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = "auto",
      height = "auto",
        shiny::fluidRow(
          column(2,
            shiny::actionButton(
              inputId = ns("btn2"),
              label = "Calculate!",
              icon = icon("spinner")
            ),
            shiny::uiOutput(ns('btn2_cont')),
          )),
          br(),
         shiny::fluidRow(
          column(2,
            shiny::downloadButton(ns("downloadData"), "Save as .csv", class="mybutton")
          ),
          shiny::uiOutput(ns('downloadData_cont'))
        ),
      style = "z-index: 10;"
    )
  })
  
  # button colors:
  output$btn2_cont <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML(paste0('#', session$ns("btn2"),'{color: #ffffff; background-color:#E43157;}')))
      )
    )
  })
  
  downloadData_cont <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML(paste0('#', session$ns("downloadData"),'{color: #ffffff; background-color:#E43157;}')))
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
            output$wrong_adsl_format_text <- shiny::renderUI({
              HTML(paste0(""))
            })
            output$wrong_adsl_format_text <- shiny::renderUI({
              HTML(paste0(""))
            })
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
            '<span style = "color:#E43157"> <i class="fa fa-exclamation"></i> Please select a treatment variable. </span>'
          )
        )
      })
      treatment_check_flag$val <- FALSE
    } else {
      treatment_check_flag$val <- TRUE
      output$sel_treatment_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
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
    if (!is.null(input$sel_treatment)) {
      if (is.null(input$sel_verum)) {
      output$sel_verum_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation"></i> 
            Please select verum term. </span>'
          )
        )
      })
      verum_check_flag$val <- FALSE
    } else {
      verum_check_flag$val <- TRUE
      output$sel_verum_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
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
            </i> Please select comparator term. </span>'
          )
        )
      })
      comparator_check_flag$val <- FALSE
    } else {
      comparator_check_flag$val <- TRUE
      output$sel_comparator_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
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

  
  #### ANALYSIS SET ####
  analysis_set_check_flag <- shiny::reactiveValues(val = FALSE)
  
  shiny::observeEvent(c(adtte_data(), input$analysis_set), {
    shiny::req(adtte_data())
    if (is.null(input$analysis_set)) {
      output$analysis_set_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            </i> Please select an analysis set variable. </span>'
          )
        )
      })
      analysis_set_check_flag$val <- FALSE
    } else {
      analysis_set_check_flag$val <- TRUE
      output$analysis_set_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
          )
        )
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  #### OUTCOME ####
  outcome_check_flag <- shiny::reactiveValues(val = FALSE)
  
  shiny::observeEvent(c(adtte_data(), input$sel_parameter), {
    shiny::req(adtte_data())
    if (is.null(input$sel_parameter)) {
      output$sel_outcome_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
             </i> Please select an outcome variable. </span>'
          )
        )
      })
      outcome_check_flag$val <- FALSE
    } else {
      outcome_check_flag$val <- TRUE
      output$sel_outcome_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
          )
        )
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
   output$parameter <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- as.list(sort(names(adtte_data())))
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

  #### Select analysis value ####
  output$sel_aval <- shiny::renderUI({
    if (is.null(adtte_data())) {
      return()
    } else {
      choices <- as.list(names(adtte_data()))
      choices <- c(choices[stringr::str_detect(choices, "AVAL")], choices[!(stringr::str_detect(choices, "AVAL"))])
    }
    if(choices[1] == "AVAL") {
      selected <- choices[1]
    } else {
      selected <- NULL
    }

    shinyWidgets::pickerInput(
      inputId = ns("sel_aval"),
      label = "Analysis value",
      choices = choices,
      selected = choices[1],
      multiple = FALSE
    )
  }) 
   
   
   
  output$analysis_set <- shiny::renderUI({
    if (is.null(adtte_data())) { return()
      } else {
      choices <- as.list(names(adtte_data()))
      choices1 <- c(choices[stringr::str_detect(choices, "SAFF|ITT|FAS")])
      choices2 <- c(choices[!(stringr::str_detect(choices, "SAFFL|ITT|FAS"))])
      
      if (length(choices1) == 0) {
        selected <- NULL
      } else {
        selected <- choices1[1]
      }
      
      shinyWidgets::pickerInput(
        inputId = ns("analysis_set"),
        label = "Analysis set",
        choices = c(choices1,choices2),
        selected = selected,
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
  
  output$effect <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- c("CID","EXCESS_CID","HR","IRD","ARD","EXCESS_IRD")
    }

    shinyWidgets::pickerInput(
      inputId = ns("effect"),
      label = "Select effect",
      choices = choices,
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
    
    #  shinyWidgets::pickerInput(
    #   inputId = ns("event_identifyer"),
    #   label = "Event identifyer",
    #   choices = choices ,
    #   selected = choices[1],
    #   multiple = FALSE,
    #   options = list(
    #     `actions-box` = TRUE,
    #     `selected-text-format` = "count > 0",
    #     `count-selected-text` = "{0} selected (of {1})",
    #     `live-search` = TRUE,
    #     `header` = "Select multiple items",
    #     `none-selected-text` = "No selection!"
    #   )
    # )
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
  
  
  #### DAY VARIABLE ####
  output$day_variable <- shiny::renderUI({
    shiny::numericInput(
      inputId = ns("day_variable"),
      label = "Day variable",
      value = NULL,
      min = 0,
      step = 1
    )
  })
  
  day_variable_check_flag <- shiny::reactiveValues(val = FALSE)
  shiny::observeEvent(c(adtte_data(), input$day_variable), {
    shiny::req(adtte_data())
    if (is.na(input$day_variable)) {
      output$day_variable_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            </i> Please select a day variable.  </span>'
          )
        )
      })
      day_variable_check_flag$val <- FALSE
    } else {
      day_variable_check_flag$val <- TRUE
      output$day_variable_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
          )
        )
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  #### EVENT IDENTTIFYER ####
  event_identifyer_check_flag <- shiny::reactiveValues(val = FALSE)
  
  shiny::observeEvent(c(adtte_data(), input$sel_event_identifyer), {
    shiny::req(adtte_data())
    if (is.null(input$sel_event_identifyer)) {
      output$sel_event_identifyer_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            </i> Please select an event identifyer variable. </span>'
          )
        )
      })
      event_identifyer_check_flag$val <- FALSE
    } else {
      event_identifyer_check_flag$val <- TRUE
      output$sel_event_identifyer_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
          )
        )
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
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
  
  
  #### DATA SCOPE ####
  datascope_check_flag <- shiny::reactiveValues(val = FALSE)
  
  shiny::observeEvent(c(adtte_data(), input$sel_data_scope), {
    shiny::req(adtte_data())
    if (is.null(input$sel_data_scope)) {
      output$sel_datascope_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color:#E43157"> <i class="fa fa-exclamation">
            </i> Please select a data scope variable. </span>'
          )
        )
      })
      datascope_check_flag$val <- FALSE
    } else {
      datascope_check_flag$val <- TRUE
      output$sel_datascope_check <- shiny::renderUI({
        shiny::HTML(
          paste0(
            '<span style = "color: #61a337"> <i class="fa-solid fa-check"></i></span>'
          )
        )
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  output$data_scope <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      choices <- as.list(sort(names(adtte_data())))
      choices <- c(choices[stringr::str_detect(choices, "AVISIT")], "No selection", choices[!(stringr::str_detect(choices, "AVISIT"))])
    }

    shinyWidgets::pickerInput(
      inputId = ns("data_scope"),
      label = "Data scope variable",
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
  
  #### Stratification ####
   output$stratification_1 <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      checkboxInput(
        inputId = ns("stratification_1"),
        label = "Use stratification",
        value = FALSE
      )
    }
  })
  
  stratification_reac_val <- shiny::reactiveValues(val = "Overall"
  )
  
  observeEvent(c(input$stratification_1,input$stratification_2), {
    if(!is.null(input$stratification_1) & !is.null(input$stratification_2)) {
      if(input$stratification_1) {
        stratification_reac_val$val <- input$stratification_2
      } else {
        stratification_reac_val$val <- "Overall"
      }
    }
  })
  
   output$stratification_2 <- shiny::renderUI({
    shiny::req(input$stratification_1)
    shiny::req(adtte_data())
    choices <- as.list(sort(names(adtte_data())))
    choices <- c("Overall", choices)

     if (isTruthy(input$stratification_1 == FALSE)) {
     return()
   } else {
      shinyWidgets::pickerInput(
        inputId = ns("stratification_2"),
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
    }  
  })

   output$sel_data_scope <- shiny::renderUI({
    shiny::req(input$data_scope)
    if (is.null(shiny::req(input$data_scope))) {
      return() 
    } else if (shiny::req(input$data_scope) == "No selection" ) {
     choices <- "No selection"
    } else {
      if (is.factor(adtte_data()[, which(names(adtte_data()) == input$data_scope)])) {
        choices <- as.list(levels(adtte_data()[, which(names(adtte_data()) == input$data_scope)]))
        #choices <- c("No selection", choices)
      } else {
        choices <- as.list(unique(adtte_data()[, which(names(adtte_data()) == input$data_scope)]))
        #choices <- c("No selection", choices)
      }
    }

    shinyWidgets::pickerInput(
      inputId = ns("sel_data_scope"),
      label = "Data scope",
      choices = choices,
      selected = choices[1],
      # should be multiple =  TRUE
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
   
  output$subgroups_1 <- shiny::renderUI({
    if (is.null(adtte_data())) return()
    else {
      checkboxInput(
        inputId = ns("subgroups_1"),
        label = "Use subgroups",
        value = FALSE
      )
    }
  })
  
  subgroups_reac_val <- shiny::reactiveValues(val = "Overall"
  )
  
  observeEvent(c(input$subgroups_1,input$subgroups_2), {
    if(!is.null(input$subgroups_1) & !is.null(input$subgroups_2)) {
      if(input$subgroups_1) {
        subgroups_reac_val$val <- input$subgroups_2
      } else {
        subgroups_reac_val$val <- "Overall"
      }
    }
  })
  output$subgroups_2 <- shiny::renderUI({
    
    if (is.null(adtte_data())) return()
    else {
      choices <- as.list(sort(names(adtte_data())))
    }
    
   if (isTruthy(input$subgroups_1 == FALSE)) {
     return()
   }
    else {

    shinyWidgets::pickerInput(
      inputId = ns("subgroups_2"),
      label = "Subgroup variable(s)",
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
  
  adtte_filtered <- shiny::reactive({
    shiny::req(adtte_data2())
    adtte_data <- adtte_data2()
    data <- adtte_data
    if (length(id_adtte_m$myList) != 0) {
      names <- id_adtte_m$myList2
      vars <- id_adtte_m$myList
      if (length(id_adtte_m$myList) && !is.null(id_adtte_m$myList2)) {
        data_filt <- data
        for (i in 1:length(id_adtte_m$myList)) {
          if(adtte_data %>%
             dplyr::pull(id_adtte_m$myList2[i]) %>%
             is.numeric()) {
            if(!is.null(input[[id_adtte_m$myList[[i]]]]))
              data_filt <- data_filt[data_filt %>%
                                       dplyr::pull(id_adtte_m$myList2[i]) %>%
                                       dplyr::between(input[[id_adtte_m$myList[[i]]]][1],input[[id_adtte_m$myList[[i]]]][2]),]
          }else{
            data_filt <- data_filt %>%
              dplyr::filter(!! rlang::sym(id_adtte_m$myList2[i]) %in% c(input[[id_adtte_m$myList[i]]]))
          }
        }
      }
    } else {
      data_filt <- data
    }
    data_filt
  })
  
  #observer to change the calculate-button color:
  shiny::observe({
    if (
      treatment_check_flag$val & verum_check_flag$val & comparator_check_flag$val &
      outcome_check_flag$val &  event_identifyer_check_flag$val & datascope_check_flag$val
    ) {
      if (
        all(dim(used_settings$adtte) == dim(adtte_filtered())) &
        all(used_settings$effect == input$effect) &
        all(used_settings$outcome == input$sel_parameter) &
        all(used_settings$scope == input$data_scope) &
        all(used_settings$datascope == input$sel_data_scope) &
        all(used_settings$population == input$analysis_set) &
        all(used_settings$treatment == input$sel_treatment) &
        all(used_settings$verum == input$sel_verum) &
        all(used_settings$comparator == input$sel_comparator) &
        all(used_settings$cnsr == input$event_identifyer) &
        all(used_settings$param == input$parameter) &
        all(used_settings$event == input$sel_event_identifyer) &
        all(used_settings$strat == stratification_reac_val$val) &
        all(used_settings$subgroup == subgroups_reac_val$val) &
        all(used_settings$aval == input$sel_aval)
      ) {
        output$btn2_cont <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML(paste0('#', session$ns("btn2"),'{color: #ffffff; background-color:#e3e3e3;}')))
            )
          )
        })
      } else if (
        all(dim(used_settings$adtte) == c(0,0)) &
        all(used_settings$effect == "") &
        all(used_settings$outcome == "") &
        all(used_settings$scope =="") &
        all(used_settings$datascope =="") &
        all(used_settings$population == "") &
        all(used_settings$treatment == "") &
        all(used_settings$verum == "") &
        all(used_settings$comparator == "") &
        all(used_settings$cnsr == "") &
        all(used_settings$param == "") &
        all(used_settings$event == "") &
        all(used_settings$strat == "") &
        all(used_settings$subgroup == "") &
        all(used_settings$aval == "")
      ) {
        output$btn2_cont <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML(paste0('#', session$ns("btn2"),'{color: #ffffff; background-color:#61a337;}')))
            )
          )
        })
      } else {
        output$btn2_cont <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML(paste0('#', session$ns("btn2"),'{color: #ffffff; background-color:#61a337;}')))
            )
          )
        })
      }
    } else {
      output$btn2_cont <- shiny::renderUI({
        list(
          shiny::tags$head(
            tags$ style(HTML(paste0('#', session$ns("btn2"),'{color: #ffffff; background-color:#E43157;}')))
          )
        )
      })
    }
  })
  
  used_settings <- shiny::reactiveValues(
    adtte = data.frame(),
    effect = "",
    outcome = "",
    scope = "",
    datascope = "",
    population = "",
    treatment= "",
    verum = "",
    comparator = "",
    cnsr = "",
    param = "",
    event = "",
    strat = "",
    subgroup = "",
    aval = ""
  )
  
  shiny::observe({
    csv_file()
  })
  
  csv_file <- shiny::eventReactive(input$btn2, {
 
      # include filtered data_
      #   need a button to update
      adtte <- adtte_filtered()
      #adtte <- adtte_data2()
      #start the calculation only if the required variables are 
      #available:
      if (
        treatment_check_flag$val & verum_check_flag$val & comparator_check_flag$val &
        outcome_check_flag$val &  event_identifyer_check_flag$val & datascope_check_flag$val
      ) {
     
      tmp <- effect_calc(
        data = adtte,
        effect = input$effect,
        outcome = input$sel_parameter,
        day = input$day_variable,
        scope = input$data_scope,
        datascope = input$sel_data_scope ,
        population = input$analysis_set,
        treatment= input$sel_treatment,
        verum = input$sel_verum,
        comparator = input$sel_comparator,
        cnsr = input$event_identifyer,
        param = input$parameter,
        event = input$sel_event_identifyer,
        strat = stratification_reac_val$val,
        subgroup = subgroups_reac_val$val,
        aval = input$sel_aval
      )
       output$btn2_cont <- shiny::renderUI({
        list(
          shiny::tags$head(
            tags$ style(HTML(paste0('#', session$ns("btn2"),'{color: #ffffff; background-color:#e3e3e3;}')))
          )
        )
      })
        used_settings$adtte <- adtte
        used_settings$effect <- input$effect
        used_settings$outcome <- input$sel_parameter
        used_settings$scope <- input$data_scope
        used_settings$datascope <- input$sel_data_scope
        used_settings$population <- input$analysis_set
        used_settings$treatment <- input$sel_treatment
        used_settings$verum <- input$sel_verum
        used_settings$comparator <- input$sel_comparator
        used_settings$cnsr <- input$event_identifyer
        used_settings$param <- input$parameter
        used_settings$event <- input$sel_event_identifyer
        used_settings$strat <- stratification_reac_val$val
        used_settings$subgroup <- subgroups_reac_val$val
        used_settings$aval <- input$sel_aval
      
      output$required_variables_text <- shiny::renderUI({
        shiny::HTML(
          paste0(
            ''
          )
        )
      })
      csv2 <- tmp
      return(csv2)
      } else {
        output$required_variables_text <- shiny::renderUI({
          shiny::HTML(
            paste0(
              '<span style = "color:#E43157"> <i class="fa fa-exclamation">
              </i> Please select all required variables! </span>'
            )
          )
        })
        return(NULL)
      }
  })

  output$table_adtte <- renderDataTable(adtte_data2(), options = list(autoWidth = FALSE))
  output$table_csv <- renderDataTable(csv_file(), options = list(autoWidth = FALSE))
  

  
  
  #### FILTER ####
  # Reset initial values if Remove Button is clicked
  shiny::observeEvent(input$removeBtn, {
    id_adtte_m$myList <- list()
    id_adtte_m$myList2 <- list()
  })

  # Delete UI Elements if Remove Button is clicked
  shiny::observeEvent(input$removeBtn, {
    for (i in 1:length(inserted_adtte)) {
      removeUI(selector = paste0("#", inserted_adtte[i]))
    }
  })
  
  output$filter_percentage <- shiny::renderUI({
    total_tmp <- dim(adtte_data2())[1]
    value_tmp <- dim(adtte_filtered())[1]
    shinyWidgets::progressBar(
            id = ns("filter_percentage"),
            value = value_tmp,
            total = total_tmp,
            title = "Number of Rows of adtte (+adsl)",
            display_pct = TRUE
    )
  })
  output$pickerinput_adtte <- shiny::renderUI({
    shiny::req(adtte_data2())
    
    adtte_data <- adtte_data2()
    
    adtte_data_variables_tmp <- purrr::map(
      adtte_data,
      function(x) attr(x, "label", exact = TRUE)
    )
    adtte_data_variables = names(adtte_data_variables_tmp)
    names(adtte_data_variables) = paste0(
      names(adtte_data_variables_tmp),
      ifelse(
        as.character(adtte_data_variables_tmp) == "NULL",
        "",
        paste0(" - ", as.character(adtte_data_variables_tmp))
      )
    )
    
    choices <- adtte_data_variables
    #colors <- rep("color: white; background: #424242;", length(choices))
    
    shinyWidgets::pickerInput(
      inputId = ns('pickerinput_adtte'),
      label = 'Select filter variable(s) for adtte data set',
      choices = choices, 
      selected = NULL,
      multiple = TRUE,
     # choicesOpt = list(style = colors),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = 'count > 0',
        `count-selected-text` = '{0} selected (of {1})',
        `live-search` = TRUE,
        `header` = 'Select multiple items',
        `none-selected-text` = 'No selection!'
      )
    )
  })
  
  inserted_adtte <- c()

  id_adtte_nr <- c()
  id_adtte_nr2 <- c()

  id_adtte_m <- shiny::reactiveValues()
  id_adtte_m$myList <- list()
  id_adtte_m$myList2 <- list()
  inserted_adtte_list <- shiny::reactive({
    list()
  })
  
  condition_filter <- shiny::reactiveValues(val = FALSE)
  output$condition_filter <- shiny::reactive(condition_filter$val)
  # observe the 'Insert' Button click:
  shiny::observeEvent(c(input$insertBtn), {
    
    shiny::req(adtte_data2())
    
    adtte_data <- adtte_data2()
    
    ins_adtte <- inserted_adtte_list()
    id_adtte_nr <<- c()
    id_adtte_nr2 <<- c()
    
    if (length(inserted_adtte) > 0) {
      for (i in 1:length(inserted_adtte)) {
        shiny::removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted_adtte[i])
        )
      }
    }
    
    inserted_adtte <<- c()
    
    btn <- input$insertBtn
    
    pickerinput_adtte <- input$pickerinput_adtte
    
    if (length(pickerinput_adtte) > 0) {
      for (i in 1: length(pickerinput_adtte)) {
        id <- paste0(pickerinput_adtte[i], btn)
        shiny::insertUI(
          selector = '#placeholder',
          ui = shiny::tags$div(
            if (!is.numeric(adtte_data %>%
                           dplyr::pull(pickerinput_adtte[i]))) {
              shinyWidgets::pickerInput(
                inputId = ns(id),
                label = paste0(pickerinput_adtte[i]),
                choices = adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  unique,
                selected = adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  unique,
                multiple = TRUE,
                choicesOpt = list(style = rep("color: white; background: #424242;",
                  length(adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  unique))),
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = 'count > 0',
                  `count-selected-text` = '{0} selected (of {1})',
                  `live-search` = TRUE,
                  `header` = 'Select multiple items',
                  `none-selected-text` = 'All dropped!'
                )
              )
            } else if (
              is.numeric(
                adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i])
              ) && !is.integer(
                adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i])
                )
            ) {
              shiny::sliderInput(
                inputId = ns(id),
                label = paste0(pickerinput_adtte[i]),
                value = c(
                  adtte_data %>%
                    dplyr::pull(pickerinput_adtte[i]) %>%
                    base::min(na.rm = TRUE), adtte_data %>%
                    dplyr::pull(pickerinput_adtte[i]) %>%
                    base::max(na.rm = TRUE)
                ),
                min = adtte_data %>%
                 dplyr::pull(pickerinput_adtte[i]) %>%
                 base::min(na.rm = TRUE),
                max = adtte_data %>%
                 dplyr::pull(pickerinput_adtte[i]) %>%
                 base::max(na.rm = TRUE)
              )
            } else if (is.numeric(adtte_data %>%
                                 dplyr::pull(pickerinput_adtte[i])) && is.integer(adtte_data %>%
                                                                                 dplyr::pull(pickerinput_adtte[i]))) {
              shiny::sliderInput(
                inputId = ns(id), 
                label = paste0(pickerinput_adtte[i]),
                value = c(adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  base::min(na.rm = TRUE),adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  base::max(na.rm = TRUE)
                ),
                min = adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  base::min(na.rm = TRUE),
                max = adtte_data %>%
                  dplyr::pull(pickerinput_adtte[i]) %>%
                  base::max(na.rm = TRUE),
                step = 1,
                sep = "",
                ticks = FALSE
              )
            },
            id = id
          )
        )
        inserted_adtte <<- c(id, inserted_adtte)
        ins_adtte[[pickerinput_adtte[i]]]  <- adtte_data %>%
          dplyr::pull(pickerinput_adtte[i])
        id_adtte_nr2 <<- c(id_adtte_nr2, pickerinput_adtte[[i]])
        id_adtte_nr <<- c(id_adtte_nr,id)
      }
    }
    
    if (length(id_adtte_nr) > 0) {
      condition_filter$val <- TRUE
    } else {
      condition_filter$val <- FALSE
    }
    id_adtte_m$myList2 <- id_adtte_nr2
    id_adtte_m$myList <- id_adtte_nr
  })
  
  
  #### save as csv ####
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BReasy_", gsub(":","-", Sys.time()), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(apply(csv_file(),2, as.character), file, row.names = FALSE)
    }
  )
  
}

## To be copied in the UI
# file_creation_ui("file_creation")

## To be copied in the server
# callModule(file_creation_server, "file_creation")
  

