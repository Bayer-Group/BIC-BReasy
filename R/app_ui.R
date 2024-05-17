#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      title = "BReasy",
      shinydashboard::dashboardHeader(
        title = img(
          src = "www/AppSign_BReasy_220x76mm_RGB_wht.png",
          height = 50,
          align = "left"
        ),
        titleWidth = 200
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = 'sidebarmenu', 
           shinydashboard::menuItem(
            "BReasy",
            tabName = "breasy_Plot",
            icon = icon("tv")
          ),
          shinydashboard::menuItem(
            text = 'Data Upload',
            tabName = 'upload',
            icon = icon('upload'),
            startExpanded = TRUE,
            shinyWidgets::prettyRadioButtons(
              inputId = 'selectdata',
              label = 'Select data', 
              shape = 'round', 
              animation = 'smooth',
              choices = c('Upload data', 'Use demo data')
            ),
            shiny::conditionalPanel(
              condition = "input.selectdata == 'Upload data'",
              shinyWidgets::prettyRadioButtons(
                inputId = 'sep',
                label = 'Select separator',
                inline = TRUE,
                choices = c(
                  'Comma' = ',',
                  'Semicolon' = ';',
                  'Tab' = '\t'
                ),
                selected = ';'
              ),
              shinyWidgets::prettyRadioButtons(
                inputId = 'quote',
                label = 'Select quote',
                inline = TRUE,
                choices = c(None = '',
                'Double Quote (")' = '"',
                "Single Quote (')" = "'"),
                selected = '"'
              ),
              shinyWidgets::prettyRadioButtons(
                inputId = 'dec',
                label = 'Select decimal character',
                inline = TRUE,
                choices = c(
                'Point (.)' = '.',
                'Comma (,)' = ','
                ),
                selected = ','
              ),
              shiny::fileInput(
                inputId = "file", 
                label = "File input:",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
              ),
              shiny::uiOutput("wrong_file_format_text"),
              shiny::conditionalPanel(condition = "output.flag3 >= 2",
                shiny::fileInput(
                  inputId = "file2",
                  label = "File input:",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                ),
                shiny::uiOutput("wrong_file2_format_text")
              ),
              shiny::conditionalPanel(condition = "output.flag3 >= 3",
                shiny::fileInput(
                  inputId = "file3",
                  label = "File input:",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                ),
                shiny::uiOutput("wrong_file3_format_text")
              ),
              shiny::conditionalPanel(condition = "output.flag3 == 4",
                shiny::fileInput(
                  inputId = "file4",
                  label = "File input:",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                ),
                shiny::uiOutput("wrong_file4_format_text")
              ),
              HTML('<p style = "color: white;"> Add/Remove more File Input(s): </p>'),
              shiny::fluidRow(
                shiny::column(2,
                  shinyWidgets::circleButton(
                    inputId = "add_file_input",
                    icon = icon("plus"),
                    status ="success", 
                    size = "xs"
                  )
                ),
                shiny::column(2,
                  shinyWidgets::circleButton(
                    inputId = "rem_file_input",
                    icon = icon("minus"), 
                    status = "warning", 
                    size = "xs"
                  )
                )
              )
            ),
            shiny::uiOutput("AnaSets"),
            shiny::uiOutput("avisit"),
            shiny::uiOutput("trialno"),
            shiny::uiOutput("stratum")
          ),
          shinydashboard::menuItem(
            "File Creation (SAS data)",
            tabName = "sas_data",
            icon = icon("calculator")
          ),
          shinydashboard::menuItem(
            text = 'Graphic Options',
            tabName ='graphic',
            icon = icon('eye'),
            startExpanded = FALSE,
            shinyWidgets::prettyRadioButtons(
              inputId = "title_input",
              label = "Title style:",
              choices = c("Default Title", "Custom Title"),
              selected = "Default Title"
            ),
            shiny::conditionalPanel(
              condition = "input.title_input == 'Custom Title'",
              shiny::uiOutput('title')
            ),
            shiny::checkboxInput(
              inputId = "Info",
              label = "Display estimates",
              TRUE
            ),
            shiny::checkboxInput(inputId = "Info2",
              label = "Display counts",
              TRUE
            ),
            shiny::conditionalPanel(
              condition = "output.flag4 == true",
              shiny::checkboxInput(inputId = "Info3",
              label = "Display NNT/NNH",
              FALSE)
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = "var_sorting",
              label = "Choose Sorting",
              choices = c("As Input", "Alphabetical", "Effect"),
              selected = "As Input"
            ),
            list(
              HTML('<p style = "color: white;"><b style = "color: #D30F4B"> Note: </b> Please confirm selection </p>'),
              HTML('<p stlye = "color: white;"> by clicking the <b style = "color: #66B512"> Update! </b>-button </p>')
            ),
            shiny::sliderInput(
              inputId = "forestplot_height",
              label = "Zoom factor (height)",
              value = 100,
              min = 50,
              max = 200,
              post = " %",
              step = 5
            ),
            shiny::numericInput(
              inputId = "limit.low",
              label = "Lower limit x-axis",
              value = NA
            ),
            shiny::numericInput(
              inputId = "limit.high",
              label = "Upper limit x-axis",
              value = NA
            ),
            shiny::uiOutput('fontsize_forestplot'),
            shiny::uiOutput('fontsize_forestplot_xlab')
          ),
          shinydashboard::menuItem(
            "About BReasy",
            tabName = "about_breasy",
            icon = icon("info-circle")
          ),
          shinydashboard::menuItem(
            "Data Manual",
            tabName = "data_manual", 
            icon = icon("folder-open")
          )
        ),
        HTML(paste0("This version (from 2022-05-04) of", br())),
        HTML(paste0(img(
          src = "www/AppSign_BReasy_220x76mm_WHT.png",
          height = 35,
          align = "center"
        ))),
        HTML(paste0(br(), "was developed under R Version 4.0.2 (2020-06-22).", br()))
      ),
      shinydashboard::dashboardBody(
        #shinyjs::useShinyjs(),
        tags$style(paste0(".btn-custom {background-color: ", breasy_blue, "; width: 180px; border-color: #FFF; color: #FFF;width: 175px;}")),
        tags$style(
          paste("
            .box.box-primary>.box-header {
              color:", breasy_grey, ";
              background:", breasy_grey, "
            }
            .radio-item-warning {color: ", breasy_purple, "}
            .btn-warning{ background-color:", breasy_purple, ";}
            .btn-warning:hover{ background-color:", breasy_purple, ";}
            .box.box-primary{
              background:", breasy_grey, ";
              border-top-color:", breasy_grey, ";
            }
              .box.box-solid.box-primary>.box-header {
              color:#fff;
              background:#222d32
            }
            ", sep = ""
          )
        ),
        tags$style(
          paste0(
            ".nav-tabs {
              background-color: ", breasy_grey ,";
            }
            .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
              background-color: transparent;
              border-color: transparent;
            }
            .nav-tabs-custom .nav-tabs li.active {
              border-top-color: ", breasy_purple, ";
            }"
          )
        ),
        tags$head(
          tags$style(
            HTML(
              paste0("
                .shiny-output-error-validation {
                   color: ", breasy_blue, ";
                   font-size:25px;
                 }
                 "
              )
            )
          )
        ),
        tags$head(
          tags$style(
            shiny::HTML(
              paste0(
                ".content-wrapper, .right-side { background-color: ", "#ffffff", ";}
                .main-sidebar .sidebar .sidebar-menu .treeview-menu  {background-color: ", breasy_grey, " !important;}
                .main-sidebar .sidebar .sidebar-menu .treeview-menu li:hover a {background-color: ", breasy_purple, " !important;}
                .skin-blue .main-header .logo { background-color: ", breasy_grey, ";}
                .skin-blue .main-header .logo:hover {background-color: ", breasy_grey, ";}
                .progress-bar{background-color:", breasy_blue, ";}
                .skin-blue .main-header .navbar {background-color: ", breasy_grey, ";}
                /* main sidebar */
                .skin-blue .main-sidebar {background-color: ", "#424242", ";}
                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: ", "#383838", ";}
                .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ", breasy_grey, ";}
                .skin-blue .main-sidebar .navbar { background-color: ", breasy_grey, ";}
                .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ",  breasy_purple, ";}"
              )
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.sidebarmenu == 'breasy_Plot'",
          shinydashboard::box(
            width = NULL,
            status ="primary",
            title = div(HTML('<p style ="color: white"><i class ="fa fa-notes-medical"; style ="color : white";></i>  Patient characteristics</p>')),
            collapsible = TRUE,
            collapsed = FALSE,
            shiny::fluidPage(
              shiny::fluidRow(
                shiny::column(2,
                  tags$style(".btn-customestimate {background-color: #999999; color: #FFF; width: 175px;}"),
                  shiny::uiOutput("estimate")
                ),
                shiny::column(2,
                  shiny::uiOutput("cont_efficacy"),
                  shiny::uiOutput("efficacy")
                ),
                shiny::column(2,
                  shiny::uiOutput("cont_safety"),
                  shiny::uiOutput("safety")
                ),
                shiny::column(2,
                  tags$style(".btn-customsubgroup {background-color: #999999; color: #FFF; width: 175px;}"),
                  shiny::uiOutput("subgroup")
                ),
                shiny::column(2,
                  tags$style(".btn-customsublevel {background-color: #999999; color: #FFF; width: 175px;}"),
                  shiny::uiOutput("SubLevel")
                ),
                shiny::conditionalPanel(condition = "output.flag2 == true",
                  shiny::column(2,
                    shinyWidgets::materialSwitch(
                      inputId = "add_color",
                      label = HTML('<p style = "color: white;"> Plot colors </p>'),
                      value = FALSE,
                      status = "primary"
                    )
                  )
                ),
                shiny::conditionalPanel(condition = "input.add_color == true",
                  shiny::column(12,
                     column(2,
                      colourpicker::colourInput(
                        inputId ="col_eff",
                        label = HTML('<p style ="color: white;"> Color Efficacy </p>'),
                        value = "#0091DF",
                        returnName = FALSE
                      )
                    ),
                    column(2,
                      colourpicker::colourInput(
                        inputId ="col_saf",
                        label = HTML('<p style ="color: white;"> Color Safety </p>'),
                        value = "#66B512",
                        returnName = FALSE
                      )
                    ),
                    column(2,
                      colourpicker::colourInput(
                        inputId = "col_leg",
                        label = HTML('<p style ="color: white;"> Color Legend </p>'),
                        value = "#D30F4B",
                        returnName = FALSE
                      ) 
                    )
                  )
                ),
                shiny::uiOutput("Stratum"),
                shiny::uiOutput("Subgroups"),
                shiny::uiOutput("SubVars")
              )
            )
          ),
          shinydashboard::tabBox(width = "auto",
            shiny::tabPanel(
              HTML('<p style ="color: white;"> Forest plot </p>'),
              shiny::plotOutput(outputId = "forest2",
              height = "auto",
              width = "auto"),
              conditionalPanel(condition = "output.flag_download == true",
                shiny::downloadButton(
                  outputId = "report",
                  label = "Download Forestplot (.html)",
                  class = "button_class"
                )
              ),
              shiny::tags$head(
                tags$style(
                  ".button_class{color: white; background-color: #66B512}"
                )
              )
            ),
            shiny::tabPanel(HTML('<p style ="color: white;"> Dataset </p>'),
              style = "overflow-y:scroll;",
              DT::dataTableOutput("dataset",
              height = "auto")
            ),
            #### value-tree ####
            shiny::tabPanel(
              HTML('<p style ="color: white;"> Value Tree </p>'),
              mod_value_tree_ui("value_tree_ui_1")
            )
          ), 
          shiny::conditionalPanel(condition = "output.flag == true",
            uiOutput('welcome_text1')
          )
        ),
        shinydashboard::tabItem(tabName = "file_creation",
          shiny::conditionalPanel(condition = "input.sidebarmenu =='sas_data'",
          file_creation_ui("file_creation")
          )
        ),
        shinydashboard::tabItem(tabName = "about_breasy",
          shiny::conditionalPanel(condition = "input.sidebarmenu =='about_breasy'",
            mod_about_breasy_ui("about_breasy_ui_1")
          )
        ),
        shinydashboard::tabItem(tabName = "data_manual",
          shiny::conditionalPanel(condition = "input.sidebarmenu =='data_manual'",
            mod_data_manual_ui("data_manual_ui_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'breasy'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

