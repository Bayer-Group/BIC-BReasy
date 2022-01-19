#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  #increase file upload size to 40MB
  options(shiny.maxRequestSize = 40*1024^2)
  
  #### Data Upload-tab ####
  # possibility to upload up to 4 files which are combined
  # to one reactive data frame called df()
  df <- shiny::reactive({
    # demo data selection
    if (input$selectdata == "Use demo data") {
      res <- read.csv(
        file = "./data/demo.csv",
        header = TRUE,
        sep = ";",
        quote = '"',
        dec = ".",
        row.names = NULL,
        na.strings = "."
      )
    # data upload selection
    } else {
      # possibility to upload up to four data sets
      if (is.null(input$file) &&
          is.null(input$file2) &&
          is.null(input$file3) &&
          is.null(input$file4)) {
        return(NULL)
      }
      res <- NULL
      if (!is.null(input$file)) {
        tmp <- read.csv(
          file = input$file$datapath,
          header = TRUE,
          sep = input$sep,
          na.strings = ".",
          quote = input$quote,
          dec = input$dec,
          row.names = NULL
        )
        res <- rbind(res, tmp)
      }
      if (!is.null(input$file2)) {
        tmp2 <- read.csv(
          file = input$file2$datapath,
          header = TRUE,
          sep = input$sep,
          na.strings = ".",
          quote = input$quote,
          dec = input$dec,
          row.names = NULL
        )
        if (!is.null(res)) {
          if (all(sort(colnames(res)) == sort(colnames(tmp2)))) {
            res <- rbind(res, tmp2)
          } else {
            stop("Different column names in selected data sets.")
          }
        } else {
          res <- rbind(res, tmp2)
        }
      }
      if (!is.null(input$file3)) {
        tmp3 <- read.csv(
          file = input$file3$datapath,
          header = TRUE,
          sep = input$sep,
          na.strings = ".",
          quote = input$quote,
          dec = input$dec,
          row.names = NULL
        )
        if (!is.null(res)) {
          if (all(sort(colnames(res)) == sort(colnames(tmp3)))) {
            res <- rbind(res, tmp3)
          } else {
            stop("Different column names in selected data sets.")
          }
        } else {
          res <- rbind(res, tmp3)
        }
      }
      if (!is.null(input$file4)) {
        tmp4 <- read.csv(
          file = input$file4$datapath,
          header = TRUE,
          sep = input$sep,
          na.strings = ".",
          quote = input$quote,
          dec = input$dec,
          row.names = NULL
        )
        if (!is.null(res)) {
          if (all(sort(colnames(res)) == sort(colnames(tmp4)))) {
            res <- rbind(res, tmp4)
          } else {
            stop("Different column names in selected data sets.")
          }
        } else {
          res <- rbind(res, tmp4)
        }
      }
    }
    res
  })
  
  # hide download forestplot button until plot is shown
  flag_download_button <- shiny::reactiveValues(
    val = FALSE
  )
  
  shiny::observeEvent(c(input$effi, input$safe), {
    flag_download_button$val <- !(is.null(input$effi) & is.null(input$safe))
  })
  
  output$flag_download <- shiny::reactive(flag_download_button$val)
  
  shiny::outputOptions(output, "flag_download", suspendWhenHidden = FALSE)
  
  #create setting options when data were uploaded successfully:
  output$AnaSets <- shiny::renderUI({
    if (is.null(df())) return()
    
    if (is.factor(df()[, which(names(df()) == "ANALYSIS_SET")])) {
      choices <- as.list(levels(df()[, which(names(df()) == "ANALYSIS_SET")]))
    } else {
      choices <- as.list(unique(df()[, which(names(df()) == "ANALYSIS_SET")]))
    }
    
    shiny::selectInput(
      inputId = "AnaSet",
      label = "Choose an analysis set",
      choices = choices ,
      selected = choices[1],
      multiple = FALSE
    )
  })
  
  
  output$avisit <- shiny::renderUI({
    if (is.null(df())) return()
    else if (any(names(df()) == "AVISIT") || any(names(df()) == "DATA_SCOPE")) {
      shiny::req(input$AnaSet)
      tmp1 <- df()
      tmp2 <- tmp1[tmp1$ANALYSIS_SET %in% input$AnaSet,]
      tmp2[, which(names(tmp2) %in% c("AVISIT", "DATA_SCOPE"))] <- factor(tmp2[, which(names(tmp2) %in% c("AVISIT", "DATA_SCOPE"))])
      choices <- as.list(levels(tmp2[, which(names(tmp2) %in% c("AVISIT", "DATA_SCOPE"))]))
      
      shiny::selectInput(
        inputId = "visit",
        label = "Choose Data scope",
        multiple = TRUE,
        choices = choices,
        selected = choices[1]
      )
    }
    else return()
  })
  
  ## Input functions should appear, if STUDY/TRIALNO, STRATUM and/or SUBGROUP available in dataset df() 
  output$trialno <- shiny::renderUI({
    if (is.null(df())) return()
    else if (any(names(df()) == "TRIALNO") || any(names(df()) == "STUDY")) {
      shiny::tagList(
        shiny::selectInput(
          inputId = "studyNr",
          label = "Choose study",
          multiple = FALSE,
          as.list(unique(df()[, which(names(df()) %in% c("STUDY", "TRIALNO"))]))
        )
      )
    }
    else return()
  })
  
  output$stratum <- shiny::renderUI({
    if (is.null(df()) || any(names(df()) == "STRATUM") == FALSE) return()
    if (is.factor(df()[, which(names(df()) == "STRATUM")])) {
      choices <- as.list(levels(df()[, which(names(df()) == "STRATUM")]))
    } else {
      choices <- as.list(unique(df()[, which(names(df()) == "STRATUM")]))
    }
    shiny::tagList(
      shiny::selectInput(
        inputId = "_stratum",
        label = "Choose stratum",
        multiple = TRUE,
        choices = choices
      )
    )
  })
  
  #### BReasy-tab ####

  ####... Patient characteristics-panel ####
  output$estimate <- shiny::renderUI({
    if (is.null(df())) return()
    if (is.factor(df()[, which(names(df()) == "ESTIMATE")])) {
      choices <- as.list(levels(df()[, which(names(df()) == "ESTIMATE")]))
    } else {
      choices <- as.list(unique(df()[, which(names(df()) == "ESTIMATE")]))
    }
    
    shinyWidgets::dropdownButton(
      inputId = "dropdown_esti",
      label = "Estimate(s)",
      tooltip = shinyWidgets::tooltipOptions(placement = "right", title = "Help Text Estimator"),
      icon = icon("sliders"),
      status = "customestimate",
      circle = FALSE,
      shiny::selectInput(
        inputId = "Dependent",
        label = HTML('<p style ="color: #999999;"> Choose estimate </p>'),
        choices = choices,
        multiple = FALSE
      )
    )
  })
  
  output$efficacy <- shiny::renderUI({
    if (!is.null(df())) {
    if (is.factor(df()[, which(names(df()) == "OUTCOME")])) {
      choices <- list('Efficacy' = levels(df()[, which(names(df()) == "OUTCOME")]))
      
      selected <- NULL
      if (input$selectdata == "Use demo data") {
        selected <- "Primary efficacy outcome"
      }
      
    } else {
      choices <- list('Efficacy' = unique(df()[, which(names(df()) == "OUTCOME")]))
      selected <- NULL
      if (input$selectdata == "Use demo data") {
        selected <- "Primary efficacy outcome"
      }
    } 
    tagList(
      shinyWidgets::dropdownButton(
        inputId = "dropdown_effi",
        label = "Efficacy-Variable(s)",
        icon = icon("list"),
        status = "customefficacy",
        width = "600px",
        circle = FALSE,
        shiny::selectizeInput(
          inputId = 'effi',
          label = HTML('<p style ="color: #999999;"> Choose outcomes of interest </p>'),
          choices = choices,
          selected = selected,
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
                         'plugins' = list('remove_button','drag_drop')
          )
        ),
        shiny::helpText("Note: Click into the box to get a variable selection.
                  To change order of the variables in the forestplot choose 'As Input' as sorting in the Graphic Options.
                  Then drag and drop selected variable names.")
        
      )
    )
    }
  })
  
  output$safety <- shiny::renderUI({
    if (is.null(df())) return()
    if (is.factor(df()[, which(names(df()) == "OUTCOME")])) {
      choices <- list('Safety' = levels(df()[, which(names(df()) == "OUTCOME")]))
      selected <- NULL
      if (input$selectdata == "Use demo data") {
        selected <- "Primary efficacy outcome"
      }
    } else {
      choices <- list('Safety' = unique(df()[, which(names(df()) == "OUTCOME")]))
      selected <- NULL
      if (input$selectdata == "Use demo data") {
        selected <- "Primary efficacy outcome"
      }
    }
    shiny::tagList(
      shinyWidgets::dropdownButton(
        inputId = "dropdown_safe",
        label = "Safety-Variable(s)  ",
        icon = icon("tasks"),
        status = "customsafety",
        width = "600px",
        circle = FALSE,
        shiny::selectizeInput(
          inputId = 'safe',
          label = HTML('<p style ="color: #999999;"> Choose outcomes of interest </p>'),
          choices = choices,
          selected =  selected,
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
                         'plugins' = list('remove_button','drag_drop')
          )
        ),
        shiny::helpText(
          "Note: Click into the box to get a variable selection. 
          To change order of the variables in the forestplot, select 'As Input' as sorting in the Graphic Options.
          Then drag and drop selected variable names."
        )
      )
    )
  })
  
  output$subgroup <- shiny::renderUI({
    
    if (is.null(df()) || !(any(names(df()) == "SUBGROUP"))) return()
    if (is.factor(df()[, which(names(df()) == "SUBGROUP")])){
      choices <- levels(df()[, which(names(df()) == "SUBGROUP")])
    } else {
      choices <- unique(df()[, which(names(df()) == "SUBGROUP")])  
    }
    
    if (any(tolower(choices) %in% tolower(c("None", "All", "Overall", "Any")))) {
      selected <- choices[which(tolower(choices) %in% tolower(c("None","All","Overall","Any")))][1] 
    } else {
      selected <- choices[1]
    }
    shinyWidgets::dropdownButton(
      inputId = "dropdown_subgroup2",
      label = "Subgroup(s)         ",
      icon = icon("align-left"),
      status = "customsubgroup",
      circle = FALSE,
      shiny::selectInput(
        inputId = "subgroup2",
        label = HTML('<p style ="color: #999999;"> Choose subgroup </p>'),
        multiple = FALSE,
        selected = selected,
        choices = choices
      )
    )
  })

  
  output$SubLevel <- shiny::renderUI({
    if (is.null(input$subgroup2) || tolower(input$subgroup2) %in% tolower(c("None", "All", "Overall", "Any"))) return()
    
    if (is.null(input$subgroup2) || tolower(input$subgroup2) %in% tolower(c("None", "All", "Overall", "Any"))) {
      choices <- NULL
    } else {
      choices <- sort(unique(as.character(df()[which(df()$SUBGROUP == input$subgroup2), which(names(df()) == "SUBLEVEL")])))
    }
    
    shinyWidgets::dropdownButton(
      inputId = "dropdown_SubLevel2",
      label = "Sublevel(s)         ",
      icon = icon("sliders"),
      status = "customsublevel",
      circle = FALSE,
      shiny::selectInput(
        inputId = "SubLevel2",
        label = HTML('<p style = "color: #999999;"> Choose sublevel(s) </p>'),
        multiple = TRUE, list('Sublevel(s)'= choices),
        selected = choices
      )
    )
  })
  ####... Forest Plot-tab ####
  
  #reactive figure height of Forest plot
  height_reac <- shiny::reactive({
    shiny::req(ds_new())
    number_pixel <- 100 + ((dim(ds_new())[1]) * 45)
  })
  
  shiny::observeEvent(ds_new(), {
    
    shiny::updateNumericInput(
      session,
      inputId = "limit.low",
      value = min(ds_new()[which(grepl("LOWER", names(ds_new())))])
    )
    shiny::updateNumericInput(
      session,
      inputId = "limit.high",
      value = max(ds_new()[which(grepl("UPPER", names(ds_new())))])
    )
  })
  
  # forest plot
  output$forest2 <- shiny::renderPlot({
    #draw new plot if app window size changed
    session$clientData[["output_forest2_width"]]
    
    dat_t <- shiny::req(ds_new())
    
    if (!is.na(input$limit.low)) {
      lower_limit_ <- input$limit.low
    } else {
      lower_limit_ <- NA
    }
    
    if (!is.na(input$limit.high)) {
      upper_limit_ <- input$limit.high
    } else {
      upper_limit_ <- NA
    }
    
    if(input$title_input == "Default Title") {
      title_ <- titl()
    } else if (input$title_input == "Custom Title") {
      title_ <- input$title
    }
    
    breasy_forestplot(
      forest_data = dat_t,
      excess_number = input$Info,
      incidence_values = input$Info2,
      NNT = input$Info3,
      lower_limit = lower_limit_,
      upper_limit = upper_limit_,
      title = title_,
      safety_color = input$col_saf,
      efficacy_color = input$col_eff,
      legend_color = input$col_leg,
      sorting = input$var_sorting,
      data_scope = input$visit
    )
   }, height = function(x) height_reac() * (input$forestplot_height/100)  + 350)
  
  # forestplot_parameter
   forestplot_parameter <- shiny::reactive({
     
    dat_t <- shiny::req(ds_new())
    
    if (!is.na(input$limit.low)) {
      lower_limit_ <- input$limit.low
    } else {
      lower_limit_ <- NA
    }
    
    if (!is.na(input$limit.high)) {
      upper_limit_ <- input$limit.high
    } else {
      upper_limit_ <- NA
    }
    
    if(input$title_input == "Default Title") {
      title_ <- titl()
    } else if (input$title_input == "Custom Title") {
      title_ <- input$title
    }
    list(
      dataset = dat_t,
      Info = input$Info,
      Info2 = input$Info2,
      Info3 = input$Info3,
      lower_limit_ = lower_limit_,
      upper_limit_ = upper_limit_,
      title_ = title_,
      col_saf = input$col_saf,
      col_eff = input$col_eff,
      col_leg = input$col_leg,
      var_sorting = input$var_sorting
    )
  })
    
    ds_new <- shiny::reactive({
      shiny::req(v_sorting$val)
      d1 <- upload_text()
      dat2 <- rbind(
        d1[which(d1$ESTIMATE == input$Dependent & d1$SUBGROUP == input$subgroup2 & d1$OUTCOME %in% input$effi & d1$ANALYSIS_SET %in% input$AnaSet),],
        d1[which(d1$ESTIMATE == input$Dependent & d1$SUBGROUP == input$subgroup2 & d1$OUTCOME %in% input$safe & d1$ANALYSIS_SET %in% input$AnaSet),])
       
      dat_tmp1 <- d1[which(d1$ESTIMATE == input$Dependent & d1$SUBGROUP == input$subgroup2 & d1$OUTCOME %in% input$effi & d1$ANALYSIS_SET %in% input$AnaSet),]
      dat_tmp2 <- d1[which(d1$ESTIMATE == input$Dependent & d1$SUBGROUP == input$subgroup2 & d1$OUTCOME %in% input$safe & d1$ANALYSIS_SET %in% input$AnaSet),]
      
      if (dim(dat_tmp1)[1] == 0) {
        dat_tmp1 <- dat_tmp1 %>% 
          dplyr::mutate(BReasy_GROUP = NA)
      } else {
        dat_tmp1 <- dat_tmp1 %>% 
          dplyr::mutate(BReasy_GROUP = "Efficacy")
      }
      if (dim(dat_tmp2)[1] == 0) {
        dat_tmp2 <- dat_tmp2 %>% 
          dplyr::mutate(BReasy_GROUP = NA)
      } else {
        dat_tmp2 <- dat_tmp2 %>% 
          dplyr::mutate(BReasy_GROUP = "Safety")
      }
      
      dat <- rbind(
        dat_tmp1,
        dat_tmp2
      )
      dat
    })
  
  ####... Dataset-tab ####
  ### Display the dataset which was used for the creation of the forestplot (second tab in the app): ###
  output$dataset <- DT::renderDataTable({
    tmp <- ds_new()
    tmp[sapply(tmp, is.numeric)] <-  round(tmp[sapply(tmp, is.numeric)], 3)
    
    DT::datatable(
      tmp,
      options = list(
        initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#0091DF', 'color': '#fff'});",
        "}"
        )
      )
    )
  })
    
  #### Module call: file_creation (server) ####
  callModule(file_creation_server, "file_creation")
  #### Module call: value_tree (server) ####  
  callModule(mod_value_tree_server, "value_tree_ui_1")
  
  ####... Welcome/help-texts ####
  output$welcome_text1 <- shiny::renderUI({
    list(
      HTML(
        paste0(
          "<span style = 'font-size: 30px'> Welcome to
          <p>
          <img src='www/AppSign_BReasy_220x76mm_BLK.png' alt='Graphic cannot be displayed' height='120'>
          </span>
          <p>
          <span style = 'font-size: 30px'> 
          the R Shiny application for structured Benefit-Risk assessment.
          <p>
          <span style = 'font-size:24px'>
          For uploading CSV file(s), please use the 'Data Upload'-tab.
          In case of errors while uploading, please check the 'Data Manual'-tab and the CSV prerequisites in the 'Data Upload'-tab.

          </span>
          "
        )
      )
    )
  })
  
  start <- shiny::reactiveValues(dat = TRUE)
  
  output$flag <- shiny::reactive(start$dat)
  
  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)
  
  shiny::observeEvent(df(), {
    if (is.null(df())) {
      start$dat <- TRUE
    } else if (!is.null(df())) {
      start$dat <- FALSE
    }
  })
  
  output$flag2 <- shiny::reactive(start2$dat)
  shiny::outputOptions(output, "flag2", suspendWhenHidden = FALSE)
  start2 <- shiny::reactiveValues(dat = FALSE)
  shiny::observeEvent(df(), {
    if (is.null(df())) {
      start2$dat <- FALSE
    } else {
      start2$dat <- TRUE
    }
  })
  
  output$flag4 <- shiny::reactive(start4$dat)
  shiny::outputOptions(output, "flag4", suspendWhenHidden = FALSE)
  start4 <- shiny::reactiveValues(dat = FALSE)
  shiny::observeEvent(df(), {
    shiny::req(df())
    if(!is.null(df())) {
      if (any(colnames(df()) == "NNT")) {
        start4$dat <- TRUE
      }
    }
  })
  
  output$flag3 <- shiny::reactive(start3$dat)
  shiny::outputOptions(output, "flag3", suspendWhenHidden = FALSE)
  start3 <- shiny::reactiveValues(dat = 1)
  shiny::observeEvent(input$add_file_input, {
    if (start3$dat < 4) {
      start3$dat <- start3$dat + 1
    }
  })
  
  shiny::observeEvent(input$rem_file_input, {
    if (start3$dat > 1) {
      start3$dat <- start3$dat - 1
    }
  })
  upload_text <- shiny::reactive({
    shiny::req(df(), v_sorting$val)
    d1 <- df()
    validate(
      need(
        !is.null(d1), "Data set is missing"
      )
    )
    
    for (i in c("OUTCOME", "ESTIMATE", "ANALYSIS_SET")) {
      validate(
        need(!is.null(d1[[i]]), 
             "Warning: Please try another separator and/or quote at the upload tab or check the requirements in the'Data Manual'-tab."
        )
      )
    }
    
    for (j in c("EFFECT", "UPPER", "LOWER")) {
      validate(
        need(
          any(startsWith(colnames(d1), j)), paste0("Required variable '", j, "_xx' is missing. Please check the requirements in the'Data Manual'-tab.")
        )
      )
      
      
      if (any(startsWith(colnames(d1), j))) {
        shiny::validate(
          shiny::need(
            all(is.numeric(d1[,which(startsWith(colnames(d1), j))])), paste0("Warning: Please try another decimal character at the upload tab or check the requirements in the'Data Manual'-tab!")
          )
        )
      }
    }
    
    shiny::validate(
      shiny::need(!(is.null(input$effi) && is.null(input$safe)),
        "Please select one or more efficacy and/or safety variable(s)!"
      )
    )

    shiny::validate(
      shiny::need(!((is.null(input$visit)) && (any(names(df()) == "AVISIT") || any(names(df()) == "DATA_SCOPE"))),
        "Please select at least one data scope variable."
      )
    )
    d1
  })
  
  #### Graphic Options ####
  # Create customize title 
  titl <- shiny::reactive({
    title1 <- input$Dependent
    if (!is.null(input$subgroup2) && !(tolower(input$subgroup2) %in% tolower(c("None", "All", "Overall", "Any")))) {
      title2 <- paste(
        ifelse(
          any(names(df()) == "STUDY"),
          paste0("Forest plot based on ", title1, " by ", input$subgroup2, ", STUDY = ", input$studyNr),
          paste0("Forest plot based on ", title1, " by ", input$subgroup2)
        ),
        paste("\nAnalysis set =",
              paste(input$AnaSet, collapse = ', '),
              "Data Scope =",
              shiny::isolate(paste(input$visit, collapse = ', '))
        )
      )
    } else {
      title2 <- paste(
        ifelse(
          any(names(df()) == "STUDY"),
          paste0("Forest plot based on ", title1, ", STUDY = ", input$studyNr),
          paste0("Forest plot based on ", title1)
        ),
        paste("\nAnalysis set =",
              paste(input$AnaSet, collapse = ', '),
              ", Data Scope =",
              paste(input$visit, collapse = ', ')
        )
      )
    }
    title2
  })
  
  output$title <- shiny::renderUI({
    title <- titl()
    shiny::textInput(
      inputId = "title", 
      label = "Choose a title",
      value = "Enter title here!"
    )
  })
  
  v_sorting <- shiny::reactiveValues(val = "As Input")
  
  shiny::observeEvent(input$var_sorting, {
    v_sorting$val <- input$var_sorting
  })
  
  #### About BReasy-tab ####
  callModule(mod_about_breasy_server, "about_breasy_ui_1")
 
  callModule(mod_data_manual_server, "data_manual_ui_1")
  
  #### REPORT ####
  output$report <- shiny::downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){
      paste(
        "BReasy_Forestplot",
        gsub(":",
        "-",
        Sys.time()),
        ".html", 
        sep = ""
      )
    },
    content = function(file) {
      withProgress(message = 'Generating Report, please wait!', {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "www/AppSign_BReasy_220x76mm_RGB_blk.png")
        file.copy("www/AppSign_BReasy_220x76mm_RGB_blk.png", tempReport_child, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          forestplot_parameter = forestplot_parameter(),
          rendered_by_shiny = TRUE
        )
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      })
    }
  )

  output$cont_safety <- shiny::renderUI({
   shiny::tags$head(tags$style(".btn-customsafety {background-color: ", input$col_saf,"; color: ", font_color(input$col_saf),";width: 175px;}"))
  })
  
  output$cont_efficacy <- shiny::renderUI({
   shiny::tags$head(tags$style(".btn-customefficacy {background-color: ", input$col_eff,"; color: ", font_color(input$col_eff),";width: 175px;}"))
  })
}
