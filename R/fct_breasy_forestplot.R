#' draw a forestplot grouped by Efficacy & Safety
#' 
#' @param forest_data data set with required columns: EFFECT_xy, LOWERxy, UPPERxy, OUTCOME
#' @param excess_number logical value if a column of excess numbers should be drawn [default: TRUE]
#' @param incidence_values logical value if a column of incidence values should be drawn [default: TRUE]
#' @param NNT logical value if a column of NNT values should be drawn [default: TRUE]
#' @param title plot title [default: NULL]
#' @param lower_limit lower x-axis value [default: NULL] 
#' @param upper_limit upper x-axis value [default: NULL]
#' @param efficacy_color color for the Efficacy font and Confidence Interval 
#' @param safety_color color for the Safety font and Confidence Interval 
#' @param legend_color color for the Legend font and Arrows
#' @param sorting Sorting variable (possible selections: "As Input", "Alphabetical", or "Effect") [default: "As Input"]
#' 

breasy_forestplot <- function(
  forest_data, 
  excess_number = TRUE,
  incidence_values = TRUE,
  NNT = TRUE,
  title = NULL,
  lower_limit = NA,
  upper_limit = NA,
  efficacy_color = "#0091DF",
  safety_color = "#66B512",
  legend_color = "#D30F4B",
  sorting = "As Input",
  data_scope = NULL
  ) {
  
  forest_data_ <<- forest_data
  excess_number_ <<- excess_number
  incidence_values_ <<- incidence_values
  NNT_ <<- NNT
  title_ <<- title
  lower_limit_ <<- lower_limit
  upper_limit_ <<- upper_limit
  efficacy_color_ <<- efficacy_color
  safety_color_ <<- safety_color
  legend_color_ <<- legend_color
  sorting_ <<- sorting
  
  # forest_data <- forest_data_
  # excess_number <- excess_number_
  # incidence_values <- incidence_values_
  # NNT <- NNT_
  # title <- title_
  # lower_limit <- lower_limit_
  # upper_limit <- upper_limit_
  # efficacy_color <- efficacy_color_
  # safety_color <- safety_color_
  # legend_color <- legend_color_
  # sorting <- sorting_
  
  # Split data in Safety/Efficacy Part
  forest_data_saf <- forest_data[forest_data$BReasy_GROUP == "Safety",]
  if (dim(forest_data_saf)[1] > 0) {
    if (sorting == "Alphabetical") {
      forest_data_saf <- forest_data_saf %>%
      dplyr::arrange(dplyr::desc(OUTCOME))
    }
    if (sorting == "Effect") {
      forest_data_saf <- forest_data_saf %>%
        dplyr::arrange(!! rlang::sym(colnames(forest_data_saf)[which(grepl("EFFECT_", names(forest_data_saf)))]))
    }
    
    forest_data_saf$BReasy_NUMBER <- 1:dim(forest_data[forest_data$BReasy_GROUP == "Safety",])[1]
    forest_data_saf$BReasy_GROUP_ID <- 1
  }
  
  forest_data_eff <- forest_data[forest_data$BReasy_GROUP == "Efficacy",]
  
  if (dim(forest_data_eff)[1] > 0) {
    if (sorting == "Alphabetical") {
      forest_data_eff <- forest_data_eff %>%
        dplyr::arrange(dplyr::desc(OUTCOME))
    }
    if (sorting == "Effect") {
    forest_data_eff <- forest_data_eff %>%
      dplyr::arrange(!! rlang::sym(colnames(forest_data_eff)[which(grepl("EFFECT_", names(forest_data_eff)))]))
    }
    forest_data_eff$BReasy_NUMBER <- 1 + ((dim(forest_data[forest_data$BReasy_GROUP == "Safety",])[1]+1):(dim(forest_data[forest_data$BReasy_GROUP == "Safety",])[1]+dim(forest_data[forest_data$BReasy_GROUP == "Efficacy",])[1]))
    forest_data_eff$BReasy_GROUP_ID <- 2
  }
  
  forest_data <- rbind(forest_data_saf, forest_data_eff)
  
  if (!is.null(data_scope)) {
    forest_data <- forest_data %>% 
      dplyr::filter(DATA_SCOPE %in% data_scope)
  }  
  
  #number of columns on the right hand side
  right_side_width <- sum(
    c(
      excess_number,
      incidence_values,
      NNT
    )
  )
  
  # set margins depending on plot window size
  plot.set <- list(
    'mai' = c(min(dev.size()[2],10)/5, dev.size()[1]/4, min(dev.size()[2],10)/5, (right_side_width/3) * dev.size()[1]/3)
  )
  
  # set font size depending on the plot width
  cex_factor <- mean(c(1, dev.size()[1]/16))
  
  
  # get or calculate x-axis limits
  forest_data[,colnames(forest_data)[which(grepl("LOWER", names(forest_data)))]]
  
  xlim <- c(
    ifelse(is.na(lower_limit), min(forest_data[,colnames(forest_data)[which(grepl("LOWER", names(forest_data)))]]), lower_limit),
    ifelse(is.na(upper_limit), max(forest_data[,colnames(forest_data)[which(grepl("UPPER", names(forest_data)))]]), upper_limit)
  )
  # get 12.5percent of the x-axis length
  xlim_125p <- diff(xlim) * 0.125
  
  # create a 25% bigger x-axis range
  xlim2 <- c(
    xlim[1] - xlim_125p, 
    xlim[2] + xlim_125p
  )
  # y-axis limits
  ylim <- range(forest_data$BReasy_NUMBER) + c(-0.5, 0.5)
      
  par(
    mai = plot.set$mai,
    bg = "#ebebeb",
    lheight = 0.8,
    xpd = TRUE
  )
 
  # draw empty Plot
  plot(
    NULL,
    xlim = xlim2,
    ylim = ylim,
    xlab = '',
    ylab = '',
    axes = FALSE,
    yaxs = 'i',
  )

  text(
    x = xlim2[1],
    y = mean(
      c(
        grconvertY(1, from = 'ndc', to = 'user'),
        grconvertY(1, from = 'npc', to = 'user')
      )
    ),
    adj = c(0.3,0.5),
    labels = title,
    cex = 1.2 * cex_factor, 
    font = c(2,3)
  )
    
  axis(
    1,
    pos = min(forest_data$BReasy_NUMBER) - 0.5,
    col = "black"
  )
  
  axis(
    1, 
    at = xlim2,
    lwd.ticks = 0,
    labels = FALSE
  )

  forest_data <- forest_data %>% dplyr::mutate(
    ll = pmax(LOWER95, xlim[1]),
    ul = pmin(UPPER95, xlim[2]),
  )
  
  forest_data <- forest_data %>%
    dplyr::mutate(
      code = dplyr::case_when(
        xlim[1] > UPPER95 | xlim[2] < LOWER95 ~ 0,
        xlim[1] > LOWER95  &  xlim[2] >= UPPER95 ~ 2,
        xlim[2] < UPPER95 &  xlim[1] <= LOWER95 ~ 1,
        
        TRUE ~ 3
      ),
      length = dplyr::case_when(
        xlim[1] > LOWER95 & xlim[2] < UPPER95 ~ 0,
        TRUE ~ 0.1
      )
    )

  forest_data <- forest_data %>% 
    dplyr::mutate(
      color = dplyr::case_when(
        BReasy_GROUP == "Efficacy" ~ efficacy_color,
        BReasy_GROUP == "Safety" ~ safety_color
      )
    )
  
  for (i in 1:3) {
    for(j in c(0, 0.1)) { 
      forest_data_tmp <- forest_data %>% dplyr::filter(code == i & length == j)
      if (dim(forest_data_tmp)[1] > 0) {
        arrows(
          x0 = forest_data_tmp$ll,
          y0 = forest_data_tmp$BReasy_NUMBER,
          x1 = forest_data_tmp$ul,
          y1 = forest_data_tmp$BReasy_NUMBER,
          angle = 90,
          code = i,
          lwd = 2,
          col = forest_data_tmp$color,
          length = j
        )
      }
    }
  }
  
  forest_data_tmp <- forest_data[forest_data[,which(grepl("EFFECT_", names(forest_data)))] >= xlim[1] & forest_data[,which(grepl("EFFECT_", names(forest_data)))] < xlim[2],]

  points(
    forest_data_tmp[,colnames(forest_data_tmp)[which(grepl("EFFECT_", names(forest_data_tmp)))]],
    forest_data_tmp$BReasy_NUMBER,
    pch = 15,
    cex = 2 * cex_factor,
    col = "grey15"
  )
  
  if (dim(forest_data_saf)[1] > 0) {
    text(
      x = grconvertX(0, from = 'npc', to = 'user'),
      y = max(forest_data_saf$BReasy_NUMBER) + (length(unique(forest_data$BReasy_GROUP))/4),
      xpd = NA,
      adj = c(1, 0.5),
      cex = 1.2 * cex_factor ,
      labels = "Safety",
      col = safety_color
    )
  }
  if (dim(forest_data_eff)[1] > 0) {
    text(
      x = grconvertX(0, from = 'npc', to = 'user'),#grconvertX(0, from = 'npc', to = 'user'),
      y = max(forest_data_eff$BReasy_NUMBER) + (length(unique(forest_data$BReasy_GROUP))/4),
      xpd = NA,
      adj = c(1, 0.5),
      cex = 1.2 * cex_factor,
      labels = "Efficacy",
      col = efficacy_color
    )
  }  
  
  if ("SUBGROUP" %in% colnames(forest_data) & all(forest_data$SUBGROUP != "None")) {  
    if (length(data_scope) > 1) {
      labels_ <- paste0(forest_data[,'OUTCOME'],paste0(" (", forest_data[,'DATA_SCOPE'], ") "), " (",forest_data[,'SUBGROUP'], ": ", forest_data[,'SUBLEVEL'], ")")
    
    } else {
      labels_ <- paste0(forest_data[,'OUTCOME'], " (",forest_data[,'SUBGROUP'], ": ", forest_data[,'SUBLEVEL'], ")")
    }
    labels_1_40 <- substr(labels_,1,40)
    labels_41_80 <- substr(labels_,41,80)
    labels_81_120 <- substr(labels_,81,120)
    if (any(nchar(labels_)>=120)) {
      labels_81_120[nchar(labels_)>=120] <- paste0(labels_81_120[nchar(labels_)>=120]," ...") 
    }
  } else {
    if (length(data_scope) > 1) {
      labels_ <- paste0(forest_data[,'OUTCOME'], " (", forest_data[, 'DATA_SCOPE'], " )")
    } else {
      labels_ <- forest_data[,'OUTCOME']
    }
    labels_1_40 <- substr(labels_,1,40)
    labels_41_80 <- substr(labels_,41,80)
    labels_81_120 <- substr(labels_,81,120)
    if (any(nchar(labels_)>=120)) {
      labels_81_120[nchar(labels_)>=120] <- paste0(labels_81_120[nchar(labels_)>=120]," ...") 
    }
  }
      
  text(
    x = grconvertX(0, from = 'ndc', to = 'user'),
    y = forest_data$BReasy_NUMBER,
    xpd = NA,
    adj = c(0,0.5),
    cex = 1 * cex_factor ,
    labels = labels_1_40,
    col = "black"
  )
  text(
    x = grconvertX(0, from = 'ndc', to = 'user'),
    y = forest_data$BReasy_NUMBER-0.3,
    xpd = NA,
    adj = c(0,0.5),
    cex = 1 * cex_factor ,
    labels = labels_41_80,
    col = "black"
  )
  
  text(
    x = grconvertX(0, from = 'ndc', to = 'user'),
    y = forest_data$BReasy_NUMBER-0.6,
    xpd = NA,
    adj = c(0,0.5),
    cex = 1 * cex_factor ,
    labels = labels_81_120,
    col = "black"
  )
  
  text_coord <- seq(xlim2[2], grconvertX(1, from = 'ndc', to = 'user'), length = 2+right_side_width)[-c(1, 2 + right_side_width)]
  
  j <- 1
  k <- 0.7
  
  # draw columns on right hand side
  if (excess_number) {
    text(
      x = text_coord[j], 
      y = max(forest_data$BReasy_NUMBER) + ((length(unique(forest_data$BReasy_GROUP)) + 0.1) / 2),
      xpd = NA,
      adj = c(k, 0.5),
      cex = 1 * cex_factor ,
      labels = paste("Excess number ","of subjects(95%-CI)", sep = "\n"),
      col = "grey15"
    )
  
    text(
      x = text_coord[j],
      y = forest_data$BReasy_NUMBER,
      xpd = NA,
      adj = c(k, 0.5),
      cex = 1 * cex_factor,
      labels = paste0(
        forest_data[,colnames(forest_data)[which(grepl("EFFECT_", names(forest_data)))]],
        " (", 
        round(forest_data[,colnames(forest_data)[which(grepl("LOWER", names(forest_data)))]],2),
        ", ", 
        round(forest_data[,colnames(forest_data)[which(grepl("UPPER", names(forest_data)))]],2),
        ")"
      ),
      col = "grey15"
    )
    j <- j + 1
    k <- k - 0.2 
  }
  
  if (incidence_values) {
    text(
      x = text_coord[j],
      y = max(forest_data$BReasy_NUMBER) + ((length(unique(forest_data$BReasy_GROUP)) + 0.1) / 2),
      xpd = NA,
      adj = c(k, 0.5),
      cex = 1 * cex_factor ,
      labels = paste(
        "Events Verum",
        "/ Patients Verum vs.",
        "Events Comparator ",
        "/ Patients Comparator)",
        sep = "\n"
      ),
      col = "grey15"
    )
        
    text(
      x = text_coord[j],
      y = forest_data$BReasy_NUMBER,
      xpd = NA,
      adj = c(k, 0.5),
      cex = 1* cex_factor,
      labels = paste0(
        forest_data[,'NUMBER_EVENTS_VERUM'], 
        "/",
        forest_data[,'NUMBER_PATIENTS_VERUM'],
        " vs. \n",
        forest_data[,'NUMBER_EVENTS_COMP'],
        "/",
        forest_data[,'NUMBER_PATIENTS_COMP']
      ),
      col = "grey15"
    )
    j <- j + 1
    k <- k - 0.2
  }
  
  if (NNT) {
    text(
      x = text_coord[j],
      y = forest_data$BReasy_NUMBER,
      xpd = NA,
      adj = c(k, 0.5),
      cex = 1* cex_factor,
      labels = forest_data[,'NNT'],
      col = "grey15"
    )
    
    text(
      x = text_coord[j],
      y = max(forest_data$BReasy_NUMBER) + ((length(unique(forest_data$BReasy_GROUP)) + 0.1)/2),
      xpd = NA,
      adj = c(k, 0.5),
      cex = 1* cex_factor,
      labels = "NNT",
      col = "grey15"
    )
  }
  
  # create dotted line at x = 0  
  if (xlim[1] < 0 & 0 < xlim[2]) {
    arrows(
      0,
      min(forest_data$BReasy_NUMBER) - 0.5,
      0,
      max(forest_data$BReasy_NUMBER) + 0.75,
      col = "grey70",
      lty = 2,
      length = 0
    )
  }
  # Create arrows under the main plot   
  par(
    xpd = TRUE,
    mar = c(0,0,0,0)
  )

  arrows(
    x0 = xlim[2] - (diff(xlim) / 2) - xlim_125p,
    y0 = mean(c(ylim[1],grconvertY(0, from = 'npc', to = 'user'))),
    x1 = xlim[1],
    y1 = mean(c(ylim[1],grconvertY(0, from = 'npc', to = 'user'))),
    col = legend_color,
    lwd = 2.5 * cex_factor
  )
      
  text(
    x = xlim[2] - (diff(xlim)/2) - xlim_125p,
    y = mean(c(ylim[1], grconvertY(0, from = 'npc', to = 'user'))) + mean(ylim)/30,
    xpd = NA,
    adj = c(1, 0.5),
    cex = 1* cex_factor,
    labels = "Favours Verum",
    col = legend_color
  )
      
  arrows(
    x0 = xlim[1]+(diff(xlim)/2)+xlim_125p,
    y0 = mean(c(ylim[1],grconvertY(0, from = 'npc', to = 'user'))),
    x1 = xlim[2],
    y1 = mean(c(ylim[1],grconvertY(0, from = 'npc', to = 'user'))),
    col = legend_color,
    lwd = 2.5 * cex_factor
  )

  text(
    x = xlim[1] + (diff(xlim)/2) + xlim_125p,
    y = mean(c(ylim[1],grconvertY(0, from = 'npc', to = 'user'))) + mean(ylim)/30,
    xpd = NA,
    adj = c(0, 0.5),
    cex = 1 * cex_factor,
    labels = "Favours Comparator",
    col = legend_color
  )
      
  text(
    x = xlim[1] + (diff(xlim)/2),
    y = mean(c(ylim[1], grconvertY(0, from = 'npc', to = 'user'))) - mean(ylim)/30,
    xpd = NA,
    adj = c(0.5, 0.5),
    cex=1* cex_factor,
    labels = "Excess number of subjects",
    col = legend_color
  )
}
