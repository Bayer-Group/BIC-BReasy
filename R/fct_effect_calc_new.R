#' file creation function for BReasy - Creates BReasy input from SAS files
#' 
#' 
#' @param data data frame with adtte data (merged with adsl data)
#' @param effect effect estimate (possible selection: CID, EXCESS_CID, ARD, EXCESS_ARD, HR, IRD, EXCESS_IRD)
#' @param day integer value for CID calculation
#' @param param character with parameter variable
#' @param outcome character(s) with outcome from param
#' @param scope character with scope variable
#' @param datascope character with scope selection
#' @param population character with population flag variable
#' @param population_value numeric or character with population flag value
#' @param treatment character with treatment variable
#' @param verum character(s) with treatment selection for verum
#' @param comparator character(s) with treatment selection for comparator
#' @param cnsr character with censoring variable
#' @param event character with event selection from cnsr
#' @param strat character(s) with stratification variable
#' @param subgroup character(s) with subgroup variable 
#' @param aval character with aval variable
#' 

effect_calc_new <- function(
    data = data,
    effect = effect,
    day = "7",
    param = param,
    outcome = outcome,
    scope = scope,
    datascope = datascope,
    population = population,
    population_value = population_value,
    treatment = treatment,
    verum = verum,
    comparator = comparator,
    cnsr = cnsr,
    event = event,
    strat = strat,
    subgroup = subgroup,
    aval = aval
) {

  
  '%notin%' <- Negate('%in%')
  
  if (length(subgroup) != 1) {
    if (any(subgroup %notin% "Overall")){
       data[,subgroup][data[,subgroup] == ""] <- "N/A"
    }
  }
  if (length(subgroup) == 1) {
    if (subgroup != "Overall") {
       data[,subgroup][data[,subgroup] == ""] <- "N/A"
    }
  }
  #helperfunction 
  #get trialnumbers/names
  if ("STUDYID" %in% colnames(data)) {
    if (nlevels(as.factor(data$STUDYID)) > 1) {
      trials <- paste(levels(as.factor(data$STUDYID)), collapse = "/")
      trialno <- paste("Pool of", trials)
    } else {
      trialno <- paste(levels(as.factor(data$STUDYID)), collapse = "/")
    }
  } else {
    trialno <- ""
  }
  #select all scope variables if no selection is done 
  if (scope != "No selection") {
    if ("No selection" %in% datascope) {datascope <- unique(data[[scope]])}
  
    #### 1. Filter by datascope, population and outcome ####
    data_filtered <- data %>%
      dplyr::filter(!!rlang::sym(scope) %in% datascope) %>%
      dplyr::filter(!!rlang::sym(population) %in% population_value) %>%
      dplyr::filter(!!rlang::sym(param) %in% outcome)
  } else {
    data_filtered <- data %>%
    dplyr::filter(!!rlang::sym(population) %in% population_value) %>%
    dplyr::filter(!!rlang::sym(param) %in% outcome)
  }
  #create custom treatment variables breasy_treatment, breasy_treatment_n (0/1 coded)
  #breasy_treatment_n2 (1/2 coded), breasy_csnr_n (0,1) and breasy_csnr_n_factor
  data_w_treat <- data_filtered %>%
    dplyr::mutate(
      breasy_treatment = dplyr::case_when(
         !!rlang::sym(treatment) %in% comparator ~ "comparator",
         !!rlang::sym(treatment) %in% verum ~ "verum",
         TRUE ~ NA_character_
      ),
      breasy_treatment_n = dplyr::case_when(
         !!rlang::sym(treatment) %in% comparator ~ 0,
         !!rlang::sym(treatment) %in% verum ~ 1
      ),
      breasy_treatment_n2 = dplyr::case_when(
         !!rlang::sym(treatment) %in% comparator ~ 2,
         !!rlang::sym(treatment) %in% verum ~ 1
      ),
      breasy_cnsr_n = dplyr::case_when(
         !!rlang::sym(cnsr) %notin% event ~ 0,
         !!rlang::sym(cnsr) %in% event ~ 1
      ),
      breasy_cnsr_n_factor = breasy_cnsr_n + 1,
      breasy_aval = !!rlang::sym(aval)
    )

  #### 2. Group ####
  # group by treatment and summarize data
  # add Variable STRATUM
  data_grouped_overall <- data_w_treat %>%
     dplyr::group_by(
       breasy_treatment, !!rlang::sym(param)
     )
  
  # check if stratification is selected
  # and get stratification factor names
  if (all(length(strat) == 1 & strat == "Overall")) {
    stratification_used <- FALSE
  } else {
    strat <- strat[strat != "Overall"]
    stratification_used <- TRUE
  }
  
  # check if subgroups are selected
  # and get subgroup names
  if (all(length(subgroup) == 1 & subgroup == "Overall")) {
    subgroup_used <- FALSE
  } else {
    subgroup <- subgroup[subgroup != "Overall"]
    subgroup_used <- TRUE
  }

  #### 3. Effect calculation ####
  # distinguish between cases with or without stratification and subgroup
  # calculations
  if (!stratification_used & !subgroup_used) {
    data_used <- data_grouped_overall
    join_by <- c(param)
    grouping_vars <- c(param)
  } else if (stratification_used & !subgroup_used) {
    data_used <- data_grouped_overall
    join_by <- c(param,"STRATA")
    join_by_strat <- c(param,"STRATA")
    grouping_vars <- c(param)
  } else if (!stratification_used & subgroup_used) {
    data_used <- data_grouped_overall
    grouping_vars <- join_by <- c(param,"SUBLEVEL","SUBGROUP")
    if (effect %in% c("HR")) {
      grouping_vars <- c(param ,subgroup)
    }
  } else if (stratification_used & subgroup_used) {
    data_used <- data_grouped_overall
    join_by_strat_sub <- c(param ,"STRATA", "SUBGROUP","SUBLEVEL")
    join_by_strat <- c(param,"STRATA")
    join_by <- c(param,"SUBLEVEL","SUBGROUP")
    grouping_vars <- c(param , "SUBLEVEL")
    if (effect %in% c("HR")) {
      grouping_vars <- c(param ,subgroup)
    }
  }
  join_by_overall <- c(param)
  
  data_summarized_overall <- data_grouped_overall %>% 
     dplyr::group_by(
       breasy_treatment,
       !!rlang::sym(param)
     ) %>% 
     dplyr::summarise(
       t = sum(!!rlang::sym(aval), na.rm = TRUE)/(100 * 365.25),
       n = n(),
       x = sum(!!rlang::sym(cnsr) %in% event, na.rm =TRUE),
       x_complement = sum(!!rlang::sym(cnsr) %notin% event, na.rm =TRUE),
       .groups ="drop"
     )
    
  if (subgroup_used) {
    merge_subgroup_data <- c()
    for(st in subgroup) {
      rd_summary <- data_used %>% 
        group_by(breasy_treatment,
         !!rlang::sym(param),
         !!rlang::sym(st)
       ) %>% 
       dplyr::summarise(
         t = sum(!!rlang::sym(aval), na.rm = TRUE)/(100 * 365.25),
         n = n(),
         x = sum(!!rlang::sym(cnsr) %in% event, na.rm =TRUE),
         x_complement = sum(!!rlang::sym(cnsr) %notin% event, na.rm =TRUE),
         .groups ="drop"
       ) %>% 
        dplyr::rename("SUBLEVEL" = !!rlang::sym(st)) %>% 
        dplyr::mutate(
          SUBGROUP = st
        )
      merge_subgroup_data <- rbind(merge_subgroup_data, rd_summary)
    }
  }

  #summarise data for number events and number subjects in each treatment arm
  if (stratification_used) {
    merge_strata_data <- c()
    for(st in strat) {
      rd_summary <- data_used %>% 
        group_by(breasy_treatment,
         !!rlang::sym(param),
         !!rlang::sym(st)
       ) %>% 
       dplyr::summarise(
         t = sum(!!rlang::sym(aval), na.rm = TRUE)/(100 * 365.25),
         n = n(),
         x = sum(!!rlang::sym(cnsr) %in% event, na.rm =TRUE),
         x_complement = sum(!!rlang::sym(cnsr) %notin% event, na.rm =TRUE),
         .groups ="drop"
       ) %>% dplyr::rename("STRATA" = !!rlang::sym(st))
      merge_strata_data <- rbind(merge_strata_data, rd_summary)
    }
  }
 if (stratification_used & subgroup_used) {
    merge_strata_subgroup_data <- c()
    for(st in strat) {
      for(su in subgroup) {
      rd_summary <- data_used %>% 
        group_by(breasy_treatment,
         !!rlang::sym(param),
         !!rlang::sym(su),
         !!rlang::sym(st)
       ) %>% 
       dplyr::summarise(
         t = sum(!!rlang::sym(aval), na.rm = TRUE)/(100 * 365.25),
         n = n(),
         x = sum(!!rlang::sym(cnsr) %in% event, na.rm =TRUE),
         x_complement = sum(!!rlang::sym(cnsr) %notin% event, na.rm =TRUE),
         .groups ="drop"
       ) %>% dplyr::rename(
         "STRATA" = !!rlang::sym(st),
         "SUBLEVEL" = !!rlang::sym(su)
        ) %>% 
        dplyr::mutate(
          "SUBGROUP" = su
        )
      
      merge_strata_subgroup_data <- rbind(merge_strata_subgroup_data, rd_summary)
      }
    }
  }
  #get data in 'wide' format by joining
  overall_summary_wide <- dplyr::full_join(
    data_summarized_overall %>% dplyr::filter(breasy_treatment == "verum") %>%  dplyr::select(-breasy_treatment) %>% rename(t_1 = t, n_1 = n, x_1 = x, x_complement_1 = x_complement),
    data_summarized_overall %>% dplyr::filter(breasy_treatment == "comparator") %>% dplyr::select(-breasy_treatment) %>% rename(t_2 = t, n_2 = n, x_2 = x, x_complement_2 = x_complement),
    by = join_by_overall
  )
  if (stratification_used) {
    #get data in 'wide' format by joining
    strat_summary_wide <- dplyr::full_join(
      merge_strata_data %>% dplyr::filter(breasy_treatment == "verum") %>%  dplyr::select(-breasy_treatment) %>% rename(t_1 = t, n_1 = n, x_1 = x, x_complement_1 = x_complement),
      merge_strata_data %>% dplyr::filter(breasy_treatment == "comparator") %>% dplyr::select(-breasy_treatment) %>% rename(t_2 = t, n_2 = n, x_2 = x, x_complement_2 = x_complement),
      by = join_by_strat
    )
  }
  
  if (subgroup_used) {
    #get data in 'wide' format by joining
    subgroup_summary_wide <- dplyr::full_join(
      merge_subgroup_data %>% dplyr::filter(breasy_treatment == "verum") %>%  dplyr::select(-breasy_treatment) %>% rename(t_1 = t, n_1 = n, x_1 = x, x_complement_1 = x_complement)
      ,
      merge_subgroup_data %>% dplyr::filter(breasy_treatment == "comparator") %>% dplyr::select(-breasy_treatment) %>% rename(t_2 = t, n_2 = n, x_2 = x, x_complement_2 = x_complement)
      ,
      by = join_by
    ) %>%
    dplyr::mutate(x_1 = ifelse(is.na(x_1),0,x_1),n_1 = ifelse(is.na(n_1),0,n_1),t_1 = ifelse(is.na(t_1),0,t_1)) %>%
      dplyr::mutate(x_2 = ifelse(is.na(x_2),0,x_2),n_2 = ifelse(is.na(n_2),0,n_2),t_2 = ifelse(is.na(t_2),0,t_2))
  }

  if (subgroup_used & stratification_used) {
    #get data in 'wide' format by joining
    strata_subgroup_summary_wide <- dplyr::full_join(
      merge_strata_subgroup_data %>% dplyr::filter(breasy_treatment == "verum") %>%  dplyr::select(-breasy_treatment) %>% rename(t_1 = t, n_1 = n, x_1 = x, x_complement_1 = x_complement),
      merge_strata_subgroup_data %>% dplyr::filter(breasy_treatment == "comparator") %>% dplyr::select(-breasy_treatment) %>% rename(t_2 = t, n_2 = n, x_2 = x, x_complement_2 = x_complement),
      by = join_by_strat_sub
    )
  }
  #remove subgroups with time at-risk t_1 or t_2 equals zero, to be used for IRD & ARD
  overall_summary_wide_non_zero_times <- overall_summary_wide %>%
    dplyr::filter(t_1 > 0 & t_2 > 0)

  if (stratification_used) {
    strat_summary_wide_non_zero_times <- strat_summary_wide %>%
      dplyr::filter(t_1 > 0 & t_2 > 0)
  }
  
   if (subgroup_used) {
    subgroup_summary_wide_non_zero_times <- subgroup_summary_wide %>%
      dplyr::filter(t_1 > 0 & t_2 > 0)
   }
  
   if (subgroup_used & stratification_used) {
    strata_subgroup_summary_wide_non_zero_times <- strata_subgroup_summary_wide %>%
      dplyr::filter(t_1 > 0 & t_2 > 0)
  }

 #create different functions for each effect estimate and assign it to rd_func
 #### IRD ####
  if (effect %in% c("IRD","EXCESS_IRD","ARD","EXCESS_ARD")) {
    data_used1 <- overall_summary_wide_non_zero_times
    
    if (subgroup_used) {
      data_used1 <- subgroup_summary_wide_non_zero_times
    }
    if (stratification_used & !subgroup_used) {
      data_used2 <- strat_summary_wide_non_zero_times
    }
    if (!stratification_used & !subgroup_used) {
      data_used2 <- overall_summary_wide_non_zero_times
    }
    if (subgroup_used & !stratification_used) {
      data_used2 <- subgroup_summary_wide_non_zero_times
    }
    if (subgroup_used & stratification_used) {
      data_used2 <- strata_subgroup_summary_wide_non_zero_times
    }
    if (effect %in% c("IRD","EXCESS_IRD")) {
      rd_func <- function(df){
        confint(metafor::rma.mh(x1i=x_1, x2i=x_2, t1i=t_1, t2i=t_2, measure = "IRD", level = 95, data = df))$fixed
      }
    } else if (effect %in% c("ARD","EXCESS_ARD")) {
      rd_func <- function(df){
        confint(metafor::rma.mh(ai = x_1, bi = x_complement_1, ci = x_2, di = x_complement_2, measure = "RD", level = 95, data = df))$fixed
      }
    } 
  }
   
  if (effect %in% c("CID", "EXCESS_CID")) {
  #### CID ####
   data_used1 <- overall_summary_wide
    data_used1
    if (subgroup_used) {
      data_used1 <- subgroup_summary_wide
    }
    data_used2 <- data_used
    
   rd_func <- function(df) {
# creating event variable for Aalen-Johansen (3 levels) or Kaplan-Meier analysis (2 levels)
   df$CNSR_1 <- as.factor(df[[cnsr]])
   if (nlevels(as.factor(df[[cnsr]])) > 2) {
    if (event == 0){
      levels(df$CNSR_1) <- c("2","1","3")
    }else if (event == 1){
      levels(df$CNSR_1) <- c("1","2","3")
    }else if (event == 2){
      levels(df$CNSR_1) <- c("1","3","2")
    }
    df$cnsr_factor <- factor(df$CNSR_1, levels = c("1", "2", "3"))
  } else if (nlevels(as.factor(df[[cnsr]])) == 2) {
      df$CNSR_1 <- ifelse(df$CNSR_1 != event , "0" , "1")
    df$cnsr_factor <- factor(df$CNSR_1)
  } else if (nlevels(as.factor(df[[cnsr]])) == 1) {
    df$CNSR_1 <- ifelse(df$CNSR_1 != event , "0" , "1")
    df$cnsr_factor <- factor(df$CNSR_1, levels = c("0","1"))
  } else if (nlevels(as.factor(df[[cnsr]])) == 0) {
    df$CNSR_1 <- ifelse(df$CNSR_1 != event , "0" , "1")
    df$cnsr_factor <- factor(df$CNSR_1, levels = c("0","1"))
  }
   n1 <- length(which(df$breasy_treatment=="verum"))
   n2 <- length(which(df$breasy_treatment=="comparator"))
   # Run survival analysis only in case data for both treatment groups and at least one event
   if (is.element(1,df$CNSR_1) & n1>0 & n2>0)
   {
      tmp <- summary(survival::survfit(survival::Surv(breasy_aval, cnsr_factor) ~ breasy_treatment_n2, data = df))
      tmp2 <- as.data.frame(cbind(tmp$strata,tmp$time,tmp$n.event[,2],tmp$pstate[,2],tmp$std.err[,2]))
      names(tmp2) <- c("strata","time","n.event","cum.inc","std.err")

      if (day %in% tmp2[which(tmp2$strata == 1),]$time) {
      new_day_verum <- day
    } else if (day %notin% tmp2[which(tmp2$strata == 1),]$time) {
      new_time_verum <- c(day,tmp2[which(tmp2$strata == 1),]$time)
      new_time_sort_verum <- sort(new_time_verum)
      pos_verum <- match(day,new_time_sort_verum)
      new_day_verum <- new_time_sort_verum[pos_verum - 1]
    }
    if (day %in% tmp2[which(tmp2$strata == 2),]$time) {
      new_day_comp <- day
    } else if (day %notin% tmp2[which(tmp2$strata == 2),]$time) {
      new_time_comp <- c(day,tmp2[which(tmp2$strata == 2),]$time)
      new_time_sort_comp <- sort(new_time_comp)
      pos_comp <- match(day,new_time_sort_comp)
      new_day_comp <- new_time_sort_comp[pos_comp - 1]
    }
    x1 <- sum(tmp2[which(tmp2$time <= new_day_verum & tmp2$strata == 1),]$n.event)
    x2 <- sum(tmp2[which(tmp2$time <= new_day_comp & tmp2$strata == 2),]$n.event)
   }
   else {
     x1 <- sum(as.integer(df$CNSR_1[which(df$breasy_treatment=="verum")]))
     x2 <- sum(as.integer(df$CNSR_1[which(df$breasy_treatment=="comparator")]))
   }
   # Generate estiamte and CI in case at least one event and one patient per treatment group
   if ((x1 >= 1 & x2 >= 1) & n1>0 & n2>0) {
      cvf_diff <- (tmp2[which(tmp2$time == new_day_verum & tmp2$strata == 1),]$cum.inc - tmp2[which(tmp2$time == new_day_comp & tmp2$strata == 2),]$cum.inc)
      cvf_lower <- (cvf_diff - 1.96 * sqrt((tmp2[which(tmp2$time == new_day_verum & tmp2$strata == 1),]$std.err)**2 + (tmp2[which(tmp2$time == new_day_comp & tmp2$strata == 2),]$std.err)**2))#*100
      cvf_upper <- (cvf_diff + 1.96 * sqrt((tmp2[which(tmp2$time == new_day_verum & tmp2$strata == 1),]$std.err)**2 + (tmp2[which(tmp2$time == new_day_comp & tmp2$strata == 2),]$std.err)**2))#*100
      cvf_diff <- cvf_diff*100
      cvf_lower <- cvf_lower*100
      cvf_upper <- cvf_upper*100
    } else {
      cvf_diff <- "NA"
      cvf_lower <- "NA"
      cvf_upper <- "NA"
    }
    return(data.frame("estimate" = cvf_diff, "ci.lb" = cvf_lower, "ci.ub" = cvf_upper, "x1_cid" = x1, "x2_cid" = x2))
    }
  }
      
  #### HR ####
  if (effect == c("HR")) {
    data_used1 <- overall_summary_wide
    data_used1
    if (subgroup_used) {
      data_used1 <- subgroup_summary_wide
    }
    data_used2 <- data_used
    rd_func <- function(df) {
       if (stratification_used) {
         # Run survival analysis only in case data available for both treatment groups and at least 4 patients
         if (length(unique(df$breasy_treatment)) > 1 & (length(df$breasy_treatment))>4)
         {
         tmp <- data.frame(
           t(
            round(
              summary(
                rlang::inject(
                  survival::coxph(survival::Surv(breasy_aval, breasy_cnsr_n) ~ breasy_treatment_n + survival::strata(!!!rlang::syms(strat)), data = df)
                )
              )$conf.int["breasy_treatment_n",c("exp(coef)","lower .95","upper .95")],
              3
            )
          )
        )
         }
         else
         {
           tmp <- data.frame(t(rep(NA,3)))
         }
       #tmp <- data.frame(t(round(summary(survival::coxph(survival::Surv(!!AVAL, breasy_cnsr_n) ~ breasy_treatment_n + strat, df))$conf.int[,c("exp(coef)","lower .95","upper .95")],3)))
       colnames(tmp) <- c("estimate","ci.lb","ci.ub")
      } else {
        # Run survival analysis only in case data available for both treatment groups
        if (length(unique(df$breasy_treatment)) > 1)
          {
          tmp <- data.frame(t(round(summary(survival::coxph(survival::Surv(breasy_aval, breasy_cnsr_n) ~ breasy_treatment_n , df))$conf.int[,c("exp(coef)","lower .95","upper .95")],3)))
          }
          else
          {
          tmp <- data.frame(t(rep(NA,3)))
          }
         names(tmp) <- c("estimate","ci.lb","ci.ub")
      }
      if (is.element("Inf",tmp))
      {
        tmp <- data.frame(t(rep(NA,3)))
        names(tmp) <- c("estimate","ci.lb","ci.ub")
      }
      return(tmp)
    }
  }
    
    
  if (effect %in% c("CID", "EXCESS_CID","HR") & subgroup_used) {
    data_used1 <- subgroup_summary_wide
    rd_rma_mh_hr <- c()
    for(st in subgroup) {
      rd_rma_mh_overall <- cbind(
      data_used1 %>%
        dplyr::arrange(!!rlang::sym(param)) %>%
        dplyr::filter(SUBGROUP == st),
      data_used2 %>%
        dplyr::group_by(!!rlang::sym(param),!!rlang::sym(st)) %>%
        dplyr::group_map(~ rd_func(.)) %>%
        plyr::ldply(data.frame)
      ) %>%
        dplyr::mutate(
          NNT = dplyr::case_when(
            1/(as.numeric(estimate))*100 < 0 ~ floor(1/(as.numeric(estimate))*100),
            1/(as.numeric(estimate))*100 >= 0 ~ ceiling(1/(as.numeric(estimate))*100)
          )
        ) %>% suppressWarnings()
      rd_rma_mh_hr <- rbind(rd_rma_mh_hr, rd_rma_mh_overall)
    }
    rd_rma_mh <- rd_rma_mh_hr
  } else {
    rd_rma_mh <- cbind(
      data_used1 %>%
        dplyr::arrange(!!!rlang::syms(grouping_vars)),
      data_used2 %>%
        dplyr::group_by(!!!rlang::syms(grouping_vars)) %>%
        dplyr::group_map(~ rd_func(.)) %>%
        plyr::ldply(data.frame)
      ) %>%
        dplyr::mutate(
          NNT = dplyr::case_when(
            1/(as.numeric(estimate))*100 < 0 ~ floor(1/(as.numeric(estimate))*100),
            1/(as.numeric(estimate))*100 >= 0 ~ ceiling(1/(as.numeric(estimate))*100)
          )
        ) %>% suppressWarnings()
  }
  # perform function for overall if subgroup is used
  if (subgroup_used) {
    data_used1 <- data_used2 <- overall_summary_wide
    if (stratification_used){
      data_used2 <- strat_summary_wide
    }
    if (effect %in% c("HR","CID","EXCESS_CID")) {
      data_used2 <- data_used
    }
    
    rd_rma_mh_overall <- cbind(
      data_used1 %>%
        dplyr::arrange(!!rlang::sym(param)),
      data_used2 %>%
        dplyr::group_by(!!rlang::sym(param)) %>%
        dplyr::group_map(~ rd_func(.)) %>%
        plyr::ldply(data.frame)
    ) %>%
      dplyr::mutate(
        NNT = dplyr::case_when(
          1 / as.numeric(estimate) * 100 < 0 ~ floor(1 / (as.numeric(estimate)) * 100),
          1 / as.numeric(estimate) * 100 >= 0 ~ ceiling(1 / (as.numeric(estimate)) * 100)
        ),
        SUBGROUP = "Overall",
        SUBLEVEL = "All"
      ) %>% suppressWarnings()
    
    rd_rma_mh <- rbind(rd_rma_mh_overall,rd_rma_mh)
  }
  
  #### 4. Transform/Rename variables in desired form ####
  if (effect %in% c("IRD","EXCESS_IRD")){
    effect_name <- "Incidence Rate by 100 patient years"
    effect_var_name <- "EFFECT_IRD"
  }
  if (effect %in% c("ARD","EXCESS_ARD")){
    effect_name <- "Crude Incidence"
    effect_var_name <- "EFFECT_ARD"
  }
  if (effect == "HR"){
    effect_name <- "Hazard Ratio"
    effect_var_name <- "EFFECT_HR"
  }
  if (effect %in% c("CID") & nlevels(as.factor(data[[cnsr]])) > 2){
    effect_name <- "Cumulative Incidence based on AJ"
    effect_var_name <- "EFFECT_CID"
  }
  if (effect %in% c("EXCESS_CID") & nlevels(as.factor(data[[cnsr]])) > 2){
    effect_name <- "Excess numbers based on AJ Cumulative Incidences"
    effect_var_name <- "EFFECT_CID"
  }
    if (effect %in% c("CID") & nlevels(as.factor(data[[cnsr]])) == 2){
    effect_name <- "Cumulative Incidence based on KM"
    effect_var_name <- "EFFECT_CID"
  }
  if (effect %in% c("EXCESS_CID") & nlevels(as.factor(data[[cnsr]])) == 2){
    effect_name <- "Excess numbers based on KM Cumulative Incidences"
    effect_var_name <- "EFFECT_CID"
  }
  
  
  if (!subgroup_used) {
    rd_rma_mh <- rd_rma_mh %>% 
      dplyr::mutate(
        SUBGROUP = "Overall",
        SUBLEVEL = "All"
      )
  }
   if (!stratification_used) {
    rd_rma_mh <- rd_rma_mh %>% 
      dplyr::mutate(
        STRATUM = "None"
      )
   } else {
    rd_rma_mh <- rd_rma_mh %>% 
      dplyr::mutate(
        STRATUM = paste(strat,collapse = "/")
      )
   }
    
  if (effect %in% c("CID","EXCESS_CID")) {
    rd_rma_mh <- rd_rma_mh %>% 
      dplyr::rename(
        OUTCOME = !!rlang::sym(param),
        NUMBER_EVENTS_VERUM = x1_cid,
        NUMBER_PATIENTS_VERUM = n_1,
        NUMBER_EVENTS_COMP = x2_cid,
        NUMBER_PATIENTS_COMP = n_2
      ) %>% 
      dplyr::mutate(DAY = day)
  } else {
    rd_rma_mh <- rd_rma_mh %>% 
      dplyr::rename(
        OUTCOME = !!rlang::sym(param),
        NUMBER_EVENTS_VERUM = x_1,
        NUMBER_PATIENTS_VERUM = n_1,
        NUMBER_EVENTS_COMP = x_2,
        NUMBER_PATIENTS_COMP = n_2
      ) 
  }
  rd_rma_mh <- rd_rma_mh %>% 
    dplyr::mutate(
      !!rlang::sym(effect_var_name) := round(as.numeric(estimate),3),
      LOWER95 = round(as.numeric(ci.lb),3),
      UPPER95 = round(as.numeric(ci.ub),3),
      TRIALNO = trialno,
      ESTIMATE = effect_name, 
      ANALYSIS_SET = population,
      DATA_SCOPE = paste(datascope, collapse = "/")
    ) %>% suppressWarnings()
  
    if(effect %in% c("CID","EXCESS_CID")){
    rd_rma_mh <- rd_rma_mh %>%  
    dplyr::select(
      TRIALNO, ESTIMATE, DAY, ANALYSIS_SET,
      OUTCOME, DATA_SCOPE, 
      SUBGROUP, SUBLEVEL,
      STRATUM,
      NUMBER_EVENTS_VERUM, NUMBER_PATIENTS_VERUM,
      NUMBER_EVENTS_COMP,NUMBER_PATIENTS_COMP,
      !!rlang::sym(effect_var_name),
      LOWER95, UPPER95, NNT
    ) %>% suppressWarnings()
    } else {
      rd_rma_mh <- rd_rma_mh %>%  
    dplyr::select(
      TRIALNO, ESTIMATE, ANALYSIS_SET,
      OUTCOME, DATA_SCOPE, 
      SUBGROUP, SUBLEVEL,
      STRATUM,
      NUMBER_EVENTS_VERUM, NUMBER_PATIENTS_VERUM,
      NUMBER_EVENTS_COMP,NUMBER_PATIENTS_COMP,
      !!rlang::sym(effect_var_name),
      LOWER95, UPPER95, NNT
    ) %>% suppressWarnings()
  }
  
 if (startsWith(effect, "EXCESS_")) {
   rd_rma_mh <- rd_rma_mh %>% 
     dplyr::mutate(
       !!rlang::sym(effect_var_name) := !!rlang::sym(effect_var_name)*100,
       LOWER95 = LOWER95 * 100,
       UPPER95 = UPPER95 * 100
     )
 }
  
  if (effect == "HR") {
    rd_rma_mh <- rd_rma_mh %>% 
      dplyr::select(-NNT)
  }
  
  return(rd_rma_mh)
}
