#' file creation function
#' 
#' @param data
#' @param effect
#' @param day
#' @param param
#' @param outcome
#' @param scope
#' @param datascope
#' @param population
#' @param treatment
#' @param verum
#' @param comparator
#' @param cnsr
#' @param event
#' @param strat
#' @param subgroup
#' @param aval
#' 

effect_calc <- function(
    data = data, 
    effect = effect,
    day = day,
    param = param, 
    outcome = outcome,
    scope = scope, 
    datascope = datascope, 
    population = population, 
    treatment = treatment, 
    verum = verum, 
    comparator = comparator, 
    cnsr = cnsr, 
    event = event, 
    strat = strat, 
    subgroup = subgroup,
    aval = aval
) {
 
 result <- total <- total_final <- total_end <- total_res <-  c()  
 result_test_1 <- vector(mode = "list")
 '%notin%' <- Negate('%in%')
 
 
 if(nlevels(as.factor(data$STUDYID)) > 1) {
   trials <- paste(levels(as.factor(data$STUDYID)), collapse = "/")
   trialno <- paste("Pool of",trials)
 } else {
   trialno <- paste(levels(as.factor(data$STUDYID)), collapse = "/")
 }
 #### create output for subgroup = Overall #####
  
 for (j in 1:length(outcome)) { 
   
  if (datascope != "No selection") {
    if(is.numeric(data[[population]])) {
      data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "1"),]
    } else {
    data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "Y"),]
    } 
    } else {
      if(is.numeric(data[[population]])) {
      data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "1"),]
    } else {
    data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "Y"),]
    }
    }

  if("Overall" %notin% strat) { 
    
    numlevels <- alllevels <- allnames <- strat_names <- c()
    
    adtte_trt <- data_test[(data_test[[treatment]] %in% verum),]
    adtte_com <- data_test[(data_test[[treatment]] %in% comparator),]
    x1_all <- length(which(adtte_trt[[cnsr]] %in% event))
    x2_all <- length(which(adtte_com[[cnsr]] %in% event))
    n1_all <- nrow(adtte_trt)
    n2_all <- nrow(adtte_com)

    for (g in 1:length(strat)) {
      strat_factor <- factor(data[strat][,g])
      alllevels[[g]] <- levels(strat_factor)
      numlevels[g] <- nlevels(strat_factor)
      for (i in 1:nlevels(strat_factor)) {
        allnames[[i]] <- strat[[g]]
      }
      strat_names[[g]] <- allnames
      }
      
    total_levels <- sum(numlevels)
    name_levels <- unlist(alllevels)
    name_strat <- unlist(strat_names)
    
    pre_adtte <- pre_adtte_trt <- pre_adtte_com <- vector(mode = "list")
    adtte <- adtte_trt <- adtte_com <- c()
    
    pre_x1 <- x1 <- pre_x2 <- x2 <- pre_t1 <- t1 <- pre_t2 <- t2 <- pre_n1 <- n1 <- pre_n2 <- n2 <- pre_a1 <- a1 <- pre_b1 <- b1 <- pre_c1 <- c1 <- pre_d1 <- d1 <- c()
    
    for (g in strat){
      pre_adtte <- pre_adtte_trt <- pre_adtte_com <- vector(mode = "list")
      pre_x1 <- pre_x2 <- pre_t1 <- pre_t2 <- pre_n1 <- pre_n2 <- pre_a1 <- pre_b1 <- pre_c1 <- pre_d1 <- c()
      
      for (i in 1:nlevels(factor(data[strat][[g]]))) {
        pre_adtte[[i]] <- data_test[(data_test[strat][[g]]==levels(factor(data_test[strat][[g]]))[i]),]
        pre_adtte_trt[[i]] <- pre_adtte[[i]][(pre_adtte[[i]][[treatment]] %in% verum),]
        pre_adtte_com[[i]] <- pre_adtte[[i]][(pre_adtte[[i]][[treatment]] %in% comparator),]
        
        pre_x1[[i]] <- length(which(pre_adtte_trt[[i]][[cnsr]] %in% event))
        pre_x2[[i]] <- length(which(pre_adtte_com[[i]][[cnsr]] %in% event))
        pre_t1[[i]] <- sum(pre_adtte_trt[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
        pre_t2[[i]] <- sum(pre_adtte_com[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
        pre_n1[[i]] <- nrow(pre_adtte_trt[[i]])
        pre_n2[[i]] <- nrow(pre_adtte_com[[i]])
        pre_a1[[i]] <- length(which(pre_adtte_trt[[i]][[cnsr]] %in% event))
        pre_b1[[i]] <- length(which(pre_adtte_trt[[i]][[cnsr]] %notin% event))
        pre_c1[[i]] <- length(which(pre_adtte_com[[i]][[cnsr]] %in% event))
        pre_d1[[i]] <- length(which(pre_adtte_com[[i]][[cnsr]] %notin% event))
      }
      adtte <- c(adtte, pre_adtte)
      adtte_trt <- c(adtte_trt, pre_adtte_trt)
      adtte_com <- c(adtte_com, pre_adtte_com)
      x1 <- c(x1, pre_x1)
      x2 <- c(x2, pre_x2)
      t1 <- c(t1, pre_t1)
      t2 <- c(t2, pre_t2)
      n1 <- c(n1, pre_n1)
      n2 <- c(n2, pre_n2)
      a1 <- c(a1, pre_a1)
      b1 <- c(b1, pre_b1)
      c1 <- c(c1, pre_c1)
      d1 <- c(d1, pre_d1)
    }
    
    x1 <- unlist(x1)
    x2 <- unlist(x2)
    t1 <- unlist(t1)
    t2 <- unlist(t2)
    n1 <- unlist(n1)
    n2 <- unlist(n2)
    a1 <- unlist(a1)
    b1 <- unlist(b1)
    c1 <- unlist(c1)
    d1 <- unlist(d1)
    res_extract.effect.ci.single <- c()
    res_extract.effect.ci <- c()
   
    
   if (effect %in% c("IRD", "EXCESS_IRD"))  {
     
    data_meta <- data.frame(x1,x2,t1,t2) 
    data_meta <- subset(data_meta, t1>0 & t2>0)
    
    if (nrow(data_meta)>0){
    res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95, data=data_meta)
    res_extract.effect.ci.meta <- res %>%
      confint() %>%
      .$fixed
    } else if (nrow(data_meta)==0){
      res_extract.effect.ci.meta <- c("NA","NA","NA")
    }
   } else if (effect %in% c("ARD", "EXCESS_ARD")) {
      
    data_meta <- data.frame(a1,b1,c1,d1)
    
    res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95, data=data_meta)
    res_extract.effect.ci.meta <- res %>%
      confint() %>%
      .$fixed
    }
    
    if (effect %in% c("HR"))  {
      
      data_meta <- data.frame(a1,b1,c1,d1)
      
      data_test_hr <- data_test[(data_test[[treatment]] %in% c(verum,comparator)),]
      data_test_hr[[treatment]] <- ifelse(data_test_hr[[treatment]] == verum , 1 , 0)
      data_test_hr[[cnsr]] <- ifelse(data_test_hr[[cnsr]] != event , 0 , 1)
      
      a <- data_test_hr$AVAL
      b <- data_test_hr[[cnsr]]
      c <- data_test_hr[[treatment]]
      d <- data_test_hr[strat]

      cox_hr <- survival::coxph(survival::Surv(a, b) ~ c + strata(d) , data_test_hr) 
      res_extract.effect.ci.meta <- summary(cox_hr)$conf.int[,c("exp(coef)","lower .95","upper .95")]
    }
    
    if (effect %notin% c("HR"))  {
    res_extract.effect.ci <- rbind(res_extract.effect.ci.meta)
    res_extract.effect.ci <- as.data.frame(res_extract.effect.ci)
    } else {
      res_extract.effect.ci <- rbind(res_extract.effect.ci.meta)
    }
    if (effect == "HR" & is.na(res_extract.effect.ci[1]) != TRUE) { 
    res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
    
    nnt <- 1/(res_extract.effect.ci[1])*100
    nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
    
    excess <- round((res_extract.effect.ci[1])*100)
    excess_lower <- round((res_extract.effect.ci[2])*100)
    excess_upper <- round((res_extract.effect.ci[3])*100)
    } else if (effect == "HR" & is.na(res_extract.effect.ci[1]) == TRUE) {
      
      res_extract.effect.ci.rounded <- res_extract.effect.ci
      nnt <- "NA"
      
      excess <- "NA"
      excess_lower <- "NA"
      excess_upper <- "NA"
    }
    
    if (effect %notin% c("HR") & res_extract.effect.ci[1] != "NA") { 
      res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
      
      nnt <- 1/(res_extract.effect.ci[1])*100
      nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
      
      excess <- round((res_extract.effect.ci[1])*100)
      excess_lower <- round((res_extract.effect.ci[2])*100)
      excess_upper <- round((res_extract.effect.ci[3])*100)
    } else if (effect %notin% c("HR") & res_extract.effect.ci[1] == "NA") {
      
      res_extract.effect.ci.rounded <- res_extract.effect.ci
      nnt <- "NA"
      
      excess <- "NA"
      excess_lower <- "NA"
      excess_upper <- "NA"
    }

    stratum_level <- "All"
    stratum_name <- paste(strat, collapse = "/")
    x_1 <- sum(x1)
    x_2 <- sum(x2)
    n_1 <- sum(n1)
    n_2 <- sum(n2)
    
    if (effect %in% c("ARD", "IRD")) {
    result_test <- c(res_extract.effect.ci.rounded, nnt)
    names(result_test) <- c("Esimate","lower","upper","nnt")
    result_test <- as.data.frame(result_test)
    result_test <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level, events_verum = x1_all, patients_verum = n1_all, events_comp = x2_all, patients_comp = n2_all,  result_test)
    }
    
    if (effect %in% c("EXCESS_ARD", "EXCESS_IRD")) {
    result_test_excess <- c(excess, excess_lower, excess_upper, nnt)
    names(result_test_excess) <- c("Excess","Excess_lower","Excess_upper","nnt")
    result_test_excess <- as.data.frame(result_test_excess)
    result_test_excess <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level,events_verum = x1_all, patients_verum = n1_all, events_comp = x2_all, patients_comp = n2_all,  result_test_excess)
    }
    
    if (effect == c("HR")) {
    result_test_hr <- res_extract.effect.ci.rounded
    names(result_test_hr) <- c("Esimate","lower","upper")
    result_test_hr <- as.data.frame(result_test_hr)
    result_test_hr <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level, events_verum = x1_all, patients_verum = n1_all, events_comp = x2_all, patients_comp = n2_all,  result_test_hr)
    }
    
    if (effect == "IRD") {
    result_test_1[[j]] <- c(trialno,"Incidence Rate",population,outcome[[j]],datascope,"Overall","All",result_test)
    names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
    } 
    if (effect == "ARD") {
      result_test_1[[j]] <- c(trialno,"Crude Incidence",population,outcome[[j]],datascope,"Overall","All",result_test)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
    }
    if (effect == "EXCESS_IRD") {
      result_test_1[[j]] <- c(trialno,"Incidence Rate",population,outcome[[j]],datascope,"Overall","All",result_test_excess)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
    }
    if (effect == "EXCESS_ARD") {
      result_test_1[[j]] <- c(trialno,"Crude Incidence",population,outcome[[j]],datascope,"Overall","All",result_test_excess)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
    }
    if (effect == "HR") {
      result_test_1[[j]] <- c(trialno,"Hazard Ratio",population,outcome[[j]],datascope,"Overall","All",result_test_hr)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_HR","LOWER95","UPPER95")
    }
  }
   
  else if ("Overall" %in% strat) {
    
    adtte_trt <- data_test[(data_test[[treatment]] %in% verum),]
    adtte_com <- data_test[(data_test[[treatment]] %in% comparator),]
    x1 <- length(which(adtte_trt[[cnsr]] %in% event))
    x2 <- length(which(adtte_com[[cnsr]] %in% event))
    t1 <- sum(adtte_trt$AVAL, na.rm=TRUE)/(100*365.25)
    t2 <- sum(adtte_com$AVAL, na.rm=TRUE)/(100*365.25)
    n1 <- nrow(adtte_trt)
    n2 <- nrow(adtte_com)
    a1 <- length(which(adtte_trt[[cnsr]] %in% event))
    b1 <- length(which(adtte_trt[[cnsr]] %notin% event))
    c1 <- length(which(adtte_com[[cnsr]] %in% event))
    d1 <- length(which(adtte_com[[cnsr]] %notin% event))
    
    
  if (effect %in% c("IRD", "EXCESS_IRD"))  {
    
    if (t1>0 & t2>0) {
    res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95)
    res_extract.effect.ci <- res %>%
      confint() %>%
      .$fixed
    } else {
      res_extract.effect.ci <- c("NA","NA","NA")
    }
  }
    
  if (effect %in% c("ARD", "EXCESS_ARD"))  {
    
    res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95)
    res_extract.effect.ci <- res %>%
      confint() %>%
      .$fixed
  }
    
    if (effect %in% c("CID" , "EXCESS_CID"))  {
      
      if(datascope != "No selection") {
        if(is.numeric(data[[population]])) {
          data_decision <- data[(data[[scope]] %in% datascope) & (data[[population]] %in% "1"),]
        } else {
          data_decision <- data[(data[[scope]] %in% datascope) & (data[[population]] %in% "Y"),]
        } 
      } else {
        if(is.numeric(data[[population]])) {
          data_decision <- data[(data[[population]] %in% "1"),]
        } else {
          data_decision <- data[(data[[population]] %in% "Y"),]
        }
      }
      
      
    data_adtte_cid <- data_test[(data_test[[treatment]] %in% c(verum,comparator)),]
    data_adtte_cid[[treatment]] <- ifelse(data_adtte_cid[[treatment]] == verum , 1 , 2)
    data_adtte_cid$CNSR_1 <- as.factor(data_adtte_cid[[cnsr]])
    
    
    
    if (nlevels(as.factor(data_decision[[cnsr]])) > 2){
      
    if (event == 0){
    levels(data_adtte_cid$CNSR_1) <- c("2","1","3")
    }
    else if (event == 1){
      levels(data_adtte_cid$CNSR_1) <- c("1","2","3")
    }
    else if (event == 2){
      levels(data_adtte_cid$CNSR_1) <- c("1","3","2")
    }
    
    a <- data_adtte_cid$AVAL
    b <- factor(data_adtte_cid$CNSR_1, levels = c("1", "2", "3"))
    c <- data_adtte_cid[[treatment]]
    
    if (event %notin% levels(as.factor(data_adtte_cid[[cnsr]]))) {
      x1 <- length(which(data_adtte_cid[[cnsr]] == 2 & data_adtte_cid[[treatment]] %in% "1"))
      x2 <- length(which(data_adtte_cid[[cnsr]] == 2 & data_adtte_cid[[treatment]] %in% "2"))
      n1 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "1",])
      n2 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "2",]) 
      cvf_diff <- "NA"
      cvf_lower <- "NA"
      cvf_upper <- "NA"
    } else {
      cum <- survival::survfit(survival::Surv(a, b) ~ c, data = data_adtte_cid) 
      cum_sum <- summary(cum)
      
      cum_data <- as.data.frame(cbind(cum_sum$strata,cum_sum$time,cum_sum$n.event[,2],cum_sum$pstate[,2],cum_sum$std.err[,2]))
      names(cum_data) <- c("strata","time","n.event","cum.inc","std.err")
    
      
      if (day %in% cum_data[which(cum_data$strata == 1),]$time) {
        new_day_verum=day
      } else if (day %notin% cum_data[which(cum_data$strata == 1),]$time) {
        new_time_verum <- c(day,cum_data[which(cum_data$strata == 1),]$time)
        new_time_sort_verum <- sort(new_time_verum)
        pos_verum <- match(day,new_time_sort_verum)
        new_day_verum <- new_time_sort_verum[pos_verum - 1]
      }
      if (day %in% cum_data[which(cum_data$strata == 2),]$time) {
        new_day_comp=day
      } else if (day %notin% cum_data[which(cum_data$strata == 2),]$time) {
        new_time_comp <- c(day,cum_data[which(cum_data$strata == 2),]$time)
        new_time_sort_comp <- sort(new_time_comp)
        pos_comp <- match(day,new_time_sort_comp)
        new_day_comp <- new_time_sort_comp[pos_comp - 1]
      }
      
      n1 <- cum_sum$n[1]
      x1 <- sum(cum_data[which(cum_data$time <= new_day_verum & cum_data$strata == 1),]$n.event)
      n2 <- cum_sum$n[2]
      x2 <- sum(cum_data[which(cum_data$time <= new_day_comp & cum_data$strata == 2),]$n.event)
      
      if(is.na(n1))  {n1 <- 0} else {n1 <- n1}
      if(is.na(n2))  {n2 <- 0} else {n2 <- n2}

      if (x1 >= 1 & x2 >= 1) {
        cvf_diff <- cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$cum.inc - cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$cum.inc
        cvf_lower <- cvf_diff - 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2) 
        cvf_upper <- cvf_diff + 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2)
        cvf_diff <- cvf_diff*100
        cvf_lower <- cvf_lower*100
        cvf_upper <- cvf_upper*100
      } else {
        cvf_diff <- "NA"
        cvf_lower <- "NA"
        cvf_upper <- "NA"
      }
    }
    }
    
    
    if (nlevels(as.factor(data_decision[[cnsr]])) == 2){
      
      data_adtte_cid$CNSR_2 <- ifelse(data_adtte_cid$CNSR_1 != event , 0 , 1)
      
      a <- data_adtte_cid$AVAL
      b <- factor(data_adtte_cid$CNSR_2)
      c <- data_adtte_cid[[treatment]]
      
      if (event %notin% levels(as.factor(data_adtte_cid[[cnsr]]))) {
        x1 <- length(which(data_adtte_cid[[cnsr]] %notin% event & data_adtte_cid[[treatment]] %in% "1"))
        x2 <- length(which(data_adtte_cid[[cnsr]] %notin% event & data_adtte_cid[[treatment]] %in% "2"))
        n1 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "1",])
        n2 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "2",]) 
        cvf_diff <- "NA"
        cvf_lower <- "NA"
        cvf_upper <- "NA"
      } else {
        cum <- survival::survfit(survival::Surv(a, b) ~ c, data = data_adtte_cid) 
        cum_sum <- summary(cum)
        
        cum_data <- as.data.frame(cbind(cum_sum$strata,cum_sum$time,cum_sum$n.event[,2],cum_sum$pstate[,2],cum_sum$std.err[,2]))
        names(cum_data) <- c("strata","time","n.event","cum.inc","std.err")
        
        
        if (day %in% cum_data[which(cum_data$strata == 1),]$time) {
          new_day_verum=day
        } else if (day %notin% cum_data[which(cum_data$strata == 1),]$time) {
          new_time_verum <- c(day,cum_data[which(cum_data$strata == 1),]$time)
          new_time_sort_verum <- sort(new_time_verum)
          pos_verum <- match(day,new_time_sort_verum)
          new_day_verum <- new_time_sort_verum[pos_verum - 1]
        }
        if (day %in% cum_data[which(cum_data$strata == 2),]$time) {
          new_day_comp=day
        } else if (day %notin% cum_data[which(cum_data$strata == 2),]$time) {
          new_time_comp <- c(day,cum_data[which(cum_data$strata == 2),]$time)
          new_time_sort_comp <- sort(new_time_comp)
          pos_comp <- match(day,new_time_sort_comp)
          new_day_comp <- new_time_sort_comp[pos_comp - 1]
        }
        
        n1 <- cum_sum$n[1]
        x1 <- sum(cum_data[which(cum_data$time <= new_day_verum & cum_data$strata == 1),]$n.event)
        n2 <- cum_sum$n[2]
        x2 <- sum(cum_data[which(cum_data$time <= new_day_comp & cum_data$strata == 2),]$n.event)
        
        if(is.na(n1))  {n1 <- 0} else {n1 <- n1}
        if(is.na(n2))  {n2 <- 0} else {n2 <- n2}
        
        if (x1 >= 1 & x2 >= 1) {
          cvf_diff <- cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$cum.inc - cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$cum.inc
          cvf_lower <- cvf_diff - 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2) 
          cvf_upper <- cvf_diff + 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2)
          cvf_diff <- cvf_diff*100
          cvf_lower <- cvf_lower*100
          cvf_upper <- cvf_upper*100
        } else {
          cvf_diff <- "NA"
          cvf_lower <- "NA"
          cvf_upper <- "NA"
        }
      }
    }
      res_extract.effect.ci <- c(cvf_diff, cvf_lower, cvf_upper)
    }
    
    
    if (effect == "HR")  {
      
      data_test_hr <- data_test[(data_test[[treatment]] %in% c(verum,comparator)),]
      data_test_hr[[treatment]] <- ifelse(data_test_hr[[treatment]] == verum , 1 , 0)
      data_test_hr[[cnsr]] <- ifelse(data_test_hr[[cnsr]] != event , 0 , 1)
      
      a <- data_test_hr$AVAL
      b <- data_test_hr[[cnsr]]
      c <- data_test_hr[[treatment]]
      
      cox_hr <- survival::coxph(survival::Surv(a, b) ~ c , data_test_hr) 
      res_extract.effect.ci <- summary(cox_hr)$conf.int[,c("exp(coef)","lower .95","upper .95")]
      res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
    }
    
     if (effect %in% c("HR") & is.na(res_extract.effect.ci[1]) != TRUE)  {
      
      res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
      
      nnt <- 1/(res_extract.effect.ci[1])*100
      nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
      
      excess <- round((res_extract.effect.ci[1])*100)
      excess_lower <- round((res_extract.effect.ci[2])*100)
      excess_upper <- round((res_extract.effect.ci[3])*100)
     } else if (effect == "HR" & is.na(res_extract.effect.ci[1]) == TRUE)  {
      
      res_extract.effect.ci.rounded <- res_extract.effect.ci
      
      nnt <- "NA"
      
      excess <- "NA"
      excess_lower <- "NA"
      excess_upper <- "NA"
    } 
    
    if (effect %notin% c("HR") & res_extract.effect.ci[1] != "NA")  {
      
      res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
      
      nnt <- 1/(res_extract.effect.ci[1])*100
      nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
      
      excess <- round((res_extract.effect.ci[1])*100)
      excess_lower <- round((res_extract.effect.ci[2])*100)
      excess_upper <- round((res_extract.effect.ci[3])*100)
    } else if (effect %notin% c("HR") & res_extract.effect.ci[1] == "NA")  {
      
      res_extract.effect.ci.rounded <- res_extract.effect.ci
      
      nnt <- "NA"
      
      excess <- "NA"
      excess_lower <- "NA"
      excess_upper <- "NA"
    } 
  
    if (effect == "IRD") {
    result_test_1[[j]] <- c(trialno, "Incidence Rate",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
    names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
    }
    if (effect == "ARD") {
      result_test_1[[j]] <- c(trialno, "Crude Incidence",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
    }
    if (effect == "EXCESS_IRD") {
      result_test_1[[j]] <- c(trialno, "Incidence Rate",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
    }
    if (effect == "EXCESS_ARD") {
      result_test_1[[j]] <- c(trialno, "Crude Incidence",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
      names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
    }
   if (effect == "HR") {
     result_test_1[[j]] <- c(trialno, "Hazard Ratio",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded)
     names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_HR","LOWER95","UPPER95")
   }
    if (effect == "CID") {
      if (nlevels(as.factor(data_decision[[cnsr]])) > 2) {
        result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on AJ",day,population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
        names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_CID","LOWER95","UPPER95","NNT")
      }
    }
    if (effect == "EXCESS_CID") {
      if (nlevels(as.factor(data_decision[[cnsr]])) > 2) {
        result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on AJ",day,population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
        names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
      }
    }
    if (effect == "CID") {
      if (nlevels(as.factor(data_decision[[cnsr]])) == 2) {
        result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on KM",day,population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
        names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_CID","LOWER95","UPPER95","NNT")
      }
    }
    if (effect == "EXCESS_CID") {
      if (nlevels(as.factor(data_decision[[cnsr]])) == 2) {
        result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on KM",day,population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
        names(result_test_1[[j]]) <- c("TRIALNO","ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
      }
    }
  }
}
 
   if("Overall" %notin% strat) {
     for(k in 1:length(outcome)) {total <- rbind(total,as.data.frame(result_test_1[[k]]))}
     total_res <- as.data.frame(total)
   }
   else if("Overall" %in% strat){
     for(k in 1:length(outcome)) {total <- cbind(total,result_test_1[[k]])}
     total_res <- as.data.frame(t(total))
   }
 
   
if ("Overall" %notin% subgroup) {


     for (q in 1:length(subgroup)) {

       subgroup_factor <- factor(data[[subgroup[[q]]]])
       subgroup_level <- levels(subgroup_factor)[1:nlevels(subgroup_factor)]


       for (l in 1:nlevels(subgroup_factor)) {

         for (j in 1:length(outcome)) {

           if(datascope != "No selection") {
             if (is.numeric(data[[population]])) {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "1") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             } else {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "Y") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
           } else {
             if (is.numeric(data[[population]])) {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "1") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             } else {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "Y") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
           }

           if("Overall" %notin% strat) {

             numlevels <- alllevels <- allnames <- strat_names <- c()
             
             adtte_trt <- data_test[(data_test[[treatment]] %in% verum),]
             adtte_com <- data_test[(data_test[[treatment]] %in% comparator),]
             x1_all <- length(which(adtte_trt[[cnsr]] %in% event))
             x2_all <- length(which(adtte_com[[cnsr]] %in% event))
             n1_all <- nrow(adtte_trt)
             n2_all <- nrow(adtte_com)

             for (g in 1:length(strat)) {
               strat_factor <- factor(data[strat][,g])
               alllevels[[g]] <- levels(strat_factor)
               numlevels[g] <- nlevels(strat_factor)
               for (i in 1:nlevels(strat_factor)) {
                 allnames[[i]] <- strat[[g]]
               }
               strat_names[[g]] <- allnames
             }
             
             total_levels <- sum(numlevels)
             name_levels <- unlist(alllevels)
             name_strat <- unlist(strat_names)
             

             pre_adtte <- pre_adtte_trt <- pre_adtte_com <- vector(mode = "list")
             adtte <- adtte_trt <- adtte_com <- c()
             pre_x1 <- x1 <- pre_x2 <- x2 <- pre_t1 <- t1 <- pre_t2 <- t2 <- pre_n1 <- n1 <- pre_n2 <- n2 <- pre_a1 <- a1 <- pre_b1 <- b1 <- pre_c1 <- c1 <- pre_d1 <- d1 <- c()

             for (g in strat){
               pre_adtte <- pre_adtte_trt <- pre_adtte_com <- vector(mode = "list")
               pre_x1 <- pre_x2 <- pre_t1 <- pre_t2 <- pre_n1 <- pre_n2 <- pre_a1 <- pre_b1 <- pre_c1 <- pre_d1 <- c()
                
               for (i in 1:nlevels(factor(data[strat][[g]]))) {
                 pre_adtte[[i]] <- data_test[(data_test[strat][[g]]==levels(factor(data_test[strat][[g]]))[i]),]
                 pre_adtte_trt[[i]] <- pre_adtte[[i]][(pre_adtte[[i]][[treatment]] %in% verum),]
                 pre_adtte_com[[i]] <- pre_adtte[[i]][(pre_adtte[[i]][[treatment]] %in% comparator),]
        
                 pre_x1[[i]] <- length(which(pre_adtte_trt[[i]][[cnsr]] %in% event))
                 pre_x2[[i]] <- length(which(pre_adtte_com[[i]][[cnsr]] %in% event))
                 pre_t1[[i]] <- sum(pre_adtte_trt[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
                 pre_t2[[i]] <- sum(pre_adtte_com[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
                 pre_n1[[i]] <- nrow(pre_adtte_trt[[i]])
                 pre_n2[[i]] <- nrow(pre_adtte_com[[i]])
                 pre_a1[[i]] <- length(which(pre_adtte_trt[[i]][[cnsr]] %in% event))
                 pre_b1[[i]] <- length(which(pre_adtte_trt[[i]][[cnsr]] %notin% event))
                 pre_c1[[i]] <- length(which(pre_adtte_com[[i]][[cnsr]] %in% event))
                 pre_d1[[i]] <- length(which(pre_adtte_com[[i]][[cnsr]] %notin% event))
               }
               adtte <- c(adtte, pre_adtte)
               adtte_trt <- c(adtte_trt, pre_adtte_trt)
               adtte_com <- c(adtte_com, pre_adtte_com)
               x1 <- c(x1, pre_x1)
               x2 <- c(x2, pre_x2)
               t1 <- c(t1, pre_t1)
               t2 <- c(t2, pre_t2)
               n1 <- c(n1, pre_n1)
               n2 <- c(n2, pre_n2)
               a1 <- c(a1, pre_a1)
               b1 <- c(b1, pre_b1)
               c1 <- c(c1, pre_c1)
               d1 <- c(d1, pre_d1)
             }

             x1 <- unlist(x1)
             x2 <- unlist(x2)
             t1 <- unlist(t1)
             t2 <- unlist(t2)
             n1 <- unlist(n1)
             n2 <- unlist(n2)
             a1 <- unlist(a1)
             b1 <- unlist(b1)
             c1 <- unlist(c1)
             d1 <- unlist(d1)
             res_extract.effect.ci.single <- c()
             res_extract.effect.ci <- c()
             
            if (effect %in% c("IRD", "EXCESS_IRD"))  {
               
             data_meta <- data.frame(x1,x2,t1,t2)
             data_meta <- subset(data_meta, t1>0 & t2>0)
             
             if (nrow(data_meta)>0){
             res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95, data=data_meta)
             res_extract.effect.ci.meta <- res %>%
               confint() %>%
               .$fixed
             } else {
               res_extract.effect.ci.meta <- c("NA","NA","NA")
             }
             }
             
             else if (effect %in% c("ARD", "EXCESS_ARD"))  {
               
             data_meta <- data.frame(a1,b1,c1,d1)
               
               
             res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95, data=data_meta)
             res_extract.effect.ci.meta <- res %>%
               confint() %>%
               .$fixed
            }
             
             
             if (effect %in% c("HR"))  {
               
               data_meta <- data.frame(a1,b1,c1,d1)
               
               data_test_hr <- data_test[(data_test[[treatment]] %in% c(verum,comparator)),]
               data_test_hr[[treatment]] <- ifelse(data_test_hr[[treatment]] == verum , 1 , 0)
               data_test_hr[[cnsr]] <- ifelse(data_test_hr[[cnsr]] != event , 0 , 1)
               
               a <- data_test_hr$AVAL
               b <- data_test_hr[[cnsr]]
               c <- data_test_hr[[treatment]]
               d <- data_test_hr[strat]
               
               cox_hr <- survival::coxph(survival::Surv(a, b) ~ c + strata(d) , data_test_hr) 
               res_extract.effect.ci.meta <- summary(cox_hr)$conf.int[,c("exp(coef)","lower .95","upper .95")]
             }
             
             if (effect %notin% c("HR"))  {
               #res_extract.effect.ci <- rbind(as.data.frame(res_extract.effect.ci.meta))
               res_extract.effect.ci <- rbind(res_extract.effect.ci.meta)
               res_extract.effect.ci <- as.data.frame(res_extract.effect.ci)
             } else {
               res_extract.effect.ci <- rbind(res_extract.effect.ci.meta)
             }
             
             if (effect == "HR" &  is.na(res_extract.effect.ci[1]) != TRUE) {
             res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
             
             nnt <- 1/(res_extract.effect.ci[1])*100
             nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
             
             excess <- round((res_extract.effect.ci[1])*100)
             excess_lower <- round((res_extract.effect.ci[2])*100)
             excess_upper <- round((res_extract.effect.ci[3])*100)
             } else if (effect == "HR" & is.na(res_extract.effect.ci[1]) == TRUE) {
               
               res_extract.effect.ci.rounded <- res_extract.effect.ci
               nnt <- "NA"
               
               excess <- "NA"
               excess_lower <- "NA"
               excess_upper <- "NA"
             }
             
             if (effect %notin% c("HR") &  res_extract.effect.ci[1] != "NA") {
               res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
               
               nnt <- 1/(res_extract.effect.ci[1])*100
               nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
               
               excess <- round((res_extract.effect.ci[1])*100)
               excess_lower <- round((res_extract.effect.ci[2])*100)
               excess_upper <- round((res_extract.effect.ci[3])*100)
             } else if (effect %notin% c("HR") & res_extract.effect.ci[1] == "NA") {
               
               res_extract.effect.ci.rounded <- res_extract.effect.ci
               #res_extract.effect.ci.rounded <- c("NA","NA","NA")
               nnt <- "NA"
               
               excess <- "NA"
               excess_lower <- "NA"
               excess_upper <- "NA"
             }

             stratum_level <- "All"
             stratum_name <- paste(strat, collapse = "/")
             x_1 <- sum(x1)
             x_2 <- sum(x2)
             n_1 <- sum(n1)
             n_2 <- sum(n2)
             a_1 <- sum(a1)
             b_1 <- sum(b1)
             c_1 <- sum(c1)
             d_1 <- sum(d1)
             

             if (effect %in% c("ARD", "IRD")) {
             result_test <- c(res_extract.effect.ci.rounded, nnt)
             names(result_test) <- c("Esimate","lower","upper","nnt")
             result_test <- as.data.frame(result_test)
             result_test <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level,events_verum = x1_all, patients_verum = n1_all, events_comp = x2_all, patients_comp = n2_all,  result_test)
             }
             
             if (effect %in% c("EXCESS_ARD", "EXCESS_IRD")) {
             result_test_excess <- c(excess, excess_lower, excess_upper, nnt)
             names(result_test_excess) <- c("Excess","Excess_lower","Excess_upper","nnt")
             result_test_excess <- as.data.frame(result_test_excess)
             result_test_excess <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level,events_verum = x1_all, patients_verum = n1_all, events_comp = x2_all, patients_comp = n2_all,  result_test_excess)
             }
             
             if (effect %in% c("HR")) {
               result_test_hr <- res_extract.effect.ci.rounded
               names(result_test_hr) <- c("Esimate","lower","upper")
               result_test_hr <- as.data.frame(result_test_hr)
               result_test_hr <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level, events_verum = x1_all, patients_verum = n1_all, events_comp = x2_all, patients_comp = n2_all,  result_test_hr)
             }
             
             
            
             if (effect == "IRD") {
               result_test_1[[j]] <- c(trialno, "Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
             }
             if (effect == "ARD") {
               result_test_1[[j]] <- c(trialno, "Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_IRD") {
               result_test_1[[j]] <- c(trialno, "Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test_excess)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_ARD") {
               result_test_1[[j]] <- c(trialno, "Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test_excess)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             if (effect == "HR") {
               result_test_1[[j]] <- c(trialno, "Hazard Ratio",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test_hr)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_HR","LOWER95","UPPER95")
             }
             }
           
           
           else if ("Overall" %in% strat) {
             adtte_trt <- data_test[(data_test[[treatment]] %in% verum),]
             adtte_com <- data_test[(data_test[[treatment]] %in% comparator),]
             x1 <- length(which(adtte_trt[[cnsr]] %in% event))
             x2 <- length(which(adtte_com[[cnsr]] %in% event))
             t1 <- sum(adtte_trt$AVAL, na.rm=TRUE)/(100*365.25)
             t2 <- sum(adtte_com$AVAL, na.rm=TRUE)/(100*365.25)
             n1 <- nrow(adtte_trt)
             n2 <- nrow(adtte_com)
             a1 <- length(which(adtte_trt[[cnsr]] %in% event))
             b1 <- length(which(adtte_trt[[cnsr]] %notin% event))
             c1 <- length(which(adtte_com[[cnsr]] %in% event))
             d1 <- length(which(adtte_com[[cnsr]] %notin% event))

             
          if (effect %in% c("IRD", "EXCESS_IRD"))  {
            
             if (t1>0 & t2>0) {
               res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95)
               res_extract.effect.ci <- res %>%
                 confint() %>%
                 .$fixed
             } else {
               res_extract.effect.ci <- c("NA","NA","NA")
             }
          } else if (effect %in% c("ARD", "EXCESS_ARD"))  {
            
             res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95)
             res_extract.effect.ci <- res %>%
               confint() %>%
               .$fixed
          }
             
             
             if (effect %in% c("CID" , "EXCESS_CID"))  {
               
               if(datascope != "No selection") {
                 if(is.numeric(data[[population]])) {
                   data_decision <- data[(data[[scope]] %in% datascope) & (data[[population]] %in% "1"),]
                 } else {
                   data_decision <- data[(data[[scope]] %in% datascope) & (data[[population]] %in% "Y"),]
                 } 
               } else {
                 if(is.numeric(data[[population]])) {
                   data_decision <- data[(data[[population]] %in% "1"),]
                 } else {
                   data_decision <- data[(data[[population]] %in% "Y"),]
                 }
               }
               
               
               data_adtte_cid <- data_test[(data_test[[treatment]] %in% c(verum,comparator)),]
               data_adtte_cid[[treatment]] <- ifelse(data_adtte_cid[[treatment]] == verum , 1 , 2)
               data_adtte_cid$CNSR_1 <- as.factor(data_adtte_cid[[cnsr]])
               
               
               
               if (nlevels(as.factor(data_decision[[cnsr]])) > 2){
                 
                 if (event == 0){
                   levels(data_adtte_cid$CNSR_1) <- c("2","1","3")
                 }
                 else if (event == 1){
                   levels(data_adtte_cid$CNSR_1) <- c("1","2","3")
                 }
                 else if (event == 2){
                   levels(data_adtte_cid$CNSR_1) <- c("1","3","2")
                 }
                 
                 a <- data_adtte_cid$AVAL
                 b <- factor(data_adtte_cid$CNSR_1, levels = c("1", "2", "3"))
                 c <- data_adtte_cid[[treatment]]
                 
                 if (event %notin% levels(as.factor(data_adtte_cid[[cnsr]]))) {
                   x1 <- length(which(data_adtte_cid[[cnsr]] == 2 & data_adtte_cid[[treatment]] %in% "1"))
                   x2 <- length(which(data_adtte_cid[[cnsr]] == 2 & data_adtte_cid[[treatment]] %in% "2"))
                   n1 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "1",])
                   n2 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "2",]) 
                   cvf_diff <- "NA"
                   cvf_lower <- "NA"
                   cvf_upper <- "NA"
                 } else {
                   cum <- survival::survfit(survival::Surv(a, b) ~ c, data = data_adtte_cid) 
                   cum_sum <- summary(cum)
                   
                   cum_data <- as.data.frame(cbind(cum_sum$strata,cum_sum$time,cum_sum$n.event[,2],cum_sum$pstate[,2],cum_sum$std.err[,2]))
                   names(cum_data) <- c("strata","time","n.event","cum.inc","std.err")
                   
                   
                   if (day %in% cum_data[which(cum_data$strata == 1),]$time) {
                     new_day_verum=day
                   } else if (day %notin% cum_data[which(cum_data$strata == 1),]$time) {
                     new_time_verum <- c(day,cum_data[which(cum_data$strata == 1),]$time)
                     new_time_sort_verum <- sort(new_time_verum)
                     pos_verum <- match(day,new_time_sort_verum)
                     new_day_verum <- new_time_sort_verum[pos_verum - 1]
                   }
                   if (day %in% cum_data[which(cum_data$strata == 2),]$time) {
                     new_day_comp=day
                   } else if (day %notin% cum_data[which(cum_data$strata == 2),]$time) {
                     new_time_comp <- c(day,cum_data[which(cum_data$strata == 2),]$time)
                     new_time_sort_comp <- sort(new_time_comp)
                     pos_comp <- match(day,new_time_sort_comp)
                     new_day_comp <- new_time_sort_comp[pos_comp - 1]
                   }
                   
                   n1 <- cum_sum$n[1]
                   x1 <- sum(cum_data[which(cum_data$time <= new_day_verum & cum_data$strata == 1),]$n.event)
                   n2 <- cum_sum$n[2]
                   x2 <- sum(cum_data[which(cum_data$time <= new_day_comp & cum_data$strata == 2),]$n.event)
                   
                   if(is.na(n1))  {n1 <- 0} else {n1 <- n1}
                   if(is.na(n2))  {n2 <- 0} else {n2 <- n2}
                   
                   if (x1 >= 1 & x2 >= 1) {
                     cvf_diff <- cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$cum.inc - cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$cum.inc
                     cvf_lower <- cvf_diff - 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2) 
                     cvf_upper <- cvf_diff + 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2)
                     cvf_diff <- cvf_diff*100
                     cvf_lower <- cvf_lower*100
                     cvf_upper <- cvf_upper*100
                   } else {
                     cvf_diff <- "NA"
                     cvf_lower <- "NA"
                     cvf_upper <- "NA"
                   }
                 }
               }
               
               
               if (nlevels(as.factor(data_decision[[cnsr]])) == 2){
                 
                 data_adtte_cid$CNSR_2 <- ifelse(data_adtte_cid$CNSR_1 != event , 0 , 1)
                 
                 a <- data_adtte_cid$AVAL 
                 b <- as.factor(data_adtte_cid$CNSR_2)
                 c <- data_adtte_cid[[treatment]]
                 
                 if (event %notin% levels(as.factor(data_adtte_cid[[cnsr]]))) {
                   x1 <- length(which(data_adtte_cid[[cnsr]] %notin% event & data_adtte_cid[[treatment]] %in% "1"))
                   x2 <- length(which(data_adtte_cid[[cnsr]] %notin% event & data_adtte_cid[[treatment]] %in% "2"))
                   n1 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "1",])
                   n2 <- nrow(data_adtte_cid[data_adtte_cid[[treatment]] %in% "2",]) 
                   cvf_diff <- "NA"
                   cvf_lower <- "NA"
                   cvf_upper <- "NA"
                 } else {
                   cum <- survival::survfit(survival::Surv(a, b) ~ c, data = data_adtte_cid) 
                   cum_sum <- summary(cum)
                   
                   cum_data <- as.data.frame(cbind(cum_sum$strata,cum_sum$time,cum_sum$n.event[,2],cum_sum$pstate[,2],cum_sum$std.err[,2]))
                   names(cum_data) <- c("strata","time","n.event","cum.inc","std.err")
                   
                   
                   if (day %in% cum_data[which(cum_data$strata == 1),]$time) {
                     new_day_verum=day
                   } else if (day %notin% cum_data[which(cum_data$strata == 1),]$time) {
                     new_time_verum <- c(day,cum_data[which(cum_data$strata == 1),]$time)
                     new_time_sort_verum <- sort(new_time_verum)
                     pos_verum <- match(day,new_time_sort_verum)
                     new_day_verum <- new_time_sort_verum[pos_verum - 1]
                   }
                   if (day %in% cum_data[which(cum_data$strata == 2),]$time) {
                     new_day_comp=day
                   } else if (day %notin% cum_data[which(cum_data$strata == 2),]$time) {
                     new_time_comp <- c(day,cum_data[which(cum_data$strata == 2),]$time)
                     new_time_sort_comp <- sort(new_time_comp)
                     pos_comp <- match(day,new_time_sort_comp)
                     new_day_comp <- new_time_sort_comp[pos_comp - 1]
                   }
                   
                   n1 <- cum_sum$n[1]
                   x1 <- sum(cum_data[which(cum_data$time <= new_day_verum & cum_data$strata == 1),]$n.event)
                   n2 <- cum_sum$n[2]
                   x2 <- sum(cum_data[which(cum_data$time <= new_day_comp & cum_data$strata == 2),]$n.event)
                   
                   if(is.na(n1))  {n1 <- 0} else {n1 <- n1}
                   if(is.na(n2))  {n2 <- 0} else {n2 <- n2}
                   
                   if (x1 >= 1 & x2 >= 1) {
                     cvf_diff <- cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$cum.inc - cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$cum.inc
                     cvf_lower <- cvf_diff - 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2) 
                     cvf_upper <- cvf_diff + 1.96 * sqrt((cum_data[which(cum_data$time == new_day_verum & cum_data$strata == 1),]$std.err)**2 + (cum_data[which(cum_data$time == new_day_comp & cum_data$strata == 2),]$std.err)**2)
                     cvf_diff <- cvf_diff*100
                     cvf_lower <- cvf_lower*100
                     cvf_upper <- cvf_upper*100
                   } else {
                     cvf_diff <- "NA"
                     cvf_lower <- "NA"
                     cvf_upper <- "NA"
                   }
                 }
               }
               res_extract.effect.ci <- c(cvf_diff, cvf_lower, cvf_upper)
             }
             
             
             if (effect %in% c("HR"))  {
               
               data_test_hr <- data_test[(data_test[[treatment]] %in% c(verum,comparator)),]
               data_test_hr[[treatment]] <- ifelse(data_test_hr[[treatment]] == verum , 1 , 0)
               data_test_hr[[cnsr]] <- ifelse(data_test_hr[[cnsr]] != event , 0 , 1)
               
               a <- data_test_hr$AVAL
               b <- data_test_hr[[cnsr]]
               c <- data_test_hr[[treatment]]
               
               cox_hr <- survival::coxph(survival::Surv(a, b) ~ c , data_test_hr) 
               res_extract.effect.ci <- summary(cox_hr)$conf.int[,c("exp(coef)","lower .95","upper .95")]
               res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
             }
             
             if (effect == "HR" & is.na(res_extract.effect.ci[1]) != TRUE)  {
               
               res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
               
               nnt <- 1/(res_extract.effect.ci[1])*100
               nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
               
               excess <- round((res_extract.effect.ci[1])*100)
               excess_lower <- round((res_extract.effect.ci[2])*100)
               excess_upper <- round((res_extract.effect.ci[3])*100)
             } else if (effect == "HR" & is.na(res_extract.effect.ci[1]) == TRUE)  {
                 
               res_extract.effect.ci.rounded <- res_extract.effect.ci
               
               nnt <- "NA"
               
               excess <- "NA"
               excess_lower <- "NA"
               excess_upper <- "NA"
              } 
             
             if (effect %notin% c("HR") & res_extract.effect.ci[1] != "NA")  {
               
               res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
               
               nnt <- 1/(res_extract.effect.ci[1])*100
               nnt <- ifelse(nnt<0 ,floor(nnt) ,ceiling(nnt))
               
               excess <- round((res_extract.effect.ci[1])*100)
               excess_lower <- round((res_extract.effect.ci[2])*100)
               excess_upper <- round((res_extract.effect.ci[3])*100)
             } else if (effect %notin% c("HR") & res_extract.effect.ci[1] == "NA")  {
               
               res_extract.effect.ci.rounded <- res_extract.effect.ci
               
               nnt <- "NA"
               
               excess <- "NA"
               excess_lower <- "NA" 
               excess_upper <- "NA"
             } 
             

             if (effect == "IRD") {
               result_test_1[[j]] <- c(trialno, "Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
             }
             if (effect == "ARD") {
               result_test_1[[j]] <- c(trialno, "Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_IRD") {
               result_test_1[[j]] <- c(trialno, "Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_ARD") {
               result_test_1[[j]] <- c(trialno, "Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             if (effect == "HR") {
               result_test_1[[j]] <- c(trialno, "Hazard Ratio",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded)
               names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_HR","LOWER95","UPPER95")
             }
             if (effect == "CID"){
               if (nlevels(as.factor(data_decision[[cnsr]])) > 2) {
                 result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on AJ",day,population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
                 names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_CID","LOWER95","UPPER95","NNT")
               }
             }
             if (effect == "EXCESS_CID"){
               if (nlevels(as.factor(data_decision[[cnsr]])) > 2) {
                 result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on AJ",day,population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
                 names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
               }
             }
             if (effect == "CID"){
               if (nlevels(as.factor(data_decision[[cnsr]])) == 2) {
                 result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on KM",day,population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
                 names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_CID","LOWER95","UPPER95","NNT")
               }
             }
             if (effect == "EXCESS_CID"){
               if (nlevels(as.factor(data_decision[[cnsr]])) == 2) {
                 result_test_1[[j]] <- c(trialno, "Cumulative Incidence based on KM",day,population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
                 names(result_test_1[[j]]) <- c("TRIALNO", "ESTIMATE","DAY","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
               }
             }

           }
         } 

         if("Overall" %notin% strat) {
           for(k in 1:length(outcome)) {total <- rbind(total,as.data.frame(result_test_1[[k]]))}
           total_final[[l]] <- as.data.frame(total)
         }
         else if("Overall" %in% strat) {
           for(k in 1:length(outcome)) {total <- cbind(total,result_test_1[[k]])}
           total_final[[l]] <- as.data.frame(t(total))
         }
       }
       total_end[[q]] <- total_final[[nlevels(subgroup_factor)]]
     }
     total_end <- total_end[[length(subgroup)]]
   }
 else {return(total_res)}
 }