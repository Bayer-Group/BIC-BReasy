#' file creation function
#' 
#' @param data
#' @param effect
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
#' 

effect_calc <- function(
  data = data, 
  effect = effect,
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
  subgroup = subgroup
  ) {

 result <- c()
 total <- c()
 total_final <- c()
 total_end <- c()
 total_res <- c()
 result_test_1 <- vector(mode = "list")
 
 if(subgroup == "Overall") { 
  
 for (j in 1:length(outcome)) {
   
  if(datascope != "No selection") {
    if(is.numeric(population)) {
      data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "1"),]
    }
    else {
    data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "Y"),]
    } 
    }
  else {
    if(is.numeric(population)) {
      data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "1"),]
    }
    else {
    data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "Y"),]
    }
    }

  if(strat != "Overall") { 
    strat_factor <- factor(data_test[[strat]])
    adtte <- vector(mode = "list", length=nlevels(strat_factor))
    adtte_trt <- vector(mode = "list", length=nlevels(strat_factor))
    adtte_com <- vector(mode = "list", length=nlevels(strat_factor))
    x1 <- c()
    x2 <- c()
    t1 <- c()
    t2 <- c()
    n1 <- c()
    n2 <- c()
    
    
    for (i in 1:nlevels(strat_factor)) {
      adtte[[i]] <- data_test[(data_test[[strat]]==levels(factor(data_test[[strat]]))[i]),]
      adtte_trt[[i]] <- adtte[[i]][(adtte[[i]][[treatment]] %in% verum),]
      adtte_com[[i]] <- adtte[[i]][(adtte[[i]][[treatment]] %in% comparator),]
      x1[[i]] <- length(which(adtte_trt[[i]][[cnsr]] %in% event))
      x2[[i]] <- length(which(adtte_com[[i]][[cnsr]] %in% event))
      t1[[i]] <- sum(adtte_trt[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
      t2[[i]] <- sum(adtte_com[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
      n1[[i]] <- nrow(adtte_trt[[i]])
      n2[[i]] <- nrow(adtte_com[[i]])
    }
    
    x1 <- unlist(x1)
    x2 <- unlist(x2)
    t1 <- unlist(t1)
    t2 <- unlist(t2)
    n1 <- unlist(n1)
    n2 <- unlist(n2)
    res_extract.effect.ci.single <- c()
    res_extract.effect.ci <- c()
    
    
    data_meta <- data.frame(x1,x2,t1,t2)

    res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95, data=data_meta)
    res_extract.effect.ci.meta <- res %>%
      confint() %>%
      .$fixed
    
    
    for (m in 1:nrow(data_meta)) {
      data_single <- data_meta[m,]
      res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95, data=data_single)
      res_extract.effect.ci.single[[m]] <- res %>%
        confint() %>%
        .$fixed
       }
    
    for(p in 1:nrow(data_meta)) {res_extract.effect.ci <- rbind(res_extract.effect.ci,res_extract.effect.ci.single[[p]])}
    res_extract.effect.ci <- rbind(res_extract.effect.ci.meta, as.data.frame(res_extract.effect.ci))
    res_extract.effect.ci.rounded <- round(res_extract.effect.ci,2)

    
    nnt <- (1/(res_extract.effect.ci[1])*100)
    nnt <- unlist(nnt)
    nnt[is.finite(nnt) & nnt>0] <- floor(nnt)
    nnt[is.finite(nnt) & nnt<0] <- ceiling(nnt)
    nnt <- list(nnt)
    
    
    excess <- round((res_extract.effect.ci[1])*100)
    excess_lower <- round((res_extract.effect.ci[2])*100)
    excess_upper <- round((res_extract.effect.ci[3])*100)
    

    stratum_level <- c("All",levels(factor(data_test[[strat]]))[1:nlevels(strat_factor)])
    x_1 <- c(sum(x1),x1)
    x_2 <- c(sum(x2),x2)
    n_1 <- c(sum(n1),n1)
    n_2 <- c(sum(n2),n2)
    
    
    result_test_ird <- c(res_extract.effect.ci.rounded, nnt)
    names(result_test_ird) <- c("Esimate","lower","uper","nnt")
    result_test_ird <- as.data.frame(result_test_ird)
    result_test_ird <- cbind(STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test_ird)

    result_test_excess <- c(excess, excess_lower, excess_upper, nnt)
    names(result_test_excess) <- c("Excess","Excess_lower","Excess_upper","nnt")
    result_test_excess <- as.data.frame(result_test_excess)
    result_test_excess <- cbind(STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test_excess)
    
    if (effect == "IRD") {
    result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,"Overall","All",strat,result_test_ird)
    names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
    } 
    if (effect == "excess") {
      result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,"Overall","All",strat,result_test_excess)
      names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
    }
  }
  
  else {
    adtte_trt <- data_test[(data_test[[treatment]] %in% verum),]
    adtte_com <- data_test[(data_test[[treatment]] %in% comparator),]
    x1 <- length(which(adtte_trt[[cnsr]] %in% event))
    x2 <- length(which(adtte_com[[cnsr]] %in% event))
    t1 <- sum(adtte_trt$AVAL, na.rm=TRUE)/(100*365.25)
    t2 <- sum(adtte_com$AVAL, na.rm=TRUE)/(100*365.25)
    n1 <- nrow(adtte_trt)
    n2 <- nrow(adtte_com)
    
    res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95)
    res_extract.effect.ci <- res %>%
      confint() %>%
      .$fixed
    res_extract.effect.ci.rounded <- round(res_extract.effect.ci,2)
    
    
    nnt <- (1/(res_extract.effect.ci[1])*100)
    nnt <- unlist(nnt)
    nnt[is.finite(nnt) & nnt>0] <- floor(nnt)
    nnt[is.finite(nnt) & nnt<0] <- ceiling(nnt)
    nnt <- list(nnt)    
    
    
    excess <- round((res_extract.effect.ci[1])*100)
    excess_lower <- round((res_extract.effect.ci[2])*100)
    excess_upper <- round((res_extract.effect.ci[3])*100)
    
    if (effect == "IRD") {
    result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
    names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
    }
    if (effect == "excess") {
      result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
      names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
    }
  }
 }
   if(strat != "Overall") {
     for(k in 1:length(outcome)) {total <- rbind(total,as.data.frame(result_test_1[[k]]))}
     return(as.data.frame(total))
   }
   else{
     for(k in 1:length(outcome)) {total <- cbind(total,result_test_1[[k]])}
     return(as.data.frame(t(total)))
   }
 }
 
   
   else { 
     
     
     for (q in 1:length(subgroup)) {
       
       subgroup_factor <- factor(data[[subgroup[[q]]]])
       subgroup_level <- levels(subgroup_factor)[1:nlevels(subgroup_factor)]
       
       
       for (l in 1:nlevels(subgroup_factor)) {
         
         for (j in 1:length(outcome)) {
           
           if(datascope != "No selection") {
             if(is.numeric(population)) {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "1") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
             else {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "Y") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             } 
           }
           else {
             if(is.numeric(population)) {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "1") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
             else {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "Y") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
           }
           
           
           if(strat != "Overall") { 
             strat_factor <- factor(data_test[[strat]])
             adtte <- vector(mode = "list", length=nlevels(strat_factor))
             adtte_trt <- vector(mode = "list", length=nlevels(strat_factor))
             adtte_com <- vector(mode = "list", length=nlevels(strat_factor))
             x1 <- c()
             x2 <- c()
             t1 <- c()
             t2 <- c()
             n1 <- c()
             n2 <- c()
             
             
             for (i in 1:nlevels(strat_factor)) {
               adtte[[i]] <- data_test[(data_test[[strat]]==levels(factor(data_test[[strat]]))[i]),]
               adtte_trt[[i]] <- adtte[[i]][(adtte[[i]][[treatment]] %in% verum),]
               adtte_com[[i]] <- adtte[[i]][(adtte[[i]][[treatment]] %in% comparator),]
               x1[[i]] <- length(which(adtte_trt[[i]][[cnsr]] %in% event))
               x2[[i]] <- length(which(adtte_com[[i]][[cnsr]] %in% event))
               t1[[i]] <- sum(adtte_trt[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
               t2[[i]] <- sum(adtte_com[[i]]$AVAL, na.rm=TRUE)/(100*365.25)
               n1[[i]] <- nrow(adtte_trt[[i]])
               n2[[i]] <- nrow(adtte_com[[i]])
             }
             
             x1 <- unlist(x1)
             x2 <- unlist(x2)
             t1 <- unlist(t1)
             t2 <- unlist(t2)
             n1 <- unlist(n1)
             n2 <- unlist(n2)
             res_extract.effect.ci.single <- c()
             res_extract.effect.ci <- c()
             
             
             data_meta <- data.frame(x1,x2,t1,t2)
             
             res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95, data=data_meta)
             res_extract.effect.ci.meta <- res %>%
               confint() %>%
               .$fixed
             
             
             for (m in 1:nrow(data_meta)) {
               data_single <- data_meta[m,]
               res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95, data=data_single)
               res_extract.effect.ci.single[[m]] <- res %>%
                 confint() %>%
                 .$fixed
             }
             
             for(p in 1:nrow(data_meta)) {res_extract.effect.ci <- rbind(res_extract.effect.ci,res_extract.effect.ci.single[[p]])}
             res_extract.effect.ci <- rbind(res_extract.effect.ci.meta, as.data.frame(res_extract.effect.ci))
             res_extract.effect.ci.rounded <- round(res_extract.effect.ci,2)
             
             nnt <- (1/(res_extract.effect.ci[1])*100)
             nnt <- unlist(nnt)
             nnt[is.finite(nnt) & nnt>0] <- floor(nnt)
             nnt[is.finite(nnt) & nnt<0] <- ceiling(nnt)
             nnt <- list(nnt)
              
             
             excess <- round((res_extract.effect.ci[1])*100)
             excess_lower <- round((res_extract.effect.ci[2])*100)
             excess_upper <- round((res_extract.effect.ci[3])*100)
             
             
             stratum_level <- c("All",levels(factor(data_test[[strat]]))[1:nlevels(strat_factor)])
             x_1 <- c(sum(x1),x1)
             x_2 <- c(sum(x2),x2)
             n_1 <- c(sum(n1),n1)
             n_2 <- c(sum(n2),n2)
             
             
             result_test_ird <- c(res_extract.effect.ci.rounded, nnt)
             names(result_test_ird) <- c("Esimate","lower","uper","nnt")
             result_test_ird <- as.data.frame(result_test_ird)
             result_test_ird <- cbind(STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test_ird)
             
             result_test_excess <- c(excess, excess_lower, excess_upper, nnt)
             names(result_test_excess) <- c("Excess","Excess_lower","Excess_upper","nnt")
             result_test_excess <- as.data.frame(result_test_excess)
             result_test_excess <- cbind(STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test_excess)
             
             if (effect == "IRD") {
               result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],strat,result_test_ird)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
             } 
             if (effect == "excess") {
               result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],strat,result_test_excess)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
           }
           
           else {
             adtte_trt <- data_test[(data_test[[treatment]] %in% verum),]
             adtte_com <- data_test[(data_test[[treatment]] %in% comparator),]
             x1 <- length(which(adtte_trt[[cnsr]] %in% event))
             x2 <- length(which(adtte_com[[cnsr]] %in% event))
             t1 <- sum(adtte_trt$AVAL, na.rm=TRUE)/(100*365.25)
             t2 <- sum(adtte_com$AVAL, na.rm=TRUE)/(100*365.25)
             n1 <- nrow(adtte_trt)
             n2 <- nrow(adtte_com)
             
             res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95)
             res_extract.effect.ci <- res %>%
               confint() %>%
               .$fixed
             res_extract.effect.ci.rounded <- round(res_extract.effect.ci,2)
             
             
             nnt <- (1/(res_extract.effect.ci[1])*100)
             nnt <- unlist(nnt)
             nnt[is.finite(nnt) & nnt>0] <- floor(nnt)
             nnt[is.finite(nnt) & nnt<0] <- ceiling(nnt)
             nnt <- list(nnt)
             
             
             excess <- round((res_extract.effect.ci[1])*100)
             excess_lower <- round((res_extract.effect.ci[2])*100)
             excess_upper <- round((res_extract.effect.ci[3])*100)
             
             if (effect == "IRD") {
               result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
             }
             if (effect == "excess") {
               result_test_1[[j]] <- c("Incidence Rate by 100 pat-yrs",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             
           }
         }
         
         if(strat != "Overall") {
           for(k in 1:length(outcome)) {total <- rbind(total,as.data.frame(result_test_1[[k]]))}
           total_final[[l]] <- as.data.frame(total)
           
         }
         else{
           for(k in 1:length(outcome)) {total <- cbind(total,result_test_1[[k]])}
           total_final[[l]] <- as.data.frame(t(total))
         }
         
         
         
       }
       total_end[[q]] <- total_final[[nlevels(subgroup_factor)]]
     }
     return (total_end[[length(subgroup)]])
   }

}


