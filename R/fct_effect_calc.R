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
  subgroup = subgroup,
  # add aval
  aval = aval
  ) {
  result <- total <- total_final <- total_end <- total_res <- c()
  result_test_1 <- vector(mode = "list")
  '%notin%' <- Negate('%in%')
  
  #### create output for subgroup = Overall #####
  for (j in 1:length(outcome)) {
   
    if(datascope != "No selection") {
      if (is.numeric(population)) {
        data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "1"),]
      } else {
        data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "Y"),]
      } 
    } else {
      if (is.numeric(population)) {
        data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "1"),]
      } else {
        data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "Y"),]
      }
    }

    if(strat != "Overall") { 
    
      numlevels <- alllevels <- allnames <- strat_names <- c()
  
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
      
      for (g in strat) {
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
      } else if (effect %in% c("ARD", "EXCESS_ARD")) {
        
        data_meta <- data.frame(a1,b1,c1,d1)
        
        res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95, data=data_meta)
        res_extract.effect.ci.meta <- res %>%
          confint() %>%
          .$fixed
        
        for (m in 1:nrow(data_meta)) {
          data_single <- data_meta[m,]
          res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95, data=data_single)
          res_extract.effect.ci.single[[m]] <- res %>%
            confint() %>%
            .$fixed
        }
      }
      
      for (p in 1:nrow(data_meta)) {
        res_extract.effect.ci <- rbind(res_extract.effect.ci,res_extract.effect.ci.single[[p]])
      }
      
      res_extract.effect.ci <- rbind(res_extract.effect.ci.meta, as.data.frame(res_extract.effect.ci))
      res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
  
      excess <- round((res_extract.effect.ci[1])*100, 3)
      excess_lower <- round((res_extract.effect.ci[2])*100, 3)
      excess_upper <- round((res_extract.effect.ci[3])*100, 3)
      
      
      if (effect %in% c("IRD", "EXCESS_IRD"))  {
        nnt <- round(1/(res_extract.effect.ci[1])*100)
      } else if (effect %in% c("ARD", "EXCESS_ARD")) {
        nnt <- round(1/(res_extract.effect.ci[1]))
      }
  
      stratum_level <- c("All",name_levels)
      stratum_name <- c(paste(strat, collapse = "/"), name_strat)
      x_1 <- c(sum(x1),x1)
      x_2 <- c(sum(x2),x2)
      n_1 <- c(sum(n1),n1)
      n_2 <- c(sum(n2),n2)
  
      
      result_test <- c(res_extract.effect.ci.rounded, nnt)
      names(result_test) <- c("Esimate","lower","upper","nnt")
      result_test <- as.data.frame(result_test)
      result_test <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level, events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test)
  
      result_test_excess <- c(excess, excess_lower, excess_upper, nnt)
      names(result_test_excess) <- c("Excess","Excess_lower","Excess_upper","nnt")
      result_test_excess <- as.data.frame(result_test_excess)
      result_test_excess <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test_excess)
      
      
      if (effect == "IRD") {
        result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,"Overall","All",result_test)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
      } 
      if (effect == "ARD") {
        result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,"Overall","All",result_test)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
      }
      if (effect == "EXCESS_IRD") {
        result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,"Overall","All",result_test_excess)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
      }
      if (effect == "EXCESS_ARD") {
        result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,"Overall","All",result_test_excess)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
      }
    } else {
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
        res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95)
        res_extract.effect.ci <- res %>%
          confint() %>%
          .$fixed
      } else if (effect %in% c("ARD", "EXCESS_ARD")) {
      
        res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95)
        res_extract.effect.ci <- res %>%
          confint() %>%
          .$fixed
      }
      
      res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
      
      excess <- round((res_extract.effect.ci[1])*100, 3)
      excess_lower <- round((res_extract.effect.ci[2])*100, 3)
      excess_upper <- round((res_extract.effect.ci[3])*100, 3)
      
    
      if (effect %in% c("IRD", "EXCESS_IRD")) {
        nnt <- round(1/(res_extract.effect.ci[1]) * 100)
      } else if (effect %in% c("ARD", "EXCESS_ARD"))  {
        nnt <- round(1/(res_extract.effect.ci[1]))
      }
      
    
      if (effect == "IRD") {
        result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
      }
      if (effect == "ARD") {
        result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
      }
      if (effect == "EXCESS_IRD") {
        result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
      }
      if (effect == "EXCESS_ARD") {
        result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,"Overall","All",x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
        names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
      }
    }
  }
 
  if (strat != "Overall") {
    for(k in 1:length(outcome)) {total <- rbind(total,as.data.frame(result_test_1[[k]]))}
    total_res <- as.data.frame(total)
  } else{
    for(k in 1:length(outcome)) {total <- cbind(total,result_test_1[[k]])}
    total_res <- as.data.frame(t(total))
  }
 
 
   
  if (subgroup != "Overall") {
    for (q in 1:length(subgroup)) {

       subgroup_factor <- factor(data[[subgroup[[q]]]])
       subgroup_level <- levels(subgroup_factor)[1:nlevels(subgroup_factor)]


       for (l in 1:nlevels(subgroup_factor)) {

         for (j in 1:length(outcome)) {

           if (datascope != "No selection") {
             if (is.numeric(population)) {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "1") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             } else {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[scope]] %in% datascope) & (data[[population]] %in% "Y") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
           } else {
             if (is.numeric(population)) {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "1") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
             else {
               data_test <- data[(data[[param]] %in% outcome[[j]]) & (data[[population]] %in% "Y") & (data[[subgroup[[q]]]] %in% subgroup_level[[l]]),]
             }
           }

           if (strat != "Overall") {

             numlevels <- alllevels <- allnames <- strat_names <- c()

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

             for (g in strat) {
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
           } else if (effect %in% c("ARD", "EXCESS_ARD"))  {
               
            data_meta <- data.frame(a1,b1,c1,d1)
               
               
             res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95, data=data_meta)
             res_extract.effect.ci.meta <- res %>%
               confint() %>%
               .$fixed
             
             for (m in 1:nrow(data_meta)) {
               data_single <- data_meta[m,]
               res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95, data=data_single)
               res_extract.effect.ci.single[[m]] <- res %>%
                 confint() %>%
                 .$fixed
             }
            }
             
             for(p in 1:nrow(data_meta)) {res_extract.effect.ci <- rbind(res_extract.effect.ci,res_extract.effect.ci.single[[p]])}
             
             res_extract.effect.ci <- rbind(res_extract.effect.ci.meta, as.data.frame(res_extract.effect.ci))
             res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
             
             excess <- round((res_extract.effect.ci[1])*100, 3)
             excess_lower <- round((res_extract.effect.ci[2])*100, 3)
             excess_upper <- round((res_extract.effect.ci[3])*100, 3)

             
            if (effect %in% c("IRD", "EXCESS_IRD"))  {

             nnt <- round(1/(res_extract.effect.ci[1])*100)
            } else if (effect %in% c("ARD", "EXCESS_ARD"))  {
               
             nnt <- round(1/(res_extract.effect.ci[1]))
            }


             stratum_level <- c("All",name_levels)
             stratum_name <- c(paste(strat, collapse = "/"),name_strat)
             x_1 <- c(sum(x1),x1)
             x_2 <- c(sum(x2),x2)
             n_1 <- c(sum(n1),n1)
             n_2 <- c(sum(n2),n2)
             a_1 <- c(sum(a1),a1)
             b_1 <- c(sum(b1),b1)
             c_1 <- c(sum(c1),c1)
             d_1 <- c(sum(d1),d1)
             

             result_test <- c(res_extract.effect.ci.rounded, nnt)
             names(result_test) <- c("Esimate","lower","upper","nnt")
             result_test <- as.data.frame(result_test)
             result_test <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test)

             result_test_excess <- c(excess, excess_lower, excess_upper, nnt)
             names(result_test_excess) <- c("Excess","Excess_lower","Excess_upper","nnt")
             result_test_excess <- as.data.frame(result_test_excess)
             result_test_excess <- cbind(STRATUM_NAME = stratum_name, STRATUM_LEVEL = stratum_level,events_verum = x_1, patients_verum = n_1, events_comp = x_2, patients_comp = n_2,  result_test_excess)
             
            
             if (effect == "IRD") {
               result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
             }
             if (effect == "ARD") {
               result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_IRD") {
               result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test_excess)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_ARD") {
               result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],result_test_excess)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","STRATVAR","STRATUM","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
           } else {
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
              
               res <- metafor::rma.mh(x1i=x1, x2i=x2, t1i=t1, t2i=t2, measure = "IRD", level = 95)
               res_extract.effect.ci <- res %>%
                 confint() %>%
                 .$fixed
            } else if (effect %in% c("ARD", "EXCESS_ARD"))  {
            
              res <- metafor::rma.mh(ai=a1, bi=b1, ci=c1, di=d1, measure = "RD", level = 95)
              res_extract.effect.ci <- res %>%
               confint() %>%
               .$fixed
            }

             
             res_extract.effect.ci.rounded <- round(res_extract.effect.ci,3)
             
             excess <- round((res_extract.effect.ci[1])*100, 3)
             excess_lower <- round((res_extract.effect.ci[2])*100, 3)
             excess_upper <- round((res_extract.effect.ci[3])*100, 3)
             
          
              if (effect %in% c("IRD", "EXCESS_IRD"))  {
                   
                 nnt <- round(1/(res_extract.effect.ci[1])*100)
              } else if (effect %in% c("ARD", "EXCESS_ARD"))  {
                
                 nnt <- round(1/(res_extract.effect.ci[1]))
              }


             if (effect == "IRD") {
               result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_IRD","LOWER95","UPPER95","NNT")
             }
             if (effect == "ARD") {
               result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,res_extract.effect.ci.rounded,nnt)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_ARD","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_IRD") {
               result_test_1[[j]] <- c("Incidence Rate",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
             if (effect == "EXCESS_ARD") {
               result_test_1[[j]] <- c("Crude Incidence",population,outcome[[j]],datascope,subgroup[[q]],subgroup_level[[l]],x1,n1,x2,n2,excess,excess_lower,excess_upper,nnt)
               names(result_test_1[[j]]) <- c("ESTIMATE","ANALYSIS_SET","OUTCOME","DATA_SCOPE","SUBGROUP","SUBLEVEL","NUMBER_EVENTS_VERUM","NUMBER_PATIENTS_VERUM","NUMBER_EVENTS_COMP","NUMBER_PATIENTS_COMP","EFFECT_EXCESS","LOWER95","UPPER95","NNT")
             }
           }
         }

         if (strat != "Overall") {
           for(k in 1:length(outcome)) {total <- rbind(total,as.data.frame(result_test_1[[k]]))}
           total_final[[l]] <- as.data.frame(total)

         } else{
           for(k in 1:length(outcome)) {total <- cbind(total,result_test_1[[k]])}
           total_final[[l]] <- as.data.frame(t(total))
         }
       }
       total_end[[q]] <- total_final[[nlevels(subgroup_factor)]]
     }
     total_end <- total_end[[length(subgroup)]]
  } else {
     return(total_res)
  }
}


