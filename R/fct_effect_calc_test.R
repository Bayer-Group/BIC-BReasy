#' Function to generate the dataset required for shiny application BReasy
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
#' @param csnr
#' @param event
#' @param strat
#' @param subgroup

eff_calc <- function(
  data = data,
  effect = "IRD", 
  param = param,
  outcome = outcome, 
  scope = scope, 
  datascope = "20001", 
  population = "SAFFL",
  treatment = "TRT01AN", 
  verum = "32", 
  comparator = "16", 
  cnsr = cnsr,
  event = event, 
  strat = "Overall", 
  subgroup = "SEX"
) {

  #1.: filter by outcome/datascope and population flag
  tmp1 <- data %>%
    dplyr::filter(
      !!rlang::sym(param) %in% outcome &
      !!rlang::sym(scope) %in% datascope &
      !!rlang::sym(population) == "Y"
    )

  # add "empty" columns for subgroup and stratification if
  # these are not used (-> "Overall"). These empty columns
  # will be used for a groupin, without using conditions.
  if (length(subgroup) == 1 & subgroup == "Overall") {
      tmp1$subgroup <- NA
      subgroup <- "subgroup"
  }
  
  if (length(strat) == 1 & strat == "Overall") {
      tmp1$strat <- NA
      strat <- "strat"
  }
  
  # group by subgroup and stratification factor
  tmp1 <- tmp1 %>% 
    dplyr::group_by(!!rlang::sym(subgroup), !!rlang::sym(strat))

  #2.: create new variable breasy_treatment and breasy_cnsr for grouping in step 3
  tmp2 <- tmp1 %>%
    dplyr::mutate(
      breasy_treatment = dplyr::case_when(
        !!rlang::sym(treatment) %in% verum  ~ "1",
        !!rlang::sym(treatment) %in% comparator ~ "2",
        TRUE ~ "NA"
      ),
      breasy_cnsr = dplyr::case_when(
        !!rlang::sym(cnsr) %in% event  ~ 1,
        TRUE ~ 0
      )
    )
  #3.: calculate number of subjects in verum/compartor for all outcomes
  tmp3 <- tmp2 %>%
    dplyr::group_by(breasy_treatment, !!rlang::sym(param), !!rlang::sym(subgroup),!!rlang::sym(strat)) %>%
    dplyr::mutate(
      breasy_n = n(),
      breasy_x = sum(breasy_cnsr),
      breasy_t = sum(AVAL, na.rm=TRUE)/(100*365.25)
    )

  #4.: calculate number of subjects with an event
  tmp4 <- tmp3 %>%
    dplyr::select(
      !!rlang::sym(strat),
      !!rlang::sym(subgroup),
      !!rlang::sym(param),
      breasy_treatment,
      breasy_n,
      breasy_x,
      breasy_t
    ) %>% distinct()

  #delete missing treatments (breasy_treatment == NA)
  tmp4 <- tmp4 %>% 
    dplyr::filter(breasy_treatment != "NA")

  #5. Change long to wide format 
  tmp5 <- tmp4 %>% 
    tidyr::pivot_wider(
      names_from = breasy_treatment,
      values_from = c(breasy_n,breasy_x,breasy_t)
    ) %>%
    dplyr::ungroup()

  #replace NA's in breasy_ columns with 0
  tmp_columns <- c(
    "breasy_n_1","breasy_n_2",
    "breasy_x_1","breasy_x_2",
    "breasy_t_1","breasy_t_2"
  )
  tmp5[,tmp_columns][is.na(tmp5[,tmp_columns])] <- 0

  #6. select only the required variables for function rma.mh in data.frame
  # format
  
  tmp6 <- tmp5 %>% 
    dplyr::select(
      breasy_x_1,
      breasy_x_2,
      breasy_t_1,
      breasy_t_2
    ) %>%
    as.data.frame()

  #7. Calculate effect and confidence interval values with rma.mh()
  tmp7 <- as.data.frame(
    t(
      apply(tmp6, 1, function(x) {
        confint(
          metafor::rma.mh(
            x1i = x[1],
            x2i = x[2],
            t1i = x[3],
            t2i = x[4],
            measure = "IRD",
            level = 95
          )
        )$fixed
      })
    )
  )

  # Perform the calculations in step 6 & 7 for the stratifiaction variable
  if (strat != "strat") {
  
    all <- tmp5 %>% 
      dplyr::group_by(!!rlang::sym(param),!!rlang::sym(subgroup)) %>% 
      dplyr::summarize(
        !!rlang::sym(strat) := "All",
        !!rlang::sym(subgroup) := unique(!!rlang::sym(subgroup)),
        !!rlang::sym(param) := unique(!!rlang::sym(param)),
        breasy_n_1 = sum(breasy_n_1),
        breasy_n_2 = sum(breasy_n_2),
        breasy_x_1 = sum(breasy_x_1),
        breasy_x_2 = sum(breasy_x_2),
        breasy_t_1 = sum(breasy_t_1),
        breasy_t_2 = sum(breasy_t_2),
        .groups='drop'
      )
    
    all_tmp <- all %>% 
      dplyr::select(
        breasy_x_1,
        breasy_x_2,
        breasy_t_1,
        breasy_t_2
      ) %>% as.data.frame()
        
    all_tmp <- as.data.frame(
      t(
        apply(all_tmp, 1, function(x) {
          confint(
            metafor::rma.mh(
              x1i = x[1],
              x2i = x[2],
              t1i = x[3],
              t2i = x[4],
              measure = "IRD",
              level = 95
            )
          )$fixed
        })
      )
    )
    
    if (effect == "IRD") {
      all <- cbind(all, all_tmp) %>%
        dplyr::mutate(
          NNT = dplyr::case_when(
            V1 <= 0  ~ ceiling(1 / V1 * 100),
            V1 > 0  ~ floor(1 / V1 * 100),
            TRUE ~ as.numeric(NA)
          ),
          EFFECT_IRD = round((V1),2),
          LOWER95 = round(V2,2),
          UPPER95 = round(V3,2)
        ) %>% 
        dplyr::select(-c(V1, V2, V3))
    } else if (effect == "excess") {
    
    all <- cbind(all, all_tmp) %>% 
      dplyr::mutate(
        NNT = dplyr::case_when(
          V1 <= 0  ~ ceiling(1 / V1 * 100),
          V1 > 0  ~ floor(1 / V1 * 100),
          TRUE ~ as.numeric(NA)
        ),
        EFFECT_EXCESS = round((V1)*100),
        LOWER95 = round(V2 * 100),
        UPPER95 = round(V3 * 100)
      ) %>% 
      dplyr::select(-c(V1, V2, V3))
    }
  } #end stratification calculation

  #8. Calculate effect and CI's
  if (effect == "IRD") {
    tmp8 <- tmp7 %>% 
      dplyr::mutate(
        NNT = dplyr::case_when(
          V1 <= 0  ~ ceiling(1 / V1 * 100),
          V1 > 0  ~ floor(1 / V1 * 100),
          TRUE ~ as.numeric(NA)
        ),
        EFFECT_IRD = round((V1),2),
        LOWER95 = round(V2,2),
        UPPER95 = round(V3,2)
      ) %>% 
      dplyr::select(-c(V1, V2, V3))
    
  } else if (effect == "excess") {
    tmp8 <- tmp7 %>% 
      dplyr::mutate(
        NNT = dplyr::case_when(
          V1 <= 0  ~ ceiling(1 / V1 * 100),
          V1 > 0  ~ floor(1 / V1 * 100),
          TRUE ~ as.numeric(NA)
        ),
        EFFECT_EXCESS = round((V1)*100),
        LOWER95 = round(V2 * 100),
        UPPER95 = round(V3 * 100)
      ) %>% 
      dplyr::select(-c(V1, V2, V3))
  }

  #9. merge calculations from step 6.-8. to data frame in step 5.
  tmp9 <- cbind(tmp5, tmp8)

  #add stratification if selected
  if (strat != "strat") {
    tmp9 <- rbind(all, tmp9)
  }

  #10. Create, rename and select all variable in required format
  tmp10 <- tmp9 %>%
    dplyr::mutate(
      ESTIMATE = "Incidence Rate by 100 pat-yrs",
      ANALYSIS_SET = population,
      SUBGROUP = !!subgroup,
      "DATA_SCOPE" =  !!datascope,
      STRATVAR = !!strat,
    ) %>% 
    dplyr::rename(
      "SUBLEVEL" = !!subgroup,
      "OUTCOME" = !!param,
      "NUMBER_EVENTS_VERUM" = "breasy_x_1",
      "NUMBER_EVENTS_COMP" = "breasy_x_2",
      "NUMBER_PATIENTS_VERUM" = "breasy_n_1",
      "NUMBER_PATIENTS_COMP" = "breasy_n_2",
      "STRATUM" = !!strat
      ) %>% 
    dplyr::select(
      -c("breasy_t_1","breasy_t_2")
    )

  #reorder columns
  tmp10 <- tmp10 %>% 
    dplyr::select(
      ESTIMATE, 
      ANALYSIS_SET,
      OUTCOME, 
      DATA_SCOPE, 
      SUBGROUP, 
      SUBLEVEL, 
      STRATVAR,
      STRATUM,
      NUMBER_EVENTS_VERUM, 
      NUMBER_PATIENTS_VERUM, 
      NUMBER_EVENTS_COMP, 
      NUMBER_PATIENTS_COMP, 
      names(tmp10[startsWith(colnames(tmp10),"EFFECT")]),
      LOWER95, 
      UPPER95, 
      NNT
    )
  # deselect STRATVAR and STRATUM if stratification isnt used
  if (strat == "strat") {
    tmp10 <- tmp10 %>% 
      dplyr::select(-c("STRATVAR","STRATUM"))
  }
  # add Variable SUBGROUP and SUBLEVEL when option subgroup isnt used
  if (subgroup == "subgroup") {
    tmp10 <- tmp10 %>% 
      dplyr::mutate(
        SUBGROUP = "Overall",
        SUBLEVEL = "All"
      )
  }

  return(tmp10)
}



 
# ######### examples
# 
# #ex 1 effect ird
# old_1 <- ratediff(data = data, effect = "IRD", param = param,
#   outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL",
#   treatment = "TRT01AN", verum = "32", comparator = "16", 
#          cnsr = cnsr, event = event, strat = "Overall", subgroup = "SEX")
# old_1
# 
# new_1 <- eff_calc(data = data, effect = "IRD", param = param,
#   outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL",
#   treatment = "TRT01AN", verum = "32", comparator = "16", 
#          cnsr = cnsr, event = event, strat = "Overall", subgroup = "SEX")
# new_1
# ### ALL EQUAL
# 
# #ex1b effect excess
# old_1b <- ratediff(data = data, effect = "excess", param = param,
#   outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL",
#   treatment = "TRT01AN", verum = "32", comparator = "16", 
#          cnsr = cnsr, event = event, strat = "Overall", subgroup = "SEX")
# old_1b
# 
# new_1b <- eff_calc(data = data, effect = "excess", param = param,
#   outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL",
#   treatment = "TRT01AN", verum = "32", comparator = "16", 
#          cnsr = cnsr, event = event, strat = "Overall", subgroup = "SEX")
# new_1b
# 
# new_1
# ### ALL EQUAL
# 
# #ex 2
# old2 <- ratediff(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = "Overall")
# 
# old2
# new2 <- eff_calc(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = "Overall")
# 
# new2
# #ex 3
# 
# old_3 <- ratediff(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "Overall")
# old_3
# 
# new_3 <- eff_calc(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "Overall")
# new_3 %>% dplyr::arrange(OUTCOME)
# 
# #
# ## ex4
# 
# old_4 <- ratediff(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "Overall")
# old_4
# 
# new_4 <- eff_calc(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "Overall")
# 
# new_4 %>% dplyr::arrange(OUTCOME)
# 
# 
# ## ex5
# 
# old_5 <- ratediff(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = "SEX")
# old_5
# 
# new_5 <- eff_calc(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = "SEX")
# new_5
# 
# 
# old_6 <- ratediff(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "SEX")
# old_6
# 
# new_6 <- eff_calc(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "SEX")
# new_6 
# 
# #### (!) differences
# 
# #write.csv(res_example_6,"res_example_6.csv")
# 
# old_7 <- ratediff(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "RACE")
# old_7
# 
# new_7 <- eff_calc(data = data, effect = "excess", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = "RACE")
# new_7 ## differences (!)
# 
# #ex 8
# old_8 <- ratediff(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = "RACE")
# old_8
# new_8 <- ratediff(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = "RACE")
# new_8
# 
# 
# 
# old_9 <- ratediff(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = subgroup)
# old_9
# 
# new_9 <- eff_calc(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "Overall", subgroup = subgroup)
# new_9
# 
# old_10 <- ratediff(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = subgroup)
# old_10
# 
# new_10 <- eff_calc(data = data, effect = "IRD", param = param, outcome = outcome, scope = scope, datascope = "20001", population = "SAFFL", treatment = "TRT01AN", verum = "32", comparator = "16", 
#                           cnsr = cnsr, event = event, strat = "STUDYID", subgroup = subgroup)
# new_10
# #write.csv(res_example_10,"res_example_10.csv")