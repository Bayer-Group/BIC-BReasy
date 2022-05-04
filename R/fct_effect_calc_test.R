
#1.: filter by outcome/datascope and population flag
  tmp1 <- data %>%
    dplyr::filter(!!rlang::sym(param) %in% outcome & !!rlang::sym(scope) %in% datascope & !!rlang::sym(population) == "Y")
#2.: create new variable breasy_treatment for grouping in step 3
tmp2 <- tmp1 %>% 
  dplyr::mutate(
    breasy_treatment = case_when(
      !!rlang::sym(treatment) %in% verum  ~ "1",
      !!rlang::sym(treatment) %in% comparator ~ "2",
      TRUE ~ "NA"
    ),
    breasy_cnsr = case_when(
      !!rlang::sym(cnsr) %in% event  ~ 1,
      TRUE ~ 0
    )
  )
#3.: calculate number of subjects in verum/compartor for all outcomes
tmp3 <- tmp2 %>%
  dplyr::group_by(breasy_treatment, !!rlang::sym(param)) %>%
  dplyr::mutate(
    breasy_n = n(),
    breasy_x = sum(breasy_cnsr),
    breasy_t = sum(AVAL, na.rm=TRUE)/(100*365.25)
    )


#4.: calculate number of subjects with an event
tmp4 <- tmp3 %>% 
  dplyr::select(PARAM,breasy_treatment, breasy_n, breasy_x, breasy_t) %>% distinct()

tmp5 <- tmp4 %>% tidyr::pivot_wider(names_from = breasy_treatment, values_from =c(breasy_n,breasy_x,breasy_t)) %>% dplyr::ungroup()


tmp6 <- tmp5 %>% dplyr::select(breasy_x_1,breasy_x_2,breasy_t_1,breasy_t_2) %>% as.data.frame()

tmp7 <- cbind(tmp6,apply(tmp6,1,function(x){
   confint(metafor::rma.mh(
           x1i = x[1],
           x2i = x[2], 
           t1i = x[3], 
           t2i = x[4], 
           measure = "IRD", 
           level = 95
   ))$fixed
  })
)
          