# Automatic tests for effect_calc_new-function with testthat package
# set values
# data:
set.seed(100)
N <- 5000
data<-tibble(USUBJID=round(100000000*runif(N),0),TRT01P=rbinom(N,1,0.5),FASFL=rbinom(N,1,0.98),AVISIT=rep("ITT",N),PARAM="All-cause Mortality",AVAL=round(10+720*runif(N)),CNSR=rbinom(N,1,0.9),SEX=ifelse(rbinom(N,1,0.5)==1,"M","F"),STRATUM=round(1+3*runif(N)))
data <- rbind(data,tibble(USUBJID=data$USUBJID,TRT01P=data$TRT01P,FASFL=data$FASFL,AVISIT=rep("ITT",N),PARAM="Primary efficacy",AVAL=min(data$AVAL,round(10+720*runif(N))),CNSR=rbinom(N,1,0.8),SEX=data$SEX,STRATUM=data$STRATUM))
data <- rbind(data,tibble(USUBJID=data$USUBJID[1:N],TRT01P=data$TRT01P[1:N],FASFL=data$FASFL[1:N],AVISIT=rep("ITT",N),PARAM="Primary safety",AVAL=min(data$AVAL,round(10+720*runif(N))),CNSR=rbinom(N,1,0.7),SEX=data$SEX[1:N],STRATUM=data$STRATUM[1:N]))
data$STUDYID <- rep(12345,N*3)
# other parameter:
scope <- "AVISIT"
datascope <- "ITT"
population <- "FASFL"
population_value <- "1"
treatment <- "TRT01P"
param <- "PARAM"
event <- "0"
verum <- "1"
cnsr <- "CNSR"
comparator <- "0" 
aval <- "AVAL"
strat <- "Overall"
subgroup <- "Overall"

#### HR (1) #### 
# 1 outcome / effect HR / no stratification / no subgroups
test1 <- effect_calc_new(
  data = data,
  effect = "HR",
  outcome = c("All-cause Mortality"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  #day = 7,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions Hazard ratio (1):", {
    testthat::expect_equal(dim(test1)[1], 1)
    testthat::expect_equal(dim(test1)[2], 15)
  }
)


#### HR (2) #### 
# 3 outcomes / effect HR / no stratification / no subgroups
test2 <- effect_calc_new(
  data = data,
  effect = "HR",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions Hazard ratio (2):", {
    testthat::expect_equal(dim(test2)[1], 3)
    testthat::expect_equal(dim(test2)[2], 15)
  }
)

#### HR (3) #### 
# 3 outcomes / effect HR / no stratification / subgroup SEX
test3 <- effect_calc_new(
  data = data,
  effect = "HR",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions Hazard ratio (3):", {
    testthat::expect_equal(dim(test3)[1], 9)
    testthat::expect_equal(dim(test3)[2], 15)
  }
)

#### HR (4) #### 
# 3 outcomes / effect HR / stratification STRATUM / no subgroup 
test4 <- effect_calc_new(
  data = data,
  effect = "HR",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions Hazard ratio (4):", {
    testthat::expect_equal(dim(test4)[1], 3)
    testthat::expect_equal(dim(test4)[2], 15)
  }
)

#### HR (5) #### 
# 3 outcomes / effect HR / stratification STRATUM / subgroup SEX
test5 <- effect_calc_new(
  data = data,
  effect = "HR",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions Hazard ratio (5):", {
    testthat::expect_equal(dim(test5)[1], 9)
    testthat::expect_equal(dim(test5)[2], 15)
  }
)

#### CID  #### 

#### CID (1) #### 
# 1 outcome / effect CID / no stratification / no subgroup
test6 <- effect_calc_new(
  data = data,
  effect = "CID",
  outcome = c("All-cause Mortality"),
  day = 365,
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)
testthat::test_that("Check dimensions CID (1):", {
    testthat::expect_equal(dim(test6)[1], 1)
    testthat::expect_equal(dim(test6)[2], 17)
  }
)


#### CID (2) #### 
# 3 outcomes / effect CID / no stratification / no subgroup
test7 <- effect_calc_new(
  data = data,
  effect = "CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  day = 365,
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)
testthat::test_that("Check dimensions CID (2):", {
    testthat::expect_equal(dim(test7)[1], 3)
    testthat::expect_equal(dim(test7)[2], 17)
  }
)

#### CID (3) #### 
# 3 outcomes / effect CID / no stratification / subgroup SEX
test8 <- effect_calc_new(
  data = data,
  effect = "CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  day = 365,
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "SEX",
  aval = aval
)
testthat::test_that("Check dimensions CID (3):", {
    testthat::expect_equal(dim(test8)[1], 9)
    testthat::expect_equal(dim(test8)[2], 17)
  }
)

#### CID (4) #### 
# 3 outcomes / effect CID / stratification STRATUM / no subgroup 
test9 <- effect_calc_new(
  data = data,
  effect = "CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  day = 365,
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions CID (4):", {
    testthat::expect_equal(dim(test9)[1], 3)
    testthat::expect_equal(dim(test9)[2], 17)
  }
)

#### CID (5) #### 
# 3 outcomes / effect CID / stratification STRATUM / subgroup SEX
test10 <- effect_calc_new(
  data = data,
  effect = "CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  day = 365,
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions CID (5):", {
    testthat::expect_equal(dim(test10)[1], 9)
    testthat::expect_equal(dim(test10)[2], 17)
  }
)

#### ARD  #### 

#### ARD (1) #### 
# 1 outcome / effect ARD / no stratification / no subgroup
test11 <- effect_calc_new(
  data = data,
  effect = "ARD",
  outcome = c("All-cause Mortality"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)
testthat::test_that("Check dimensions ARD (1):", {
    testthat::expect_equal(dim(test11)[1], 1)
    testthat::expect_equal(dim(test11)[2], 16)
  }
)

#### ARD (2) #### 
# 3 outcomes / effect ARD / no stratification / no subgroup
test12 <- effect_calc_new(
  data = data,
  effect = "ARD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)
testthat::test_that("Check dimensions ARD (2):", {
    testthat::expect_equal(dim(test12)[1], 3)
    testthat::expect_equal(dim(test12)[2], 16)
  }
)

#### ARD (3) #### 
# 3 outcomes / effect ARD / no stratification / subgroup SEX
test13 <- effect_calc_new(
  data = data,
  effect = "ARD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "SEX",
  aval = aval
)
testthat::test_that("Check dimensions ARD (3):", {
    testthat::expect_equal(dim(test13)[1], 9)
    testthat::expect_equal(dim(test13)[2], 16)
  }
)

#### ARD (4) #### 
# 3 outcomes / effect ARD / stratification STRATUM / no subgroup
test14 <- effect_calc_new(
  data = data,
  effect = "ARD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "Overall",
  aval = aval
)
testthat::test_that("Check dimensions ARD (4):", {
    testthat::expect_equal(dim(test14)[1], 3)
    testthat::expect_equal(dim(test14)[2], 16)
  }
)


#### ARD (5) #### 
# 3 outcomes / effect ARD / stratification STRATUM / subgroup SEX
test15 <- effect_calc_new(
  data = data,
  effect = "ARD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "SEX",
  aval = aval
)
testthat::test_that("Check dimensions ARD (5):", {
    testthat::expect_equal(dim(test15)[1], 9)
    testthat::expect_equal(dim(test15)[2], 16)
  }
)

#### IRD (1) #### 
# 1 outcome / effect IRD / no stratification / no subgroup
test16 <- effect_calc_new(
  data = data,
  effect = "IRD",
  outcome = c("All-cause Mortality"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions IRD (1):", {
    testthat::expect_equal(dim(test16)[1], 1)
    testthat::expect_equal(dim(test16)[2], 16)
  }
)


#### IRD (2) #### 
# 3 outcomes / effect IRD / no stratification / no subgroup
test17 <- effect_calc_new(
  data = data,
  effect = "IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions IRD (2):", {
    testthat::expect_equal(dim(test17)[1], 3)
    testthat::expect_equal(dim(test17)[2], 16)
  }
)


#### IRD (3) #### 
# 3 outcomes / effect IRD / no stratification / subgroup SEX
test18 <- effect_calc_new(
  data = data,
  effect = "IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions IRD (3):", {
    testthat::expect_equal(dim(test18)[1], 9)
    testthat::expect_equal(dim(test18)[2], 16)
  }
)


#### IRD (4) #### 
# 3 outcomes / effect IRD / stratification STRATUM / no subgroup
test19 <- effect_calc_new(
  data = data,
  effect = "IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions IRD (4):", {
    testthat::expect_equal(dim(test19)[1], 3)
    testthat::expect_equal(dim(test19)[2], 16)
  }
)

#### IRD (5) #### 
# 3 outcomes / effect IRD / stratification STRATUM / subgroup SEX
test20 <- effect_calc_new(
  data = data,
  effect = "IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions IRD (5):", {
    testthat::expect_equal(dim(test20)[1], 9)
    testthat::expect_equal(dim(test20)[2], 16)
  }
)


#### EXCESS_CID  (1) #### 
# 1 outcome / effect EXCESS_CID / no stratification / no subgroup
test21 <- effect_calc_new(
  data = data,
  effect = "EXCESS_CID",
  outcome = c("All-cause Mortality"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_CID (1):", {
    testthat::expect_equal(dim(test21)[1], 1)
    testthat::expect_equal(dim(test21)[2], 17)
  }
)


#### EXCESS_CID  (2) #### 
# 3 outcomes / effect EXCESS_CID / no stratification / no subgroup
test22 <- effect_calc_new(
  data = data,
  effect = "EXCESS_CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_CID (2):", {
    testthat::expect_equal(dim(test22)[1], 3)
    testthat::expect_equal(dim(test22)[2], 17)
  }
)

#### EXCESS_CID  (3) #### 
# 3 outcomes / effect EXCESS_CID / no stratification / subgroup SEX
test23 <- effect_calc_new(
  data = data,
  effect = "EXCESS_CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_CID (3):", {
    testthat::expect_equal(dim(test23)[1], 9)
    testthat::expect_equal(dim(test23)[2], 17)
  }
)

#### EXCESS_CID  (4) #### 
# 3 outcomes / effect EXCESS_CID / stratification STRATUM / no subgroup
test24 <- effect_calc_new(
  data = data,
  effect = "EXCESS_CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "Overall",
  aval = aval
)


testthat::test_that("Check dimensions EXCESS_CID (4):", {
    testthat::expect_equal(dim(test24)[1], 3)
    testthat::expect_equal(dim(test24)[2], 17)
  }
)

#### EXCESS_CID  (5) #### 
# 3 outcomes / effect EXCESS_CID / stratification STRATUM / subgroup SEX
test25 <- effect_calc_new(
  data = data,
  effect = "EXCESS_CID",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "SEX",
  aval = aval
)


testthat::test_that("Check dimensions EXCESS_CID (5):", {
    testthat::expect_equal(dim(test25)[1], 9)
    testthat::expect_equal(dim(test25)[2], 17)
  }
)


#### EXCESS_IRD  (1) #### 
# 1 outcome / effect EXCESS_IRD / no stratification / no subgroup
test26 <- effect_calc_new(
  data = data,
  effect = "EXCESS_IRD",
  outcome = c("All-cause Mortality"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_IRD (1):", {
    testthat::expect_equal(dim(test26)[1], 1)
    testthat::expect_equal(dim(test26)[2], 16)
  }
)


#### EXCESS_IRD  (2) #### 
# 3 outcomes / effect EXCESS_IRD / no stratification / no subgroup
test27 <- effect_calc_new(
  data = data,
  effect = "EXCESS_IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_IRD (2):", {
    testthat::expect_equal(dim(test27)[1], 3)
    testthat::expect_equal(dim(test27)[2], 16)
  }
)


#### EXCESS_IRD  (3) #### 
# 3 outcomes / effect EXCESS_IRD / no stratification / subgroup SEX
test28 <- effect_calc_new(
  data = data,
  effect = "EXCESS_IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "Overall",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_IRD (3):", {
    testthat::expect_equal(dim(test28)[1], 9)
    testthat::expect_equal(dim(test28)[2], 16)
  }
)


#### EXCESS_IRD  (4) #### 
# 3 outcomes / effect EXCESS_IRD / stratification STRATUM / no subgroup
test29 <- effect_calc_new(
  data = data,
  effect = "EXCESS_IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "Overall",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_IRD (4):", {
    testthat::expect_equal(dim(test29)[1], 3)
    testthat::expect_equal(dim(test29)[2], 16)
  }
)

#### EXCESS_IRD  (5) #### 
# 3 outcomes / effect EXCESS_IRD / stratification STRATUM / subgroup SEX
test30 <- effect_calc_new(
  data = data,
  effect = "EXCESS_IRD",
  outcome = c("All-cause Mortality", "Primary efficacy", "Primary safety"),
  scope = scope,
  datascope = datascope ,
  population = population,
  population_value = population_value,
  treatment= treatment,
  verum =  verum,
  comparator = comparator,
  cnsr = cnsr,
  param = param,
  event = event,
  strat = "STRATUM",
  subgroup = "SEX",
  aval = aval
)

testthat::test_that("Check dimensions EXCESS_IRD (5):", {
    testthat::expect_equal(dim(test30)[1], 9)
    testthat::expect_equal(dim(test30)[2], 16)
  }
)
