#Create test data similiar to app structure

test_data <- data.frame(
  ESTIMATE = rep(c("Risk differences", "Relative risks"),100),
  OUTCOME = sort(rep(paste("Outcome",1:50),2)),
  AVISIT = sort(rep(paste("VISIT", 1:4),25)),
  NUMBER_EVENTS_VERUM = as.vector(replicate(50,rep(sample(1:20,1), 2))),
  NUMBER_EVENTS_COMPARATOR = as.vector(replicate(50,rep(sample(1:20,1), 2))),
  NUMBER_PATIENTS_VERUM = as.vector(replicate(50,rep(sample(250:270,1), 2))),
  NUMBER_PATIENTS_COMP = as.vector(replicate(50,rep(sample(420:440,1), 2))),
  EFFECT = 0,
  LOWER95 = 0,
  UPPER95 = 0
)

head(test_data)
res <- breasy_risk_differences(test_data)



# Create CSV file based on ADTTE with following estimates:
# •  Hazard Ratios (use survival::coxph); 
# •  Excess number of subjects (including NNT/NNHs), 
# - based on Kaplan-Meier differences at a selected day (use survival::Surv)
# - based on risk differences of incidence proportions (for CIss use DescTools:: BinomDiffCI(…method="mn",sides="two.sided")
#  - based on  risk differences of incidence rates (for Cis use DescTools:: BinomRatioCI(…, method="katz.log", sides="two.sided")
# •  Odds Ratios; 
# •  Relative risks (for CIss use DescTools:: BinomRatioCI(…method="katz.log",sides="two.sided")


# breasy_hazard_ratio(test_data)
# 
# breasy_excess_number(test_data)

# breasy_relative_risks(test_data)