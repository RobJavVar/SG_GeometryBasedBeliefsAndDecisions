#This is a notebook to run the cross-validated multimembership mixed-effects model for:
##SG - Geometry-based Belief & Decisions project

#Run R script with utility functions
source("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/src/crossval_lmer_R.R")

#Load exported data
graph_alignment <- read.csv("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/output/graphalignment_combination.csv")
participantID <- read.csv("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/output/participantID_combination.csv")
bd_alignmnet_combination <- read.csv("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/output/bdalignment_combination.csv")

#Remove indexing columns from data frames
#graph_alignment$X <- NULL; participantID$X <- NULL; bd_alignmnet_combination$X <- NULL

# Data prep
df <- bind_cols(bd_alignmnet_combination, graph_alignment, participantID) %>%
  select(-X) %>%
  mutate(across(c(participant_A, participant_B), as.factor))

outcome_var <- "BelBehAgreement"
random_effects <- c("participant_A", "participant_B")
predictor_vars <- setdiff(names(df), c(outcome_var, random_effects))

# Run function
run_cv_multimem_lmer(df, outcome_var, random_effects, predictor_vars, k = 10)
