#This is a notebook to run the cross-validated multimembership mixed-effects model for:
##SG - Geometry-based Belief & Decisions project

#Run R script with utility functions
source("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/src/crossval_lmer_R.R")

#Load exported data
graph_alignment <- read.csv("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/output/graphalignment_combination.csv")
participantID <- read.csv("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/output/participantID_combination.csv")
bd_alignmnet_combination <- read.csv("C:/Users/rober/Dropbox/Professional/GitHub/SG_GeometryBasedBeliefsAndDecisions/output/bdalignment_combination.csv")

#Remove indexing columns from data frames
graph_alignment$X <- NULL; participantID$X <- NULL; bd_alignmnet_combination$X <- NULL

#Define function inputs
Y <- bd_alignmnet_combination; X <- graph_alignment; Z <- participantID %>% mutate(across(everything(), as.factor))
df <- bind_cols(Y, X, Z)

#Clean column names
#colnames(df) <- sapply(colnames(df), clean_column_name)
