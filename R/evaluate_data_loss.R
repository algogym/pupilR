###----------------------------------------------------------
### evaluate_data_loss()
###----------------------------------------------------------
# Evaluates artifact vector created by find_artifacts()
# Function should be used within data.table
# Function should be applied after find_artifacts() on the resulted vector
# Function arguments:
## artifacts: vector of TRUE and FALSE, where TRUE means outlier/artifact
## limit: limit of data_loss. value between .1 and 1. Only single digit after decimal point
### Default value: .4: This means, that the function returns TRUE, when more than 40% of the data points are outliers or NA


evaluate_data_loss <- function(artifacts, limit= .4){
    artifacts[is.na(artifacts)] <- TRUE #NAs are considered as lost data
    (sum(artifacts)/ length(artifacts)  >= limit)
}


###----------------------------------------------------------
### evaluate_participant_performance()
###----------------------------------------------------------
# Evaluates excluded_trials vector created by evaluate_data_loss
# Function should be used within data.table
# Function should be applied after evalute_data_loss() on the resulted vector
# Function arguments:
## excluded_trials: vector of TRUE and FALSE, where TRUE means trial is rejected
# limit: limit of data_loss. value between .1 and 1. Only single digit after decimal point
### Default value: .4: This means, that the function returns TRUE, when more than 60% of the trials are rejected



evaluate_participant_performance <- function(excluded_trials, limit=.6){
    excluded_trials[is.na(excluded_trials)] <- TRUE #NAs are considered as lost data
    (sum(excluded_trials)/ length(excluded_trials) >= limit)
}
