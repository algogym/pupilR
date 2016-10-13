###----------------------------------------------------------
### find_artifacts()
###----------------------------------------------------------
# Check for outlier created through artifacts
# Function should be used within data.table
# Function arguments:
## pupil_size: column containing the measured pupil_size
## tol: factor for spread tolerance. Actually the factor the standard deviation is multiplied by; Default=3
## Functions returns TRUE, when...
### pupil size sample is bigger than the mean of the whole vector + 'tol' times the sd of the whole vector
### pupil size sample is smaller than the mean of the whole vector - 'tol' times the sd of the whole vector


find_artifacts <- function(pupil_size, tol=3){
    (pupil_size > mean(pupil_size, na.rm=TRUE) + tol*sd(pupil_size, na.rm=TRUE)|
         pupil_size < mean(pupil_size, na.rm=TRUE) - tol*sd(pupil_size, na.rm=TRUE)|is.na(pupil_size))
}
