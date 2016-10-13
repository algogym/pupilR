###----------------------------------------------------------
### get_pupil_avg
###----------------------------------------------------------

# function gets the mean of pupilsize samples around the peak pupil size
# it is thought to be used as a function within a data.table call
# Function arguments:
## pupil_size: column containing the measured pupil size
## avg_window: Two element vector containing window around the peak pupil size.
## Default value: 48 samples before and 48 samples after peak pupil size
## First element refers to samples before peak pupil size
## Second Element refers to samples after peak pupil size
## If the absolute values of both elements is identical, that the window is symmetrical
## region: Two element vector containing the range of samples to use.
## This keeps the function flexible, so that it can be used for baseline and recall
## returns one numerical value


get_pupil_avg <- function(pupil_size, avg_window=c(-48,48), region){
    # restricts the pupil size vector to the chosen region
    pupil_size <- pupil_size[region[1]:region[2]]
    # peak pupil size is included in the avg_window as index 0, so the upper bound is reduced by 1
    avg_window[2] <- avg_window[2]-1
    if (all(is.na(pupil_size))){
        pupil_avg <- as.numeric(NA)
    }else{
        # checks which element is the first maximal value in the pupil_size vector
        # adding the avg_window vektor leads the range, where averaging takes place
        avg_window_index <- which(pupil_size==max(pupil_size,na.rm = TRUE))[1] + avg_window
        # Check, whether the indices are out of bound.
        # If yes, set them to the bounds
        # ATTENTION: This leads to fewer values being averaged
        if (avg_window_index[1] < 1)                  avg_window_index[1] <- 1
        if (avg_window_index[2] > length(pupil_size)) avg_window_index[2] <- length(pupil_size)
        # get the mean of the selected value and return it
        pupil_avg  <- mean(pupil_size[avg_window_index[1]:avg_window_index[2]], na.rm=TRUE)
    }
    pupil_avg
}
