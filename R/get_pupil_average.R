#' Title Average a pupil size vector around a peak pupil size
#'
#' \code{get_pupil_avg} gets the mean of pupil size samples around the peak pupil size. Actually this function is used to create the measure for inferential statistic analysis
#'
#' @param pupil_size input: numeric vector of pupil sizes
#' @param avg_window input: Two element vector of integers determining the number of samples before and after the peak pupil size, over which the average is calculated; Default value adds up to around 200ms before and after peak pupil size, when sampling with 250 Hz.
#' @param region Two element vector containing the range of samples to use; keeps the function flexible, so that it can be used for baseline and recall measure.
## returns one numerical value
#'
#' @return numeric object: Average pupil size round peak pupil size.
#' @export
#'
#' @examples
#' x <- rnorm(100, 5, 3)
#' x[50] <- 80
#' get_pupil_avg(x, c(-5,5), 1:100)
get_pupil_avg <- function(pupil_size, avg_window=c(-48L,48L), region){
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
