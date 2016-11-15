


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


#' Check for outlier created through artifacts
#'
#' \code{find_artifact} checks an numeric vector for outliers, which deviate from the mean by more than a variable multiple of the standard deviation.
#'
#' @param pupil_size input: numeric vector of pupil sizes
#' @param tol input: numeric value for spread tolerance. Actually it is the factor the standard deviation is multiplied by; Default=3
#'
#' @return logical vector indicating, whether data point is an outlier. Value is TRUE, when
#' pupil size sample is bigger than the mean of the whole vector + \code{tol} times the sd of the whole vector
#' pupil size sample is smaller than the mean of the whole vector - \code{tol} times the sd of the whole vector
#'
#' @export
#' @examples
#' x <- rnorm(100, 5, 3)
#' x[10] <- 80
#' find_artifacts(x)
#'
#'@importFrom stats sd
#'
find_artifacts <- function(pupil_size, tol=3){
    (pupil_size > mean(pupil_size, na.rm=TRUE) + tol*sd(pupil_size, na.rm=TRUE)|
         pupil_size < mean(pupil_size, na.rm=TRUE) - tol*sd(pupil_size, na.rm=TRUE)|is.na(pupil_size))
}

