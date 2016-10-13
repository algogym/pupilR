###----------------------------------------------------------
### evaluate_response(correct, response)
###----------------------------------------------------------
## Evaluate the behavioral response of a old/new- Experiment.
## function should be used within data.table
# Function arguments
## correct: column with the correct answer. Is equal to condition
## response: subject response; 1 = new, 3 = old
## return value of the evaluation as in the following table:

##  correct   | response  | evaluation
##-----------------------------------------------------------
##  new       | 1         | correct reject
##  new       | 3         | false alarm
##  old       | 1         | miss
##  new       | 3         | hit
##  Either    | NA        | NA




evaluate_response <- function(correct, response){
    ifelse(correct == "new" & response == 1, "correct_reject",
           ifelse(correct == "new" & response == 3, "false_alarm",
                  ifelse(correct == "old" & response == 1, "miss",
                         ifelse(correct == "old" & response == 3, "hit", NA))))
}

evaluate_response_pseudo <- function(name_source, response){
    ifelse(name_source == "nonword" & response == 1, "correct_reject",
           ifelse(name_source == "nonword" & response == 3, "false_alarm",
                  ifelse(name_source == "word" & response == 1, "miss",
                         ifelse(name_source == "word" & response == 3, "hit", NA))))

}

###----------------------------------------------------------
### evaluate_correct(correct, response)
###----------------------------------------------------------
## Evaluate the behavioral response of a old/new- Experiment.
## function should be used within data.table
# Function arguments
## correct: column with the correct answer. Is equal to condition
## response: subject response; 1 = new, 3 = old
## return value of the evaluation as in the following table:
## Function is equivalent to evaluate_response, but only checks, whether response was correct


##  correct   | response  | evaluation
##-----------------------------------------------------------
##  new       | 1         | TRUE
##  new       | 3         | FALSE
##  old       | 1         | FALSE
##  new       | 3         | TRUE
##  Either    | NA        | NA


evaluate_correct <- function(correct, response){
    ifelse(correct == "new" & response == 1, TRUE,
           ifelse(correct == "new" & response == 3, FALSE,
                  ifelse(correct == "old" & response == 1, FALSE,
                         ifelse(correct == "old" & response == 3, TRUE, NA))))
}

evaluate_correct_pseudo <- function(correct, response){
    ifelse(correct == "nonword" & response == 1, TRUE,
           ifelse(correct == "nonword" & response == 3, FALSE,
                  ifelse(correct == "word" & response == 1, FALSE,
                         ifelse(correct == "word" & response == 3, TRUE, NA))))
}



