read_file <- function(file = "outcome-of-care-measures.csv") {
    read.csv(file, colClasses = "character")
}

get_states <- function(outcomes) {
    unique(outcomes$State)
}

check_and_get_outcome <- function(outcome) {
    selected_outcomes <- data.frame(outcome = c("heart attack", "heart failure", "pneumonia"),
                                    column = c(11, 17, 23))
    
    if (outcome %in% selected_outcomes$outcome) {
        selected_outcomes$column[selected_outcomes$outcome == outcome]    
    } else {
        stop("invalid outcome")
    }
}

check_state <- function(state, valid_states) {
    if (state %in% valid_states) {
        return(TRUE)
    } else {
        stop("invalid state")
    }
}

get_selected_hospitals <- function(outcome_of_care, outcome_column, state) {
    # make shorter data frame with Hospital Name, State and Outcome
    hospitals <- outcome_of_care[,c(2, 7, outcome_column)]
    # change "Not Available" with NA in "outcome_column
    hospitals[hospitals[,3] == "Not Available", 3] <- NA
    # select hospitals in "state" 
    hospitals <- hospitals[complete.cases(hospitals[,3]) &
                               hospitals[,2] == state, c(1,3)]
    # change column with results to num
    hospitals[,2] <- sapply(hospitals[,2], as.numeric)
    hospitals
}

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_of_care <- read_file()
    states <- get_states(outcome_of_care)
    
    ## Check that state and outcome are valid
    outcome_column <- check_and_get_outcome(outcome)
    check_state(state, states)
    
    ## Return hospital name in that state with lowest 30-day death rate
    # get list of hospitals in state
    hospitals <- get_selected_hospitals(outcome_of_care, outcome_column, state)
    # sort based on name
    hospitals <- hospitals[order(hospitals[,2]),]
    # add 3rd column Rank and rename 2nd column
    hospitals$Rank <- 1:nrow(hospitals)
    names(hospitals)[2] <- "Rate"

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- nrow(hospitals)
    } else if (num > nrow(hospitals)) {
        return(NA)
    }
    hospitals[num, 1]
}
