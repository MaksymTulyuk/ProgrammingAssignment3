read_file <- function(file = "outcome-of-care-measures.csv") {
    read.csv(file, na.strings = "Not Available", colClasses = "character")
}

get_states <- function(outcomes) {
    states <- unique(outcomes$State)
    states[order(states)]
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

get_selected_hospitals <- function(outcome_of_care, outcome_column, state) {
    # make shorter data frame with Hospital Name, State and Outcome
    hospitals <- outcome_of_care[,c(2, 7, outcome_column)]
    # change "Not Available" with NA in "outcome_column
    #hospitals[hospitals[,3] == "Not Available", 3] <- NA
    # select hospitals
    hospitals <- hospitals[complete.cases(hospitals[,3]),]
    # change column with results to num
    names(hospitals)[3] <- "Rate"
    hospitals[,3] <- sapply(hospitals[,3], as.numeric)
    hospitals
}

get_hospitals_per_state <- function(hospitals, states, num) {
    # create empty data frame
    hospitals_per_state <- data.frame(hospital = as.character(rep(NA, length(states))), 
                                     state = states, row.names = states, stringsAsFactors = FALSE)
    # fill data frame per state
    for (state in states) {
        # create and sort data frame in "state"
        hospitals_in_state <- hospitals[hospitals[,2] == state,]
        hospitals_in_state <- hospitals_in_state[order(hospitals_in_state[,3],hospitals_in_state[,1]),]
        # get row based on "num"
        if (num == "best") {
            row <- 1
        } else if (num == "worst") {
            row <- nrow(hospitals_in_state)
        } else if (num > nrow(hospitals)) {
            row <- NA
        } else {
            row <- num
        }
        # fill hospitals
        if (!is.na(row)) {
            hospitals_per_state[hospitals_per_state$state == state,][1,1] <- hospitals_in_state[row, 1]
        }
    }
    hospitals_per_state
}

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_of_care <- read_file()
    states <- get_states(outcome_of_care)
    
    ## Check that state and outcome are valid
    outcome_column <- check_and_get_outcome(outcome)

    # get hospitals with output
    hospitals <- get_selected_hospitals(outcome_of_care, outcome_column)
    
    #
    get_hospitals_per_state(hospitals, states, num)
}
