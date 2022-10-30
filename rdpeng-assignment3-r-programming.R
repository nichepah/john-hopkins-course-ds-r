## Written by Aneesh on 29 Oct 2022
best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv")
  ## check for errors
  ## get states list
  list_states <- data[, 7]
  ## unique to clean the list
  list_states <- unique(list_states)
  ## now the checking
  if(is.element(state, list_states) == FALSE){
    stop("Invalid State")
  }
  
  ## check if outcome is "heart failure" or "heart attack" or "pneumonia"
  expected_outcomes <- c("heart failure", "heart attack", "pneumonia")
  if(!is.element(outcome, expected_outcomes)){
    stop("Invalid outcome")
  }
  else{ ## parameters are perfect, now get the results
    ## get all records of the state
    # focus_rows <- data[, c("State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
    #                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
    #                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") ]
    if(outcome == "heart attack"){
      ## col 2, 7, 11 have values hospital name, state and heart attack mortality
      hospitals_heart_attack <- data[, c(2, 7, 11)]
      ## now select only for the particular state
      hospitals_heart_attack <- hospitals_heart_attack[hospitals_heart_attack$State == state,]
      ## order the data table according to death rates, na to teh last, ascending order; top most you have best hospital
      row_least_heart_attack_mortality <- hospitals_heart_attack[order(hospitals_heart_attack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.last = TRUE),]
      ## dim(row_least_heart_attack_mortality[1])
      row_least_heart_attack_mortality[1,1]
    }else 
      if(outcome == "heart failure"){
        ## col 2, 7, 17 have values hospital name, state and heart failure mortality
        #hospitals <- data[, c(2, 7, 17)]
        ## now select only for the particular state
        hospitals_in_the_state <- data[data$State == state,]
        # convert to numeric relevant figures...mortality rates; 17 heart failure mort
        hospitals_in_the_state[, 17] = suppressWarnings(as.numeric(hospitals_in_the_state[, 17]))
        ## order the data table according to death rates, na to teh last, ascending order; top most you have best hospital
        sorted_hospital_list <- hospitals_in_the_state[order(hospitals_in_the_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.last = TRUE),]
        ## dim(row_least_heart_attack_mortality[1])
        sorted_hospital_list[1,2]
      }else
        if(outcome == "pneumonia"){
          hospitals_in_the_state <- data[data$State == state,]
          # convert to numeric relevant figures...mortality rates; 23 pneumonia mort
          hospitals_in_the_state[, 23] = suppressWarnings(as.numeric(hospitals_in_the_state[, 23]))
          ## order the data table according to death rates, na to teh last, ascending order; top most you have best hospital
          sorted_hospital_list <- hospitals_in_the_state[order(hospitals_in_the_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.last = TRUE),]
          ## dim(row_least_heart_attack_mortality[1])
          sorted_hospital_list[1,2]
        }
  }
}
