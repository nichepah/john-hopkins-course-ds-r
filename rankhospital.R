## Written by Aneesh on 30 Oct 2022
## Ranks hospital in a state for a particular outcome
rankhospital <- function(state, outcome, num="best"){
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
  else
    { ## parameters are perfect, now get the results
    ## get all records of the state
    # focus_rows <- data[data$State == state,]
    focus_rows <- data[data$State == state, c("Hospital.Name",
                                              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                                              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                                              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") ]
    # heart attack mortality
    focus_rows[, 2] <- suppressWarnings(as.numeric(focus_rows[, 2]))
    # mortality for heart failure
    focus_rows[, 3] <- suppressWarnings(as.numeric(focus_rows[, 3]))
    # mortality for pneumonia
    focus_rows[, 4] <- suppressWarnings(as.numeric(focus_rows[, 4]))
    if(outcome == "heart attack"){
      ranked_rows <- focus_rows[order(focus_rows$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, focus_rows$Hospital.Name, na.last = NA),]
      if(num == "best"){ranked_rows[1, 1]} 
      else 
        if(num == "worst"){ranked_rows[nrow(ranked_rows), 1]}
      else{ranked_rows[num, 1]}
    }else
      if(outcome == "heart failure"){
        ranked_rows <- focus_rows[order(focus_rows$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, focus_rows$Hospital.Name, na.last = NA),]
        if(num == "best"){ranked_rows[1, 1]} 
        else 
          if(num == "worst"){ranked_rows[nrow(ranked_rows), 1]}
        else{ranked_rows[num, 1]}
      }else
        if(outcome == "pneumonia"){
          ranked_rows <- focus_rows[order(focus_rows$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, focus_rows$Hospital.Name, na.last = NA),]
          if(num == "best"){ranked_rows[1, 1]} 
          else 
            if(num == "worst"){ranked_rows[nrow(ranked_rows), 1]}
          else{ranked_rows[num, 1]}
        }
    }
  }
