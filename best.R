
best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("Data/outcome-of-care-measures.csv", colClasses = "character")
    head(outcome_data)
    #outcome[, 11] <- as.numeric(outcome[, 11])
    
    outcomeNames <- data.frame(colomn = c(11,17,23), Name = c("heart attack", "heart failure", "pneumonia"))
    #hist(outcome[, 11])
    stateResutls <- nrow( subset( outcome_data, outcome_data$State == state))
    outcomeResults <- nrow( subset( outcomeNames, outcomeNames$Name == outcome))
    
    #print(stateResutls)
    #print(outcomeResults)
    
    if (stateResutls == 0 && outcomeResults == 0) {
        print(paste('Error in best("',state,'"',outcome,'": invalid state and invalid outcome ' ))
        return(NULL)
    } 
    else if (stateResutls == 0 ) {
        print(paste('Error in best("',state,'"',outcome,'" : invalid state' ))
        return(NULL)
    }
    else if (outcomeResults == 0) {
        print(paste('Error in best("',state,'"',outcome,'": invalid outcome ' ))
        return(NULL)
    }
    
    x <- outcome_data[, c(2, 7, as.numeric(outcomeNames[1, outcomeNames$Name == outcome])) ]
    x <- subset(x , State == state & !is.na(x[,3]))
    result <- x[order(x[,3],x[,1]),]
    print(result[1,1])
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}