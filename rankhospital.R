rankhospital <- function(state, outcome, num = "best") {

    ## Read outcome data
    outcome_data <- read.csv("Data/outcome-of-care-measures.csv", colClasses = "character")
    #head(outcome_data)
    
    ## Check that state and outcome are valid    
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    outcomeNames <- subset(outcomeNames, outcomeNames$Name == outcome )
    x <- outcome_data[, c(2, 7, as.numeric(outcomeNames[1, 1])) ]
    x <- subset(x , State == state & !is.na(x[,3]) & x[,3] != "Not Available" )
    x[,3] <- as.numeric(x[,3])
    result <- x[order(x[,3],x[,1]),]
    rownum <- NULL
    if (num == "best") {
        rownum = 1
    }
    else if (num == "worst") {
        rownum = nrow(result)
    }
    else {
        rownum = as.numeric(num)
    }
    if (rownum > 0 || rownum <= nrow(result)) {
        print(result[rownum,1])
    }
    else {
        print(NA)
    }
    #print(rownum)
    #print(result)
}