rankall <- function(outcome, num = "best") {

    
    ## Read outcome data
    outcome_data <- read.csv("Data/outcome-of-care-measures.csv", colClasses = "character")

    
    ## Check that state and outcome are valid    
    outcomeNames <- data.frame(colomn = c(11,17,23), Name = c("heart attack", "heart failure", "pneumonia"))

    #stateResutls <- nrow( subset( outcome_data, outcome_data$State == state))
    outcomeResults <- nrow( subset( outcomeNames, outcomeNames$Name == outcome))
    
    
    # if (stateResutls == 0 && outcomeResults == 0) {
    #     print(paste('Error in best("',state,'"',outcome,'": invalid state and invalid outcome ' ))
    #     return(NULL)
    # } 
    # else if (stateResutls == 0 ) {
    #     print(paste('Error in best("',state,'"',outcome,'" : invalid state' ))
    #     return(NULL)
    # }
    # else 
    if (outcomeResults == 0) {
        stop("invalid outcome")
        #print(paste('Error in best("',state,'"',outcome,'": invalid outcome ' ))
        #return(NULL)
    }
    
    
    ## For each state, find the hospital of the given rank
    
    states_name <- unique(outcome_data[,7])
    hospitals_list <- NULL
    States_list <- NULL
    
    for (state in states_name) {
        hosp_name <- NULL
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
            hosp_name <- result[rownum,1]
        }
        else {
            hosp_name <- NA
        }
        hospitals_list <- c(hospitals_list,hosp_name)
        States_list <- c(States_list, state)
        
    }
    ## Return a data frame with the hospital names and the
    result <- data.frame(hospital = hospitals_list, state = States_list)
    result <- result[order(result[,2]),]
    
    ## (abbreviated) state name
}