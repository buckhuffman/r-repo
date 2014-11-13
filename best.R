best <- function (state, outcome) {
        ## Read outcome data
        file <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        if (!state %in% file[,7]) {
                stop("invalid state")
        }
        
        if (outcome == "heart attack") {
                field <- 11
        } else if (outcome == "heart failure") {
                field <- 17
        } else if (outcome == "pneumonia") {
                field <- 23
        } else {
                stop ("invalid outcome")
        }
        
        s <- subset(file, file[,7] == state)
        ## Return hospital name in that state with the lowest 30-day death rate
        s[,field] <- as.numeric(s[,field])
        mins <- min(s[,field], na.rm=TRUE)
        sub.mins <- subset(s, s[,field] == mins)
        sub.mins$Hospital.Name
}