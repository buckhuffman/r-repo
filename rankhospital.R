rankhospital <- function (state, outcome, num) {
        ## Read outcome data
        file <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
        
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
        
        file[,field] <- as.numeric(as.character(file[,field]))
        s <- subset(file, !is.na(file[, field]) & file[,7] == state, select = c(2,field))
        sorder <- s[order(s[, 2],s[, 1]),]
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if (num == "best") {
                srow <- 1
        } else if (num == "worst")  {
                srow <- nrow(s)
        } else if (!is.integer(num)) {
                srow <- num 
        } else {
                stop ("invalid num")
        }
        
        as.character(sorder [srow, 1])
}