rankall <- function(outcome, num = "best") {
        df <- data.frame()
        
        ## Read outcome data
        file <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
        
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
        s <- subset(file, !is.na(file[, field]), select = c(2,7,field), drop = TRUE)
        colnames(s) <- c("Hospital.Name", "State", "Outcome")
        
        if (num == "best") {
                srow <- 1
        } else if (num == "worst")  {
                srow <- nrow(s)
        } else if (!is.integer(num)) {
                srow <- num 
        } else {
                stop ("invalid num")
        }
       
        ## For each state, find the hospital of the given rank
        split_by_state <- split(s, f = s$State)

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        for (i in 1:length(split_by_state)) {
                j <- list()
                j[[i]] <- split_by_state[[i]][order(split_by_state[[i]][,3],split_by_state[[i]][,1]),]
                ##j <- rbind(split_by_state[[i]][order(split_by_state[[i]][,3]),])
                if (num == "worst") {
                        srow <- nrow(j[[i]])
                }  
                df <- rbind(df, data.frame(hospital=j[[i]][srow, 1], state=j[[i]][srow, 2]))
                
        }
        df
}