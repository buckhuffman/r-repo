complete <- function(directory, id = 1:332) {
        df <- data.frame()        
        files <- list.files(directory, full.names=TRUE)
        for (i in id) {
                fr <- read.csv(files[i])
                x <- length(which(!is.na(fr$sulfate) & !is.na(fr$nitrate)))
                df <- rbind(df, c(i, x))       
        }
        colnames(df) <- c("id", "nobs")
        df
}