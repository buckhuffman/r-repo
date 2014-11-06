corr <- function(directory, threshold = 0) {
        df <- complete(directory)     
        sub_id <- df[which(df[, "nobs"] > threshold), "id"]

        files <- list.files(directory, full.names=TRUE)
        vec <- vector()
        
        for (i in sub_id){
                fr <- read.csv(files[i])        
                cr <- cor(fr$sulfate, fr$nitrate, use="complete.obs")
                vec <- c(vec, cr)
        }
        if (length(vec) > 0){
                vec
        } else {
                list()
        }
        
}