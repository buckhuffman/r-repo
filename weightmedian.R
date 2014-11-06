weightmedian <- function(directory, day) {
        df <- data.frame()
        files <- list.files(directory, full.names=TRUE)
        howmanyfiles <- length(files)
        for (i in 1:howmanyfiles) {
                df <- rbind(df, read.csv(files[i]))
        }
        df_subset <- df[which(df[, "Day"] == day),]
        median(df_subset[, "Weight"], na.rm=TRUE)
}