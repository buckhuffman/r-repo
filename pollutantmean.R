pollutantmean <- function (directory, pollutant, id = 1:332){
        df <- data.frame()
        files <- list.files(directory, full.names=TRUE)
 
        for (i in id) {
                df <- rbind(df, read.csv(files[i]))
        }

        mean(df[, pollutant], na.rm=TRUE)
}