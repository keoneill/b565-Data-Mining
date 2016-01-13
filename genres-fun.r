# Question 1

getFreq <- function(df) {
    total.len <- nrow(df)
    genres <- c("blues", "classical", "jazz", "metal", "pop", "rock")
    #result <- data.frame(Genre = character(0), freq = numeric(0), rel_freq = numeric(0))
    g.splits <- split(g, g$GENRE)
    len <- length(g.splits)
    g.splits <- split(df, df$GENRE)
    g.names <- names(g.splits)
    for(i in 1:len) {
        freq <- nrow(g.splits[[i]])
        new.row <- c(g.names[i], freq, freq/total.len)
        result <- rbind(new.row, result)
    }
    return(result)
}

getHighCorr <- function(mat) {
    r <- nrow(mat)
    c <- ncol(mat)
    rnames <- rownames(mat)
    cnames <- colnames(mat)
    for (i in 1:r) {
        for(j in 1:c) {
            
            if (mat[i,j] >= .9) {
                
            }
        }# end loop through columns
    }# end loop through rows
}

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}