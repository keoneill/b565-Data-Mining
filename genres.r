

g <- read.csv("genresRaw.csv", header=TRUE)

# Question 1, there are six genres, what are the relative frequencies
g.nrows <- nrow(g)
g.splits <- split(g, g$GENRE)
g.names <- names(g.splits)
# "Blues"     "Classical" "Jazz"      "Metal"     "Pop"       "Rock"  
freqs <- c(nrow(g.splits[['Blues']]),
           nrow(g.splits[['Classical']]),
           nrow(g.splits[['Jazz']]),
           nrow(g.splits[['Metal']]),
           nrow(g.splits[['Pop']]),
           nrow(g.splits[['Rock']]))

g.freq <- data.frame(Genre = g.names, Freq = freqs, Rel_Freq = freqs/g.nrows)
g.freq
#     Genre Freq   Rel_Freq
#     Blues 2268 0.09963100
# Classical 5586 0.24538745
#      Jazz 6909 0.30350554
#     Metal 1659 0.07287823
#       Pop 2751 0.12084871
#     Rock 3591 0.15774908

###########################################################
# Question 2

require(Hmisc)
g.sub <- g[c(-1:-4)]
g.mat <- as.matrix(g.sub)
g.cor <- rcorr(g.mat, type="pearson")
# write.csv(g.cor$r, "genre-correlation.csv")
g.cor.mat <- g.cor$r 
# delete extra row and column that was produced
g.cor.mat <- g.cor.mat[,-192]
g.cor.mat <- g.cor.mat[-192,]
dim(g.cor.mat)
# 191 191
rnames <- rownames(g.cor.mat)
cnames <- colnames(g.cor.mat)


##############################################################
# Question 3

unique(g$ARTIST_ID)

