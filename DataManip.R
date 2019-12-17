data <- read.csv("RealTournamentData442.csv")

data <- data[,-c(27)]

write.csv(data, "Updated_RealTournamentData442.csv")
#positive = high seed

data$winner <- ifelse(data$ScD > 0, 1, 0)

count(data, winner)

pairs(data[,2:10], lower.panel = NULL )

for(i in 2:28) {
plot(data$winner, data[,i])
}

  
library(corrplot)
correlations <- cor(data[,17:27])
corrplot(correlations, method="circle")
data$ScD <- data$ScoreDiff

data$SRSxSOS <- data$DSRS * data$DSOS
data$FGxThreeP <- data$DFG * data$DThreeP
data$FGxFT <- data$DFG * data$DFT
data$FGxTRB <- data$DFG * data$DTRB
data$FGxAST <- data$DFG * data$DAST
data$FGxSTL <- data$DFG * data$DSTL
data$FGxBLK <- data$DFG * data$DBLK
data$FGxTOV <- data$DFG * data$DTOV
data$FGxSD <- data$DFG * data$SeedDiff

data$ThreePxFTpct <- data$DThreeP * data$DFTpct
data$ThreePxAST <- data$DThreeP * data$DAST
data$ThreePxTRB <- data$DThreeP * data$DTRB
data$ThreePxSTL <- data$DThreeP * data$DSTL
data$ThreePxSD <- data$DThreeP * data$SeedDiff

data$FTAxTRB <- data$DFTA * data$DTRB
data$FTAxAST <- data$DFTA * data$DAST
data$FTAxSTL <- data$DFTA * data$DSTL
data$FTAxBLK <- data$DFTA * data$DBLK
data$FTAxTOV <- data$DFTA * data$DTOV
data$FTAxSD <- data$DFTA * data$SeedDiff

data$TRBxAST <- data$DTRB * data$DAST
data$TRBxSTL <- data$DTRB * data$DSTL
data$TRBxBLK <- data$DTRB * data$DBLK
data$TRBxTOV <- data$DTRB * data$DTOV
data$TRBxSD <- data$DTRB * data$SeedDiff

data$ASTxSTL <- data$DAST * data$DSTL
data$ASTxBLK <- data$DAST * data$DBLK
data$ASTxTOV <- data$DAST * data$DTOV
data$ASTxSD <- data$DAST * data$SeedDiff

data$STLxBLK <- data$DSTL * data$DBLK
data$STLxTOV <- data$DSTL * data$DTOV
data$STLxSD <- data$DSTL * data$SeedDiff

data$BLKxTOV <- data$DBLK * data$DTOV
data$BLKxSD <- data$DBLK * data$SeedDiff

data$TOVxSD <- data$DTOv * data$SeedDiff


data$GamesSq <- data$DGames * data$DGames
data$SeedDiffSq <- data$SeedDiff * data$SeedDiff
data$WinsxSOS <- data$DWins * data$DSOS


data <- data[-c(18,250),-c(10, 11, 12, 13, 16, 26, 32)]
data <- data[,-c(1,2)]


corrplot( cor(data[,1:66], data$winner), method="circle")