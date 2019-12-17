smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

tempdat <- as.data.frame(lapply(tempdat[1:8], normalize))

data_norm <- subset(data_norm,select=c(DGames,DWins,WinsxSOS,GamesSq,
                                         FGxAST,DTOV,DSRS, winner))

tempdat <- subset(data,select=c(DGames,DWins,WinsxSOS,GamesSq,
                                     FGxAST,DTOV,DSRS, winner))

train <- data_norm[train_ind, ]
test <- data_norm[-train_ind, ]


train.y <- subset(train,select = c(winner))
train.x <- subset(train,select = -c(winner))
test.x <- subset(test,select=-c(winner))
test.y <- subset(test,select=c(winner))

temptestx <- subset(tempdat, select=-c(winner))
temptesty <- subset(tempdat, select=c(winner))
set.seed(123)
knnFunc <- function(knots) {
  knn.model <- knn(train.x, test.x, train.y$winner, k = knots)
  table <- table(knn.model, test.y$winner)
  return ( (table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]) )
}


temp<-matrix(nrow=40,ncol=1)
for(x in 1:40){
  temp[x,] <- knnFunc(x)
}

knn.model <- knn(train.x, temptestx, train.y$winner, k = 15)
knn.model
table <- table(knn.model, temptesty$winner)
table
( (table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]) )