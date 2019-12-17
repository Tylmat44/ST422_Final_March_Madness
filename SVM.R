smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)


data_mod <- subset(data,select=c(DGames,DWins,WinsxSOS,GamesSq,
                                       FGxAST,DTOV,DSRS, winner))

train <- data[train_ind, ]
test <- data[-train_ind, ]
train_mod <- data_mod[train_ind, ]
test_mod <- data_mod[-train_ind, ]


train <- subset(train,select = -c(X.1,X,ScD))
test <- subset(test,select=-c(X.1,X,ScD))
tempdat <- data
tempdat[-26] = scale(tempdat[-26])
test[-61] = scale(test[-61])
train_mod[-7] = scale(train_mod[-7])
test_mod[-7] = scale(test_mod[-7])

svm.model1 <- svm(winner ~ ., data = train, kernel = "linear", )

y_pred = predict(svm.model1, newdata = tempdat)

table = table(tempdat[, 26], y_pred >= .66) 
table
((table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]))



svm.model2 <- svm(winner ~ ., data = train_mod, kernel = "linear")

y_pred = predict(svm.model2, newdata = test_mod)

table = table(test[, 61], y_pred >= -0.109) 
table
((table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]))


temp<-matrix(nrow=3000,ncol=3)
for(c in 1:3){
  for(g in 1:1000) {
  temp[x,1] = svm.func(c,g)
  temp[x,2] = c
  temp[x,3] = g
}
}
svm.func(3,10)
place = -.5
temp<-matrix(nrow=400,ncol=1)

acccheck <- function(num){
  table <- table(test[, 61], y_pred >= num) 
  place
  return( (table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]) )
  
}

for(x in 1:400){
  temp[x,] <- acccheck(place)
  place <- place + .001
}