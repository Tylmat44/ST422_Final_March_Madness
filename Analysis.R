smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


train.y <- subset(train,select = c(winner))
train <- subset(train,select = -c(ScD,TOVxSD))
test <- subset(test,select=-c(ScD,TOVxSD))

train.xx <- subset(train,select = -c(DGames,ScD,TOVxSD))
glm.model <- glm(train$winner ~ ., data = train)
summary(glm.model)

predictTest = predict(glm.model, type = "response", newdata = test)
table <- table(test$winner,predictTest >= 0.499)
table

(table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2])

glm.model2 <- glm(train.y$winner ~ DGames + Dwins +SeedDiffSq + WinsxSOS + GamesSq + STLxSD +
                  ThreePxTRB + FGxAST + FGxThreeP + DTOV + DSOS + DSRS + DGames, data = train.x)
summary(glm.model2)

predictTest = predict(glm.model2, type = "response", newdata = test)
table <- table(test$winner,predictTest >= 0.549)
table

  (table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2])



glm.model3 <- glm(train.y$winner ~ DGames + DWins + WinsxSOS + GamesSq +
                  FGxAST + DTOV + DSRS, data = train.x)
summary(glm.model3)

tempdata <- subset(data, select=-c(winner))
predictTest = predict(glm.model3, type = "response", newdata = tempdata)
table <- table(data$winner,predictTest >= 0.549)
table

(table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2])


place = .3
temp<-matrix(nrow=400,ncol=1)

acccheck <- function(num){
  table <- table(test$winner,predictTest >= num)
  place
 return( (table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]) )
  
}

for(x in 1:400){
  temp[x,] <- acccheck(place)
  place <- place + .001
}

0.8701923 386

