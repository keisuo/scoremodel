
#计算每一个借款人的信用评分

#age
score.age <- 0
for(i in 1:nrow(train)) {
  if(train$x2[i] <= 30)
    score.age[i] <- Agelessthan30.SCORE
  else if(train$x2[i] <= 35)
    score.age[i] <- Age30to35.SCORE
  else if(train$x2[i] <= 40)
    score.age[i] <- Age35to40.SCORE
  else if(train$x2[i] <= 45)
    score.age[i] <- Age40to45.SCORE
  else if(train$x2[i] <= 50)
    score.age[i] <- Age45to50.SCORE
  else if(train$x2[i] <= 55)
    score.age[i] <- Age50to55.SCORE
  else if(train$x2[i] <= 60)
    score.age[i] <- Age55to60.SCORE
  else if(train$x2[i] <= 65)
    score.age[i] <- Age60to65.SCORE
  else if(train$x2[i] <= 75)
    score.age[i] <- Age65to75.SCORE
  else
    score.age[i] <- Agemorethan.SCORE
}
train$score.age
table(score.age)
score.age[1:10]
#NumberOfTime30-59DaysPastDueNotWorse变量(x3)
score.PastDue <- 0
for(i in 1:nrow(train)) {
  if(train$x3[i] <= 0)
    score.PastDue[i] <- PastDuelessthan0.SCORE
  else if(train$x3[i] <= 1)
    score.PastDue[i] <- PastDue0to1.SCORE
  else if(train$x3[i] <= 3)
    score.PastDue[i] <- PastDue1to3.SCORE
  else if(train$x3[i] <= 5)
    score.PastDue[i] <- PastDue3to5.SCORE
  else
    score.PastDue[i] <- PastDuemorethan.SCORE
}

table(score.PastDue)

#MonthIncome变量(x5)
score.MonthIncome <- 0
for(i in 1:nrow(train)) {
  if(train$x5[i] <= 1000)
    score.MonthIncome[i] <- MonthIncomelessthan1000.SCORE
  else if(train$x5[i] <= 2000)
    score.MonthIncome[i] <- MonthIncome1000to2000.SCORE
  else if(train$x5[i] <= 3000)
    score.MonthIncome[i] <- MonthIncome2000to3000.SCORE
  else if(train$x5[i] <= 4000)
    score.MonthIncome[i] <- MonthIncome3000to4000.SCORE
  else if(train$x5[i] <= 5000)
    score.MonthIncome[i] <- MonthIncome4000to5000.SCORE
  else if(train$x5[i] <= 6000)
    score.MonthIncome[i] <- MonthIncome5000to6000.SCORE
  else if(train$x5[i] <= 7500)
    score.MonthIncome[i] <- MonthIncome6000to7500.SCORE
  else if(train$x5[i] <= 9500)
    score.MonthIncome[i] <- MonthIncome7500to9500.SCORE
  else if(train$x5[i] <= 12000)
    score.MonthIncome[i] <- MonthIncome9500to12000.SCORE
  else
    score.MonthIncome[i] <- MonthIncomemorethan.SCORE
}

table(score.MonthIncome)
score.MonthIncome[1:10]

#NumberOfTimes90DaysLate变量(x7)
score.Days90PastDue <- 0
for(i in 1:nrow(train)) {
  if(train$x7[i] <= 0)
    score.Days90PastDue[i] <- Days90PastDuelessthan0.SCORE
  else if(train$x7[i] <= 1)
    score.Days90PastDue[i] <- Days90PastDue0to1.SCORE
  else if(train$x7[i] <= 3)
    score.Days90PastDue[i] <- Days90PastDue1to3.SCORE
  else if(train$x7[i] <= 5)
    score.Days90PastDue[i] <- Days90PastDue3to5.SCORE
  else if(train$x7[i] <= 10)
    score.Days90PastDue[i] <- Days90PastDue5to10.SCORE
  else
    score.Days90PastDue[i] <- Days90sPastDuemorethan.SCORE
}

table(score.Days90PastDue)

#NumberRealEstateLoansOrLines变量(x8)
score.RealEstate <- 0
for(i in 1:nrow(train)) {
  if(train$x8[i] <= 0)
    score.RealEstate[i] <- RealEstatelessthan0.SCORE
  else if(train$x8[i] <= 1)
    score.RealEstate[i] <- RealEstate0to1.SCORE
  else if(train$x8[i] <= 2)
    score.RealEstate[i] <- RealEstate1to2.SCORE
  else if(train$x8[i] <= 3)
    score.RealEstate[i] <- RealEstate2to3.SCORE
  else if(train$x8[i] <= 5)
    score.RealEstate[i] <- RealEstate3to5.SCORE
  else
    score.RealEstate[i] <- RealEstatemorethan.SCORE
}

table(score.RealEstate)

#NumberOfTime60.89DaysPastDueNotWorse变量(x9)
score.Days60.89PastDue <- 0
for(i in 1:nrow(train)) {
  if(train$x9[i] <= 0)
    score.Days60.89PastDue[i] <- Days60.89PastDuelessthan0.SCORE
  else if(train$x9[i] <= 1)
    score.Days60.89PastDue[i] <- Days60.89PastDue0to1.SCORE
  else if(train$x9[i] <= 3)
    score.Days60.89PastDue[i] <- Days60.89PastDue1to3.SCORE
  else if(train$x9[i] <= 5)
    score.Days60.89PastDue[i] <- Days60.89PastDue3to5.SCORE
  else
    score.Days60.89PastDue[i] <- Days60.89PastDuemorethan.SCORE
}

table(score.Days60.89PastDue)

#NumberOfDependents变量(x10)
score.Dependents <- 0
for(i in 1:nrow(train)) {
  if(train$x8[i] <= 0)
    score.Dependents[i] <- Dependentslessthan0.SCORE
  else if(train$x8[i] <= 1)
    score.Dependents[i] <- Dependents0to1.SCORE
  else if(train$x8[i] <= 2)
    score.Dependents[i] <- Dependents1to2.SCORE
  else if(train$x8[i] <= 3)
    score.Dependents[i] <- Dependents2to3.SCORE
  else if(train$x8[i] <= 5)
    score.Dependents[i] <- Dependents3to5.SCORE
  else
    score.Dependents[i] <- Dependentsmorethan.SCORE
}

table(score.Dependents)
#计算每个人的信用评分
#baseScore <- a*factor+offset
creditScore<-0
for(i in 1:nrow(train)){
  creditScore[i]<-score.age[i]+score.PastDue[i]+score.MonthIncome[i]+
    score.Days90PastDue[i]+score.RealEstate[i]+score.Days60.89PastDue[i]+
    score.Dependents[i]+baseScore
}
train$creditScore<-round(creditScore,0)

