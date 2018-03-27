
#计算WOE的函数
totalgood = as.numeric(table(train$y))[1]
totalbad = as.numeric(table(train$y))[2]
getWOE <- function(a,p,q)
{
  Good <- as.numeric(table(train$y[a > p & a <= q]))[1]
  Bad <- as.numeric(table(train$y[a > p & a <= q]))[2]
  WOE <- log((Bad/totalbad)/(Good/totalgood),base = exp(1))
  return(WOE)
}

#age变量(x2)
Agelessthan30.WOE=getWOE(train$x2,-Inf,30)
Age30to35.WOE=getWOE(train$x2,30,35)
Age35to40.WOE=getWOE(train$x2,35,40)
Age40to45.WOE=getWOE(train$x2,40,45)
Age45to50.WOE=getWOE(train$x2,45,50)
Age50to55.WOE=getWOE(train$x2,50,55)
Age55to60.WOE=getWOE(train$x2,55,60)
Age60to65.WOE=getWOE(train$x2,60,65)
Age65to75.WOE=getWOE(train$x2,65,75)
Agemorethan.WOE=getWOE(train$x2,75,Inf)
age.WOE=c(Agelessthan30.WOE,Age30to35.WOE,Age35to40.WOE,
          Age40to45.WOE,Age45to50.WOE,Age50to55.WOE,
          Age55to60.WOE,Age60to65.WOE,Age65to75.WOE,
          Agemorethan.WOE)
age.WOE

#NumberOfTime30-59DaysPastDueNotWorse变量(x3)
PastDuelessthan0.WOE=getWOE(train$x3,-Inf,0)
PastDue0to1.WOE=getWOE(train$x3,0,1)
PastDue1to3.WOE=getWOE(train$x3,1,3)
PastDue3to5.WOE=getWOE(train$x3,3,5)
PastDuemorethan.WOE=getWOE(train$x3,5,Inf)
PastDue.WOE=c(PastDuelessthan0.WOE,PastDue0to1.WOE,
              PastDue1to3.WOE,PastDue3to5.WOE,
              PastDuemorethan.WOE)
PastDue.WOE

#MonthIncome变量(x5)
MonthIncomelessthan1000.WOE=getWOE(train$x5,-Inf,1000)
MonthIncome1000to2000.WOE=getWOE(train$x5,1000,2000)
MonthIncome2000to3000.WOE=getWOE(train$x5,2000,3000)
MonthIncome3000to4000.WOE=getWOE(train$x5,3000,4000)
MonthIncome4000to5000.WOE=getWOE(train$x5,4000,5000)
MonthIncome5000to6000.WOE=getWOE(train$x5,5000,6000)
MonthIncome6000to7500.WOE=getWOE(train$x5,6000,7500)
MonthIncome7500to9500.WOE=getWOE(train$x5,7500,9500)
MonthIncome9500to12000.WOE=getWOE(train$x5,9500,12000)
MonthIncomemorethan.WOE=getWOE(train$x5,12000,Inf)
MonthIncome.WOE=c(MonthIncomelessthan1000.WOE,MonthIncome1000to2000.WOE,
                  MonthIncome2000to3000.WOE,MonthIncome3000to4000.WOE,
                  MonthIncome4000to5000.WOE,MonthIncome5000to6000.WOE,
                  MonthIncome6000to7500.WOE,MonthIncome7500to9500.WOE,
                  MonthIncome9500to12000.WOE,MonthIncomemorethan.WOE)
MonthIncome.WOE

#NumberOfTime90DaysPastDueNotWorse变量(x7)
Days90PastDuelessthan0.WOE = getWOE(train$x7,-Inf,0)
Days90PastDue0to1.WOE=getWOE(train$x7,0,1)
Days90PastDue1to3.WOE=getWOE(train$x7,1,3)
Days90PastDue3to5.WOE=getWOE(train$x7,3,5)
Days90PastDue5to10.WOE=getWOE(train$x7,5,10)
Days90sPastDuemorethan.WOE=getWOE(train$x7,10,Inf)
Days90sPastDue.WOE=c(Days90PastDuelessthan0.WOE,Days90PastDue0to1.WOE,
                     Days90PastDue1to3.WOE,Days90PastDue3to5.WOE,
                     Days90PastDue5to10.WOE,Days90sPastDuemorethan.WOE)
Days90sPastDue.WOE

#NumberRealEstateLoansOrLines变量(x8)
RealEstatelessthan0.WOE = getWOE(train$x8,-Inf,0)
RealEstate0to1.WOE=getWOE(train$x8,0,1)
RealEstate1to2.WOE=getWOE(train$x8,1,2)
RealEstate2to3.WOE=getWOE(train$x8,2,3)
RealEstate3to5.WOE=getWOE(train$x8,3,5)
RealEstatemorethan.WOE=getWOE(train$x8,5,Inf)
RealEstate.WOE=c(RealEstatelessthan0.WOE,RealEstate0to1.WOE,
                 RealEstate1to2.WOE,RealEstate2to3.WOE,
                 RealEstate3to5.WOE,RealEstatemorethan.WOE)
RealEstate.WOE

#NumberOfTime60.89DaysPastDueNotWorse变量(x9)
Days60.89PastDuelessthan0.WOE = getWOE(train$x9,-Inf,0)
Days60.89PastDue0to1.WOE=getWOE(train$x9,0,1)
Days60.89PastDue1to3.WOE=getWOE(train$x9,1,3)
Days60.89PastDue3to5.WOE=getWOE(train$x9,3,5)
Days60.89PastDuemorethan.WOE=getWOE(train$x9,5,Inf)
Days60.89PastDue.WOE=c(Days60.89PastDuelessthan0.WOE,Days60.89PastDue0to1.WOE,
                       Days60.89PastDue1to3.WOE,Days60.89PastDue3to5.WOE,
                       Days60.89PastDuemorethan.WOE)
Days60.89PastDue.WOE

#NumberOfDependents变量(x10)
Dependentslessthan0.WOE = getWOE(train$x10,-Inf,0)
Dependents0to1.WOE=getWOE(train$x10,0,1)
Dependents1to2.WOE=getWOE(train$x10,1,2)
Dependents2to3.WOE=getWOE(train$x10,2,3)
Dependents3to5.WOE=getWOE(train$x10,3,5)
Dependentsmorethan.WOE=getWOE(train$x10,5,Inf)
Dependents.WOE=c(Dependentslessthan0.WOE,Dependents0to1.WOE,
                 Dependents1to2.WOE,Dependents2to3.WOE,
                 Dependents3to5.WOE,Dependentsmorethan.WOE)
Dependents.WOE


#6.3对变量进行WOE变换
#age
tmp.age <- 0
for(i in 1:nrow(train)) {
  if(train$x2[i] <= 30)
    tmp.age[i] <- Agelessthan30.WOE
  else if(train$x2[i] <= 35)
    tmp.age[i] <- Age30to35.WOE
  else if(train$x2[i] <= 40)
    tmp.age[i] <- Age35to40.WOE
  else if(train$x2[i] <= 45)
    tmp.age[i] <- Age40to45.WOE
  else if(train$x2[i] <= 50)
    tmp.age[i] <- Age45to50.WOE
  else if(train$x2[i] <= 55)
    tmp.age[i] <- Age50to55.WOE
  else if(train$x2[i] <= 60)
    tmp.age[i] <- Age55to60.WOE
  else if(train$x2[i] <= 65)
    tmp.age[i] <- Age60to65.WOE
  else if(train$x2[i] <= 75)
    tmp.age[i] <- Age65to75.WOE
  else
    tmp.age[i] <- Agemorethan.WOE
}

table(tmp.age)
tmp.age[1:10]
#NumberOfTime30-59DaysPastDueNotWorse变量(x3)
tmp.PastDue <- 0
for(i in 1:nrow(train)) {
  if(train$x3[i] <= 0)
    tmp.PastDue[i] <- PastDuelessthan0.WOE
  else if(train$x3[i] <= 1)
    tmp.PastDue[i] <- PastDue0to1.WOE
  else if(train$x3[i] <= 3)
    tmp.PastDue[i] <- PastDue1to3.WOE
  else if(train$x3[i] <= 5)
    tmp.PastDue[i] <- PastDue3to5.WOE
  else
    tmp.PastDue[i] <- PastDuemorethan.WOE
}

table(tmp.PastDue)

#MonthIncome变量(x5)
tmp.MonthIncome <- 0
for(i in 1:nrow(train)) {
  if(train$x5[i] <= 1000)
    tmp.MonthIncome[i] <- MonthIncomelessthan1000.WOE
  else if(train$x5[i] <= 2000)
    tmp.MonthIncome[i] <- MonthIncome1000to2000.WOE
  else if(train$x5[i] <= 3000)
    tmp.MonthIncome[i] <- MonthIncome2000to3000.WOE
  else if(train$x5[i] <= 4000)
    tmp.MonthIncome[i] <- MonthIncome3000to4000.WOE
  else if(train$x5[i] <= 5000)
    tmp.MonthIncome[i] <- MonthIncome4000to5000.WOE
  else if(train$x5[i] <= 6000)
    tmp.MonthIncome[i] <- MonthIncome5000to6000.WOE
  else if(train$x5[i] <= 7500)
    tmp.MonthIncome[i] <- MonthIncome6000to7500.WOE
  else if(train$x5[i] <= 9500)
    tmp.MonthIncome[i] <- MonthIncome7500to9500.WOE
  else if(train$x5[i] <= 12000)
    tmp.MonthIncome[i] <- MonthIncome9500to12000.WOE
  else
    tmp.MonthIncome[i] <- MonthIncomemorethan.WOE
}

table(tmp.MonthIncome)
tmp.MonthIncome[1:10]

#NumberOfTimes90DaysLate变量(x7)
tmp.Days90PastDue <- 0
for(i in 1:nrow(train)) {
  if(train$x7[i] <= 0)
    tmp.Days90PastDue[i] <- Days90PastDuelessthan0.WOE
  else if(train$x7[i] <= 1)
    tmp.Days90PastDue[i] <- Days90PastDue0to1.WOE
  else if(train$x7[i] <= 3)
    tmp.Days90PastDue[i] <- Days90PastDue1to3.WOE
  else if(train$x7[i] <= 5)
    tmp.Days90PastDue[i] <- Days90PastDue3to5.WOE
  else if(train$x7[i] <= 10)
    tmp.Days90PastDue[i] <- Days90PastDue5to10.WOE
  else
    tmp.Days90PastDue[i] <- Days90sPastDuemorethan.WOE
}

table(tmp.Days90PastDue)

#NumberRealEstateLoansOrLines变量(x8)
tmp.RealEstate <- 0
for(i in 1:nrow(train)) {
  if(train$x8[i] <= 0)
    tmp.RealEstate[i] <- RealEstatelessthan0.WOE
  else if(train$x8[i] <= 1)
    tmp.RealEstate[i] <- RealEstate0to1.WOE
  else if(train$x8[i] <= 2)
    tmp.RealEstate[i] <- RealEstate1to2.WOE
  else if(train$x8[i] <= 3)
    tmp.RealEstate[i] <- RealEstate2to3.WOE
  else if(train$x8[i] <= 5)
    tmp.RealEstate[i] <- RealEstate3to5.WOE
  else
    tmp.RealEstate[i] <- RealEstatemorethan.WOE
}

table(tmp.RealEstate)

#NumberOfTime60.89DaysPastDueNotWorse变量(x9)
tmp.Days60.89PastDue <- 0
for(i in 1:nrow(train)) {
  if(train$x9[i] <= 0)
    tmp.Days60.89PastDue[i] <- Days60.89PastDuelessthan0.WOE
  else if(train$x9[i] <= 1)
    tmp.Days60.89PastDue[i] <- Days60.89PastDue0to1.WOE
  else if(train$x9[i] <= 3)
    tmp.Days60.89PastDue[i] <- Days60.89PastDue1to3.WOE
  else if(train$x9[i] <= 5)
    tmp.Days60.89PastDue[i] <- Days60.89PastDue3to5.WOE
  else
    tmp.Days60.89PastDue[i] <- Days60.89PastDuemorethan.WOE
}

table(tmp.Days60.89PastDue)

#NumberOfDependents变量(x10)
tmp.Dependents <- 0
for(i in 1:nrow(train)) {
  if(train$x8[i] <= 0)
    tmp.Dependents[i] <- Dependentslessthan0.WOE
  else if(train$x8[i] <= 1)
    tmp.Dependents[i] <- Dependents0to1.WOE
  else if(train$x8[i] <= 2)
    tmp.Dependents[i] <- Dependents1to2.WOE
  else if(train$x8[i] <= 3)
    tmp.Dependents[i] <- Dependents2to3.WOE
  else if(train$x8[i] <= 5)
    tmp.Dependents[i] <- Dependents3to5.WOE
  else
    tmp.Dependents[i] <- Dependentsmorethan.WOE
}

table(tmp.Dependents)


#6.4、WOE DataFrame构建：
trainWOE =cbind.data.frame(tmp.age, tmp.PastDue, tmp.MonthIncome,
                           tmp.Days90PastDue, tmp.RealEstate, 
                           tmp.Days60.89PastDue, tmp.Dependents)
