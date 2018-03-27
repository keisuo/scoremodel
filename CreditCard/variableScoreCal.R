#7.2计算各变量分箱得分
#age变量(x2)
Agelessthan30.SCORE = getscore(2,Agelessthan30.WOE)
Age30to35.SCORE = getscore(2,Age30to35.WOE)
Age35to40.SCORE = getscore(2,Age35to40.WOE)
Age40to45.SCORE = getscore(2,Age40to45.WOE)
Age45to50.SCORE = getscore(2,Age45to50.WOE)
Age50to55.SCORE = getscore(2,Age50to55.WOE)
Age55to60.SCORE = getscore(2,Age55to60.WOE)
Age60to65.SCORE = getscore(2,Age60to65.WOE)
Age65to75.SCORE = getscore(2,Age65to75.WOE)
Agemorethan.SCORE = getscore(2,Agemorethan.WOE)
Age.SCORE = c(Agelessthan30.SCORE,Age30to35.SCORE,Age35to40.SCORE,
              Age40to45.SCORE,Age45to50.SCORE,Age50to55.SCORE,
              Age55to60.SCORE,Age60to65.SCORE,Age65to75.SCORE,
              Agemorethan.SCORE)
Age.SCORE

#NumberOfTime30-59DaysPastDueNotWorse变量(x3)
PastDuelessthan0.SCORE =getscore(3,PastDuelessthan0.WOE/10) 
PastDue0to1.SCORE = getscore(3,PastDue0to1.WOE/10)
PastDue1to3.SCORE = getscore(3,PastDue1to3.WOE/10)
PastDue3to5.SCORE = getscore(3,PastDue3to5.WOE/10)
PastDuemorethan.SCORE = getscore(3,PastDuemorethan.WOE/10)

PastDue.SCORE = c(PastDuelessthan0.SCORE,PastDue0to1.SCORE,
              PastDue1to3.SCORE,PastDue3to5.SCORE,
              PastDuemorethan.SCORE)
PastDue.SCORE

#MonthlyIncome变量(x5)
MonthIncomelessthan1000.SCORE = getscore(4,MonthIncomelessthan1000.WOE*1000)
MonthIncome1000to2000.SCORE = getscore(4,MonthIncome1000to2000.WOE*1000)
MonthIncome2000to3000.SCORE = getscore(4,MonthIncome2000to3000.WOE*1000)
MonthIncome3000to4000.SCORE = getscore(4,MonthIncome3000to4000.WOE*1000)
MonthIncome4000to5000.SCORE = getscore(4,MonthIncome4000to5000.WOE*1000)
MonthIncome5000to6000.SCORE = getscore(4,MonthIncome5000to6000.WOE*1000)
MonthIncome6000to7500.SCORE = getscore(4,MonthIncome6000to7500.WOE*1000)
MonthIncome7500to9500.SCORE = getscore(4,MonthIncome7500to9500.WOE*1000)
MonthIncome9500to12000.SCORE = getscore(4,MonthIncome9500to12000.WOE*1000)
MonthIncomemorethan.SCORE = getscore(4,MonthIncomemorethan.WOE*1000)
MonthIncome.SCORE = c(MonthIncomelessthan1000.SCORE,MonthIncome1000to2000.SCORE,
                      MonthIncome2000to3000.SCORE,MonthIncome3000to4000.SCORE,
                      MonthIncome4000to5000.SCORE,MonthIncome5000to6000.SCORE,
                      MonthIncome6000to7500.SCORE,MonthIncome7500to9500.SCORE,
                      MonthIncome9500to12000.SCORE,MonthIncomemorethan.SCORE)
MonthIncome.SCORE

#NumberOfTime90DaysPastDueNotWorse变量(x7)
Days90PastDuelessthan0.SCORE =getscore(5,Days90PastDuelessthan0.WOE/10) 
Days90PastDue0to1.SCORE =getscore(5,Days90PastDue0to1.WOE/10) 
Days90PastDue1to3.SCORE = getscore(5,Days90PastDue1to3.WOE/10)
Days90PastDue3to5.SCORE = getscore(5,Days90PastDue3to5.WOE/10)
Days90PastDue5to10.SCORE = getscore(5,Days90PastDue5to10.WOE/10)
Days90sPastDuemorethan.SCORE = getscore(5,Days90sPastDuemorethan.WOE/10)

Days90sPastDue.SCORE = c(Days90PastDuelessthan0.SCORE,Days90PastDue0to1.SCORE,
                         Days90PastDue1to3.SCORE,Days90PastDue3to5.SCORE,
                         Days90PastDue5to10.SCORE,Days90sPastDuemorethan.SCORE)
Days90sPastDue.SCORE

#NumberRealEstateLoansOrLines变量(x8)
RealEstatelessthan0.SCORE =getscore(6,RealEstatelessthan0.WOE) 
RealEstate0to1.SCORE =getscore(6,RealEstate0to1.WOE) 
RealEstate1to2.SCORE = getscore(6,RealEstate1to2.WOE)
RealEstate2to3.SCORE = getscore(6,RealEstate2to3.WOE)
RealEstate3to5.SCORE = getscore(6,RealEstate3to5.WOE)
RealEstatemorethan.SCORE = getscore(6,RealEstatemorethan.WOE)

RealEstate.SCORE = c(RealEstatelessthan0.SCORE,RealEstate0to1.SCORE,
                     RealEstate1to2.SCORE,RealEstate2to3.SCORE,
                     RealEstate3to5.SCORE,RealEstatemorethan.SCORE)
RealEstate.SCORE

#NumberOfTime60.89DaysPastDueNotWorse变量(x9)
Days60.89PastDuelessthan0.SCORE =getscore(7,Days60.89PastDuelessthan0.WOE/10) 
Days60.89PastDue0to1.SCORE =getscore(7,Days60.89PastDue0to1.WOE/10) 
Days60.89PastDue1to3.SCORE = getscore(7,Days60.89PastDue1to3.WOE/10)
Days60.89PastDue3to5.SCORE = getscore(7,Days60.89PastDue3to5.WOE/10)
Days60.89PastDuemorethan.SCORE = getscore(7,Days60.89PastDuemorethan.WOE/10)


Days60.89PastDue.SCORE = c(Days60.89PastDuelessthan0.SCORE,Days60.89PastDue0to1.SCORE,
                           Days60.89PastDue1to3.SCORE,Days60.89PastDue3to5.SCORE,
                           Days60.89PastDuemorethan.SCORE)
Days60.89PastDue.SCORE

#NumberOfDependents变量(x10)
Dependentslessthan0.SCORE =getscore(8,Dependentslessthan0.WOE) 
Dependents0to1.SCORE =getscore(8,Dependents0to1.WOE) 
Dependents1to2.SCORE = getscore(8,Dependents1to2.WOE)
Dependents2to3.SCORE = getscore(8,Dependents2to3.WOE)
Dependents3to5.SCORE = getscore(8,Dependents3to5.WOE)
Dependentsmorethan.SCORE = getscore(8,Dependentsmorethan.WOE)

Dependents.SCORE = c(Dependentslessthan0.SCORE,Dependents0to1.SCORE,
                     Dependents1to2.SCORE,Dependents2to3.SCORE,
                     Dependents3to5.SCORE,Dependentsmorethan.SCORE)
Dependents.SCORE

#
