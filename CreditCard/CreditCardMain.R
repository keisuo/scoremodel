#--------------------------------------------------#
# 评分卡模型                                       #
# Author：周荣技 Cliff Zhou
# 微信：15019248387                                #
#--------------------------------------------------#

#载入需要的R包
source("LoadRPackage.R")

#设置工作路径
setwd("D:/RESLIO/workpace/creditCard/CreditCard")
#载入数据
cs_training <- read.csv('cs_training.csv')
#去掉第一列
cs_training<- cs_training[,-1]

#变量重命名
names(cs_training)<-c("y","x1","x2","x3","x4",
                      "x5","x6","x7","x8","x9","x10")
#1、缺失值分析及处理
#查看数据集缺失数据
missmap(cs_training,main = "Missing values vs observed")
#matrixplot(cs_training)
#具体缺失参数，空值
sapply(cs_training,function(z)sum(is.na(z)))
#缺失值级联表
md.pattern(cs_training)
#使用KNN法对缺失值进行填补(由于经过半个多小时的运算不能出结果，故采用其他算法)
#cs_training<-knnImputation(cs_training,k=10,meth = "weighAvg")
#x5(MonthlyIncome)缺失值处理(使用中位数)
cs_training$x5 <- na.roughfix(cs_training$x5)
#x10(NumberOfDependents)3924个缺失值，所占比重3924/150000不大，故直接删除
cs_training <- cs_training[!is.na(cs_training$x10),]
sapply(cs_training,function(z)sum(is.na(z)))
#2、异常值分析及处理
#对x2变量(客户的年龄)定量分析
unique(cs_training$x2)
#年龄中存在0值，显然是异常值，予以剔除
#View(cs_training[which(cs_training$x2==0),])
cs_training<-cs_training[-which(cs_training$x2==0),]
boxplot(cs_training$x5)

#绘制x3,x7,x9三个变量的箱线图
boxplot(cs_training$x3,cs_training$x7,cs_training$x9)
#对x3,x7,x9变量定量分析
unique(cs_training$x3)
unique(cs_training$x7)
unique(cs_training$x9)
#去掉异常值96和98
#View(cs_training[which(cs_training$x3==96),])
#View(cs_training[which(cs_training$x3==98),])
#因为有96和98值的x3、x7、x9是在同一行，所以动一个变量即可
cs_training<-cs_training[-which(cs_training$x3==96),]
cs_training<-cs_training[-which(cs_training$x3==98),]
#三、变量分析
#3.1、单变量分析
#x2(age)分布
ggplot(cs_training, aes(x = x2, y = ..density..)) + 
  geom_histogram(fill = "blue", colour = "grey60",
                 size = 0.2, alpha = 0.2) + geom_density()
#x5(MonthlyIncome)分布
ggplot(cs_training, aes(x = x5, y = ..density..)) + 
  geom_histogram(fill = "blue", colour = "grey60", 
                 size = 0.2, alpha = 0.2) + geom_density() + 
  xlim(1, 20000)
#3.2、变量之间的相关性
cor1<-cor(cs_training[,1:11])
corrplot(cor1,method = "number")
#四、切分数据集
table(cs_training$y)

#4.1采用SMOTE算法，用R对稀有事件进行超级采样
set.seed(1234) 
splitIndex<-createDataPartition(cs_training$y,time=1,
                                p=0.5,list=FALSE) 
train<-cs_training[splitIndex,] 
test<-cs_training[-splitIndex,] 
prop.table(table(train$y))
prop.table(table(test$y))
#五、Logistic回归
#5.1基本公式
#5.2建立模型
fit<-glm(y~.,train,family = "binomial")
summary(fit)
#直接剔除未通过验证的x1、x4、x6，利用剩余的变量对y进行回归
fit2<-glm(y~x2+x3+x5+x7+x8+x9+x10,train,family = "binomial")
summary(fit2)
#5.3模型评估
#利用模型对test数据进行预测，生成概率预测值
pre <- predict(fit2,test)
#量化模型预测效果
modelroc <- roc(test$y,pre)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
#六、WOE转换
#6.1、进行分箱
#age
cutx2= c(-Inf,30,35,40,45,50,55,60,65,75,Inf)
plot(cut(train$x2,cutx2))
#NumberOfTime30-59DaysPastDueNotWorse变量(x3)：
cutx3 = c(-Inf,0,1,3,5,Inf)
plot(cut(train$x3,cutx3))
#MonthlyIncome变量(x5)：
cutx5 = c(-Inf,1000,2000,3000,4000,5000,6000,7500,9500,12000,Inf)
plot(cut(train$x5,cutx5))
#NumberOfOpenCreditLinesAndLoans变量(x6)：
cutx6 = c(-Inf,0,5,8,11,58,Inf)
plot(cut(train$x6,cutx6))
#NumberOfTimes90DaysLate变量(x7)：
cutx7 = c(-Inf,0,1,3,5,10,Inf)
plot(cut(train$x7,cutx7))
#NumberRealEstateLoansOrLines变量(x8)：
cutx8= c(-Inf,0,1,2,3,5,Inf)
plot(cut(train$x8,cutx8))
#NumberOfTime60-89DaysPastDueNotWorse变量(x9)：
cutx9 = c(-Inf,0,1,3,5,Inf)
plot(cut(train$x9,cutx9))
#NumberOfDependents变量(x10)：
cutx10 = c(-Inf,0,1,2,3,5,Inf)
plot(cut(train$x10,cutx10))
#6.2、计算WOE值
source("woeCal.R")

#七、评分卡的创建和实施
#逻辑回归建模：
#因为数据中“1”代表的是违约，直接建模预测，求的是“发生违约的概率”，
#log(odds)即为“坏好比”。为了符合常规理解，分数越高，信用越好，
#所有就调换“0”和“1”，使建模预测结果为“不发生违约的概率”，
#最后log(odds)即表示为“好坏比”。
trainWOE<-train
trainWOE$y = 1-train$y
glm.fit = glm(y~x2+x3+x5+x7+x8+x9+x10,data = trainWOE,
              family = binomial(link = logit))
summary(glm.fit)
coe = (glm.fit$coefficients)
coe
# 下面开始设立评分，假设按好坏比15为600分，
# 每高20分好坏比翻一倍算出factor,offset。如果后期结果不明显，
# 可以高30-50分好坏比才翻一倍。
#Score = offset + factor * log(odds)

620 = offset + factor * log(15*2,base = 10)
600 = offset + factor * log(15)

factor <- 20/(log(30,base = 10)-log(15,base = 10))
offset <- 600-factor*log(15,base = 10)
#截距项a
a<-log(totalgood/totalbad,base = 10)
#个人总评分=基础分+各部分得分
#基础分为:
baseScore <- a*factor+offset

#对各变量进行打分
#age变量(x2)
Agelessthan30.SCORE = factor*as.numeric(coe[2])*Agelessthan30.WOE
Age30to35.SCORE = factor*as.numeric(coe[2])*Age30to35.WOE
Age35to40.SCORE = factor*as.numeric(coe[2])*Age35to40.WOE
Age40to45.SCORE = factor*as.numeric(coe[2])*Age40to45.WOE
Age45to50.SCORE = factor*as.numeric(coe[2])*Age45to50.WOE
Age50to55.SCORE = factor*as.numeric(coe[2])*Age50to55.WOE
Age55to60.SCORE = factor*as.numeric(coe[2])*Age55to60.WOE
Age60to65.SCORE = factor*as.numeric(coe[2])*Age60to65.WOE
Age65to75.SCORE = factor*as.numeric(coe[2])*Age65to75.WOE
Agemorethan.SCORE=factor*as.numeric(coe[2])*Agemorethan.WOE

Age.SCORE =c(Age30to35.SCORE,Age35to40.SCORE,Age40to45.SCORE,Age45to50.SCORE,Age50to55.SCORE,Age55to60.SCORE,Age60to65.SCORE,Age65to75.SCORE,Agemorethan.SCORE)
Age.SCORE
#构造计算分值函数：
getscore<-function(i,x){
  score = round(factor*as.numeric(coe[i])*x,0)
  return(score)
}

#7.2计算各变量分箱得分
source("variableScoreCal.R")
#7.3最终生成的评分卡
creditCard<-data.frame(Age.SCORE, PastDue.SCORE, 
                             MonthIncome.SCORE, Days90sPastDue.SCORE,
                             RealEstate.SCORE, Days60.89PastDue.SCORE, 
                             Dependents.SCORE)

