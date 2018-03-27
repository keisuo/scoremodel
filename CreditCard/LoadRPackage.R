#载入需要的R包
library('grid')
library('DMwR')#knnImputation
library('corrplot')#corrplot
library('iterators')#迭代，caret依赖包
library('caret')#createDataPartition（数据分割功能）
library('pROC')#modelroc（用于分类器比较）
library('VIM') #matrixplot

library(InformationValue) # IV / WOE calculation
library(ggplot2)
library(GGally)
library(scales)
library(lattice)
library(MASS)
library(memisc)
library(Rcpp)#使用c++语言
library(Amelia)#有缺失值绘图的函数
library(gridExtra)
library(tidyr)
library(mice)
library(dplyr)
library(stringr)
library(splines) #数据差值包
library('randomForest') # 分类算法
library("rpart")
library('ggthemes') # 可视化
