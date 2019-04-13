#패키지설치
install.packages("knitr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plyr")
install.packages("corrplot")
install.packages("caret")
install.packages("gridExtra")
install.packages("scales")
install.packages("Rmisc")
install.packages("ggrepel")
install.packages("randomForest")
install.packages("psych")
install.packages("xgboost")
install.packages("dlookr")
install.packages("rapportools")
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(dlookr)
library(rapportools)


train <- read.csv("C:/Users/MSI/Desktop/kaggle/2ndML_KagleKorea/train.csv", stringsAsFactors = F)
test <- read.csv("C:/Users/MSI/Desktop/kaggle/2ndML_KagleKorea/test.csv", stringsAsFactors = F)

dim(train)
#15035 21
dim(test)
#6468 20

head(train)
# date:T000000지우기 앞의 8자리만 필요
# bathrooms: 화장실에 소수점... 0.25단위?(평인가?)
# yr_renovated:0은 재건축하지 않음 의미
# sqft_living 과 sqft_living15 : 같은 값이 있기도 다르기도... 차이값구해서 활용해보기

str(train[,c(1:10, 21)]) #display first 10 variables and the response variable
str(test)
str(train)
#Getting rid of the IDs but keeping the test IDs in a vector. These are needed to compose the submission file
#id필요없음.

test_labels <- test$id
test$id <- NULL
train$id <- NULL
test$price <- NA
all <- rbind(train, test)
dim(all)
str(all)

#결측치 확인하기
sum(is.na(all)) #6468
colSums(is.na(all))#price에만 na값이 들어있다.
find_na(all, rate = TRUE) #비율이 30.08% 무시할 수준은 아님 방법이 필요합니당



ggplot(data=all[!is.na(all$price),], aes(x=price)) +
  geom_histogram(fill="blue", binwidth = 50000) +
  scale_x_continuous(labels = comma)

summary(all$price)
#    Min.  1st Qu. Median   Mean   3rd Qu.    Max.     NA's 
#   78000  322000  450000  540683  645000   7700000    6468 

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- all[, numericVars]#numeric변수 열추추
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))) #행단위로 적용 근데 모두 0.5이상임
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#grade
ggplot(data=all[!is.na(all$price),], aes(x=factor(grade), y=price))+
  geom_boxplot(col='blue') + labs(x='grade') +
  scale_y_continuous( labels = comma)
#4등급의 이상치.. 11등급 이상치 12등급 3등급 평균이크다

#sqft_living--------메모리부족으로 터짐
ggplot(data=all[!is.na(all$price),], aes(x=sqft_living, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous( labels = comma)+
  geom_text_repel(aes(label = ifelse(all$sqft_living[!is.na(all$price)]>7100, rownames(all), '')))

ggplot(data=all[!is.na(all$price),], aes(x=sqft_living, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous( labels = comma)+
  geom_text_repel(aes(label = ifelse(all$sqft_living[!is.na(all$price)]>11000, rownames(all), '')))

#10000이상 8913 // 2860 1942 12922

str(all)

all[c(8913,2860,1942,12922), c('price', 'sqft_living', 'condition', 'grade')]

#date변수에서 T000000
all$date<-substr(all$date,1,8)
str(all)
all$year<-substr(all$date,1,4)
all$month<-substr(all$date,5,6)
str(all)
Charcol <- names(all[,sapply(all, is.character)])
Charcol

#년도는 2년뿐... 그렇다면 월과 연도 모두 factor인게 맞지 않을까?
str(all$year)
all$month <- as.factor(all$month)
all$date<- as.integer(all$date)
all$year<-as.factor(all$year)
all$waterfront<-as.factor(all$waterfront)
str(all)

ys <- ggplot(all[!is.na(all$price),], aes(x=as.factor(year), y=price)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 8000000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(300000, 500000)) +
  geom_hline(yintercept=450000, linetype="dashed", color = "red") #dashed line is median SalePrice

ms <- ggplot(all[!is.na(all$price),], aes(x=month, y=price)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 8000000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(300000, 500000)) +
  geom_hline(yintercept=450000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(ys, ms, widths=c(1,2))

summary(all$price)
#별로 유의미한거 모르겠당... 3~7월에 평균이상의 가격, 2월이 가장 가격이 낮았다

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')


all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex =2,cl.cex =1, number.cex=1.5)


View(all)
str(all)
set.seed(2019)
quick_RF <- randomForest(x=all[1:15035,-2], y=all$SalePrice[1:15035], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE))
+ geom_bar(stat = 'identity') 
+ labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') 
+ coord_flip() + theme(legend.position="none")


s1 <- ggplot(data= all, aes(x=yr_built)) +
  geom_density() + labs(x='지어진 년도')
s2 <- ggplot(data= all, aes(x=sqft_living)) +
  geom_density() + labs(x='주거공간 면적')
s3 <- ggplot(data= all, aes(x=sqft_above)) +
  geom_density() + labs(x='지하실 제외 면적')
s4 <- ggplot(data=all, aes(x=as.factor(grade))) +
  geom_histogram(stat='count') + labs(x='등급')

s3 <- ggplot(data= all, aes(x=X1stFlrSF)) +
  geom_density() + labs(x='Square feet first floor')
s4 <- ggplot(data= all, aes(x=X2ndFlrSF)) +
  geom_density() + labs(x='Square feet second floor')
s5 <- ggplot(data= all, aes(x=TotalBsmtSF)) +
  geom_density() + labs(x='Square feet basement')
s6 <- ggplot(data= all[all$LotArea<100000,], aes(x=LotArea)) +
  geom_density() + labs(x='Square feet lot')
s7 <- ggplot(data= all, aes(x=LotFrontage)) +
  geom_density() + labs(x='Linear feet lot frontage')
s8 <- ggplot(data= all, aes(x=LowQualFinSF)) +
  geom_histogram() + labs(x='Low quality square feet 1st & 2nd')

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)