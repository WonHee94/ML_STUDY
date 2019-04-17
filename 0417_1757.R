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


#train <- read.csv("C:/Users/MSI/Desktop/kaggle/2ndML_KagleKorea/train.csv", stringsAsFactors = F)
#test <- read.csv("C:/Users/MSI/Desktop/kaggle/2ndML_KagleKorea/test.csv", stringsAsFactors = F)

train <- read.csv("C:/Users/ezen-test/Desktop/train.csv", stringsAsFactors = F)
test <- read.csv("C:/Users/ezen-test/Desktop/test.csv", stringsAsFactors = F)


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

ggplot(imp_DF[1:21,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE))+ geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")



#-----------------의미없음---------------------
s1 <- ggplot(data= all, aes(x=yr_built)) +
  geom_density() + labs(x='지어진 년도')
s2 <- ggplot(data= all, aes(x=sqft_living)) +
  geom_density() + labs(x='주거공간 면적')
s3 <- ggplot(data= all, aes(x=sqft_above)) +
  geom_density() + labs(x='지하실 제외 면적')
s4 <- ggplot(data=all, aes(x=as.factor(grade))) +
  geom_histogram(stat='count') + labs(x='등급')
s5 <- ggplot(data= all, aes(x=sqft_living15)) +
  geom_density() + labs(x='주거공간 면적15')
s6 <- ggplot(data= all, aes(x=sqft_basement)) +
  geom_density() + labs(x='지하실 면적')
s7 <- ggplot(data= all, aes(x=sqft_lot)) +
  geom_density() + labs(x='부지면적')
s8 <- ggplot(data= all, aes(x=sqft_lot)) +
  geom_density() + labs(x='부지면적15')

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)


q1 <- ggplot(data=all, aes(x=as.factor(condition))) +
  geom_histogram(stat='count')
q2 <- ggplot(data=all, aes(x=as.factor(waterfront))) +
  geom_histogram(stat='count')
q3 <- ggplot(data=all, aes(x=as.factor(view))) +
  geom_histogram(stat='count')
layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
multiplot(q1, q2, q3, layout=layout)

#----------------------------------------------------------------------

summary(all$bathrooms)


tb1 <- ggplot(data=all[!is.na(all$price),], aes(x=as.factor(bathrooms), y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 8000000, by=1000000), labels = comma)
tb2 <- ggplot(data=all, aes(x=as.factor(bathrooms))) +
  geom_histogram(stat='count')
grid.arrange(tb1, tb2)

rall<-subset(all,select=c(price,yr_built,yr_renovated,sqft_living,sqft_living15,sqft_lot,sqft_lot15))
View(rall)        

#sqftliving = sqftabove + sqftbasement.=sum
x<-subset(all,select=c(sqft_above,sqft_basement))
x$sum<-apply(x,1,sum)
View(x)
x<-cbind(x,all$sqft_living,all$sqft_living15)
x<-cbind(all$zipcode,x)
ox<-subset(x,sum==all$sqft_living)
View(ox)  #이상치들 없음
#이해가 안가는게 sqftliving와 sum값은 동일함 하지만 sqftliving15값과는 차이가 있음
#리모델링 하지 않은 집 조차 이 값이 달라진다는 건 무슨 뜻인가? 피트를 재는 방식이 달라졌다? 말이 안됨
#디스커션에 올라오는 것 처럼 15가 들어간 값들은 주변 평균으로 보는게 좋지 않을까?
#..그럼 oy에대한 것도 다시해야함...

#주변평균이면.. zipcode순서로 나열해서 평균값이 비슷한지 확인해보자
all$zipcode <-order(all$zipcode)
View(all)

#sqftliving/floors<sqftlot
y<-subset(all,select=c(zipcode,sqft_living,sqft_living15,floors))
y$d<-y$sqft_living/y$floors 
y<-cbind(y,all$sqft_lot)
y$d15<-y$sqft_living15/y$floors
y<-cbind(y,all$sqft_lot15)
View(y)
oy1<-subset(y,d>y$`all$sqft_lot`)
oy1$m<-(oy1$d-oy1$`all$sqft_lot`)
oy2<-subset(y,d15>y$`all$sqft_lot15`)
oy2$m<-(oy2$d15-oy2$`all$sqft_lot15`)
oy<-rbind(oy1,oy2)
View(oy)
nrow(oy)#이상치들이 존재
summary(oy$m)

str(all)
all$yr_renovated2<-ifelse(all$yr_renovated == 0,all$yr_built,all$yr_renovated)
View(all)
all$year<-as.numeric(all$year)
all$year<-ifelse(year==1,2014,2015)

all$Remod <- ifelse(all$yr_built==all$yr_renovated2, 0, 1) #0=No Remodeling, 1=Remodeling
all$Age <- (all$year-all$yr_renovated2)
ggplot(data=all[!is.na(all$price),], aes(x=Age, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 8000000, by=1000000), labels = comma)

x <- all[c("price","Age")]
cor(x,  y=NULL, use="complete.obs", method=c("pearson"))

cor(all$price[!is.na(all$price)], all$Age[!is.na(all$price)])
#큰 차이는 없지만 오래된 집들은 가격이 그리 높지 않다. 상관관계가 너무 약함


ggplot(all[!is.na(all$price),], aes(x=as.factor(Remod), y=price)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 8000000, by=500000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=450000, linetype="dashed") 

all$IsNew <- ifelse(all$year==all$yr_built, 1, 0)
table(all$IsNew)
#새 집이 430개


ggplot(all[!is.na(all$price),], aes(x=as.factor(IsNew), y=price)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 8000000, by=500000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=450000, linetype="dashed") 

all$year <- as.factor(all$year) 
str(all)

#총 거주공간
ggplot(data=all[!is.na(all$price),], aes(x=sqft_living, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 8000000, by=1000000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$sqft_living[!is.na(all$price)]>10000, rownames(all), '')))

#10000이상 8913 // 2860 1942 12922
cor(all$price, all$sqft_living, use= "pairwise.complete.obs")
cor(all$price[-c(8913,2860,1942,12922)], all$sqft_living[-c(8913,2860,1942,12922)], use= "pairwise.complete.obs")
#cor(all$price[-c(6470,2779,5109,8913,2860,1942,12922)], all$sqft_living[-c(6470,2779,5109,8913,2860,1942,12922)], use= "pairwise.complete.obs")







