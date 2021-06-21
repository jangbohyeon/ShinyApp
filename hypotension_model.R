setwd("C:/")
- 데이터 불러오기
#R에서 csv 파일을 불러와, [] 제거 후, ,로 데이터 나눠 csv로 저장
#저장된 csv 파일에서 필요한 데이터만 가져와 저장한다. 
#데이터 불러오기 (불러온 예시)
slp04 <- read.csv("slp04.csv")
slp04 <- slp04[-c(1,2),]
slp04 <- slp04[,c(4,5)]
slp04 <- slp04$signal

#[] 제거
slp04 <-substr(slp04, 2, nchar(slp04)-1)
#,로 데이터 분리하는 동시에 list 형태를 풀고 csv로 저장
a1<- matrix(unlist(strsplit(slp04, split=",")),nrow=5400000,byrow=T)
write.csv(a1,file='C:/Users/hansung/Desktop/slp04.csv')

#하나의 파일로 만든 데이터 불러오기
slp <-read.csv("slp.csv")

#불러온 데이터 중 불필요한 데이터 제외 후, class 구하기 (저혈압)
SRATE<-250
MINUTES_AHEAD<-1
Data_set<-list() # 샘플 생성후 저장할 공간

i <- 1
IBP_data<-data.frame()
IBP<-slp

while (i < length(IBP) - SRATE*(1+1+MINUTES_AHEAD)*60){
  segx <- IBP[i:(i+SRATE*1*60-1)]
  segy <- IBP[(i+SRATE*(1+MINUTES_AHEAD)*60):(i+SRATE*(1+1+MINUTES_AHEAD)*60-1)]
  segxd <- IBP[i:(i+SRATE*(1+MINUTES_AHEAD)*60-1)]
  if(is.na(mean(segx)) |
     is.na(mean(segy)) |
     max(segx)>200 | min(segx)<20 |
     max(segy)>200 | max(segy)<20 |
     max(segx) - min(segx) < 30 |
     max(segy) - min(segy) < 30|(min(segxd,na.rm=T) <= 50)){
  }
  else{ #나머지의 경우
    # segy <- ma(segy, 2*SRATE)
    event <- ifelse(min(segy,na.rm=T) <= 50, 1, 0)
    print(event)
    IBP_data<- rbind(IBP_data, cbind(t(segx), event))
  }
  
  i <- i+1*60*SRATE
}

#하나의 데이터로 합치기
slp_data<-rbind(Data_set[[1]], Data_set[[2]], Data_set[[3]], Data_set[[4]], Data_set[[5]],
                Data_set[[6]], Data_set[[7]], Data_set[[8]], Data_set[[9]], Data_set[[10]])

#class 와 데이터 나누기
#class 따로 설정, slp_data데이터에 class 제거
class <- slp_data[,15001]
slp_data <- slp_data[,1:15000]

#변수 생성하기
#함수만들기
rss <- function(x){
  rms(x)*(length(x))^0.5
}
skewness <- function(x){
  (sum((x-mean(x))^3)/length(x))/((sum((x-mean(x))^2)/length(x)))^(3/2)
}
cv <- function(x){
  sd(x)/mean(x)
}

#변수
data_min <- apply(slp_data ,1, min)
data_sd <- apply(slp_data ,1, sd)
data_mean <- apply(slp_data ,1,mean)
data_max <- apply(slp_data ,1,max)
data_skewness <- apply(slp_data ,1,skewness)
data_rss <- apply(slp_data ,1,rss)
data_rms <- apply(slp_data ,1,rms)
data_kurtosis <- apply(slp_data ,1,kurtosis)
data_IQR <- apply(slp_data ,1,IQR)

#변수 데이터 하나로 합치기
slp_data <- cbind(data_mean, data_min, data_max, data_sd, data_skewness, data_rss, data_rms, data_kurtosis, data_IQR)
#모델 학습하기
slp_data : dataframe 형태로 만들기. 이때 class 변수와 합치기
slp_data <- cbind(slp_data,class)
summary(slp_data)
slp_data <- as.data.frame(slp_data)

#J48
Bayes_net <- make_Weka_classifier("weka/classifiers/bayes/BatesNet")
J48_model <- J48(as.factor(class)~., data=slp_data)
summary(J48_model)
 
#RF
RF <- make_Weka_classifier(("weka/classifiers/trees/RandomForest"))
RF_model <- RF(as.factor(class)~., data=slp_data)
summary(RF_model)
 
#XGB
library(xgboost)
#변수, label 나누기
#class데이터 따로 빼고 xgboost 모델을 구현할 수 있는 메트릭스 형태로 변경
slp_data_feature <- slp_data%>%select(-class) %>% data.matrix
str(slp_data_feature)
#class 는 이미 숫자형이라 변경안해도 됨
class <- slp_data$class
mode(class)

#데이터 train/test 로 분할 -> 분리한 train(train, label), test(test, label)를 각각 하나의 매트릭스로 만들기 
train_index <- caret::createDataPartition(y = slp_data$class, p = 0.70, list = FALSE)
train_data <- slp_data_feature[train_index, ]
train_label <- class[train_index[,1]]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

# test data 
test_data <- slp_data_feature[-train_index, ]
test_label <- class[-train_index[,1]]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

#xgboost 모델 사용, 10-fold 사용, 필요한 파라메터 입력
#eta : 학습단계별 가중치를 어느정도 적용할지 0~1사이 입력
#max_path : 의사결정나무 마지막 깊이
#num_class : label 개수 몇개인지
#objective : 목적
#nfold : train과 validation을 몇개로 쪼갤지
#nround : iteration이 몇번 반복할 건지 
#verbose : 매 학습시 결과 출력 여부 T/F
#maximize : 결과값 최대화
params <- list("objective" = "multi:softprob", "num_class" = length(unique(train_label)),
               eta = 0.3, max_depth = 5)
model <- xgboost(params = params, data = train_matrix, nrounds = 100,
                 nfold = 10, verbose = F, maximize = T)
#모델 평가하기
#J48
e_J48 <- evaluate_Weka_classifier(J48_model, numFolds = 10, complexity = TRUE, class = TRUE)
e_J48
 
#RF
e_RF <- evaluate_Weka_classifier(RF_model, numFolds = 10, complexity = TRUE, class=TRUE)
e_RF
 
#XGB
pred <- predict(model, test_matrix)
#정답과 비교하기 위해, label 개수만큼 matrix 정렬 (가로 2개)
pred <- matrix(pred, ncol=length(unique(train_label)), byrow = TRUE)
#정답과 예측 결과 비교
fold_pred <- data.frame(pred) %>% mutate(predict = max.col(., ties.method = "last"),label = test_label + 1)

table(fold_pred$predict, fold_pred$label)

sum(fold_pred$predict != fold_pred$label) #0 errors

predict <- as.factor(fold_pred$predict)
actual <- as.factor(fold_pred$label) 

library(caret)
xgboost_pred <- confusionMatrix(predict, actual)
xgboost_pred
