setwd('C:/Users/hansung/Desktop/저장')
getwd()
load("summary_peak_changepoint.RData")

#변수, label 나누기
#불필요한 데이터파일경로, 동작센서 제거
data_feature <- summary_peak_changepoint%>%select(-d,-activity) %>% data.matrix
str(data_feature)
test <- summary_peak_changepoint$activity
#ifelse를 이용하여 숫자로 변경
label_n <- ifelse("dws"==test,0,ifelse("jog"==test,1,ifelse("sit"==test,2,ifelse("std"==test,3,ifelse("ups"==test, 4,ifelse("wlk"==test,5,test))))))
label_n <- as.numeric(label_n)
#데이터 train/test 로 분할 -> 분리한 train(train, label), test(test, label)를 각각 하나의 매트릭스로 만들기 
train_index <- caret::createDataPartition(y = summary_peak_changepoint$activity, p = 0.70, list = FALSE)
train_data <- data_feature[train_index, ]
train_label <- label_n[train_index[,1]]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

# test data 
test_data <- data_feature[-train_index, ]
test_label <- label_n[-train_index[,1]]
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

#모델 학습 후, 평가
pred <- predict(model, test_matrix)
#정답과 비교하기 위해, label 개수만큼 matrix 정렬 (가로 6개)
pred <- matrix(pred, ncol=length(unique(train_label)), byrow = TRUE)
#정답과 예측 결과 비교
fold_pred <- data.frame(pred) %>% mutate(predict = max.col(., ties.method = "last"),label = test_label + 1)

table(fold_pred$predict, fold_pred$label)

sum(fold_pred$predict != fold_pred$label) #0 errors

predict <- as.factor(fold_pred$predict)
actual <- as.factor(fold_pred$label) 
library(caret)
xgboost_pred <- confusionMatrix(predict, actual)

#변수 중요도 확인
importance_feature <- xgb.plot.importance(importance_matrix = xgb.importance(colnames(train_matrix), model))

svm_model<-svm(train_label~., data=train_data)
b<-predict(svm_model, test_data)

confusionMatrix(as.factor(b),as.factor(test_label+1))


