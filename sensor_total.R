#경로지정
setwd("C:/")

#경로 확인
d <- getwd()
print(d)

#경로불러오기
fls <- dir(d, recursive = TRUE)
fls

#경로안에 존재하는 파일 불러오기
for(f in fls){
  a <- file.path(str_c(d, "/", f))
  temp <- read.csv(a)
  assign(f, temp)
}

print(fls)
#불러온 csv 파일중 하나 읽어오기
summary(`wlk_15/sub_1.csv`)

#mag함수설정
mag <- function(df, column){
  df[, str_c("mag", column)] <- with(df, sqrt(get(str_c(column,".x"))^2 + get(str_c(column, ".y"))^2 + get(str_c(column, ".z"))^2))
  return(df)
}

#가속도센서 
temp <- mag(`wlk_15/sub_1.csv`, "userAcceleration")
colnames(temp)

#회전센서
ration <- mag(`wlk_15/sub_1.csv`, "rotationRate")  
colnames(ration)  

#해당 파일 불러오기
fls

#피실험자 1번만 불러오기
user1 <- fls[str_detect(fls,"sub_1.csv")]
#user1 <- fls[str_detect(fls,"sub_1\\.")]

#걷기
user1_walking <- user1[str_detect(user1,"wlk")]

user1_walking_total <- data.frame()
#정보 추출
for(f in user1_walking){
  temp <- get(f)
  print(f)
  user1_walking_total <- rbind(user1_walking_total, temp %>% 
                                 mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[1],
                                        id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[2]))
}
user1_walking_total
#회전 센서
user1_walking_total <- mag(user1_walking_total, "userAcceleration")
#실험자 번호로 그룹핑하기
user1_walking_total <- user1_walking_total%>%group_by(exp_no) %>%mutate(time=row_number())%>%ungroup()
#시각화
ggplot(user1_walking_total, aes(x=time, y=maguserAcceleration))+
  geom_line() + facet_wrap(.~exp_no,nrow=3)

#조깅
user1_jogging <- user1[str_detect(user1,"jog")]

user1_jogging_total <- data.frame()
for(f in user1_jogging){
  temp <- get(f)
  print(f)
  user1_jogging_total <- rbind(user1_jogging_total, temp %>% 
                                 mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[1],
                                        id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[2]))
}
user1_jogging_total
#회전센서
user1_jogging_total <- mag(user1_jogging_total, "userAcceleration")
#실험자번호로 그룹핑하기
user1_jogging_total <- user1_jogging_total%>%group_by(exp_no) %>%mutate(time=row_number())%>%ungroup()
#시각화
ggplot(user1_jogging_total, aes(x=time, y=maguserAcceleration))+
  geom_line() + facet_wrap(.~exp_no,nrow=3)

#전체 데이터
HAR_total <- data.frame()
#데이터가 전체 다 들어왔는지 확인할 것
fls
#해당 조건에 맞게 데이터의 정보를 추출하고, 몇번 시행했는지 확인할 것.,
i=1

for(f in fls){
#  print(i) #몇번 시행했는지 확인해야함
#  i <- i+1
  temp <- get(f)
#  print(f) #추출이 잘되는지, 확인
  HAR_total <- rbind(HAR_total, temp %>% mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[1],
                                                id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[2],
                                                activity=unlist(str_split(f,"\\_"))[1]))
}
#상위 백개만 추출
head(HAR_total, n=100)

#mag함수설정
mag <- function(df, column){
  df[, str_c("mag", column)] <- with(df, sqrt(get(str_c(column,".x"))^2 + get(str_c(column, ".y"))^2 + get(str_c(column, ".z"))^2))
  return(df)
}

#mag 함수를 이용해서 회전센서, 가속도 센서 구하기
HAR_total <- mag(HAR_total, "userAcceleration")
HAR_total <- mag(HAR_total, "rotationRate")

#왜도 구하기
skewness <- function(x){
  (sum((x-mean(x))^3)/length(x))/((sum((x-mean(x))^2)/length(x)))^(3/2)
}

#실험자 번호, 피실험자번호, 센서값으로 그룹핑을 한다.
#두가지 센서값을 통계량으로 계산
HAR_summary <- HAR_total %>% group_by(id, exp_no,activity) %>%
  summarise_at(.vars=c("maguserAcceleration", "magrotationRate"),.funs=c(mean, min, max, sd, skewness))

#통계 특징 추출 데이터 결과 확인
HAR_summary

#모델학습 지정
RF <- make_Weka_classifier(("weka/classifiers/trees/RandomForest"))
Bayes_net <- make_Weka_classifier("weka/classifiers/bayes/BatesNet")

#학습하기 전, 센서값은 팩터형으로 변경할 것.
HAR_summary$activity <- as.factor(HAR_summary$activity)
#그룹핑한것 풀 것.
activity <- HAR_summary %>% ungroup() %>% 
  select(c(colnames(HAR_summary)[str_detect(colnames(HAR_summary),"mag")],"activity"))

#학습하기
m_J48 <- J48(activity~., data=activity)
m_RF <- RF(activity~., data=activity)

#학습 데이터 성능 보기
summary(m_J48)
summary(m_RF)

#평가하기
e_J48 <- evaluate_Weka_classifier(m_J48, numFolds = 10, complexity = TRUE, class = TRUE)
e_RF <- evaluate_Weka_classifier(m_RF, numFolds = 10, complexity = TRUE, class = TRUE)

#개인정보 데이터와 결합시키기
#이때, 센서데이터 경로와 다른 위치에 정보 데이터 저장함.
#같은 공간에 저장하게 되면, 다음에 경로 지정에서 경로 가져오고 데이터 추출할때, 문제생김
setwd('C:/Users/hansung/Desktop/저장')
getwd()

#csv 데이터 가져오기
info <- read.csv("info.csv")
#불필요한 데이터 지우기
info <- info[-c(2:4)]

#전체데이터와 합칠건데, merge를 이용해서 할거야. 그러므로, 조건에 맞는 변수 설정
#첫번째 데이터 x 두번째데이터 y 로, 두변수명은 다르지만 같은거야
#all : 한쪽 데이터에 추가되는 것이아닌, 전체 합친다는 의미
HAR_total1 <- merge(HAR_summary, info, x.by="id",y.by="code", all=TRUE)
head(HAR_total1)

#성별 팩터형으로 변경
HAR_total1$gender <- as.factor(HAR_total1$gender)
#그룹화 된거 풀고, 성별에 맞게 데이터 만들음 11개 변수 임. 
activity_gender <- HAR_total1 %>% ungroup() %>% 
  select(c(colnames(HAR_total1)[str_detect(colnames(HAR_total1),"mag")],"gender"))

#센서값 기준으로 학습말고, 성별기준으로 학습시키기
m_gender_J48 <- J48(gender~., data=activity_gender)
m_gender_RF <- RF(gender~., data=activity_gender)

#학습 평가
e_gender_J48 <- evaluate_Weka_classifier(m_gender_J48, numFolds = 10, complexity = TRUE, class = TRUE)
e_gender_RF <- evaluate_Weka_classifier(m_gender_RF, numFolds = 10, complexity = TRUE, class = TRUE)

#RSS 값 구하기 : RMS*N
rss <- function(x){
  rms(x)*(length(x))^0.5
}

#전체데이터에서 실험자번호, 피실험자번호, 센서 로 그룹화함
#두가시 센서를 통계값들로 요약함 summary_at
HAR_summary_extend <- HAR_total %>% group_by(id, exp_no, activity) %>% 
  summarize_at(.vars=c("maguserAcceleration", "magrotationRate"),.funs = c(mean,min,max,sd,skewness,rms,rss,IQR,e1071::kurtosis))

#21개 변수, 요약
summary(HAR_summary_extend)

#불필요한 변수 제거 id, exp_no
HAR_summary_extend2 <- HAR_summary_extend %>% ungroup() %>% select(-c("id","exp_no"))

#모델 학습
m_extend_J48 <- J48(as.factor(activity)~., data = HAR_summary_extend2)
summary(m_extend_J48)

#모델 평가
e_extend_J48 <- evaluate_Weka_classifier(m_extend_J48, numFolds = 10, complexity = TRUE, class = TRUE)
e_extend_J48


save.image("sensor_total.RData")
