#경로지정
setwd("C:/Users/hansung/Desktop/A_DeviceMotion_data/A_DeviceMotion_data")

#경로 확인
d <- getwd()
print(d)

#폴더에 들어갈거야
library(stringr)
fls <- dir(d, recursive = TRUE)

#경로안에 들어가서 /을 기준으로 나누고, csv를 읽어 temp에 저장하고 f에 붙일거야
for(f in fls){
  a <- file.path(str_c(d, "/", f))
  temp <- read.csv(a)
  assign(f, temp)
}

#sqrt 센서 크기 구하는 함수를 만들거야 
mag <- function(df, column){
  df[, str_c("mag", column)] <- with(df, sqrt(get(str_c(column, ".x"))^2 + 
                                                get(str_c(column, ".y"))^2 + 
                                                get(str_c(column, ".z"))^2))
  return(df)
}

#객체명 d의 객체를 f에 입력해서 함수에 대입할거야
for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
  assign(d,f)
}

#패키지 불러와~
library(pracma)

Peak_rslt<-data.frame()
#피크값을 가져올건데, 이때 dataframe 형태로 특징들을 뽑아 가져올거야.
#빈값인지 아닌지를 구분하고, 빈값이 아니라면 비교해서 구해야하는 변수라면 
#두개이상인지 확인을 하고 diff함수를 이용하여 차이를 구할 거야. 
Peak_rslt_R <- data.frame()
#회전센서입력
for(d in fls){
  f<-get(d)
  p<-findpeaks(f$magrotationRate, threshold = 5)
  Peak_rslt_R<-rbind(Peak_rslt_R, data.frame(d,
                                         f_R_n=ifelse(!is.null(p), dim(p)[1],0),
                                         p_R_interval=ifelse(!is.null(p), ifelse(dim(p)[1]>2,mean(diff(p[,2])),0),0),
                                         p_R_interval_std=ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(diff(p[,2])),0),0),
                                         p_R_mean=ifelse(!is.null(p),mean(p[,1]),0),
                                         p_R_max=ifelse(!is.null(p),max(p[,1]),0),
                                         p_R_min=ifelse(!is.null(p),max(p[,1]),0),
                                         p_R_std=ifelse(!is.null(p),std(p[,1]),0)))}
Peak_rslt_A <- data.frame()
#가속도 센서입력
for(d in fls){
  f<-get(d)
  p<-findpeaks(f$maguserAcceleration, threshold = 5)
  Peak_rslt_A<-rbind(Peak_rslt_A, data.frame(d,
                                         f_A_n=ifelse(!is.null(p), dim(p)[1],0),
                                         p_A_interval=ifelse(!is.null(p), ifelse(dim(p)[1]>2,mean(diff(p[,2])),0),0),
                                         p_A_interval_std=ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(diff(p[,2])),0),0),
                                         p_A_mean=ifelse(!is.null(p),mean(p[,1]),0),
                                         p_A_max=ifelse(!is.null(p),max(p[,1]),0),
                                         p_A_min=ifelse(!is.null(p),max(p[,1]),0),
                                         p_A_std=ifelse(!is.null(p),std(p[,1]),0)))}


#잘들어왔는지 값 확인하기
Peak_rslt_R
Peak_rslt_A

Peak_rslt <- merge(Peak_rslt_R, Peak_rslt_A, by="d")
Peak_rslt

summary(Peak_rslt)
#파일의 첫번째 객체값 읽어오기
temp<-get(fls[1])

#시각화
plot(temp$magrotationRate)
plot(1:length(temp$magrotationRate),temp$magrotationRate,'l')
#기준점은 내마음대로~
p_temp <- findpeaks(temp$magrotationRate, threshold = 5)
points(p_temp[,2],p_temp[,1])

#파고율 : 신호, 1초에 몇번 기록, 시각화 할거야?
seewave::crest(temp$magrotationRate,50, plot=TRUE)
temp <- data.frame()

#가속도와 회전센서의 파고율을 포문을 이용해서 가져올거야
#이때, 신호데이터를 기록한 사람(생성자)가 정보를 기록할거야 *Hz인지.
#그러면 저 Hz 값을 보고 1초에 몇 번 샘플링 프리퀀시(빈도수)인지 이해하면 됨.
for(d in fls){
  f <-get(d)
  f <- f%>%select(magrotationRate, maguserAcceleration)
  cfR <- crest(f$magrotationRate,50,plot=TRUE)
  cfA <- crest(f$maguserAcceleration,50,plot=TRUE)
  temp <- rbind(temp,data.frame(d, cfR=cfR$C, cfA=cfA$C))
}

#경로를 조건으로 두 데이터(변수 6개, 변수 2개) 머지할거야
#피크의 특징 6 + 회전센서와 가속도센서의 파고율 2
peak_final <- merge(Peak_rslt, temp, by="d")
head(peak_final)

#데이터 정보 추출
id_f <- function(x){
  exp_no=unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[2]
  activity=unlist(str_split(x,"\\_"))[1]
  return(cbind(exp_no, id, activity))
}

temp <- data.frame()
#정보를 반폭해서 추출하고 temp 에 합칠거야
for(i in 1:nrow(peak_final)){
  temp <- rbind(temp, id_f(peak_final$d[i]))
}
#피크데이터와 정보데이터를 합칠거야
peak_final2 <- cbind(peak_final,temp)
str(peak_final2)
summary(peak_final2)

#학습하기전에 필요없는 변수 제거
activity_peak <- peak_final2%>%ungroup()%>%select(-d,-exp_no,-id)
table(activity_peak$activity)#데이터의 치우침?이 크면 데이터 업샘플링, 다운 샘플링을 해야함.

#학습
library(RWeka)
RF<-make_Weka_classifier("weka/classifiers/trees/RandomForest")
#활동센서값은 팩터형/요소형이어야 함.
#앙상블 기법 중 랜덤포레스트 기법을 사용하여 학습 
m_peak_RF <- RF(as.factor(activity)~., data=activity_peak)
#학습모델 평가
summary(m_peak_RF)

#학습 평가
e_peak_RF <- evaluate_Weka_classifier(m_peak_RF, numFolds = 10, complexity = TRUE, class=TRUE)
e_peak_RF


save.image("peak_total.RData")
