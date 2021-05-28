#경로지정
setwd("C:/Users/hansung/Desktop/A_DeviceMotion_data/A_DeviceMotion_data")

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

#mag함수설정
mag <- function(df, column){
  df[, str_c("mag", column)] <- with(df, sqrt(get(str_c(column,".x"))^2 + get(str_c(column, ".y"))^2 + get(str_c(column, ".z"))^2))
  return(df)
}

for(d in fls){
  f<-get(d)
  f<-mag(f,"rotationRate")
  f<-mag(f,"userAcceleration")
#  f<-mag(f, "gravity") 중력가속도는 0.9이니까 의미 없음
  assign(d,f)
}


statistics <- data.frame()
#통계적
for(d in fls){
  f <- get(d)
  statistics <- rbind(statistics, data.frame(d, f %>% mutate(exp_no=unlist(regmatches(d,gregexpr("[[:digit:]]+",d)))[1],
                                                   id=unlist(regmatches(d,gregexpr("[[:digit:]]+",d)))[2],
                                                   activity=unlist(str_split(d,"\\_"))[1])))
}


#센서값 없는 데이터의 공분산 및 pca 구하기
summary(statistics)
#불필요한 변수 제거
statistics <- statistics %>% select(-X, -exp_no, -id)
cov(statistics%>%select(-d,-activity))

mtcars_PCA <- prcomp(statistics%>%select(-d, -activity),center=TRUE, scale.=TRUE)
screeplot(mtcars_PCA, type="l")
biplot(mtcars_PCA)

statistics$activity <- as.factor(statistics$activity)
statistics_lm<-lm(activity~log(statistics[2:12]), data=statistics)
anova(statistics_lm)

summary(factor(statistics$activity))

statistics <- mag(statistics, "userAcceleration")
statistics <- mag(statistics, "rotationRate")
statistics <- mag(statistics, "gravity")

rss <- function(x){
  rms(x)*(length(x))^0.5
}
skewness <- function(x){
  (sum((x-mean(x))^3)/length(x))/((sum((x-mean(x))^2)/length(x)))^(3/2)
}

cv <- function(x){
  sd(x)/mean(x)
}
statistics_d <- statistics$d
statistics <- statistics%>% select(-d)
#실험자 번호, 피실험자번호, 센서값으로 그룹핑을 한다.
#두가지 센서값을 통계량으로 계산
statistics_summary <-data.frame()
statistics_summary <- statistics %>% group_by(d, id, exp_no, activity) %>%
  summarise_at(.vars=c("maguserAcceleration", "magrotationRate", "gravity.x","gravity.y","gravity.z"),.funs=c(mean, min, max, var,sd, skewness, cv, rss, rms, IQR, e1071::kurtosis))
#mean, min, max, sd, skewness, cv, rss, rms, IQR, e1071::kurtosis
#통계 특징 추출 데이터 결과 확인
statistics_summary
summary(statistics_summary)

summary_rslt
head(statistics_summary)
summary(statistics_summary)

colnames(statistics_summary)


summary_rslt <- statistics_summary %>% ungroup() %>% select(-id,-exp_no, -activity)
summary(summary_rslt)

summary_rslt_pca<-prcomp(summary_rslt,center=TRUE, scale.=TRUE)
summary_rslt_pca$rotation[,1:3]#pc1,pc2,pc3 추출
screeplot(summary_rslt_pca, type="l")
biplot(summary_rslt_pca)


Peak_rslt_R <- data.frame()
#회전센서입력
for(d in fls){
  f<-get(d)
  p<-findpeaks(f$magrotationRate, threshold = 5)
  cfR <- crest(f$magrotationRate,50,plot=TRUE)
  Peak_rslt_R<-rbind(Peak_rslt_R, data.frame(d, cfR=cfR$C,
                                             f_R_n=ifelse(!is.null(p), dim(p)[1],0),
                                             p_R_interval=ifelse(!is.null(p), ifelse(dim(p)[1]>2,mean(diff(p[,2])),0),0),
                                             p_R_interval_std=ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(diff(p[,2])),0),0),
                                             p_R_mean=ifelse(!is.null(p),mean(p[,1]),0),
                                             p_R_max=ifelse(!is.null(p),max(p[,1]),0),
                                             p_R_min=ifelse(!is.null(p),max(p[,1]),0),
                                             p_R_std=ifelse(!is.null(p),std(p[,1]),0)))}
summary(Peak_rslt_R)

Peak_rslt_A <- data.frame()
#가속도 센서입력
for(d in fls){
  f<-get(d)
  p<-findpeaks(f$maguserAcceleration, threshold = 5)
  cfA <- crest(f$maguserAcceleration,50,plot=TRUE)
  Peak_rslt_A<-rbind(Peak_rslt_A, data.frame(d,cfA=cfA$C,
                                             f_A_n=ifelse(!is.null(p), dim(p)[1],0),
                                             p_A_interval=ifelse(!is.null(p), ifelse(dim(p)[1]>2,mean(diff(p[,2])),0),0),
                                             p_A_interval_std=ifelse(!is.null(p),ifelse(dim(p)[1]>2,std(diff(p[,2])),0),0),
                                             p_A_mean=ifelse(!is.null(p),mean(p[,1]),0),
                                             p_A_max=ifelse(!is.null(p),max(p[,1]),0),
                                             p_A_min=ifelse(!is.null(p),max(p[,1]),0),
                                             p_A_std=ifelse(!is.null(p),std(p[,1]),0)))}

Peak_rslt <-data.frame()
Peak_rslt <- merge(Peak_rslt_R, Peak_rslt_A, by="d")

colnames(Peak_rslt)


Peak_rslt_G <- data.frame()
#중력 센서입력
for(d in fls){
  f<-get(d)
  p_x<-findpeaks(f$gravity.x, threshold = 5)
  p_y<-findpeaks(f$gravity.y, threshold = 5)
  p_z<-findpeaks(f$gravity.z, threshold = 5)
  cfG_x <- crest(f$gravity.x,50,plot=TRUE)
  cfG_y <- crest(f$gravity.y,50,plot=TRUE)
  cfG_z <- crest(f$gravity.z,50,plot=TRUE)
  Peak_rslt_G<-rbind(Peak_rslt_G, data.frame(d,cfG_x=cfG_x$C,cfG_y=cfG_y$C,cfG_z=cfG_z$C,
                                             f_G_x_n=ifelse(!is.null(p_x), dim(p_x)[1],0),
                                             p_G_x_interval=ifelse(!is.null(p_x), ifelse(dim(p_x)[1]>2,mean(diff(p_x[,2])),0),0),
                                             p_G_x_interval_std=ifelse(!is.null(p_x),ifelse(dim(p_x)[1]>2,std(diff(p_x[,2])),0),0),
                                             p_G_x_mean=ifelse(!is.null(p_x),mean(p_x[,1]),0),
                                             p_G_x_max=ifelse(!is.null(p_x),max(p_x[,1]),0),
                                             p_G_x_min=ifelse(!is.null(p_x),max(p_x[,1]),0),
                                             p_G_x_std=ifelse(!is.null(p_x),std(p_x[,1]),0),
                                             f_G_y_n=ifelse(!is.null(p_y), dim(p_y)[1],0),
                                             p_G_y_interval=ifelse(!is.null(p_y), ifelse(dim(p_y)[1]>2,mean(diff(p_y[,2])),0),0),
                                             p_G_y_interval_std=ifelse(!is.null(p_y),ifelse(dim(p_y)[1]>2,std(diff(p_y[,2])),0),0),
                                             p_G_y_mean=ifelse(!is.null(p_y),mean(p_y[,1]),0),
                                             p_G_y_max=ifelse(!is.null(p_y),max(p_y[,1]),0),
                                             p_G_y_min=ifelse(!is.null(p_y),max(p_y[,1]),0),
                                             p_G_y_std=ifelse(!is.null(p_y),std(p_y[,1]),0),
                                             f_G_z_n=ifelse(!is.null(p_z), dim(p_z)[1],0),
                                             p_G_z_interval=ifelse(!is.null(p_z), ifelse(dim(p_z)[1]>2,mean(diff(p_x[,2])),0),0),
                                             p_G_z_interval_std=ifelse(!is.null(p_z),ifelse(dim(p_x)[1]>2,std(diff(p_x[,2])),0),0),
                                             p_G_z_mean=ifelse(!is.null(p_z),mean(p_z[,1]),0),
                                             p_G_z_max=ifelse(!is.null(p_z),max(p_z[,1]),0),
                                             p_G_z_min=ifelse(!is.null(p_z),max(p_z[,1]),0),
                                             p_G_z_std=ifelse(!is.null(p_z),std(p_z[,1]),0)))}

#####아직안함
Peak_rslt <- merge(Peak_rslt, Peak_rslt_G, by="d")

colnames(Peak_rslt)

prcomp(Peak_rslt%>%select(-d),center=TRUE, scale.=TRUE)

#변화
ch_pt <- data.frame()
#변화 횟수 데이터프레임 만들기
for(d in fls){
  f <- get(d)
  rslt_mean <- sapply(f%>%select(magrotationRate, maguserAcceleration, gravity.x,gravity.y,gravity.z), cpt.mean)
  rslt1_cpt1 <- cpts(rslt_mean$magrotationRate)
  rslt1_cpt2 <- cpts(rslt_mean$maguserAcceleration)
  rslt1_cpt3 <- cpts(rslt_mean$gravity.x)
  rslt1_cpt4 <- cpts(rslt_mean$gravity.y)
  rslt1_cpt5 <- cpts(rslt_mean$gravity.z)
  rslt_var <- sapply(f%>%select(magrotationRate, maguserAcceleration, gravity.x,gravity.y,gravity.z), cpt.var)
  rslt2_cpt1 <- cpts(rslt_var$magrotationRate)
  rslt2_cpt2 <- cpts(rslt_var$maguserAcceleration)
  rslt2_cpt3 <- cpts(rslt_var$gravity.x)
  rslt2_cpt4 <- cpts(rslt_var$gravity.y)
  rslt2_cpt5 <- cpts(rslt_var$gravity.z)
  rslt_meanvar <- sapply(f%>%select(magrotationRate, maguserAcceleration,gravity.x,gravity.y,gravity.z), cpt.meanvar)
  rslt3_cpt1 <- cpts(rslt_meanvar$magrotationRate)
  rslt3_cpt2 <- cpts(rslt_meanvar$maguserAcceleration)
  rslt3_cpt3 <- cpts(rslt_meanvar$gravity.x)
  rslt3_cpt4 <- cpts(rslt_meanvar$gravity.y)
  rslt3_cpt5 <- cpts(rslt_meanvar$gravity.z)
  
  ch_pt <- rbind(ch_pt, data.frame(d, cp1=length(rslt1_cpt1),cp2=length(rslt1_cpt2),cp3=length(rslt1_cpt3),
                                   cp4=length(rslt1_cpt4),cp5=length(rslt1_cpt5),cp6=length(rslt2_cpt1),
                                   cp7=length(rslt2_cpt2),cp8=length(rslt2_cpt3),cp9=length(rslt2_cpt4),
                                   cp10=length(rslt2_cpt5),cp11=length(rslt3_cpt1),cp12=length(rslt3_cpt2),
                                   cp13=length(rslt3_cpt3),cp14=length(rslt3_cpt4),cp15=length(rslt3_cpt5)))
}

for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  rslt_mean <- sapply(f%>%select(magrotationRate, maguserAcceleration), cpt.mean)
  rslt1_cpt1 <- cpts(rslt_mean$magrotationRate)
  rslt1_cpt2 <- cpts(rslt_mean$maguserAcceleration)
  rslt_var <- sapply(f%>%select(magrotationRate, maguserAcceleration), cpt.var)
  rslt2_cpt1 <- cpts(rslt_var$magrotationRate)
  rslt2_cpt2 <- cpts(rslt_var$maguserAcceleration)
  rslt_meanvar <- sapply(f%>%select(magrotationRate, maguserAcceleration), cpt.meanvar)
  rslt3_cpt1 <- cpts(rslt_meanvar$magrotationRate)
  rslt3_cpt2 <- cpts(rslt_meanvar$maguserAcceleration)
  
  ch_pt <- rbind(ch_pt, data.frame(d, cp1=length(rslt1_cpt1),cp2=length(rslt1_cpt2),cp3=length(rslt2_cpt1),
                                   cp4=length(rslt2_cpt2),cp5=length(rslt3_cpt1),cp6=length(rslt3_cpt2)))
}



#,cp7=length(rslt3_cpt1),cp8=length(rslt3_cpt2),cp9=length(rslt3_cpt3
temp <- data.frame()

#정보추출
id_f<-function(x){
  exp_no<-unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[1]
  id<-unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[2]
  activity<-unlist(str_split(x,"\\_"))[1]
  return(cbind(exp_no,id,activity))
}

for(i in 1:nrow(ch_pt)){
  temp <- rbind(temp, id_f(ch_pt$d[i]))
}

#변화와 정보 데이터 합치기
ch_pt <- cbind(ch_pt, temp)
summary(ch_pt)

ch_pt <- ch_pt %>% ungroup() %>% select(-id, -exp_no)

colnames(ch_pt)

head(summary_rslt)
head(Peak_rslt)
head(ch_pt)

peak_changepoint <- merge(Peak_rslt, ch_pt, by="d")
summary_peak_changepoint <-data.frame()
summary_peak_changepoint <- merge(summary_rslt,peak_changepoint, by="d")
summary_peak_changepoint
summary(summary_peak_changepoint)


colnames(summary_rslt)
colnames(Peak_rslt)
colnames(ch_pt)

colnames(summary_peak_changepoint)

str(summary_peak_changepoint)

#pca
PCA <- data.frame()
for(d in fls){
  f <-get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  PCA <- rbind(PCA,data.frame(d,f %>% mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[1],
                                          id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)))[2],
                                          activity=unlist(str_split(f,"\\_"))[1])))
}



PCA[is.na(PCA)] <-0
length(PCA)
cor(PCA%>%select(-d,-activity))

str(summary_peak_changepoint)

total_PCA <- prcomp(summary_peak_changepoint %>%ungroup()%>%select(-activity),center=TRUE, scale.=TRUE)
screeplot(total_PCA, type="l")
summary_peak_changepoint
total_PCA
total_PCA<-total_PCA$rotation[,1:3]

#정보추출
id_f<-function(x){
  exp_no<-unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[1]
  id<-unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[2]
  activity<-unlist(str_split(x,"\\_"))[1]
  return(cbind(exp_no,id,activity))
}

temp <- data.frame()
for(i in 1:nrow(summary_peak_changepoint)){
  temp <- rbind(temp, id_f(summary_peak_changepoint$d[i]))
}


#변화와 정보 데이터 합치기
summary_peak_changepoint <- cbind(summary_peak_changepoint, temp)
summary(summary_peak_changepoint)
'
summary_peak_changepoint <- summary_peak_changepoint %>% ungroup() %>% select(-d)

RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
summary_peak_changepoint_model <- RF(as.factor(activity)~., data=summary_peak_changepoint)
summary(summary_peak_changepoint_model)


#평가
e_model <- evaluate_Weka_classifier(summary_peak_changepoint_model, numFolds = 10, complexity = TRUE, class=TRUE)
e_model


setwd('C:/Users/hansung/Desktop/저장')
getwd()


save.image("summary_peak_changepoint_model.RData")
load("summary_peak_changepoint_model.RData")

