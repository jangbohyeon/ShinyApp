setwd('C:/Users/hansung/Desktop/저장')
getwd()
load("sensor_peak_total.RData")

#peak_final2 : 피크 전체데이터
#HAR_total 데이터를 이용하여 통계특징 추출하기 HAR_summary_extend
#ch_pt : 변화 전체 데이터

#센서와 하토탈 합쳐 ==> 모델링 => 정확도 추출 => word 작성
#전체 데이터

HAR_summary_extend <- HAR_total %>% group_by(d, id, exp_no, activity) %>% 
  summarize_at(.vars=c("maguserAcceleration", "magrotationRate"),.funs = c(mean,min,max,sd,skewness,rms,rss,IQR,e1071::kurtosis))

for(d in fls){
  f <- get(d)
  f <- f%>%select(magrotationRate, maguserAcceleration)
  cfR <- crest(f$magrotationRate,50,plot=TRUE)
  cfA <- crest(f$maguserAcceleration,50,plot=TRUE)
  temp <- rbind(temp,data.frame(d, cfR=cfR$C, cfA=cfA$C))
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
#불필요한 변수 제거
summary <- HAR_summary_extend %>% ungroup() %>% select(-id, -exp_no)
peak <- peak_final2%>%ungroup()%>%select(-exp_no,-id)
changepoint <- ch_pt %>% ungroup() %>% select(-exp_no, -id)
summary(summary)

summary_peak_data <- merge(summary, peak, by=c("d","activity"))
summary_peak_data <- summary_peak_data %>% select(-d) 
summary_peak_changepoint <- merge(summary_peak_data, changepoint, by=c("d","activity"))
summary_peak_changepoint <- summary_peak_changepoint %>% select(-d) 


#학습
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
total_model <- RF(as.factor(activity)~., data=summary_peak_changepoint)
summary(total_model)
#평가
e_ch <- evaluate_Weka_classifier(total_model, numFolds = 10, complexity = TRUE, class=TRUE)
e_ch

