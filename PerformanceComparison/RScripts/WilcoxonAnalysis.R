#Version:  August 8th 2021
#requires NewSpiders.R and start.R  

#Wilcoxon test 
#DEfault categories of values
weights<-c(0.147,0.33,0.474)
names(weights)<-c("negligible","small","medium")
#It returns the wicoxon, cliff delta and the magnitude of Cliff delta
effectSizeCompute<-function(set1,set2){
  size<-length(set1)*length(set2)
  #percentage at which set1 > set2; i.e. arch1 more scalable than arch2
  w<-wilcox.test(set1,set2)
  e_w<- round(w$statistic/size,2)
  w_pValue<-round(w$p.value,4)
  names(w_pValue)<-"W_P-value"
  cliff_w<-round(2*(w$statistic/size)-1,2)
  i<-min(which(abs(2*(w$statistic/size)-1)<=weights))
  names(e_w)<-"WilcoxonEffectSize"
  names(cliff_w)<-"Cliff"
  o_e_w<-names(weights)[i]
  if(abs(2*(w$statistic/size)-1)>0.474){o_e_w<-"large"}
  names(o_e_w)<-"magnitude"
  result<-c(e_w,w_pValue,cliff_w,o_e_w)
  return(result)
}
#temp file below is in NewSpiders.R
mv_result<-NULL
mv_result_e<-NULL
mv_result_a<-NULL
#pathData is a csv file that stores the operations that fail
for(pathData in c(pathData_I1_UB,pathData_I1_B, pathData_I2_UB, pathData_I2_B)){
  failedOperations<-as.data.frame(read.csv(pathData))
  mono_failingS<-failedOperations[grep("mono",failedOperations[,1]),]
  cqrs_failingS<-failedOperations[grep("cqrs",failedOperations[,1]),]
  role_failingS<-failedOperations[grep("role",failedOperations[,1]),]
  
  df_mono_temp<-NULL
  df_cqrs_temp<-NULL
  df_role_temp<-NULL
  #values and response times
  df_mono_temp<-compute_df(mono_failingS,operations)
  df_cqrs_temp<-compute_df(cqrs_failingS,operations)
  df_role_temp<-compute_df(role_failingS,operations)
  
  df_mono<-df_mono_temp
  df_cqrs<-df_cqrs_temp
  df_role<-df_role_temp
  for(i in 1:11){
    if(!length(grep(df_mono_temp[[1]][i],aggregatedValuesCum[,1]))==0){
      df_mono[[2]][i]<-aggregatedValuesCum[grep(df_mono_temp[[1]][i],aggregatedValuesCum[,1])-1,2]}else{
        df_mono[[2]][i]<-1.2}
    if(!length(grep(df_cqrs_temp[[1]][i],aggregatedValuesCum[,1]))==0){
      df_cqrs[[2]][i]<-aggregatedValuesCum[grep(df_cqrs_temp[[1]][i],aggregatedValuesCum[,1])-1,2]}else{df_cqrs[[2]][i]<-1.2}
    if(!length(grep(df_role_temp[[1]][i],aggregatedValuesCum[,1]))==0){
      df_role[[2]][i]<-aggregatedValuesCum[grep(df_role_temp[[1]][i],aggregatedValuesCum[,1])-1,2]}else{df_role[[2]][i]<-1.2}
  }
  
  # create a melted dataset
  dataset<-as.data.frame(rbind(df_mono[[1]],df_cqrs[[1]],df_role[[1]]))
  dataset1<-as.data.frame(rbind(df_mono[[2]],df_cqrs[[2]],df_role[[2]]))
  
  breaksAug<-c(breaks,1.2)
  dataset$model<-c("mono","cqrs", "role")
  dataset1$model<-c("mono","cqrs", "role")
  ds_melted<-NULL
  ds_melted0<-reshape2::melt(dataset)
  ds_melted1<-reshape2::melt(dataset1)
  ds_melted2<-ds_melted1
  ds_melted<-cbind(ds_melted0,ds_melted1$value)[-3]
  #add fake values to create the middle axis in the radar chart 
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[1]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[2]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[3]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[4]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[5]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[6]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[7]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[8]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",breaksAug[9]))
  
  #give names that can be used in the radar chart model and scale up the grid
  colnames(ds_melted)<-colnames(ds_melted1)
  ds_melted$value<-as.numeric(ds_melted$value)
  #y labels
  breakLabels<-paste(c(round(breaksAug[-c(8:9)],2),1,1.2),"(", c(loads,""),")", sep="")
 
  indexN<-grep(pathData,c(pathData_I1_UB,pathData_I1_B, pathData_I2_UB, pathData_I2_B))
  myFile<-c("pathData_I1_UB","pathData_I1_B", "pathData_I2_UB", "pathData_I2_B")[indexN]


ds_melted0
MonoSet<-ds_melted0$value[ds_melted0$model=="mono"]
RoleSet<-ds_melted0$value[ds_melted0$model=="role"]
CqrsSet<-ds_melted0$value[ds_melted0$model=="cqrs"]
idealSet<-rep(400, 11)  

#compute wicoxon 2 sample test and its effect size
#The magnitude of cliff.delta d is assessed using the thresholds provided in (Romano 2006), i.e. |d|<0.147 
#"negligible", |d|<0.33 "small", |d|<0.474 "medium", otherwise "large"

#J. Romano, J. D. Kromrey, J. Coraggio, J. Skowronek, Appropriate statistics for ordinal level data:
#Should we really be using t-test and cohen’s d for evaluating group differences on the NSSE and
#other surveys?, in: Annual meeting of the Florida Association of Institutional Research, 2006.
#cliff.delta d= 2*wilcoxonEffect - 1

#analysis on total set
es_mc<-effectSizeCompute(MonoSet,CqrsSet)
es_mr<-effectSizeCompute(MonoSet,RoleSet)
es_cr<-effectSizeCompute(CqrsSet,RoleSet)

mv_result<-as.data.frame(rbind(mv_result,cbind(myFile,"mc",c(es_mc)), cbind(myFile,"mr",c(es_mr)),cbind(myFile,"cr",c(es_cr))))

#analysis on components admin and enduser
MonoSet_a<-MonoSet[1:7]
MonoSet_e<-MonoSet[8:11]
RoleSet_a<-RoleSet[1:7]
RoleSet_e<-RoleSet[8:11]
CqrsSet_a<-CqrsSet[1:7]
CqrsSet_e<-CqrsSet[8:11]

w_mr_a<-effectSizeCompute(MonoSet_a,RoleSet_a)
w_mr_e<-effectSizeCompute(MonoSet_e,RoleSet_e)

w_mc_a<-effectSizeCompute(MonoSet_a,CqrsSet_a)
w_mc_e<-effectSizeCompute(MonoSet_e,CqrsSet_e)

w_cr_a<-effectSizeCompute(CqrsSet_a,RoleSet_a)
w_cr_e<-effectSizeCompute(CqrsSet_e,RoleSet_e)

mv_result_a<-as.data.frame(rbind(mv_result_a,cbind(myFile,"mc",c(w_mc_a)), cbind(myFile,"mr",c(w_mr_a)),cbind(myFile,"cr",c(w_cr_a))))
mv_result_e<-as.data.frame(rbind(mv_result_e,cbind(myFile,"mc",c(w_mc_e)), cbind(myFile,"mr",c(w_mr_e)),cbind(myFile,"cr",c(w_cr_e))))
}
setwd("../")
setwd("Results")
    print("Wilcoxon test all")
write.csv(
  mv_result,
  "wilcoxonAll.csv",
  row.names = F
)

print("Wilcoxon test endUser")
write.csv(
  mv_result_e,
  "wilcoxonEndUser.csv",
  row.names = F
)

print("Wilcoxon test admin")
write.csv(
  mv_result_a,
  "wilcoxonAdmin.csv",
  row.names = F
)

