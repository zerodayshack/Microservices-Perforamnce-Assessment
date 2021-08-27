#requires NewSpiders.R and start.R  
library(knitr) 

setwd("/Users/barbaramini/Research/Dropbox/JupyterNotebooks/Unisannio/ScriptPaper")
source("NewSpiders.R")
wd<-getwd()
#list.files(paste(wd,"/Results/ISTPaper/", sep=""))
workingDir<-paste(wd,"/Results/ISTPaper/", sep="")
files<-list.files(workingDir,pattern=".csv", recursive=FALSE)
iter1<-files[grep("iter1",files)]
iter2<-files[grep("iter2",files)]

#sample, balanced and iteration 2
pathData<-paste(wd,"/Results/ISTPaper/",iter2[grep("_bal",iter2)], sep="")

setwd(workingDir)
#failing operations
failedOperations<-as.data.frame(read.csv(pathData))
mono_failingS<-failedOperations[grep("mono",failedOperations[,1]),]
cqrs_failingS<-failedOperations[grep("cqrs",failedOperations[,1]),]
role_failingS<-failedOperations[grep("role",failedOperations[,1]),]

operations<-sort(unique(failedOperations$microservice))

df_mono_temp<-NULL
df_cqrs_temp<-NULL
df_role_temp<-NULL

#values and response times
df_mono_temp<-compute_df(mono_failingS,operations)
df_cqrs_temp<-compute_df(cqrs_failingS,operations)
df_role_temp<-compute_df(role_failingS,operations)

#operation colors
myColors<- c(rep("red",7), rep("black",4))

#POLYGON
file_S<-paste("Results/ISTPaper/Template_S.pdf", sep="")
pdf(file=file_S)
par(mar=c(4,7,0,1))
plot(aggregatedValues, xlim=c(aggregatedValues[1,1], aggregatedValues[nrow(aggregatedValues),1]), pch=19,pt.cex = 1.3, cex.axis=1.3, ylim=c(0, max(aggregatedValues[,2])+0.05),xaxt="n",yaxt="n", ylab="",xlab="",cex.lab=1.5, lwd = 2, bty="n", show.legend=F)+
    #title(main=merged_title,line=1, color="black", cex=0.9, face="italic")
    #to get the original load on the x axis. Note that the initial value on the original data is the symbol "8" which correspond to the number 2 (UNISANNIO)
  axis(1,at=c(0,aggregatedValues[-1,][,1]),labels=c(2,aggregatedValues[-1,][,1]), lwd.ticks=1, cex.axis =1.3, cex.lab=1.3)
  axis(2,at=c(0,0.25),lwd.ticks=0,  cex.axis =1.3,cex.lab=1.3)
  legend(230,0.225,"f()",text.width =1,col="black",lty=0,box.lty=0,lwd = 0, yjust=0,cex =1.5,seg.len=0.4, inset=0, y.intersp=0.7)
  polygon(c(min(aggregatedValues[, 1]),aggregatedValues[,1],max(aggregatedValues[, 1])),c(0,aggregatedValues[,2],0), col=adjustcolor("white",alpha.f = 0.1), pch=19,lty = 1, lwd = 1, border = "black", cex =1.3)
  type<-c("mono","role","cqrs")
  #print all polygons of the given configurations
  for(index in type[-3]){
    i<-which(type==index)
    polygon(c(min(aggregatedValues[, 1]),t(domainMetricList[[i]]$Users),max(aggregatedValues[, 1])),c(0,t(domainMetricList[[i]]$absoluteMass),0), col=adjustcolor(myCol[i], alpha.f = 0.1), lty = 1, lwd = 0.5,  border =myCol[i])
    lines(domainMetricList[[index]][,c("Users","absoluteMass")], type="p",lwd = 0.5,pt.cex = 1.3, pch=19,col= myCol[i])
  }
  #print legend 
  #legendNames<-c(expression(a[1]),expression(a[2]))
  #names(domainMetric)<-architectures
  # for(i in 1:length(names(domainMetric))){
  #   legendNames[i]<-paste(names(domainMetric)[i])
  # }
  #text(aggregatedValues,labels =round(aggregatedValues[1:nrow(aggregatedValues),2],3), pos=3, col="black",cex =0.7)
  #legend(10,0.17,legendNames,text.width =1,col=myCol,lty=1,box.lty=0,lwd = 4, yjust=0,cex =2,seg.len=0.4, inset=0, y.intersp=0.7)
  graphics.off()
  #ggsave(file=myFilerRT,plotLineRT_list[[i]])
  system(paste("pdfcrop --margins '10 10 10 10'", file_S, " " ,file_S, sep=" "))

    
#RADAR
file_C<-paste("Results/ISTPaper/Template_C.pdf", sep="")

#data: last non failing load, frequncy, response times; computed in NewSpiders.R
df_mono<-df_mono_temp
df_cqrs<-df_cqrs_temp
df_role<-df_role_temp
#create radar grid proportional to empirical distribution of workload intensities
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
ds_melted<-cbind(ds_melted0,ds_melted1$value)
#add fake values to create the middle axis in the radar chart 
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","2",breaksAug[1]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","50",breaksAug[2]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","100",breaksAug[3]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","150",breaksAug[4]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","200",breaksAug[5]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","250",breaksAug[6]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","300",breaksAug[7]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","350",breaksAug[8]))
ds_melted<-rbind(ds_melted,c("","Admin_addIntersection","400",breaksAug[9]))

#give names that can be used in the radar chart model and scale up the grid
colnames(ds_melted)<-c(colnames(ds_melted1)[1:2], "load","value")
ds_melted$value<-as.numeric(ds_melted$value)
breaksAug<-c(breaks,1.2)

#remove one architecture
ds_melted_CT<-ds_melted[!ds_melted$model=="cqrs",][-c(23,24,25,31),]
#remove non failing operations "400"
#ds_melted_CT<-ds_melted_OT[!ds_melted_OT$load=="400",]
#remove some operations 
ds_melted_C<-ds_melted_CT[c(5:10,15:18),]

#y labels
breakLabels<-paste(c(round(breaksAug[-c(8:9)],2),1,1.2),"(", c(loads,""),")", sep="")

loadsN<-as.integer(loads)
pdf(file=file_C)
ggplot(ds_melted_C, aes(x = variable, y = value),show.legend = F) +
  geom_polygon(aes(group = model, color = model,fill=model),  alpha=0.1, size = 0.5, show.legend = F) +
  geom_hline(yintercept = breaksAug[8], linetype = 1, size = 0.1,show.legend = F)+
  geom_hline(yintercept = breaksAug[7], linetype = 1, size = 0.1,show.legend = F)+
  geom_hline(yintercept = breaksAug[6], linetype = 1, size = 0.1,show.legend = F)+
  geom_hline(yintercept = breaksAug[5], linetype = 1, size = 0.1,show.legend = F)+
  geom_hline(yintercept = breaksAug[4], linetype = 1, size = 0.1,show.legend = F)+
  geom_hline(yintercept = breaksAug[3], linetype = 1, size = 0.1,show.legend = F)+
  geom_hline(yintercept = breaksAug[2], linetype = 1, size = 0.1,show.legend = F)+
  geom_line(aes(color=model), size=0.5,show.legend = F)+
  geom_point(aes(color=model), size=0.5,show.legend = F)+
  scale_y_continuous(name="", breaks=breaksAug, limits=c(0,breaksAug[9]))+
  scale_x_discrete(name="",labels=c(expression(italic(o[1])),expression(italic(o[2])),expression(italic(o[3])),expression(italic(o[4])),
                                    expression(italic(o[5])),expression(italic(o[6])),expression(italic(o[7])),expression(italic(o[8])),
                                    expression(italic(o[9])),expression(italic(o[10])),expression(italic(o[11]))))+
  theme_bw()+
  #guides(color = guide_legend(ncol=1)) +  
  scale_colour_manual(
    values=c("blue","violet","darkgreen","red"),
    limits = c('mono', 'role'),
    aesthetics = c("colour", "fill")
  )+
  coord_polar(start = pi/11-2*pi/11, direction=-1, clip="off")+
     theme(
      axis.text.x = element_text(colour=myColorsT, size=24, angle = 0, vjust = 0.8, hjust=0.9), 
      axis.text.y = element_blank(),
      axis.ticks.y =element_blank(),
      axis.title.y=element_text(size=24),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"), 
      panel.grid.major.x = element_line(size = 0.1, linetype = 'solid',
                                        colour = "black"), 
      legend.text=element_blank(),
      #legend.title = element_text(size=24),
      #legend.position = c(0.9,0.9),
      axis.line.x= element_blank(),
      plot.margin = margin(0, 0, 0, 0, "cm")
    ) 
  graphics.off()
  #ggsave(file=myFilerRT,plotLineRT_list[[i]])
  system(paste("pdfcrop --margins '10 10 10 10'", file_S, " " ,file_S, sep=" "))
  
#SCALABILITY GAP. 
  #file_O<-paste("Results/ISTPaper/Template_O.pdf", sep="")
  file_O<-paste("Template_O.pdf", sep="")
  #data: last non failing load, frequncy, response times; computed in NewSpiders.R
  df_mono<-df_mono_temp
  df_cqrs<-df_cqrs_temp
  df_role<-df_role_temp
  
  #Create the gap of frequencies. Only frequency 
  for(i in 1:11){
    if(!length(grep(df_mono_temp[[1]][i],aggregatedValuesCum[,1]))==0){
      df_mono[[2]][i]<-df_mono_temp[[2]][i]}else{df_mono[[2]][i]<-0}
    if(!length(grep(df_cqrs_temp[[1]][i],aggregatedValuesCum[,1]))==0){
      df_cqrs[[2]][i]<-df_cqrs_temp[[2]][i]}else{df_cqrs[[2]][i]<-0}
    if(!length(grep(df_role_temp[[1]][i],aggregatedValuesCum[,1]))==0){
      df_role[[2]][i]<-df_role_temp[[2]][i]}else{df_role[[2]][i]<-0}
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
  ds_melted<-cbind(ds_melted0,ds_melted1$value)
  #add fake values to create the middle axis in the radar chart 
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",2,breaksAug[1]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",50,breaksAug[2]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",100,breaksAug[3]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",150,breaksAug[4]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",200,breaksAug[5]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",250,breaksAug[6]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",300,breaksAug[7]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",350,breaksAug[8]))
  ds_melted<-rbind(ds_melted,c("","Admin_addIntersection",400,breaksAug[9]))
  
  #give names that can be used in the radar chart model and scale up the grid
  colnames(ds_melted)<-c("model","variable","load","value")
  ds_melted$value<-as.numeric(ds_melted$value)
  
  #remove one architecture
  ds_melted_OT<-ds_melted[!ds_melted$model=="cqrs",][-c(23,24,25,31),]
  #ds_meltedR0_O<-ds_meltedR0[!ds_meltedR0$model=="cqrs",]
  
  
  #add a column expressing the threshold for each failing microservice
  ds_melted_OT$threshold<-rep(0,nrow(ds_melted_OT))
  for(i in 1:11){
    if(!length(grep(names(threshold[i+2]),ds_melted_OT[-c(23:31),2]))==0){
      ds_melted_OT$threshold[grep(names(threshold[i+2]),ds_melted_OT[-c(23:31),2])]<-unlist(rep(threshold[i+2],length(grep(names(threshold[i+2]),ds_melted_OT[-c(22:31),2]))))}
    else{ ds_melted_O$thresholdgrep(names(threshold[i+2]),ds_melted_OT[-c(22:31),2])<-0}
  }
  
  #remove non failing operations "400"
  ds_melted_OT<-ds_melted_OT[!ds_melted_OT$load=="400",]
 
  
  #remove some operations 
  ds_melted_OT<-ds_melted_OT[c(5:10,15:17),]
  
  #make the grid of interline length 1
  t<-(2*as.numeric(ds_melted_OT$load)/100)
  loadsNum<-2*as.numeric(loads)/100
  myColorsT<-myColors[c(1:3,8:9)]
  pdf(file=file_O)
  #par(mar=c(4,7,0,1))
  ggplot(ds_melted_OT, aes(x = variable, fill=model)) +
    geom_crossbar(aes(y =+value+t, ymin= t, ymax=+value+t, fatten=0.3,group = model, color = model,fill=model), size = 0.4,wisth=0.2,show.legend = F) +
    geom_hline(yintercept = loadsNum[8], linetype = 1, size = 0.1)+
    geom_hline(yintercept = loadsNum[7], linetype = 1, size = 0.1)+
    geom_hline(yintercept = loadsNum[6], linetype = 1, size = 0.1)+
    geom_hline(yintercept = loadsNum[5], linetype = 1, size = 0.1)+
    geom_hline(yintercept = loadsNum[4], linetype = 1, size = 0.1)+
    #geom_hline(yintercept = loadsNum[3], linetype = 1, size = 0.1)+
    #geom_hline(yintercept = loadsNum[2], linetype = 1, size = 0.1)+
    #scale_y_continuous(name="Scalability gap")+
    scale_x_discrete(name="", labels=c(expression(italic(o[1])),expression(italic(o[2])),expression(italic(o[3])),expression(italic(o[4])),
                                       expression(italic(o[5])),expression(italic(o[6])),expression(italic(o[7])),expression(italic(o[8])),
                                       expression(italic(o[9])),expression(italic(o[10])),expression(italic(o[11]))))+
    theme_bw()+
    ylab("") +
    #guides(color = guide_legend(ncol=1)) +  
    scale_colour_manual(
      values=c("blue","violet","green"),
      limits =c("mono","role"),
      labels=c(expression(a[1]),expression(a[2])),
      aesthetics = c("colour")
    )+
    scale_fill_manual( values=alpha(c("blue","violet","green"), 0.1),
                       limits =c("mono","role"),
                       labels=c(expression(a[1]),expression(a[2])),
                       aesthetics = c("fill")
    ) +
    annotate("text", x = 0, y =c(loadsNum[-c(1:3,9)]), label = loads[-c(1:3)], size=6,  vjust=-0.6, hjust = 0)+
    #annotate("text", x = 0, y =c(loadsNum[-c(1,9)]), label = loadsNum[-c(1,9)], size=2.5, hjust = -0.1, vjust=-0.6)+
      theme(
      #plot.caption = element_text(color="black", size=12, face="italic",hjust=0.5),
      #plot.caption.position= "panel",
      #plot.title.position= "panel",
      #plot.title = element_text(color="black", size=12, face="italic",hjust=0.5),
      axis.text.x = element_text(colour=myColorsT, size=24, vjust = 0.8, hjust=0.3), 
      axis.text.y = element_blank(),
      axis.ticks.y =element_blank(),
      panel.border = element_blank(),
      axis.title.y=element_text(size=24),
      panel.grid.major = element_blank(), 
      panel.grid.minor= element_blank(), 
      axis.line.x= element_blank(),
      legend.text=element_text(size=24),
      legend.title = element_text(size=24),
      #aspect.ratio = 0.5, 
      plot.margin = margin(0, 0.7, 0, 0, "cm")
    ) 
  graphics.off()
  system(paste("pdfcrop --margins '0 0 0 0'", file_O, " " ,file_S, sep=" "))
  
 
  #Wilcoxon test 
  # ds_melted0
  # MonoSet<-ds_melted0$value[ds_melted0$model=="mono"]
  # RoleSet<-ds_melted0$value[ds_melted0$model=="role"]
  # CqrsSet<-ds_melted0$value[ds_melted0$model=="cqrs"]
  # idealSet<-rep(400, 11)  
  # 
  # #compute wicoxon 2 sample test and its effect size
  # #The magnitude of cliff.delta d is assessed using the thresholds provided in (Romano 2006), i.e. |d|<0.147 
  # #"negligible", |d|<0.33 "small", |d|<0.474 "medium", otherwise "large"
  # 
  # #J. Romano, J. D. Kromrey, J. Coraggio, J. Skowronek, Appropriate statistics for ordinal level data:
  # #Should we really be using t-test and cohen’s d for evaluating group differences on the NSSE and
  # #other surveys?, in: Annual meeting of the Florida Association of Institutional Research, 2006.
  # #cliff.delta d= 2*wilcoxonEffect - 1
  # weights<-c(0.147,0.33,0.474)
  # names(weights)<-c("negligible","small","medium")
  # #returns the wicoxon, cliff delta and the magnitude of Cliff delta
  # effectSizeCompute<-function(set1,set2){
  # size<-length(set1)*length(set2)
  # #percentage at which set1 > set2; i.e. arch1 more scalable than arch2
  # w<-wilcox.test(set1,set2)
  # e_w<- w$statistic/size
  # w_pValue<-w$p.value
  # names(w_pValue)<-"W_P-value"
  # cliff_w<-2*e_w-1
  # i<-min(which(abs(cliff_w)<weights))
  # names(e_w)<-"WilcoxonEffectSize"
  # names(cliff_w)<-"Cliff"
  # o_e_w<-names(weights)[i]
  # if(abs(cliff_w)>0.474){o_e_w<-"large"}
  # names(o_e_w)<-"magnitude"
  # result<-c(e_w,w_pValue,cliff_w,o_e_w)
  # return(result)
  # }
  # es_mc<-effectSizeCompute(MonoSet,CqrsSet)
  # es_mr<-effectSizeCompute(MonoSet,RoleSet)
  # es_rc<-effectSizeCompute(RoleSet,CqrsSet)
  # 
  # 
  # MonoSet_a<-MonoSet[1:7]
  # MonoSet_e<-MonoSet[8:11]
  # RoleSet_a<-RoleSet[1:7]
  # RoleSet_e<-RoleSet[8:11]
  # CqrsSet_a<-CqrsSet[1:7]
  # CqrsSet_e<-CqrsSet[8:11]
  # 
  # w_mr_a<-effectSizeCompute(MonoSet_a,RoleSet_a)
  # w_mr_e<-effectSizeCompute(MonoSet_e,RoleSet_e)
  # 
  # w_mc_a<-effectSizeCompute(MonoSet_a,CqrsSet_a)
  # w_mc_e<-effectSizeCompute(MonoSet_e,CqrsSet_e)
  # 
  # w_rc_a<-effectSizeCompute(CqrsSet_a,RoleSet_a)
  # w_rc_e<-effectSizeCompute(CqrsSet_e,RoleSet_e)
  # 
