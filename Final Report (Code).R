#Libraries
library(tidyverse)
library(ggplot2)
library(tableone)
library(table1)
library(knitr)
library(IRdisplay)
library(kableExtra)
library(gridExtra)
library(factoextra)
library(NbClust)
library(linkcomm)
library(fossil)

#Load LMdata.csv and title it df

#Evaluates Missing Data
df.matrix<-as.matrix(df)
df.row<-dim(df.matrix)[1]
df.col<-dim(df.matrix)[2]
percent.missing<-rep(0,df.col)
for(k in 1:df.col){
  NA.count<-sum(is.na(df.matrix[,k]))
  empty.count<-sum(df.matrix[,k]=="")
  if(is.na(empty.count)==T){empty.count<-0}
  missing.count<-NA.count+empty.count
  missing.percent<-missing.count/df.row
  percent.missing[k]<-missing.percent
}
Missing.Table<-data.frame(Variable=names(df),
Percent_Missing=percent.missing*100)
print(Missing.Table)

#Conflates Source Variables if needed and Removes Unneeded Variables 
for(k in 1:nrow(df)){
  if(df$Isolation.source[k]=="" & df$Isolation.type[k]!=""){
    df$Isolation.source[k]<-df$Isolation.type[k]
  }
}
#Identifies NA values
for(k in 1:nrow(df)){
  if(df$Serovar[k]==""){
    df$Serovar[k]<-NA
  }
}

#Identifies NA values
for(k in 1:nrow(df)){
  if(df$Isolation.source[k]==""){
    df$Isolation.source[k]<-NA
  }
}

#Identifies NA values
for(k in 1:nrow(df)){
  if(df$Collection.date[k]==""){
    df$Collection.date[k]<-NA
  }
}

#Removes missing data and unneeded variables
df<-df%>%drop_na()
df<-subset(df,select=-c(Isolate.identifiers,Isolate,Outbreak,
                        Create.date,Isolation.type,Min.diff))

#Quarterly data
quarter<-rep(0,nrow(df))
for(k in 1:nrow(df)){
  if(df$Collection.date[k] %in% c("2017-01","2017-02",
    "2017-03")){
    quarter[k]<-1
  }
  if(df$Collection.date[k] %in% c("2017-04","2017-05",
    "2017-06")){
    quarter[k]<-2
  }
  if(df$Collection.date[k] %in% c("2017-07","2017-08",
    "2017-09")){
    quarter[k]<-3
  }
  if(df$Collection.date[k] %in% c("2017-10","2017-11",
    "2017-12")){
    quarter[k]<-4
  }
  if(df$Collection.date[k] %in% c("2018-01","2018-02",
    "2018-03")){
    quarter[k]<-5
  }
  if(df$Collection.date[k] %in% c("2018-04","2018-05",
    "2018-06")){
    quarter[k]<-6
  }
  if(df$Collection.date[k] %in% c("2018-07","2018-08",
    "2018-09")){
    quarter[k]<-7
  }
  if(df$Collection.date[k] %in% c("2018-10","2018-11",
    "2018-12")){
    quarter[k]<-8
  }
  if(df$Collection.date[k] %in% c("2019-01","2019-02",
    "2019-03")){
    quarter[k]<-9
  }
  if(df$Collection.date[k] %in% c("2019-04","2019-05",
    "2019-06")){
    quarter[k]<-10
  }
  if(df$Collection.date[k] %in% c("2019-07","2019-08",
    "2019-09")){
    quarter[k]<-11
  }
  if(df$Collection.date[k] %in% c("2019-10","2019-11",
    "2019-12")){
    quarter[k]<-12
  }
  if(df$Collection.date[k] %in% c("2020-01","2020-02",
    "2020-03")){
    quarter[k]<-13
  }
  if(df$Collection.date[k] %in% c("2020-04","2020-05",
    "2020-06")){
    quarter[k]<-14
  }
  if(df$Collection.date[k] %in% c("2020-07","2020-08",
    "2020-09")){
    quarter[k]<-15
  }
  if(df$Collection.date[k] %in% c("2020-10","2020-11",
    "2020-12")){
    quarter[k]<-16
  }
  if(df$Collection.date[k] %in% c("2021-01","2021-02",
    "2021-03")){
    quarter[k]<-17
  }
  if(df$Collection.date[k] %in% c("2021-04","2021-05",
    "2021-06")){
    quarter[k]<-18
  }
  if(df$Collection.date[k] %in% c("2021-07","2021-08",
    "2021-09")){
    quarter[k]<-19
  }
  if(df$Collection.date[k] %in% c("2021-10","2021-11",
    "2021-12")){
    quarter[k]<-20
  }
  if(df$Collection.date[k] %in% c("2022-01","2022-02",
    "2022-03")){
    quarter[k]<-21
  }
  if(df$Collection.date[k] %in% c("2022-04","2022-05",
    "2022-06")){
    quarter[k]<-22
  }
  if(df$Collection.date[k] %in% c("2022-07","2022-08",
    "2022-09")){
    quarter[k]<-23
  }
  if(df$Collection.date[k] %in% c("2022-10","2022-11",
    "2022-12")){
    quarter[k]<-24
  }
  if(!(df$Collection.date[k] %in% c("2017-01","2017-02","2017-03",
    "2017-04","2017-05","2017-06","2017-07","2017-08","2017-09",
    "2017-10","2017-11","2017-12","2018-01","2018-02","2018-03",
    "2018-04","2018-05","2018-06","2018-07","2018-08","2018-09",
    "2018-10","2018-11","2018-12","2019-01","2019-02","2019-03",
    "2019-04","2019-05","2019-06","2019-07","2019-08","2019-09",
    "2019-10","2019-11","2019-12","2020-01","2020-02","2020-03",
    "2020-04","2020-05","2020-06","2020-07","2020-08","2020-09",
    "2020-10","2020-11","2020-12","2021-01","2021-02","2021-03",
    "2021-04","2021-05","2021-06","2021-07","2021-08","2021-09",
    "2021-10","2021-11","2021-12","2022-01","2022-02","2022-03",
    "2022-04","2022-05","2022-06","2022-07","2022-08","2022-09",
    "2022-10","2022-11","2022-12"))){
    quarter[k]<-99}
}
df$quarter<-quarter
df<-df%>%filter(quarter!=99)
names(df)[8]<-"source"
df<-subset(df,select=-Collection.date)

#Numeric sources
for(k in 1:nrow(df)){
  #1 Human Case; #2 Food; #3 Clinical #4 Environmental #5 Other
  if(df$source[k] %in% c("human")){
    df$source[k]<-1}
  if(df$source[k] %in% c("food","ice cream",
    "Turkey breast lunch meat")){
    df$source[k]<-2}
  if(df$source[k] %in% c("blood","CSF","Blood, NOS",
    "placenta","Cerebral spinal fluid","urine","wound", "sputum",
    "Cerebrospinal fluid, NOS","cerebrospinal fluid",
    "Wound Abscess","Tissue","Fluid","Synovial fluid",
    "Toe wound")){
    df$source[k]<-3}
  if(df$source[k] %in% c("environmental","Environmental sponge",
    "environmental swab")){
    df$source[k]<-4}
  if(df$source[k] %in% c("other")){
    df$source[k]<-5}
}
df<-df%>%filter(source %in% c("1","2","3","4","5"))
df$source<-as.numeric(df$source)
df<-df%>%filter(!(Serovar %in% c("not typed", "Repeat")))
df<-subset(df,select=-c(Organism.group,Location))
names(df)<-c("strain","serovar","n50","contig","source",
             "snpc","same","quarter")

#Subsets the Data by Year
df17<-df%>%filter(quarter %in% 1:4)
df18<-df%>%filter(quarter %in% 5:8)
df19<-df%>%filter(quarter %in% 9:12)
df20<-df%>%filter(quarter %in% 13:16)
df21<-df%>%filter(quarter %in% 17:20)
df22<-df%>%filter(quarter %in% 21:24)

#Figure 1: Densities of Distance 2017 to 2019
ggplot()+geom_density(aes(x=same,color="2017"),
                      data=df17,na.rm=T)+
  geom_density(aes(x=same,color="2018"),
               data=df18,na.rm=T)+
  geom_density(aes(x=same,color="2019"),
               data=df19,na.rm=T)+
  scale_color_manual(
    values=c("blue","red","green"),
    name="Year")+theme(legend.position="bottom",
                       legend.text=element_text(size=7),
                       legend.key.size=unit(0.1,"cm"),
                       plot.title=element_text(size=10))+
  labs(x="Minimum Self-Same Distance",
  y="Probability Density",title="Figure 1: Probability Densities of
  Minimum Distances (2017 - 2019)")

#Figure 2: Densities of Distance 2020 to 2022
ggplot()+geom_density(aes(x=same,color="2020"),
                      data=df20,na.rm=T)+
  geom_density(aes(x=same,color="2021"),
               data=df21,na.rm=T)+
  geom_density(aes(x=same,color="2022"),
               data=df22,na.rm=T)+
  scale_color_manual(
    values=c("orange","purple","black"),
    name="Year")+theme(legend.position="bottom",
                       legend.text=element_text(size=7),
                       legend.key.size =unit(0.1,"cm"),
                       plot.title=element_text(size=10))+
  labs(x="Minimum Self-Same Distance",
  y="Probability Density",title="Figure 2: Probability Densities of
  Minimum Distances (2020 - 2022)")

#Figure 3: Densities of Contigs 2017 to 2019
ggplot()+geom_density(aes(x=contig,color="2017"),
                      data=df17,na.rm=T)+
  geom_density(aes(x=contig,color="2018"),
               data=df18,na.rm=T)+
  geom_density(aes(x=contig,color="2019"),
               data=df19,na.rm=T)+
  scale_color_manual(
    values=c("blue","red","green"),
    name="Year")+theme(legend.position="bottom",
                       legend.text=element_text(size=7),
                       legend.key.size=unit(0.1,"cm"),
                       plot.title=element_text(size=10))+
  labs(x="Number of Contigs",
  y="Probability Density",title="Figure 3: Probability Densities of
  Contigs (2017 - 2019)")

#Figure 4: Densities of Contigs 2020 to 2022
ggplot()+geom_density(aes(x=contig,color="2020"),
                      data=df20,na.rm=T)+
  geom_density(aes(x=contig,color="2021"),
               data=df21,na.rm=T)+
  geom_density(aes(x=contig,color="2022"),
               data=df22,na.rm=T)+
  scale_color_manual(
    values=c("orange","purple","black"),
    name="Year")+theme(legend.position="bottom",
                       legend.text=element_text(size=7),
                       legend.key.size =unit(0.1,"cm"),
                       plot.title=element_text(size=10))+
  labs(x="Number of Contigs",
  y="Probability Density",title="Figure 4: Probability Densities of
  Contigs (2020 - 2022)")

#Source subsets
dfs1<-df%>%filter(source==1)
dfs2<-df%>%filter(source==2)
dfs3<-df%>%filter(source==3)
dfs4<-df%>%filter(source==4)
dfs5<-df%>%filter(source==5)

#Figure 5: Distance by Source
ggplot()+geom_density(aes(x=same,color="Human"),
                      data=dfs1,na.rm=T)+
  geom_density(aes(x=same,color="Food"),
               data=dfs2,na.rm=T)+
  geom_density(aes(x=same,color="Clinical"),
               data=dfs3,na.rm=T)+
  geom_density(aes(x=same,color="Env."),
               data=dfs4,na.rm=T)+
  geom_density(aes(x=same,color="Misc."),
               data=dfs5,na.rm=T)+
  scale_color_manual(
    values=c("blue","red","green","orange","purple"),
    name="Source")+theme(legend.position="bottom",
                         legend.text=element_text(size=5),
                         legend.key.size=unit(0.1,"cm"),
                         plot.title=element_text(size=10))+
  labs(x="Minimum Self-Same Distance",
  y="Probability Density",title="Figure 5: Probability Densities of
  Minimum Distances by Source")

#Figure 6: Densities of Contigs by Source
ggplot()+geom_density(aes(x=contig,color="Human"),
                      data=dfs1,na.rm=T)+
  geom_density(aes(x=contig,color="Food"),
               data=dfs2,na.rm=T)+
  geom_density(aes(x=contig,color="Clinical"),
               data=dfs3,na.rm=T)+
  geom_density(aes(x=contig,color="Env."),
               data=dfs4,na.rm=T)+
  geom_density(aes(x=contig,color="Misc."),
               data=dfs5,na.rm=T)+
  scale_color_manual(
    values=c("blue","red","green","orange","purple"),
    name="Source")+theme(legend.position="bottom",
                         legend.text=element_text(size=5),
                         legend.key.size=unit(0.1,"cm"),
                         plot.title=element_text(size=10))+
  labs(x="Contigs",
  y="Probability Density",title="Figure 6: Probability Densities of
  Contigs by Source")

#Serovar 4b Proportions
s1<-round(mean(dfs1$serovar %in% c("4b","Serotype 4b"))*100,2)
s2<-round(mean(dfs2$serovar %in% c("4b","Serotype 4b"))*100,2)
s3<-round(mean(dfs3$serovar %in% c("4b","Serotype 4b"))*100,2)
s4<-round(mean(dfs4$serovar %in% c("4b","Serotype 4b"))*100,2)
s5<-round(mean(dfs5$serovar %in% c("4b","Serotype 4b"))*100,2)

t1<-data.frame(Source=c("Human","Food","Clinical","Environmental","Other"),
               prop=c(s1,s2,s3,s4,s5))
names(t1)<-c("Source","Serovar 4b Prevalence (%)")

#Render table
kable(t1, "latex",
  caption="Serovar 4b Prevalence by Source")%>%
  kable_styling(latex_options="HOLD_position")

#Render table 2
t2<-data.frame(Action=c("Drawing 1000 Observations From the Original Dataset",
  "Constructing Visual Networks With 500 Interactions",
  "Constructing Analytic Networks With 1000 Interactions"),Seed=c(3,18,7))
kable(t2, "latex",
      caption="Seeds Used for Sampling")%>%
  kable_styling(latex_options="HOLD_position")

##################
#K Means Clusters#
##################

#Obtain Optimal Number of Clusters (Figure 7)
set.seed(3)
dfc<-scale(subset(df,select=c(n50,contig,source,same,quarter)))
cluster.sample<- dfc[sample(1:nrow(dfc), 1000),]
fviz_nbclust(cluster.sample, kmeans, method="silhouette")+
  labs(title="Figure 7: Optimal Number of Clusters",x="Number of Clusters (k)",
       y="Average Silhouette")+theme(title=element_text(size=8))

#Fit K-Means Model
cluster1<-kmeans(dfc,6,10)
df$cluster<-cluster1$cluster

#Sort by K-Means Cluster
df1<-df%>%filter(cluster==1)
df2<-df%>%filter(cluster==2)
df3<-df%>%filter(cluster==3)
df4<-df%>%filter(cluster==4)
df5<-df%>%filter(cluster==5)
df6<-df%>%filter(cluster==6)

#Figure 8 contigs vs ln(N50) by cluster
ggplot()+geom_point(aes(x=log(n50),y=contig,
                        col="Cluster 1"),data=df1,alpha=0.25)+
  geom_point(aes(x=log(n50),y=contig,col="Cluster 2"),
             data=df2,alpha=0.25)+
  geom_point(aes(x=log(n50),y=contig,col="Cluster 3"),
             data=df3,alpha=0.25)+
  geom_point(aes(x=log(n50),y=contig,col="Cluster 4"),
             data=df4,alpha=0.25)+
  geom_point(aes(x=log(n50),y=contig,col="Cluster 5"),
             data=df5,alpha=0.25)+
  geom_point(aes(x=log(n50),y=contig,col="Cluster 6"),
             data=df6,alpha=0.1)+
  labs(x="ln(N50)",y="Number of Contigs",
       title="Figure 8: Contigs Against ln(N50) by Cluster")+
  scale_color_manual(values=c("green","blue","red",
                              "orange","purple","cyan"),name="Legend")+
  theme(legend.position="bottom",
        legend.key.size=unit(0.2, 'cm'),legend.text=element_text(size=5),
        title=element_text(size=5))

#Figure 9 quarter vs distance by cluster
ggplot()+geom_bar(aes(x=quarter,y=same,
                      col="Cluster 1"),data=df1,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=same,col="Cluster 2"),
           data=df2,alpha=0.5,stat="identity")+
  geom_bar(aes(x=quarter,y=same,col="Cluster 3"),
           data=df3,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=same,col="Cluster 4"),
           data=df4,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=same,col="Cluster 5"),
           data=df5,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=same,col="Cluster 6"),
           data=df6,alpha=0.05,stat="identity")+
  labs(x="Quarter",y="Minimum Self-Same Distance",
       title="Figure 9: Minimum Self-Same Distance 
  Against Quarter by Cluster")+
  scale_color_manual(values=c("green","blue","red",
                              "orange","purple","cyan"),name="Legend")+
  theme(legend.position="bottom",
        legend.key.size=unit(0.2, 'cm'),legend.text=element_text(size=5),
        title=element_text(size=5))

#Figure 10 quarter vs distance by cluster
ggplot()+geom_bar(aes(x=quarter,y=contig,
                      col="Cluster 1"),data=df1,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=contig,col="Cluster 2"),
           data=df2,alpha=0.5,stat="identity")+
  geom_bar(aes(x=quarter,y=contig,col="Cluster 3"),
           data=df3,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=contig,col="Cluster 4"),
           data=df4,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=contig,col="Cluster 5"),
           data=df5,alpha=0.25,stat="identity")+
  geom_bar(aes(x=quarter,y=contig,col="Cluster 6"),
           data=df6,alpha=0.05,stat="identity")+
  labs(x="Quarter",y="Number of Contigs",
       title="Figure 10: Contigs Against Quarter by Cluster")+
  scale_color_manual(values=c("green","blue","red",
                              "orange","purple","cyan"),name="Legend")+
  theme(legend.position="bottom",
        legend.key.size=unit(0.2, 'cm'),legend.text=element_text(size=5),
        title=element_text(size=5))

#Serovar prevalence
p1<-round(mean(df1$serovar %in% c("4b","Serotype 4b"))*100,2)
p2<-round(mean(df2$serovar %in% c("4b","Serotype 4b"))*100,2)
p3<-round(mean(df3$serovar %in% c("4b","Serotype 4b"))*100,2)
p4<-round(mean(df4$serovar %in% c("4b","Serotype 4b"))*100,2)
p5<-round(mean(df5$serovar %in% c("4b","Serotype 4b"))*100,2)
p6<-round(mean(df6$serovar %in% c("4b","Serotype 4b"))*100,2)

#Table 3
t3<-data.frame(Cluster=1:6,ds=c("Human (232/604), Food (372/604)",
                                "Human (128/251), Food (102/251)",
                                "Human (115/833, Food (709/833)",
                                "Environmental (229/269), Misc. (34/269)",
                                "Food (62/95), Environmental (22/95)",
                                "Environmental (232/238), Misc. (6/238)"),
               sp=c(p1,p2,p3,p4,p5,p6))
names(t3)<-c("Cluster","Dominant Sources (Source Total/Cluster Total)", 
             "Serovar 4b Prevalence (%)")

kable(t3, "latex",
  caption="Serovar 4b Prevalence by Cluster")%>%
  kable_styling(latex_options="HOLD_position")

##################
#Network Clusters#
##################

#1000 Observed Samples
set.seed(3)
samp<-sample(1:nrow(df),1000,replace=F)
df2<-df[c(samp),]

#Creates Node-to-Node Comparison Dataset
dimzero<-nrow(df2)^2
strain.combo<-data.frame(n1=rep(0,dimzero),n2=rep(0,dimzero),
    n501=rep(0,dimzero),n502=rep(0,dimzero),contig1=rep(0,dimzero),
    contig2=rep(0,dimzero),source1=rep(0,dimzero),source2=rep(0,dimzero),
    same1=rep(0,dimzero),same2=rep(0,dimzero),
    quar1=rep(0,dimzero),quar2=rep(0,dimzero))
for(k in 1:nrow(df2)){
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),1]<-rep(df2$strain[k],
                                                           nrow(df2))
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),2]<-df2$strain
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),3]<-rep(df2$n50[k],
                                                           nrow(df2))
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),4]<-df2$n50
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),5]<-rep(df2$contig[k],
                                                           nrow(df2))
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),6]<-df2$contig
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),7]<-rep(df2$source[k],
                                                           nrow(df2))
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),8]<-df2$source
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),9]<-rep(df2$same[k],
                                                           nrow(df2))
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),10]<-df2$same
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),11]<-rep(df2$quarter[k],
                                                            nrow(df2))
  strain.combo[(((k-1)*nrow(df2))+1):(k*nrow(df2)),12]<-df2$quarter
}
strain.combo<-strain.combo%>%filter(n1!=n2)%>%filter(n1!="")%>%
  filter(n2!="")
pairs<-dim(strain.combo)[1]
weights<-rep(0,pairs)
for(k in 1:pairs){
  if(round(strain.combo$n501[k],-3)==round(strain.combo$n502[k],-3)){
    weights[k]<-weights[k]+0.4/3
  }
  if(round(strain.combo$contig1[k],-1)==round(strain.combo$contig2[k],-1)){
    weights[k]<-weights[k]+0.4/3
  }
  if(strain.combo$source1[k]==strain.combo$source2[k]){
    weights[k]<-weights[k]+0.3
  }
  if(round(strain.combo$same1[k],-1)==round(strain.combo$same2[k],-1)){
    weights[k]<-weights[k]+0.4/3
  }
  if(strain.combo$quar1[k]==strain.combo$quar2[k]){
    weights[k]<-weights[k]+0.3
  }
}
network.df<-data.frame(strain.combo$n1,strain.combo$n2,weights)

#Analyzes networks
set.seed(18)
strain.samp1<-sample(1:pairs,500,rep=F)
clusters2<-getLinkCommunities(network.df[strain.samp1,],directed=T)
strain.samp2<-sample(1:pairs[-strain.samp1],500,rep=F)
clusters3<-getLinkCommunities(network.df[strain.samp2,],directed=T)

#Creates Network 1 (Figure 11)
plot(clusters2, type = "graph", layout = layout.fruchterman.reingold,
main="Figure 11: First 500-Observation Network (Trivial Links Omitted)")

#Creates Network 2 (Figure 12)
plot(clusters3, type = "graph", layout = layout.fruchterman.reingold,
main="Figure 12: Second 500-Observation Network (Trivial Links Omitted)")

#5 1000 Observation Networks
set.seed(7)
strain.samp3<-sample(1:pairs,1000,rep=F)
clusters4<-getLinkCommunities(network.df[strain.samp3,],directed=T)
strain.samp4<-sample(1:pairs[-strain.samp3],1000,rep=F)
clusters5<-getLinkCommunities(network.df[strain.samp4,],directed=T)
strain.samp5<-sample(1:pairs[-c(strain.samp3,strain.samp4)],1000,rep=F)
clusters6<-getLinkCommunities(network.df[strain.samp5,],directed=T)
strain.samp6<-sample(1:pairs[-c(strain.samp3,strain.samp4,strain.samp5)],
                     1000,rep=F)
clusters7<-getLinkCommunities(network.df[strain.samp6,],directed=T)
strain.samp7<-sample(1:pairs[-c(strain.samp3,strain.samp4,strain.samp5,
                                strain.samp6)],1000,rep=F)
clusters8<-getLinkCommunities(network.df[strain.samp7,],directed=T)

#Centrality vectors
cc1<-as.vector(getCommunityCentrality(clusters4))
cc2<-as.vector(getCommunityCentrality(clusters5))
cc3<-as.vector(getCommunityCentrality(clusters6))
cc4<-as.vector(getCommunityCentrality(clusters7))
cc5<-as.vector(getCommunityCentrality(clusters8))

#Modularity vectors
mod1<-as.vector(getCommunityConnectedness(clusters4,conn="modularity"))
mod2<-as.vector(getCommunityConnectedness(clusters5,conn="modularity"))
mod3<-as.vector(getCommunityConnectedness(clusters6,conn="modularity"))
mod4<-as.vector(getCommunityConnectedness(clusters7,conn="modularity"))
mod5<-as.vector(getCommunityConnectedness(clusters8,conn="modularity"))

#Aggregate centralities
mcc1<-round(mean(cc1),2)
sdc1<-round(sd(cc1),2)
mcc2<-round(mean(cc2),2)
sdc2<-round(sd(cc2),2)
mcc3<-round(mean(cc3),2)
sdc3<-round(sd(cc3),2)
mcc4<-round(mean(cc4),2)
sdc4<-round(sd(cc4),2)
mcc5<-round(mean(cc5),2)
sdc5<-round(sd(cc5),2)

#Aggregate modularities
mmod1<-mean(mod1)
sdmod1<-sd(mod1)
mmod2<-mean(mod2)
sdmod2<-sd(mod2)
mmod3<-mean(mod3)
sdmod3<-sd(mod3)
mmod4<-mean(mod4)
sdmod4<-sd(mod4)
mmod5<-mean(mod5)
sdmod5<-sd(mod5)

#Strain subsets
strains4<-clusters4$nodeclusters[,1]
dfstr4<-df%>%filter(strain %in% strains4)
strains5<-clusters5$nodeclusters[,1]
dfstr5<-df%>%filter(strain %in% strains5)
strains6<-clusters6$nodeclusters[,1]
dfstr6<-df%>%filter(strain %in% strains6)
strains7<-clusters7$nodeclusters[,1]
dfstr7<-df%>%filter(strain %in% strains7)
strains8<-clusters8$nodeclusters[,1]
dfstr8<-df%>%filter(strain %in% strains8)

#Network level prevalences
pstr4<-round(mean(dfstr4$serovar %in% c("4b","Serotype 4b"))*100,2)
pstr5<-round(mean(dfstr5$serovar %in% c("4b","Serotype 4b"))*100,2)
pstr6<-round(mean(dfstr6$serovar %in% c("4b","Serotype 4b"))*100,2)
pstr7<-round(mean(dfstr7$serovar %in% c("4b","Serotype 4b"))*100,2)
pstr8<-round(mean(dfstr8$serovar %in% c("4b","Serotype 4b"))*100,2)

#Table 4
t4<-data.frame(Network=1:5,cc=c(mcc1,mcc2,mcc3,mcc4,mcc5),
sdc=c(sdc1,sdc2,sdc3,sdc4,sdc5),mmod=c(mmod1,mmod2,mmod3,mmod4,mmod5),
               sdmod=c(sdmod1,sdmod2,sdmod3,sdmod4,sdmod5),
               sp<-c(pstr4,pstr5,pstr6,pstr7,pstr8))
names(t4)<-c("Network","Mean Centrality","SD Centrality", 
             "Mean Modularity","SD Modularity","N.S.4b.P. (%)")

#Renders table 4
kable(t4, "latex",
      caption="Aggregate Network-Level Results")%>%
  kable_styling(latex_options="HOLD_position")

#############
#Diagnostics#
#############

#Match different clusters
dfnet4<-clusters4$nodeclusters
names(dfnet4)<-c("strain","netcl")
dfnet5<-clusters5$nodeclusters
names(dfnet5)<-c("strain","netcl")
dfnet6<-clusters6$nodeclusters
names(dfnet6)<-c("strain","netcl")
dfnet7<-clusters7$nodeclusters
names(dfnet7)<-c("strain","netcl")
dfnet8<-clusters8$nodeclusters
names(dfnet8)<-c("strain","netcl")
dfcomb4<-left_join(dfstr4,dfnet4,by="strain")
dfcomb5<-left_join(dfstr5,dfnet5,by="strain")
dfcomb6<-left_join(dfstr6,dfnet6,by="strain")
dfcomb7<-left_join(dfstr7,dfnet7,by="strain")
dfcomb8<-left_join(dfstr8,dfnet8,by="strain")

#SNP, k-means, and network clusters
snp4<-dfcomb4$snpc
nsnp4<-as.numeric(factor(snp4,levels=unique(snp4),
                         labels=1:length(unique(snp4))))
kc4<-dfcomb4$cluster
netc4<-as.numeric(dfcomb4$netcl)
snp5<-dfcomb5$snpc
nsnp5<-as.numeric(factor(snp5,levels=unique(snp5),
                         labels=1:length(unique(snp5))))
kc5<-dfcomb5$cluster
netc5<-as.numeric(dfcomb5$netcl)
snp6<-dfcomb6$snpc
nsnp6<-as.numeric(factor(snp6,levels=unique(snp6),
                         labels=1:length(unique(snp6))))
kc6<-dfcomb6$cluster
netc6<-as.numeric(dfcomb6$netcl)
snp7<-dfcomb7$snpc
nsnp7<-as.numeric(factor(snp7,levels=unique(snp7),
                         labels=1:length(unique(snp7))))
kc7<-dfcomb7$cluster
netc7<-as.numeric(dfcomb7$netcl)
snp8<-dfcomb8$snpc
nsnp8<-as.numeric(factor(snp8,levels=unique(snp8),
                         labels=1:length(unique(snp8))))
kc8<-dfcomb8$cluster
netc8<-as.numeric(dfcomb8$netcl)

#Total Rand for k means and SNP
snptotal<-df$snpc
nsnp<-as.numeric(factor(snptotal,levels=unique(snptotal),
                        labels=1:length(unique(snptotal))))
rks<-rand.index(nsnp,df$cluster)

#Rand indices
rik1<-round(rand.index(netc4,kc4),3)
ris1<-round(rand.index(netc4,nsnp4),3)
rik2<-round(rand.index(netc5,kc5),3)
ris2<-round(rand.index(netc5,nsnp5),3)
rik3<-round(rand.index(netc6,kc6),3)
ris3<-round(rand.index(netc6,nsnp6),3)
rik4<-round(rand.index(netc7,kc7),3)
ris4<-round(rand.index(netc7,nsnp7),3)
rik5<-round(rand.index(netc8,kc8),3)
ris5<-round(rand.index(netc8,nsnp8),3)

#Table 5
t5<-data.frame(Network=1:5,km=c(rik1,rik2,rik3,rik4,rik5),
               snpcs=c(ris1,ris2,ris3,ris4,ris5))
names(t5)<-c("Network","K Means Rand Index","SNP Rand Index")

#Render table 5
kable(t5, "latex",
      caption="Rand Indices for All Model Combinations")%>%
  kable_styling(latex_options="HOLD_position")