###METHODS AND ANALYSIS CODE

#Libraries
library(tidyverse)
library(ggplot2)
library(tableone)
library(table1)
library(knitr)
library(IRdisplay)
library(kableExtra)
library(MASS)
library(cluster)
library(factoextra)
library(clValid)

#Removes Unneeded Variables
#Load in Data
df<-subset(df,select=-Create.date)
df<-subset(df,select=-Organism.group)
df<-subset(df,select=-Outbreak)
df<-subset(df,select=-Strain)
df<-subset(df,select=-Isolate.identifiers)

#Conflates Distance Variables
min.dist<-rep(0,nrow(df))
for(k in 1:nrow(df)){
  if(is.na(df$Min.same[k])==T & 
     is.na(df$Min.diff[k])==T){
    min.dist[k]<-NA
  }
  if(is.na(df$Min.same[k])==F & 
     is.na(df$Min.diff[k])==T){
    min.dist[k]<-df$Min.same[k]
  }
  if(is.na(df$Min.same[k])==T & 
     is.na(df$Min.diff[k])==F){
    min.dist[k]<-df$Min.diff[k]
  }
  if(is.na(df$Min.same[k])==F & 
     is.na(df$Min.diff[k])==F){
    min.dist[k]<-min(df$Min.same[k],df$Min.diff[k])
  }
}
df$min.dist<-min.dist

#Selects fully observed data
df<-df%>%drop_na()
num_df<-subset(df,select=c("N50","Contigs","min.dist"))
num_df<-num_df%>%drop_na()

#Sets seed for clustering
set.seed(2)

#Creates clusters
cmodel<-kmeans(num_df,centers=3,nstart=20)
df$cluster<-cmodel$cluster

#Separates data by clusters
df1<-df%>%filter(cluster==1)
df2<-df%>%filter(cluster==2)
df3<-df%>%filter(cluster==3)

#Top Sources (Extracted Manually)
#tail(sort(table(df1$Isolation.source)),15)
#tail(sort(table(df2$Isolation.source)),15)
#tail(sort(table(df3$Isolation.source)),15)

#Source Table
source.table1<-data.frame(
Source=c("Food","Environmental","Clinical"),
Prevalence=c(11+12+25+30+45,12+27+39+20+11,19+82)/(nrow(df1)-173))
names(source.table1)<-c("Source (Cluster 1)","Prevalence")
source.table2<-data.frame(
Source=c("Food","Environmental","Clinical"),
Prevalence=c(182+204+1479,149+521+728+1428,
779+414+410+191)/(nrow(df2)-(130+246+2224)))
names(source.table2)<-c("Source (Cluster 2)","Prevalence")
source.table3<-data.frame(
Source=c("Food","Environmental","Clinical"),
Prevalence=c(110+129+130+248+1395,
125+136+222+458+647+1165,376+621)/(nrow(df3)-(1594)))
names(source.table3)<-c("Source (Cluster 3)","Prevalence")

#Format Table
kable(list(source.table1,source.table2,source.table3),"latex",
caption="Source Prevalence by Cluster")%>%
kable_styling(latex_options="HOLD_position")

#Format Figure Dimensions
knitr::opts_chunk$set(fig.width=3, fig.height=3) 

#Figure 1: Contigs vs Distance
ggplot()+geom_point(aes(x=Min.same,y=Contigs,col="Cluster 1"),
data=df1,alpha=0.05)+
geom_point(aes(x=Min.same,y=Contigs,col="Cluster 2"),
data=df2,alpha=0.05)+
geom_point(aes(x=Min.same,y=Contigs,col="Cluster 3"),
data=df3,alpha=0.05)+
labs(x="Minimum Distance",y="Number of Contigs",
title="Figure 1: Contigs Against Distance")+
scale_color_manual(values=c("green","blue","red"),name="Legend")+
theme(legend.position="bottom",
legend.key.size=unit(0.2, 'cm'),
legend.text=element_text(size=8),
title=element_text(size=8))

#Figure 2: N50 vs Distance
ggplot()+geom_point(aes(x=Min.same,y=N50,col="Cluster 1"),
data=df1,alpha=0.5)+
geom_point(aes(x=Min.same,y=N50,col="Cluster 2"),
data=df2,alpha=0.5)+
geom_point(aes(x=Min.same,y=N50,col="Cluster 3"),
data=df3,alpha=0.5)+
labs(x="Minimum Distance",y="N50",
title="Figure 2: N50 Against Distance")+
scale_color_manual(values=c("green","blue","red"),name="Legend")+
theme(legend.position="bottom",
legend.key.size=unit(0.2, 'cm'),
legend.text=element_text(size=8),
title=element_text(size=8))

#Figure 3: Contigs vs N50
ggplot()+geom_point(aes(x=N50,y=Contigs,
col="Cluster 1"),data=df1,alpha=0.5)+
geom_point(aes(x=N50,y=Contigs,col="Cluster 2"),data=df2,alpha=0.5)+
geom_point(aes(x=N50,y=Contigs,col="Cluster 3"),data=df3,alpha=0.5)+
labs(x="N50",y="Number of Contigs",
title="Figure 3: Contigs Against N50")+
scale_color_manual(values=c("green","blue","red"),name="Legend")+
theme(legend.position="bottom",
legend.key.size=unit(0.2, 'cm'),legend.text=element_text(size=8),
title=element_text(size=8))

#Figure 4: Optimal Number of Clusters
cluster.sample<- num_df[sample(1:nrow(num_df), 10000),]
fviz_nbclust(cluster.sample, kmeans, method="silhouette")+
labs(title="Figure 4: Optimal Number of Clusters",
x="Number of Clusters (k)",y="Average Silhouette")+
theme(title=element_text(size=8))

#Concatenation of Common SNP Clusters
SNP1<-as.data.frame(sort(table(df1$SNP.cluster)))
SNP2<-as.data.frame(sort(table(df2$SNP.cluster)))
SNP2ADJ<-SNP2%>%filter(Var1 %in% SNP1$Var1)
SNP3<-as.data.frame(sort(table(df3$SNP.cluster)))
SNP3ADJ<-SNP3%>%filter(Var1 %in% SNP1$Var1)
SNP2ADJ2<-SNP2ADJ%>%filter(Var1 %in% SNP3$Var1)
SNP3ADJ2<-SNP3ADJ%>%filter(Var1 %in% SNP2ADJ2$Var1)
SNP1ADJ<-SNP1%>%filter(Var1 %in% SNP3ADJ2$Var1)
snp.final1<-SNP1ADJ
snp.final2<-SNP2ADJ2
snp.final3<-SNP3ADJ2
df.snp<-left_join(snp.final1,snp.final2,by="Var1")
df.snp<-left_join(df.snp,snp.final3,by="Var1")
df.snp<-subset(df.snp,select=-Var1)

#Test of Association
chisq.test(df.snp)