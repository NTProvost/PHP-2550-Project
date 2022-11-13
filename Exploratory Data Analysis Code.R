#Libraries
library(tidyverse)
library(ggplot2)
library(tableone)
library(table1)
library(knitr)
library(IRdisplay)
library(kableExtra)

#Removes Unneeded Variables
#Load in Data
df<-subset(df,select=-Create.date)
df<-subset(df,select=-Organism.group)
df<-subset(df,select=-Outbreak)
df<-subset(df,select=-Strain)
df<-subset(df,select=-Isolate.identifiers)

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

#Labels Sporadic Cases
for(k in 1:nrow(df)){
  if(df$Outbreak[k]==""){
    df$Outbreak[k]<-"Sporadic"
  }
}

#Conflates Distance Variables
min.dist<-rep(0,nrow(df))
for(k in 1:nrow(df)){
  if(is.na(df$Min.same[k])==T & is.na(df$Min.diff[k])==T){
    min.dist[k]<-NA
  }
  if(is.na(df$Min.same[k])==F & is.na(df$Min.diff[k])==T){
    min.dist[k]<-df$Min.same[k]
  }
  if(is.na(df$Min.same[k])==T & is.na(df$Min.diff[k])==F){
    min.dist[k]<-df$Min.diff[k]
  }
  if(is.na(df$Min.same[k])==F & is.na(df$Min.diff[k])==F){
    min.dist[k]<-min(df$Min.same[k],df$Min.diff[k])
  }
}
df$min.dist<-min.dist

#Conflates Source Variables and Removes Unneeded Variables 
for(k in 1:nrow(df)){
  if(df$Isolation.source[k]=="" & df$Isolation.type[k]!=""){
    df$Isolation.source[k]<-df$Isolation.type[k]
  }
}
df.adj<-subset(df,select=-c(Min.same,Min.diff,Isolation.type))

#Separates Data into Subsets with/without Distance Data
#Using a Distance Indicator
dst<-rep(0,nrow(df.adj))
for(k in 1:nrow(df.adj)){
  if(is.na(df.adj$min.dist[k])==F){
    dst[k]<-1
  }
  if(is.na(df.adj$min.dist[k])==T){
    dst[k]<-0
  }
}
df.adj$dst<-dst
df.dist<-df.adj%>%filter(dst==1)
df.nodist<-df.adj%>%filter(dst==0)

#Computes Top 10 Isolates for Each Subset as a Table
dist.sources<-as.data.frame(tail(sort(table(df.dist$Isolation.source)),10))
nodist.sources<-as.data.frame(tail(sort(table(df.nodist$Isolation.source)),10))
names(dist.sources)<-c("Source (Distance)","Number of Occurances")
names(nodist.sources)<-c("Source (No Distance)","Number of Occurances")
nodist.sources$`Source (No Distance)`[10]<-NA

#Render Table 1 in Latex
#Numbers inserted manually from df.dis and df.nodist
kable(list(dist.sources,nodist.sources), 
"latex",caption="Most Frequent Sources for Each Subset")%>%
kable_styling(latex_options="HOLD_position")

#Computes Frequency of Sources by Distance Subsets
dist.compare<-data.frame(Source=c("Human","Blood","Environmental","Food"),
Frequency.With.Distance=c(941+1670,2102,2257+4503+1432,3984)/nrow(df.dist),
Frequency.Without.Distance=c(295+319,980,169+218+106,1056)/nrow(df.nodist))

#Subsets the Data by Year
df10<-df.adj%>%filter(Collection.date=="2010")
df11<-df.adj%>%filter(Collection.date=="2011")
df12<-df.adj%>%filter(Collection.date=="2012")
df13<-df.adj%>%filter(Collection.date=="2013")
df14<-df.adj%>%filter(Collection.date=="2014")
df15<-df.adj%>%filter(Collection.date=="2015")
df16<-df.adj%>%filter(Collection.date=="2016")
df17<-df.adj%>%filter(Collection.date=="2017")
df18<-df.adj%>%filter(Collection.date=="2018")
df19<-df.adj%>%filter(Collection.date=="2019")
df20<-df.adj%>%filter(Collection.date=="2020")

#Figure 1: Densities of Distance 2010 to 2015
ggplot()+geom_density(aes(x=min.dist,color="2010"),
  data=df10,na.rm=T)+
  geom_density(aes(x=min.dist,color="2011"),
  data=df11,na.rm=T)+
  geom_density(aes(x=min.dist,color="2012"),
  data=df12,na.rm=T)+
  geom_density(aes(x=min.dist,color="2013"),
  data=df13,na.rm=T)+
  geom_density(aes(x=min.dist,color="2014"),
  data=df14,na.rm=T)+
  geom_density(aes(x=min.dist,color="2015"),
  data=df15,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple","black"),
  name="Year")+theme(legend.position="bottom",
  legend.text=element_text(size=7),
  legend.key.size=unit(0.1,"cm"),
  plot.title=element_text(size=10))+
  labs(x="Minimum Distance to Another Isolate",
  y="Probability Density",title="Figure 1: Probability Densities of
  Minimum Distances (2010 - 2015)")

#Figure 2: Densities of Distance 2016 to 2020
ggplot()+geom_density(aes(x=min.dist,color="2016"),
  data=df16,na.rm=T)+
  geom_density(aes(x=min.dist,color="2017"),
  data=df17,na.rm=T)+
  geom_density(aes(x=min.dist,color="2018"),
  data=df18,na.rm=T)+
  geom_density(aes(x=min.dist,color="2019"),
  data=df19,na.rm=T)+
  geom_density(aes(x=min.dist,color="2020"),
  data=df20,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple"),
  name="Year")+theme(legend.position="bottom",
  legend.text=element_text(size=7),
  legend.key.size =unit(0.1,"cm"),
  plot.title=element_text(size=10))+
  labs(x="Minimum Distance to Another Isolate",
  y="Probability Density",title="Figure 2: Probability Densities of
  Minimum Distances (2016-2020)")

#Source Subsets
dfenv<-df.adj%>%
  filter(Isolation.source=="environmental" |
  Isolation.source=="environmental swab sponge" |
  Isolation.source=="environmental swab")
dffood<-df.adj%>%
  filter(Isolation.source=="food" |
  Isolation.source=="Food Processing")
dfhuman<-df.adj%>%
  filter(Isolation.source=="human" |
  Isolation.source=="human listeriosis")
dfclin<-df.adj%>%filter(Isolation.source=="clinical")
dfwat<-df.adj%>%filter(Isolation.source=="water sediment")
dfblood<-df.adj%>%filter(Isolation.source=="blood")

#Table 2: Contig Descriptives
t2<-data.frame(x=c("Environmental","Food","Human",
  "Clinical","Water","Blood"),y=c(mean(dfenv$min.dist,na.rm=T),
  mean(dfblood$min.dist,na.rm=T),mean(dfhuman$min.dist,na.rm=T),
  mean(dfclin$min.dist,na.rm=T),mean(dfwat$min.dist,na.rm=T),
  mean(dfblood$min.dist,na.rm=T)),z=c(sd(dfenv$min.dist,na.rm=T),
  sd(dfblood$min.dist,na.rm=T),sd(dfhuman$min.dist,na.rm=T),
  sd(dfclin$min.dist,na.rm=T),sd(dfwat$min.dist,na.rm=T),
  sd(dfblood$min.dist,na.rm=T)))
names(t2)<-c("Source","Mean","Standard Deviation")
kable(t2, "latex",
caption="Descriptive Statistics of Minimum Distances by Source")%>%
kable_styling(latex_options="HOLD_position")

#Figure 3: Densities by Source
ggplot()+geom_density(aes(x=min.dist,color="Environmental"),
  data=dfenv,na.rm=T)+
  geom_density(aes(x=min.dist,color="Food"),
  data=dffood,na.rm=T)+
  geom_density(aes(x=min.dist,color="Blood"),
  data=dfblood,na.rm=T)+
  geom_density(aes(x=min.dist,color="Human"),
  data=dfhuman,na.rm=T)+
  geom_density(aes(x=min.dist,color="Clinical"),
  data=dfclin,na.rm=T)+
  geom_density(aes(x=min.dist,color="Water"),
  data=dfwat,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple","black"),
  name="Year")+theme(legend.position="bottom",
  plot.title=element_text(size=15))+
  labs(x="Minimum Distance to Another Isolate",
  y="Probability Density",title="Figure 3: Probability Densities of
  Minimum Distances by Source")

#Outbreak/Sporadic Subsets and T Test
df.out<-df.adj%>%filter(Outbreak!="Sporadic")
df.spor<-df.adj%>%filter(Outbreak=="Sporadic")
t.test(df.out$min.dist,df.spor$min.dist)

#Table 3: Contig Descriptives
t3<-data.frame(x=c("Environmental","Food","Human",
  "Clinical","Water","Blood"),y=c(mean(dfenv$Contigs),
  mean(dfblood$Contigs),mean(dfhuman$Contigs),
  mean(dfclin$Contigs),mean(dfwat$Contigs),
  mean(dfblood$Contigs)),z=c(sd(dfenv$Contigs),
  sd(dfblood$Contigs),sd(dfhuman$Contigs),
  sd(dfclin$Contigs),sd(dfwat$Contigs),
  sd(dfblood$Contigs)))
names(t3)<-c("Source","Mean","Standard Deviation")
kable(t3, "latex",
caption="Descriptive Statistics of Contigs by Source")%>%
kable_styling(latex_options="HOLD_position")

#Figure 4: Densities by Source
ggplot()+geom_density(aes(x=Contigs,color="Environmental"),
 data=dfenv,na.rm=T)+
 geom_density(aes(x=Contigs,color="Food"),
 data=dffood,na.rm=T)+
 geom_density(aes(x=Contigs,color="Blood"),
 data=dfblood,na.rm=T)+
 geom_density(aes(x=Contigs,color="Human"),
 data=dfhuman,na.rm=T)+
 geom_density(aes(x=Contigs,color="Clinical"),
 data=dfclin,na.rm=T)+
 geom_density(aes(x=Contigs,color="Water"),
 data=dfwat,na.rm=T)+
 scale_color_manual(
 values=c("blue","red","green","orange","purple","black"),
 name="Year")+theme(legend.position="bottom",
 plot.title=element_text(size=15))+
 labs(x="Minimum Distance to Another Isolate",
 y="Probability Density",title="Figure 4: Probability Densities of
 Contig Counts by Source")

#Figure 5: Densities of Contigs 2010 to 2015
ggplot()+geom_density(aes(x=Contigs,color="2010"),
  data=df10,na.rm=T)+
  geom_density(aes(x=Contigs,color="2011"),
  data=df11,na.rm=T)+
  geom_density(aes(x=Contigs,color="2012"),
  data=df12,na.rm=T)+
  geom_density(aes(x=Contigs,color="2013"),
  data=df13,na.rm=T)+
  geom_density(aes(x=Contigs,color="2014"),
  data=df14,na.rm=T)+
  geom_density(aes(x=Contigs,color="2015"),
  data=df15,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple","black"),
  name="Year")+theme(legend.position="bottom",
  legend.key.size =unit(0.1,"cm"),
  legend.text=element_text(size=7),
  plot.title=element_text(size=10))+
  labs(x="Contig Count",
  y="Probability Density",title="Figure 5: Probability Densities of
  Contigs by Year (2010 - 2015)")

#Figure 6: Densities of Contigs 2016 to 2020
ggplot()+geom_density(aes(x=Contigs,color="2016"),
  data=df16,na.rm=T)+
  geom_density(aes(x=Contigs,color="2017"),
  data=df17,na.rm=T)+
  geom_density(aes(x=Contigs,color="2018"),
  data=df18,na.rm=T)+
  geom_density(aes(x=Contigs,color="2019"),
  data=df19,na.rm=T)+
  geom_density(aes(x=Contigs,color="2020"),
  data=df20,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple"),
  name="Year")+theme(legend.position="bottom",
  legend.key.size =unit(0.1,"cm"),
  legend.text=element_text(size=7),
  plot.title=element_text(size=10))+
  labs(x="Contig Count",
  y="Probability Density",title="Figure 6: Probability Densities of
  Contigs by Year (2016 - 2020)")

#Table of Most Common Serovars (Some Numbers were inserted manually from the
#command below) (Table 4)
#head(sort(table(df.adj$Serovar),decreasing=T),30)
t.sero<-data.frame(Serovar=c("1/2a","1/2b","1/2c","2a","2b","2c","3a","3b","3c",
  "4a","4b","4c"),ACount=c(2846+647,589+102,379+52,602+176,84+52,48,16,16,3,
  8,1136+440+17,42),AFreq=c(2846+647,589+102,379+52,602+176,84+52,48,16,16,3,
  8,1136+440+17,42)*100/nrow(df.adj))
names(t.sero)<-c("Serovar","Approximate Count","Approximate Frequency (%)")
kable(t.sero, "latex",
caption="Approximate Counts of Most Common Serovars")%>%
kable_styling(latex_options="HOLD_position")

#Serovar Subsets
df1.2a<-df.adj%>%filter(Serovar=="1/2a" | Serovar=="Serotype 1/2a")
df1.2b<-df.adj%>%filter(Serovar=="1/2b" | Serovar=="Serotype 1/2b")
df2a<-df.adj%>%filter(Serovar=="2a" | Serovar=="IIa")
df3a<-df.adj%>%filter(Serovar=="3a")
df3b<-df.adj%>%filter(Serovar=="3b")
df4b<-df.adj%>%filter(Serovar=="4b" | Serovar=="IVb" | Serovar=="Serotype 4b")

#Figure 7: Densities of Distance by Serovar
ggplot()+geom_density(aes(x=min.dist,color="1/2a"),
  data=df1.2a,na.rm=T)+
  geom_density(aes(x=min.dist,color="1/2b"),
  data=df1.2b,na.rm=T)+
  geom_density(aes(x=min.dist,color="2a"),
  data=df2a,na.rm=T)+
  geom_density(aes(x=min.dist,color="3a"),
  data=df3a,na.rm=T)+
  geom_density(aes(x=min.dist,color="3b"),
  data=df3b,na.rm=T)+
  geom_density(aes(x=min.dist,color="4b"),
  data=df4b,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple","black"),
  name="Serovar")+theme(legend.position="bottom",
  plot.title=element_text(face="bold",size=15))+
  labs(x="Minimum Distance to Another Isolate",
  y="Probability Density",title="Figure 7: Probability Densities of
  Distance by Serovar")

#Figure 8: Densities of Contigs by Serovar
ggplot()+geom_density(aes(x=Contigs,color="1/2a"),
  data=df1.2a,na.rm=T)+
  geom_density(aes(x=Contigs,color="1/2b"),
  data=df1.2b,na.rm=T)+
  geom_density(aes(x=Contigs,color="2a"),
  data=df2a,na.rm=T)+
  geom_density(aes(x=Contigs,color="3a"),
  data=df3a,na.rm=T)+
  geom_density(aes(x=Contigs,color="3b"),
  data=df3b,na.rm=T)+
  geom_density(aes(x=Contigs,color="4b"),
  data=df4b,na.rm=T)+
  scale_color_manual(
  values=c("blue","red","green","orange","purple","black"),
  name="Serovar")+theme(legend.position="bottom",
  plot.title=element_text(face="bold",size=15))+
  labs(x="Contig Count",
  y="Probability Density",title="Figure 8: Probability Densities of
  Contigs by Serovar")

#Table of Most Serovars by Location
#tail(sort(table(df.adj$Location),descending=T),20)
t.loc<-data.frame(Country=c("U.S.","Germany","France","Italy"),
 halfa=c(sum(df1.2a$Location=="USA"),sum(df1.2a$Location=="Germany"),
 sum(df1.2a$Location=="France"),sum(df1.2a$Location=="Italy")),
 halfb=c(sum(df1.2b$Location=="USA"),sum(df1.2b$Location=="Germany"),
 sum(df1.2b$Location=="France"),sum(df1.2b$Location=="Italy")),
 twoa=c(sum(df2a$Location=="USA"),sum(df2a$Location=="Germany"),
 sum(df2a$Location=="France"),sum(df2a$Location=="Italy")),
 threea=c(sum(df3a$Location=="USA"),sum(df3a$Location=="Germany"),
 sum(df3a$Location=="France"),sum(df3a$Location=="Italy")),
 threeb=c(sum(df3b$Location=="USA"),sum(df3b$Location=="Germany"),
 sum(df3b$Location=="France"),sum(df3b$Location=="Italy")),
 fourb=c(sum(df4b$Location=="USA"),sum(df4b$Location=="Germany"),
 sum(df4b$Location=="France"),sum(df4b$Location=="Italy"))
)

#Renders Table 5
names(t.loc)<-c("Country","1/2a","1/2b","2a","3a","3b","4b")
kable(t.loc, "latex",
caption="(Very Rough) Approximate Counts of Most Common Serovars by Country")%>%
kable_styling(latex_options="HOLD_position")