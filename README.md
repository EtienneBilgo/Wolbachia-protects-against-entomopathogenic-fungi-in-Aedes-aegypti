# Wolbachia-protects-against-entomopathogenic-fungi-in-Aedes-aegypti
Wolbachia confers protection against the entomopathogenic fungus Metarhizium pingshaense in African Aedes aegypti. 
Local_Fungal_Strains_Aedes_Def <- read.csv("~/Desktop/AEM 2/SRR/data_SRR/Local_Fungal_Strains_Aedes_Def.csv")
View(Local_Fungal_Strains_Aedes_Def)
levels(Local_Fungal_Strains_Aedes_Def$Treatments)
levels(Local_Fungal_Strains_Aedes_Def$Concentrarions)
str(Local_Fungal_Strains_Aedes_Def)
#Manipulations and Analysis
library(plyr)
df=ddply(Local_Fungal_Strains_Aedes_Def, .(Replicates,Treatments,Concentrations), transform, nval=sum(Mortality, na.rm=TRUE))
View(df)
df1=ddply(df, .(Replicates,Treatments,Concentrations), transform, Dead=cumsum(Mortality))
View(df1)
df1$Survival=1-df1$Dead/df1$nval
View(df1)
df2=subset(df1, Day!="Alives")
View(df2)
df2$Day=as.numeric(as.character(df2$Day))
View(df2)
df3=ddply(df2, .(Day,Treatments,Concentrations), summarize, mean=mean(Survival), replicates=length(Survival), se=sd(Survival)/sqrt(length(Survival)))
View(df3)
library(plyr)
library(plotly)
library(ggplot2)
library(scales)
limits=aes(ymax=mean+se, ymin=mean-se)
theme = theme_bw()+theme(text = element_text(size=25), axis.title.x = element_text(size=30), title = element_text(size=35))
View(df3)
levels(df3$Treatments)
S_df3=subset(df3,Day=="14")
View(S_df3)
cbPalette <- c("#17EE6D","#055926 ","#B73DE5","#3D7DE5","#17181A")
LF_Plt=ggplot(df3, aes(Day, mean, color=Treatments))+geom_line(size=2)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, size=1)+theme+scale_fill_manual(values=cbPalette)+scale_y_continuous(labels=percent)+ylab("survival(%)")+xlab("Days-Post infection")+facet_wrap(~Concentrations)
#Displays the plot
LF_Plt
## Analysis
LF_S_df3=subset(df3,Day=="14")
View(LF_S_df3)
View(df2)
LF_S_df2=subset(df2,Day=="14")
#Calculate means and standard error
View(df2)
LF_LT50=ddply(subset(df2, Survival<= 0.5), .(Replicates,Treatments,Concentrations), summarize, LT50=min(Day))
View(LF_LT50)
LF_LT50.Error=ddply(LF_LT50, .(Treatments,Concentrations), summarize, "LT50 Mean"=mean(LT50), se=sd(LT50)/sqrt(length(LT50)), Replicates=length(LT50))
View(LF_LT50.Error)
## Pairwise T-test 
View(LT50)
### Bioassays_Co_infections_Wolbachia_Fungus on Aedes aegypti###
V_dat <- read.csv("~/Desktop/AEM/V_dat.csv")
View(V_dat)
colnames(V_dat)
levels(V_dat$Mosquito.species)[1]="wAlbB"
levels(V_dat$Mosquito.species)[2]="wAu"
levels(V_dat$Treatments)[4]="Control"
#Manipulations and Analysis
library(plyr)
df=ddply(V_dat, .(Replicates,Treatments,Mosquito.species), transform, nval=sum(Mortality, na.rm=TRUE))
View(df)
df1=ddply(df, .(Replicates,Treatments,Mosquito.species), transform, Dead=cumsum(Mortality))
View(df1)
df1$Survival=1-df1$Dead/df1$nval
View(df1)
df2=subset(df1, Day!="Alives")
View(df2)
df2$Day=as.numeric(as.character(df2$Day))
View(df2)
df3=ddply(df2, .(Day,Treatments,Mosquito.species), summarize, mean=mean(Survival), replicates=length(Survival), se=sd(Survival)/sqrt(length(Survival)))
View(df3)
library(plyr)
library(plotly)
library(ggplot2)
library(scales)
limits=aes(ymax=mean+se, ymin=mean-se)
theme = theme_bw()+theme(text = element_text(size=25), axis.title.x = element_text(size=30), title = element_text(size=35))
levels(df3$Treatments)
cbPalette <- c("#17EE6D","#055926 ","#B73DE5","#3D7DE5")
Plt=ggplot(df3, aes(Day, mean, color=Treatments))+geom_line(size=2)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, size=1)+theme+scale_fill_manual(values=cbPalette)+scale_y_continuous(labels=percent)+ylab("survival(%)")+xlab("Days-Post infection")+facet_wrap(~Mosquito.species)
#Displays the plot
Plt+scale_color_manual(values=c("#17EE6D","#055926","#B73DE5","#3D7DE5"))
## Analysis
S_df3=subset(df3,Day=="14")
View(S_df3)
View(df2)
S_df2=subset(df2,Day=="14")
#Calculate means and standard error
View(df2)
LT50=ddply(subset(df2, Survival<= 0.5), .(Replicates,Treatments,Mosquito.species), summarize, LT50=min(Day))
View(LT50)
LT50.Error=ddply(LT50, .(Treatments,Mosquito.species), summarize, "LT50 Mean"=mean(LT50), se=sd(LT50)/sqrt(length(LT50)), Replicates=length(LT50))
View(LT50.Error)
## Pairwise T-test 
View(LT50)
## Calculate LT80
View(df2)
LT90=ddply(subset(df2, Survival<= 0.1), .(Replicates,Treatments,Mosquito.species), summarize, LT90=min(Day))
View(LT90)
LT80.Error=ddply(LT80, .(Treatments,Mosquito.species), summarize, "LT80 Mean"=mean(LT80), se=sd(LT80)/sqrt(length(LT80)), Replicates=length(LT80))
View(LT80.Error)
## Co-infectionn_Fecundity and Fertility Data
FF_dat <- read.csv("~/Desktop/AEM/FF_dat.csv")
View(FF_dat)
FF_dat$Vitality=(FF_dat$Larvae.number/FF_dat$Eggs.number)*100
View(FF_dat)
dat1=ddply(FF_dat, .(Mosquito.Line,Treatment), summarize, Eggs=mean(Eggs.number), se=(sd(Eggs.number)/sqrt(length(Eggs.number))), Replicates=length(Eggs.number))
View(dat1)
dat2=ddply(FF_dat, .(Mosquito.Line,Treatment), summarize, Larvae=mean(Larvae.number), se=(sd(Larvae.number)/sqrt(length(Larvae.number))), Replicates=length(Larvae.number))
View(dat2)
dat3=ddply(FF_dat, .(Mosquito.Line,Treatment), summarize, Vitality=mean(Vitality), se=(sd(Vitality)/sqrt(length(Vitality))), Replicates=length(Vitality))
View(dat3)
library(plyr)
View(FF_dat)
## Data Analysis_Wolbachia Density
densite.wolbachia <- read.csv("~/Desktop/densite-wolbachia.csv")
View(densite.wolbachia)
colnames(densite.wolbachia)
# Data manupilations
library(reshape2)
#melt(Data set, column names to keep, column names to restructure)
df=melt(densite.wolbachia, colnames(densite.wolbachia)[c(1,2)], colnames(densite.wolbachia)[3:9])
View(df)
colnames(df)[3] = "Days"
colnames(df)[4] = "HTH"
colnames(df)
library(plyr)
library(plotly)
library(ggplot2)
library(scales)
View(df)
library(dplyr)
df1=ddply(df,.(Wol.status,Met.status,Days), summarize, "mean_HTH"= mean(HTH), replicates=length(HTH),se=sd(HTH)/sqrt(length(HTH)))
View(df1)
View(M_df1)
library(ggplot2)
limits=aes(ymax=mean_HTH+se, ymin=mean_HTH-se)

theme = theme_bw()+theme(text = element_text(size=25), 
                         axis.title.x = element_text(size=25),
                         title = element_text(size=25), 
                         legend.title = element_text(size=25),
                         legend.text = element_text(size=20))
cbPalette <-c(117,139)
colnames(df1)
df1 <- na.omit(df1)
View(df1)
limits=aes(ymax=mean_HTH+se, ymin=mean_HTH-se)
theme = theme_bw()+theme(text = element_text(size=90),axis.title.x = element_text(size=90),title = element_text(size=100))
cbPalette <- c("#cc0000","#007acc")
plt <- ggplot(df1, aes(x = Days, y = mean_HTH, group = Met.status, color = Met.status))+geom_line(size = 3)+geom_errorbar(limits, width=.2, size=2)+theme_minimal()+scale_color_manual(values = cbPalette)+ylab("Delta CT Wolbachia / Delta CT Host ")+xlab("Days")+facet_wrap(~Wol.status)
plt
+scale_fill_manual(values=c("#cc0000","#007acc"))
#Additonal statistics and calculations
## Mean HTH with control Groups
M_df1=ddply(df,.(Wol.status,Met.status), summarize, "mean_HTH"= mean(HTH), replicates=length(HTH),se=sd(HTH)/sqrt(length(HTH)))
View(M_df1)
## T-test of comparing groups
Unf_df1<-subset(df,Met.status=="Uninfected")
View(Unf_df1)
colnames(Unf_df1)
t.test(Unf_df1$HTH~Unf_df1$Wol.status)
## Results
# Welch Two Sample t-test
# data:  Unf_df1$HTH by Unf_df1$Wol.status
# t = -5.1972, df = 80.267, p-value = 1.505e-06
# alternative hypothesis: true difference in means between group wAlbB and group wAu is not equal to 0
# 95 percent confidence interval:
#  -0.9448493 -0.4216352
# sample estimates:
# mean in group wAlbB   mean in group wAu 
#             0.8857577           1.5690000 

