setwd('C:\\Analytics\\MachineLearning\\water quality in India')
library(ggplot2)
library(readr)
library(RColorBrewer)
library(dplyr)
library(ggmap)
library(XML)
library(tidyr)
library(ggplot2)
install.packages("ggplot2")
install.packages("colorspace")
library(tidyverse)
train <- read.csv('IndiaAffectedWaterQualityAreas.csv',stringsAsFactors = FALSE)
head(train)
train$Year <- as.Date(train$Year,"%d/%m/%Y")
table(train$State.Name)

# Data cleaning and making dataset ready
train$State.Name<- gsub(pattern ="\\(.*","",train$State.Name)
train$State.Name <- gsub(pattern = 'CHATTISGARH','CHHATTISGARH',train$State.Name)
train$District.Name<- gsub(pattern = "\\(.*","",train$District.Name)
train$Block.Name<- gsub(pattern = "\\(.*","",train$Block.Name)
train$Panchayat.Name<- gsub(pattern = "\\(.*","",train$Panchayat.Name)
train$Quality.Parameter<- as.factor(train$Quality.Parameter)
str(train)

# Identifying which chemical is large

chemicals_present <- as.data.frame(table(train$Quality.Parameter),stringsAsFactors = FALSE)
names(chemicals_present) <- c("CHEMICAL","FREQ_REPORTED")
f <- ggplot(chemicals_present,aes(chemicals_present$CHEMICAL,chemicals_present$FREQ_REPORTED))
f+geom_bar(stat = "identity",fill= brewer.pal(5,"Set2"))+labs(title="Identifying Most Occuring Chemical",x="Chemicals",y="Cases")+
  theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
        axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2))

# As we can clearly see the content of IRON is at large in the water bodies,second comes Salinity ,then Fluoride,Arsenic and Nitrate.


# Finding The overall trend in CHEMICAL change over the years

kk<- as.data.frame(table(train$Year,train$Quality.Parameter),stringsAsFactors=FALSE)
kk$Var1<- as.Date.factor(kk$Var1)
kk$Var2<-as.factor(kk$Var2)
options(warn = -1)
qplot(Freq,Var1,data = kk,facets = Var2~.,geom = c("point"),color=Var2) + 
  labs(title = "Trend seen in Chemicals over the Years",x="Number of cases",y='Years',fill='Chemicals')+
  theme(plot.title = element_text(face = 'bold.italic',size = rel(2)),axis.text = element_text(colour = 'blue'),axis.title.x = element_text(face = 'bold.italic'),
        
        axis.title.y = element_text(face = 'bold.italic'),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
        legend.box.margin = margin(6,6,6,6))

# From above it is seen that there is a general downward trend as years goes by.



# Overview of chemical presence in Different states of India
overview<- as.data.frame(table(train$State.Name,train$Quality.Parameter,train$Year))
names(overview) <- c("State.Name",'CHEMICAL','YEAR','Freq')
str(overview)
an<- ggplot(overview,aes(overview$State.Name,overview$Freq,fill=overview$CHEMICAL))
an+geom_bar(stat="identity",position = "dodge")+theme(axis.text.x = element_text(angle = 90))+
  labs(title="Chemical Compostion instates of India",x= "States", y="Number_Of_Cases",fill="CHEMICALS")+
  theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
        axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))

#we observe  here ASSAM,BIHAR,RAJASTHAN has larger cases of chemical composition reported



#Lets take each state chemicals wise and see


goal1<-as.data.frame(table(train$State.Name,train$Quality.Parameter))
names(goal1) <- c("State.Name","CHEMICAL","Freq")
str(goal1)
an <- ggplot(goal1,aes(goal1$State.Name,goal1$Freq,fill=goal1$CHEMICAL))
an+geom_bar(stat="identity",position = "dodge")+facet_wrap(~goal1$CHEMICAL,scales="free")+
  labs(title="Specific Chemicals in states of India",x= "States", y="Number_Of_Cases",fill="CHEMICALS")+scale_fill_brewer( type = "qua", palette = "Dark2", direction = 1)+
  theme(axis.text.x = element_text(angle = 90),plot.background = element_rect(fill = NA),plot.title = element_text(size = rel(2)), panel.background = element_rect(fill =NA),axis.text = element_text(colour = "blue"),
        panel.grid.major = element_line(colour = "black"),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))

table(train$State.Name,train$Quality.Parameter)


#Iron-ASSAM ,Bihar,Chhatisgargh,orissa
#Fluoride-Rajasthan,second worst Bihar
#Arsenic-westbengal,Assam,Bihar
#Nitrate,Karnataka,Maharashtra,Rajsthan
#salinity-Rajasthan
#so the main problem occurs in ASSAM ,BIHAR&RAJASTHAN as they aranked higher in presence of more than two chemicals at larger cases.
#WESTBENGAL has the highest presence of Arsenic in them but other than that other chemical reported are relatively less.
#we will analyse the trend and see district wise report of these states to get a clearer picture.


#DISTRICT WISE FOR STATE OF ASSAM

state_ASSAM<- subset(train,train$State.Name=='ASSAM')
ASSAM<- as.data.frame(table(state_ASSAM$District.Name,state_ASSAM$Quality.Parameter,state_ASSAM$Year),stringsAsFactors=FALSE)
names(ASSAM) <- c("District.Name","CHEMICAL",'YEAR',"Freq")
assam <- ggplot(ASSAM,aes(ASSAM$CHEMICAL,ASSAM$Freq,fill=ASSAM$District.Name))
assam+geom_bar(stat="identity",position = "dodge")+facet_grid(.~ASSAM$YEAR)+
  labs(title="TREND of Chemical Compostion in ASSAM Villages",x="Chemicals",y="Number Of Cases",fill="Districts in ASSAM")+
  theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
        axis.text.x = element_text(angle = 90),axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))


# Generally the trend shows a downward trend in all the districts with a spike in the year 2011,then it has decreased. 
#District of Sontipur has the highest iron content

#DISTRICT WISE FOR STATE OF BIHAR



state_BIHAR<- subset(train,train$State.Name=="BIHAR")
BIHAR <- as.data.frame(table(state_BIHAR$District.Name,state_BIHAR$Quality.Parameter,state_BIHAR$Year),stringsAsFactors = FALSE)
names(BIHAR) <- c("District.Name","CHEMICAL","YEAR","Freq")
bihar <- ggplot(BIHAR,aes(BIHAR$CHEMICAL,BIHAR$Freq,fill=BIHAR$District.Name))
bihar+geom_bar(stat="identity",position = "dodge")+facet_grid(.~BIHAR$YEAR)+
  labs(title="TREND of Chemical Compostion in BIHAR Villages",x="Chemicals",y="Number Of Cases",fill="Districts in BIHAR")+
  theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
        axis.text.x = element_text(angle = 90),axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))
# Generally the trend shows a downward trend in all the districts,then it has decreased.
# District of Purnia has the highest iron content.


#DISTRICT WISE FOR STATE OF RAJASTHAN


state_RAJASTHAN <- subset(train,train$State.Name=="RAJASTHAN")
RAJASTHAN <- as.data.frame(table(state_RAJASTHAN$District.Name,state_RAJASTHAN$Quality.Parameter,state_RAJASTHAN$Year),stringsAsFactors = FALSE)
#str(RAJASTHAN)
names(RAJASTHAN) <- c("District.Name","CHEMICAL","YEAR","Freq")
rajasthan<- ggplot(RAJASTHAN,aes(RAJASTHAN$CHEMICAL,RAJASTHAN$Freq,fill=RAJASTHAN$District.Name))
rajasthan+geom_bar(stat="identity",position = "dodge")+facet_grid(.~RAJASTHAN$YEAR)+
  labs(title="Chemical Compostion in RAJASTHAN",x="Chemicals",y="Number Of Cases",fill="Districts in RAJASTHAN")+
  theme(plot.title = element_text(face="bold.italic",size = rel(2)),axis.text = element_text(colour = "blue"),axis.title.x=element_text(face="bold.italic"),
        axis.text.x = element_text(angle = 90),axis.title.y=element_text(face="bold.italic"),panel.grid.major.y = element_blank(),axis.ticks = element_line(size = 2),legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))


# Generally the trend shows a downward trend in all the districts,then it has decreased.
#District of Barmer has the highest salinity content.

#conclusion
#The generally chemicals reported cases is decreasing through the years 2009 to 2012.
#Iron is the most present chemical in the water.
#During these four years with the data availabe we can preassume developmental activites has not caused any increased in chemicals concentration.