library(ggplot2) 
library(gridExtra)
install.packages("RColorBrewer")
library(RColorBrewer)
suppressMessages(library(dplyr))
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("corrgram")
library(corrgram)
install.packages("GGally")
library(GGally)
install.packages("CaTools")
library(caTools)
library(psych)

rm(list = ls())

#reading in data
vg_data<-read.csv("D:/UJM/data mining/project/video games/Video_Games_Sales_as_at_22_Dec_2016.csv",sep=",",header = TRUE)

head(vg_data)
names(vg_data)
dim(vg_data)
str(vg_data)


############Top 5 games(by global sales)##############

Top_5 <- vg_data %>%
  select(Name, Global_Sales) %>% 
  group_by(Name) %>%
  summarise(Total = sum(Global_Sales)) %>% arrange(desc(Total)) %>% head(5)

Top_5$Name
pct <- round(Top_5$Total/sum(Top_5$Total)*100)
labels <- paste(Top_5$Name, pct)
labels <- paste(labels,"%",sep="") 


pie(Top_5$Total,labels = labels, col=rainbow(length(labels)),
    main="Pie Chart of Top_5 games")


#########Grouping by popular consoles##############


#subsetting Nintendo

vgdata1<-subset(vg_data,vg_data$Publisher=="Nintendo")

vgdata$Platform

Nintendo<-as.vector(vgdata1$Platform)

Nintendo<-unique(Nintendo)



#Microsoft

vgdata2<-subset(vg_data,vg_data$Publisher=="Microsoft Game Studios")

vgdata2$Platform

Microsoft<-as.vector(vgdata2$Platform)

Microsoft<- unique(Microsoft)


#Sega

vgdata3<-subset(vg_data,vg_data$Publisher=="Sega")

vgdata3$Platform

Sega<-as.vector(vgdata3$Platform)

Sega<-unique(Sega)

#Sony

vgdata4<-subset(vg_data,vg_data$Publisher=="Sony Computer Entertainment")

vgdata4$Platform


Sony<-as.vector(vgdata4$Platform)

Sony<-unique(Sony)


VG_data = vg_data %>% 
  mutate (manufacturer = ifelse(Platform %in% Nintendo, "Nintendo", 
                       ifelse( Platform %in% Sony, "Sony", 
                               ifelse(Platform %in% Microsoft, "Microsoft",
                                      ifelse( Platform %in% Sega, "Sega", 
                                              ifelse(Platform == "PC", "PC", "Others"))))))

df=VG_data
# Add a more visual order
VG_data$manufacturer <-factor(VG_data$manufacturer, levels = c("Sony", "Nintendo", "Microsoft", "PC", "Sega", "Others"))


summary(VG_data)
                                                            

#write.csv(VG_data, file = "D:/UJM/data mining/project/video games/VG_data.csv", row.names = FALSE)
cols_num = c("NA_Sales", "EU_Sales", "JP_Sales","Global_Sales", "Other_Sales")
vg_data[cols_num] = sapply (VG_data[cols_num], as.numeric)


tail(VG_data)


# Adding a column manufacturer

aggr_manufacturer <- VG_data%>% 
  select(manufacturer,Year_of_Release,Genre,Global_Sales)%>% 
  group_by(Year_of_Release, manufacturer)%>% 
  summarise_each(funs(sum), Global_Sales)%>%
  group_by(Year_of_Release)%>%
  mutate (Market_share = Global_Sales/sum(Global_Sales))


#Plot of the maufacturers according to the Gobal Sales(year-wise)
qplot(data=aggr_manufacturer,y = Global_Sales, x= Year_of_Release,color=manufacturer, group=manufacturer,geom="line", main = "Global_Sales of consoles", ylab = "Global_Sales(unit not mentioned in the dataset)", xlab = "Year_of_release")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Plot of the maufacturers according to the Market Share(year-wise)
qplot(data=aggr_manufacturer,y = Market_share, x= Year_of_Release,color=manufacturer, group=manufacturer,geom="line", main = "Market Share", ylab = "Market share percentage", xlab = "Year_of_release")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

############Genre Anaylsis####################

table(VG_data$Genre)

ggplot(data=VG_data,aes(Genre))+
  geom_bar(aes(fill=Genre))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##The two most popular Genres "Action" and "Sports"##
Action<-subset(VG_data,VG_data$Genre=="Action")

q1<-qplot(data=Action, x= Genre, ylab = "Number of Action games produced",color=I('black'),fill='#099DD9',main = "Genre Analysis", xlab = "Action")+
  facet_wrap(~manufacturer)+
  scale_y_continuous((limits=c(0,1500)),breaks = seq(0,1500,250))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

##Genre-Sports##
Sports<-subset(VG_data,VG_data$Genre=="Sports")

q2<-qplot(data=Sports, x= Genre, ylab = "Number of Sports games produced", xlab = "Sports")+
  facet_wrap(~manufacturer)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

grid.arrange(q1,q2,ncol=2)


################Sales by Genre#################

aggr_genre<-VG_data%>% 
  select(manufacturer,Year_of_Release,Genre,Global_Sales) %>% 
  group_by(Year_of_Release,Genre,manufacturer) %>%
  summarise_each(funs(sum), Global_Sales)%>%
  group_by(Year_of_Release)

qplot(data=aggr_genre,y = Global_Sales, x= Year_of_Release,color=Genre, group=Genre,geom="line", main = "Sale by Genre for different manufacturers", ylab = "Global_Sales", xlab = "Year_of_release")+
  facet_wrap(~manufacturer,scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

###############Sales by region#################

###########North America#####################

aggr_region <- VG_data%>% 
     select(manufacturer,Genre,NA_Sales,Year_of_Release,Global_Sales) %>% 
     group_by(Year_of_Release,Genre,manufacturer,NA_Sales) %>%
     summarise_each(funs(sum), NA_Sales)
     
qplot(data=aggr_region,y = NA_Sales, x= Year_of_Release,color=Genre, group=Genre,geom="line", main = "Sale by Genre for different manufacturers", ylab = "Global_Sales", xlab = "NA_REGION")+
 # scale_y_continuous(limits=c(0,10),breaks=seq(0,10,1))+
  facet_wrap(~manufacturer)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



################Europe#####################

aggr_region <- VG_data%>% 
  select(manufacturer,Genre,EU_Sales,Year_of_Release,Global_Sales) %>% 
  group_by(Year_of_Release,Genre,manufacturer,EU_Sales) %>%
  summarise_each(funs(sum), EU_Sales)

qplot(data=aggr_region,y = EU_Sales, x= Year_of_Release,color=Genre, group=Genre,geom="line", main = "Sale by Genre for different manufacturers", ylab = "Global_Sales", xlab = "Europe_REGION")+
  # scale_y_continuous(limits=c(0,10),breaks=seq(0,10,1))+
  facet_wrap(~manufacturer)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##############Japan Region################

aggr_region <- VG_data%>% 
  select(manufacturer,Genre,JP_Sales,Year_of_Release,Global_Sales) %>% 
  group_by(Year_of_Release,Genre,manufacturer,JP_Sales) %>%
  summarise_each(funs(sum), JP_Sales)

qplot(data=aggr_region,y = JP_Sales, x= Year_of_Release,color=Genre, group=Genre,geom="line", main = "Sale by Genre for different manufacturers", ylab = "Global_Sales", xlab = "JAPAN_REGION")+
  # scale_y_continuous(limits=c(0,10),breaks=seq(0,10,1))+
  facet_wrap(~manufacturer)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##############Other Region################

aggr_region <- VG_data%>% 
  select(manufacturer,Genre,Other_Sales,Year_of_Release,Global_Sales) %>% 
  group_by(Year_of_Release,Genre,manufacturer,Other_Sales) %>%
  summarise_each(funs(sum), Other_Sales)

qplot(data=aggr_region,y = Other_Sales, x= Year_of_Release,color=Genre, group=Genre,geom="line", main = "Sale by Genre for different manufacturers", ylab = "Global_Sales", xlab = "OTHER_REGION")+
  # scale_y_continuous(limits=c(0,10),breaks=seq(0,10,1))+
  facet_wrap(~manufacturer)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



########Score Prediction for those years when Metacritic was present#########
head(df)
df$year = as.numeric(as.character(df$Year_of_Release))
df$User_Score_num = as.numeric(as.character(df$User_Score))*10

VG_Pred <- na.omit(df)
VG_Pred$User_Score_num
#clearing empty strings for Rating
VG_Pred<-filter(VG_Pred,Rating!='')

#taking year after 1999 when metacritic was first introduced
VG_Pred2<-filter(VG_Pred,year>=1999)

#scaling the user score 
#VG_Pred2$User_Score_num<-(VG_Pred2$User_Score_num) *10

ggplot(data=VG_Pred2,aes(x=User_Score_num,y=Critic_Score))+geom_point(aes(color=factor(manufacturer), shape=factor(Rating)),size=2,alpha=.5) + geom_smooth(method = "lm", size=.5,color="black", formula = y ~ x)

