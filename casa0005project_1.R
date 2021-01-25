adult_obesity <- read.csv( "/nfs/cfs/home3/ucfa/ucfamw0/Casa0005project/Project/HSE19-Overweight-obesity-tab.csv")
df <- read.csv( "/nfs/cfs/home3/ucfa/ucfamw0/Casa0005project/Project/Obesity1993-2019.csv")
install.packages("ggplot2")
install.packages("ggpubr")
library(ggplot2)
colnames(df)
library(tidyverse)
library(babynames) 
library(dplyr)
library(hrbrthemes)
library(viridis)
library(shiny)
library(shinythemes)
df3 <- df %>%
  select(Year, All.adults.Mean.BMI) %>%
  gather(key = "variable", value= "value", -Year)
head(df3)
df4 <- df %>%
  select(Year, All.adults...overweight.including.obese)%>%
  gather(key = "variable", value= "value", -Year)
head(df4)

ggplot(df3, aes(x=Year, y= value, group=1)) +
  geom_line(linetype = "dashed") +
  geom_point()+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Adults' Mean BMI from 1993-2019") +
  theme_ipsum()+
  xlab("Year") + ylab("Mean BMI")+
  xlim(1990, 2019) + ylim(25.5, 28.0)

head(adult_obesity)

adult_obesity_region <- read.csv( "/nfs/cfs/home3/ucfa/ucfamw0/Casa0005project/Project/Adult-region-BMI.csv")
Income_region <- read.csv( "/nfs/cfs/home3/ucfa/ucfamw0/Casa0005project/Project/GDHI-region.csv")

names(Income_region) <- Income_region[1,]
Income_region <- Income_region[-1,]
adult_obesity_region <- adult_obesity_region[1:2,]


Income_region %>%
  pivot_longer(-c(1:2),
               names_to="year",
               values_to="value") %>%
 # filter(`Area name`=="Greater London Authority")%>%
  mutate(value=parse_integer(gsub(",","",value)),
         year=parse_integer(year))%>%
  ggplot(aes(x=year,y=value,group=1))+
  geom_line()+
  facet_wrap(~`Area name`,scales="free")+
  ggtitle("Adults' Mean BMI from 1993-2019") +
  theme_ipsum()+
  xlab("Year") + ylab("Mean BMI")

library(readxl)
library(dplyr)
library(ggplot2)
df <- read_excel("UK Region BMIIncome.xls", sheet=2)  


df %>%
  select(Year, `GDHI/Area`,`GDHI/Pounds`,Region, `Observed/BMI`,`Observed/%Overweight, including obese`) %>%
  filter(Region !="Scotland", Year!=2019) %>%
  group_by(Year, Region)%>%
  summarise(bmi=mean(`Observed/%Overweight, including obese`,na.rm=TRUE), income=mean(`GDHI/Pounds`, na.rm=TRUE))%>%
  ggplot(aes(y=income,x=bmi,color=Region))+
  geom_point(size=1.5)+
  facet_wrap(~Year)+
  theme_light()+
  xlab("Obesity Rate (%)") + ylab("GDHI (GBP)")


setwd("Ward Supermarket Data/")
tbl <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))



df <- tbl %>% 
  select(area_id, weight, volume, fat, salt, sugar,protein, carb, fibre, alcohol, saturate)


df %>%
  pivot_longer(-c(1:3),
               names_to="nutrients",
               values_to="value") %>%
  group_by(nutrients)%>%
  summarise(value=sum(value))%>%
  mutate(nutrients=fct_reorder(nutrients,-value))%>%
  ggplot(aes(x="2015", y=value, fill=nutrients))+
  geom_col(position="stack")+
  theme_light()+
  labs(y="total(groceries)",
       x="Year")+
  scale_y_continuous(labels=scales::comma_format())
  

df %>%
  ggplot(aes(x= weight , y= volume))+
  geom_point(color="blue",size=0.6)+
  theme_light()+
  geom_smooth()+
  labs(y="Weight(grams)",
       x="Volume(liters)")


GDHI_region <- read_csv("/nfs/cfs/home3/ucfa/ucfamw0/Casa0005project/Project/Income Level Data/GDHI-region.csv", 
                        +     skip = 1)


income <- GDHI_region %>%
  select(area_id='Geo code', 'Area name', '2015')

final <- inner_join(income,df)
