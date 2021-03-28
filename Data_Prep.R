#####################Libraries######################################
rm(list = ls())

library(rstudioapi)
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(zoo)
library('magrittr')
library('tidyr')
library(data.table)
library(openxlsx)
library(stringr)

setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd("../Data/")
################### Read Master Mapping File data ############################
data <- read.csv("Import_LDESC.csv")

fil_data=data%>%filter(Description!='Double D Sugar Fre Choc&Jely Ronds 1x70g',!is.na(APN))
fil_data$Date <-  as.Date(fil_data$Date, format="%Y-%m-%d")

fil_data=data%>%filter(Description!='Double D Sugar Fre Choc&Jely Ronds 1x70g',!is.na(APN))
fil_data%<>%mutate(Date= as.Date(Date, format="%Y-%m-%d"))



## Create mapping ####

cat_cols=c("APN","Description","Category","Sub.Category","Segment","Sub.Segment","Manufacturer","Brand",
           "Sub.Brand","Packsize","Product.Type","Flavour","Promo.Group")
map=fil_data %>% select(all_of(cat_cols))%>%unique()

# cap ends of cat variables with low frequencies
CapLowFreq<-function(col,prop=0.01){
  #col=map$Sub.Segment
  #prop=0.02
  #data.frame(prop.table(table(col_changed)))
  
  freq_dist=data.frame(prop.table(table(col)))
  col_changed<-ifelse(col%in%c(NA,""," ",NULL)|col%in%freq_dist[freq_dist$Freq<prop,]$col,'Others',col)
  return(col_changed)
}
map$Sub.Category<-CapLowFreq(map$Sub.Category,0.02)
map$Segment<-CapLowFreq(map$Segment,0.03)
map$Sub.Segment<-CapLowFreq(map$Sub.Segment,0.02)
map$Product.Type<-CapLowFreq(map$Product.Type,0.01)
map$Flavour<-CapLowFreq(map$Flavour,0.00125)
map$Packsize<-sapply(map$Packsize,FUN = function(x)(as.numeric(sub(x,pattern = 'g',replacement=''))))

#Check
#data.frame(prop.table(table(map$Packsize))) %>% View()
#a=sapply(map$Packsize,FUN = function(x)(as.numeric(sub(x,pattern = 'g',replacement=''))))
#summary(a)


## Weekly rollup #### 
roll_weekly=fil_data%>%
  group_by(APN,Date)%>%
  summarise(wk_sold_avg_price_bysku=mean(Price.per.Unit),
            wk_sold_qty_bysku=sum(Unit.Sales),
            wk_sold_doll_bysku=sum(Val.Sales),
            Vol_Sales=sum(Vol.Sales))
roll_weekly$Month<-month(roll_weekly$Date)
roll_weekly$Year<-year(roll_weekly$Date)

#join with map
final_set=roll_weekly %>% left_join(map)

## Creating Seasonality Index at segment level ####
si_index <- setDT(final_set %>%group_by(Segment,Date) %>% summarise(Sales = sum(wk_sold_qty_bysku,na.rm = T)))
si_index <- setorderv(si_index,c("Segment",'Date'))

si_index <- si_index %>% group_by(Segment,year(Date)) %>% mutate(mean_sales = mean(Sales,na.rm = T),SI = Sales/mean_sales) %>% ungroup()
si_index <- si_index %>% group_by(Segment,Date) %>% mutate(SI = mean(SI,na.rm = T)) %>% ungroup()

final_set <- final_set %>% left_join(si_index[,c("Segment","Date","SI")])


# creating brand and category sales
brandsales = fil_data %>% group_by(Brand,Date) %>% summarise(BrandSales=sum(Unit.Sales,na.rm=T))
cateorysales = fil_data %>% group_by(Category,Date) %>% summarise(CategorySales=sum(Unit.Sales,na.rm=T))

final_set<-final_set %>% left_join(brandsales) %>% left_join(cateorysales)


##############Median_BasePrice###############
final_set$Date <- as.Date(final_set$Date,format = "%d-%m-%Y")

final_set <- setDT(final_set)
final_set[,paste("lag_price_",1:7,sep = "") := shift(wk_sold_avg_price_bysku,1:7),by=list(APN)]
final_set[,max_price_prev := max(lag_price_1,
                                  lag_price_2,
                                  lag_price_3,
                                  lag_price_4,
                                  lag_price_5,
                                  lag_price_6,
                                  lag_price_7,na.rm = TRUE),by = list(APN,Date)]
final_set[,lag_price_1 := ifelse(abs((lag_price_1 - max_price_prev)/max_price_prev) <= 0.05,lag_price_1,NA)]
final_set[,lag_price_2 := ifelse(abs((lag_price_2 - max_price_prev)/max_price_prev) <= 0.05,lag_price_2,NA)]
final_set[,lag_price_3 := ifelse(abs((lag_price_3 - max_price_prev)/max_price_prev) <= 0.05,lag_price_3,NA)]
final_set[,lag_price_4 := ifelse(abs((lag_price_4 - max_price_prev)/max_price_prev) <= 0.05,lag_price_4,NA)]
final_set[,lag_price_5 := ifelse(abs((lag_price_5 - max_price_prev)/max_price_prev) <= 0.05,lag_price_5,NA)]
final_set[,lag_price_6 := ifelse(abs((lag_price_6 - max_price_prev)/max_price_prev) <= 0.05,lag_price_6,NA)]
final_set[,lag_price_7 := ifelse(abs((lag_price_7 - max_price_prev)/max_price_prev) <= 0.05,lag_price_7,NA)]
final_set[,median_baseprice := median(c(lag_price_1,
                                         lag_price_2,
                                         lag_price_3,
                                         lag_price_4,
                                         lag_price_5,
                                         lag_price_6,
                                         lag_price_7),na.rm = TRUE),by = list(APN,Date)]

final_set[,median_baseprice := ifelse((median_baseprice -
                                          wk_sold_avg_price_bysku)/median_baseprice <= 0.05,
                                       wk_sold_avg_price_bysku,median_baseprice)]
final_set <- final_set %>% group_by(APN) %>% mutate(Final_BasePrice = rollapply(median_baseprice,width = 26,FUN = max,na.rm = T,partial = T,align = c("right")))
final_set$Final_BasePrice[final_set$Final_BasePrice == -Inf] <- final_set$median_baseprice[final_set$Final_BasePrice == -Inf] 
final_set$Final_BasePrice[is.na(final_set$Final_BasePrice)] <- final_set$wk_sold_avg_price_bysku[is.na(final_set$Final_BasePrice)] 


final_set <- setDT(final_set)
final_set[,median_baseprice := ifelse(abs((Final_BasePrice - median_baseprice)/Final_BasePrice) >= 0.05,Final_BasePrice*.95,median_baseprice)]

final_set[,tpr_discount_bysku := ifelse((median_baseprice - wk_sold_avg_price_bysku)/median_baseprice > 0.05,
                                         (median_baseprice - wk_sold_avg_price_bysku)/median_baseprice,0.0)]
final_set[,tpr_discount_bysku := tpr_discount_bysku*100]
final_set <- data.table(final_set)
final_set <- final_set[!is.na(median_baseprice),]
# writing it out
write.csv(final_set,"Aus_SKUWeek_Data.csv",row.names = F)

