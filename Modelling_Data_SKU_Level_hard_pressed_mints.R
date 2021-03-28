#####################Libraries######################################
rm(list = ls())

library(rstudioapi)
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(bit64)
library(readr)
library(zoo)
require(data.table)
require(glmnet)
require(glinternet)
require(dplyr)
require(DMwR)
library(openxlsx)
require(caret)
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))
#source("Functions.R")
#setwd("../Data/")

Total_SKUWeek_data <- read.csv("../Data/Aus_SKUWeek_Data.csv")

# Here filter out for 6 APN similar sku and add filter out date of discontinuity 

Total_SKUWeek_data$Group <- ifelse(Total_SKUWeek_data$APN %in% c(93553407,
                                                                 9300613114288,
                                                                 93549912,
                                                                 93613477,
                                                                 93613392,
                                                                 93553391,
                                                                 9300613114264,
                                                                 93553414
                                                                 ),"ModelGroup_6","Nothing")


Model_data <- Total_SKUWeek_data[Total_SKUWeek_data$Group == "ModelGroup_6",]
Model_data$Month_Yr <- format(as.Date(paste0(Model_data$Month_Yr,"-01")), "%Y-%m")
#Model_data$Date <- as.Date(Model_data$Month_Yr,format = "%Y-%m")
##Filtering data before discontinued date of SKU
#Model_data_<- subset(Model_data, Date < as.Date("2018-03-01") )
Model_data_=Model_data[Model_data$Month_Yr<="2019-01",]
full_data=copy(Model_data)
Model_data=copy(Model_data_)
#Log Transform of DV- Qty. Sold and IDV- Price features

model_data_set_1 <- copy(Model_data)
model_data_set_1 <- data.table(Model_data)
full_data_1<-copy(full_data)
full_data_1 <- data.table(full_data)
model_data_set_1[,":="(wk_sold_qty_bysku_log = log1p(wk_sold_qty_bysku),
                       wk_sold_avg_price_bysku_log = log(wk_sold_avg_price_bysku),
                       monthly_Cat_sales_log=log(CategorySales),
                       monthly_Brand_sales_log=log(BrandSales),
                       monthly_price_log = log(wk_sold_doll_bysku/wk_sold_qty_bysku))]
full_data_1[,":="(wk_sold_qty_bysku_log = log1p(wk_sold_qty_bysku),
                       wk_sold_avg_price_bysku_log = log(wk_sold_avg_price_bysku),
                       monthly_price_log = log(wk_sold_doll_bysku/wk_sold_qty_bysku))]
setDT(full_data_1)
setDT(model_data_set_1)
setorderv(model_data_set_1,c("APN","Month_Yr"))
setorderv(full_data_1,c("APN","Month_Yr"))


#Pantry Loading feature
#model_data_set_1[, tpr_discount_bysku_lag1 := shift(tpr_discount_bysku, n = 1, fill = NA, type = "lag"),by = list(APN)]
#model_data_set_1[is.na(tpr_discount_bysku_lag1), tpr_discount_bysku_lag1 := 0]
#model_data_set_1[, tpr_discount_bysku_lag2 := shift(tpr_discount_bysku, n = 2, fill = NA, type = "lag"),by = list(APN)]
#model_data_set_1[is.na(tpr_discount_bysku_lag2), tpr_discount_bysku_lag2 := 0]
model_data_set_1[, wk_sold_qty_bysku_log_lag1 := shift(wk_sold_qty_bysku_log, n = 1, fill = NA, type = "lag"),by = list(APN)]
model_data_set_1[is.na(wk_sold_qty_bysku_log_lag1), wk_sold_qty_bysku_log_lag1 := 0]
full_data_1[, wk_sold_qty_bysku_log_lag1 := shift(wk_sold_qty_bysku_log, n = 1, fill = NA, type = "lag"),by = list(APN)]
full_data_1[is.na(wk_sold_qty_bysku_log_lag1), wk_sold_qty_bysku_log_lag1 := 0]

#model_data_set_1$Flag_Trend_year<-ifelse(year(model_data_set_1$Year) == 2017,0,ifelse(year(model_data_set_1$Date) == 2018,1,ifelse(year(model_data_set_1$Date) == 2019,2,3)))
#model_data_set_1$Flag_under_predict<-ifelse(model_data_set_1$Date %in% c("2017-05-21","2017-07-23","2017-08-20"),1,0)
#model_data_set_1$Flag_over_predict<-ifelse(model_data_set_1$Date %in% c("2017-09-17"),1,0)
model_data_set_1$Flag_Discontinued_Sku<-ifelse(model_data_set_1$APN==93613477,1,0)
model_data_set_1$Flag_Sku1<-ifelse(model_data_set_1$APN==93553407,1,0)
model_data_set_1$Flag_Sku2<-ifelse(model_data_set_1$APN==9300613114288,1,0)
model_data_set_1$Flag_Sku3<-ifelse(model_data_set_1$APN==93549912,1,0)
model_data_set_1$Flag_Sku4<-ifelse(model_data_set_1$APN==93613392,1,0)
#9300613114288 this was started 2019
#93553407 2019
# 93549912 discontinued 201802


model_data_set_1$Flag_Sku5<-ifelse(model_data_set_1$APN==93553391,1,0)
model_data_set_1$Flag_Sku6<-ifelse(model_data_set_1$APN==9300613114264,1,0)
model_data_set_1$Flag_Sku7<-ifelse(model_data_set_1$APN==93553414,1,0)

full_data_1$Flag_Discontinued_Sku<-ifelse(full_data_1$APN==93613477,1,0)
full_data_1$Flag_Sku1<-ifelse(full_data_1$APN==93553407,1,0)
full_data_1$Flag_Sku2<-ifelse(full_data_1$APN==9300613114288,1,0)
full_data_1$Flag_Sku3<-ifelse(full_data_1$APN==93549912,1,0)
full_data_1$Flag_Sku4<-ifelse(full_data_1$APN==93613392,1,0)

full_data_1$Flag_Sku5<-ifelse(full_data_1$APN==93553391,1,0)
full_data_1$Flag_Sku6<-ifelse(full_data_1$APN==9300613114264,1,0)
full_data_1$Flag_Sku7<-ifelse(full_data_1$APN==93553414,1,0)


model_data_set_1$Flag_Flavour_1<-ifelse(model_data_set_1$Flavour==2,1,0)
model_data_set_1$Flag_Flavour_2<-ifelse(model_data_set_1$Flavour==7,1,0)
model_data_set_1$Flag_Flavour_3<-ifelse(model_data_set_1$Flavour==84,1,0)
#model_data_set_1$Flag_Flavour_4<-ifelse(model_data_set_1$Flavour==53,1,0)
 
model_data_set_1$Flag_Prodtype_1<-ifelse(model_data_set_1$Flavour==5,1,0)
model_data_set_1$Flag_Prodtype_2<-ifelse(model_data_set_1$Flavour==12,1,0)



full_data_1$Flag_Flavour_1<-ifelse(full_data_1$Flavour==2,1,0)
full_data_1$Flag_Flavour_2<-ifelse(full_data_1$Flavour==7,1,0)
full_data_1$Flag_Flavour_3<-ifelse(full_data_1$Flavour==84,1,0)
#full_data_1$Flag_Flavour_4<-ifelse(full_data_1$Flavour==53,1,0)
full_data_1$Flag_Prodtype_1<-ifelse(full_data_1$Flavour==5,1,0)
full_data_1$Flag_Prodtype_2<-ifelse(full_data_1$Flavour==12,1,0)

# model_data_set_1$Flag_Packsize_1<-ifelse(model_data_set_1$Packsize==35,1,0)
# 
# model_data_set_1$Flag_Packsize_2<-ifelse(model_data_set_1$Packsize==53,1,0)
# model_data_set_1$Flag_Packsize_3<-ifelse(model_data_set_1$Packsize==45,1,0)
# model_data_set_1$Flag_Packsize_4<-ifelse(model_data_set_1$Packsize==50,1,0)
# model_data_set_1$Flag_Packsize_5<-ifelse(model_data_set_1$Packsize==46,1,0)



#If using the wk_sold_qty_bysku_log_lag1 in the model,uncomment the following line 
#model_data_set_1[is.na(wk_sold_qty_bysku_log_lag1), tpr_discount_bysku_lag1 := 0]

setorderv(model_data_set_1,c("APN","Month_Yr"))
setorderv(full_data_1,c("APN","Month_Yr"))


## Date filter ##


# ###################Flag Dummies#############################



## Picewice Variable ##


######################################################

pred_vol1 = data.frame()
#Baseline Sales Data
glbl_model_base_data <- data.frame()
model_final <- c()
model_results <- data.table(NULL)
model_results_final <- data.table(NULL)
model_results_seed = data.frame()
model_results_cv = data.frame()
#List of ppgs removed (not enough features)
ppg_feature = as.vector(NULL)
#Fixing 20 seed numbers to select common features
seeds = c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)

model_data_set_1$Month_Yr <- as.character(model_data_set_1$Month_Yr)
library(DMwR)


model_coefficients_check = data.frame()
model_coefficients_Acting = NULL
model_coefficients_final = NULL
model_coeff_acting = NULL
box_xmat = data.frame()
model <- NULL
model_lambda = NULL
model_res = data.frame()
model_results_cv1 = data.frame()
Low_cv <- NULL
High_cv <- NULL
model_base_data <- model_data_set_1[, c("APN",
                                        "Month_Yr",
                                        "Year",
                                        "wk_sold_doll_bysku",
                                        "wk_sold_qty_bysku",
                                        "wk_sold_avg_price_bysku",
                                        "monthly_price_log",
                                        "monthly_Cat_sales_log",
                                        "monthly_Brand_sales_log",
                                        
                                        
                                        # "ACV_Selling",
                                        "SI",
                                        "Flag_Discontinued_Sku",
                                        "Flag_Sku1",
                                        "Flag_Sku2",
                                        "Flag_Sku3",
                                        "Flag_Sku4",
                                        "Flag_Sku5",
                                        "Flag_Sku6",
                                        "Flag_Sku7",
                                        
                                        "Flag_Flavour_1",
                                        "Flag_Flavour_2",
                                        "Flag_Flavour_3",
                                        "Flag_Prodtype_1",
                                        "Flag_Prodtype_2"
                                        #"Flag_Flavour_4" 
                                        
), with = FALSE]
model_ppg <- model_data_set_1[, c("APN",
                                  "Month_Yr",
                                  "Year",
                                  "wk_sold_qty_bysku_log",
                                  "monthly_price_log",
                                  "monthly_Cat_sales_log",
                                  "monthly_Brand_sales_log",
                                  #"wk_sold_qty_bysku_log_lag1",
                                  
                                  # "ACV_Selling",
                                  
                                  "SI",
                                  # "Flag_under_predict",
                                  # "Flag_over_predict",
                                  "Flag_Discontinued_Sku",
                                  "Flag_Sku1",
                                  "Flag_Sku2",
                                  "Flag_Sku3",
                                  "Flag_Sku4",
                                  "Flag_Sku5",
                                  "Flag_Sku6",
                                  "Flag_Sku7",
 
                                  "Flag_Flavour_1",
                                  "Flag_Flavour_2",
                                  "Flag_Flavour_3",
                                  "Flag_Prodtype_1",
                                  "Flag_Prodtype_2"
                                  #"Flag_Flavour_4" 
                                  
), with = FALSE]

setnames(model_ppg,"wk_sold_qty_bysku_log","DV")

model_ppg$Month_Yr <- as.character(model_ppg$Month_Yr)

temporary_2 <- model_ppg

# temporary_2[,"Extra Pprmnt Chwng Gum Fmly 5pk 10's 70g"] <- ifelse(temporary_2$APN == 9300613005067,1,0)
# temporary_2[,"Irsstbl Sue Shphrds Gltn Fre Snks 1x150g"] <- ifelse(temporary_2$APN == 9345625001645,1,0)
# temporary_2[,"Wonka Red Skins Bag 1x220g"] <- ifelse(temporary_2$APN == 9300605245204,1,0)
# temporary_2[,"Extra Peppermint Bottle 1x64g"] <- ifelse(temporary_2$APN == 9300613108638,1,0)
# temporary_2[,"Extra White Peppermint Bottle 1x64g"] <- ifelse(temporary_2$APN == 9300613103541,1,0)
# temporary_2[,"P.K. Gold Roll 5x14g"] <- ifelse(temporary_2$APN == 9300613051057,1,0)

setDT(temporary_2)

#temporary_df = data.frame(DV = temporary_2$DV, tpr_temp = temporary_2$tpr_discount_bysku)
temporary_df = data.frame(DV = temporary_2$DV)
temporary_100 <- colnames(temporary_2[,-c("Month_Yr","Year","APN","DV"),with = FALSE])
box_xmat <- temporary_2[,-c("Month_Yr","Year","APN","DV"),with = FALSE]

flag_dummy <- sum(colnames(temporary_2) %like% "Flag")
tpr_count = sum(colnames(temporary_2) %like% "tpr_")
tpr_lag_count = sum(colnames(temporary_2) %like% "tpr_discount_bysku_lag")
SI_dummy = sum(colnames(temporary_2) %like% "SI")
#ACV_dummy = sum(colnames(temporary_2) %like% "ACV_Selling")

Low = c(-Inf,-Inf,-Inf, rep(0,SI_dummy), rep(-Inf,flag_dummy) )
High = c(0,Inf,Inf,rep(Inf,SI_dummy) ,rep(Inf,flag_dummy))
ranges_df = data.frame(col=colnames(box_xmat), low=Low, high = High)
wght <-rep(1,nrow(temporary_2))

#wght <- ifelse(temporary_2$Date %in% c("2017-05-21","2017-07-23","2017-08-20"),10,1)
#wght[temporary_2$Date %in% c("2017-09-17")] = 5

penalty <- rep(1,length(Low))
penalty[1]=0
#penalty[2]=0
#penalty[colnames(box_xmat) %like% c("Flag_under_predict")] <- 0 


####################

model_coefficients_final = NULL
for (se in seeds) {
  set.seed(se)
  temp <- cv.glmnet(data.matrix(box_xmat), 
                    temporary_2$DV, 
                    alpha = 1,
                    lower.limits = c(Low),
                    upper.limits = c(High),
                    penalty.factor = penalty
                    #,weights = wght
  )
  
  model = temp
  model_lambda <- model$lambda.1se
  model_coefficients <- as.matrix(predict(model, s = model_lambda,type = "coefficients"))
  model_coefficients = as.matrix(model_coefficients[which(abs(model_coefficients[,1]) > 0),])
  model_coefficients_2 <- data.frame(model_coefficients)
  model_coefficients_2$X2 <- row.names(model_coefficients_2)
  model_coefficients_check <- rbind(model_coefficients_check,model_coefficients_2)
  box_xmat = data.matrix(box_xmat)
  model_RSq <- model$glmnet.fit$dev.ratio[which(model$glmnet.fit$lambda == model_lambda)]
  train_pred_vol <- data.frame(Pred = exp(predict(model,box_xmat,s = model_lambda)))
  train_pred_vol$Date = temporary_2$Date
  train_MAPE = regr.eval(exp(temporary_2$DV),train_pred_vol$X1)[4]
  model_res = data.frame(R_Square = model_RSq,
                         model_coefficient_name = model_coefficients_2$X2,
                         model_coefficient_value = model_coefficients_2$model_coefficients,
                         train_MAPE = train_MAPE, seed = se)
  
  if (se == seeds[1]) {
    model_results_seed <- copy(model_res)
  } else{
    model_results_seed <- rbind(model_results_seed,model_res)
  }
}

# Extracting features which turned out more than 10 times out of 20 Seeds
model_coefficients_check$model_coefficients = NULL
model_coefficients_Acting = model_coefficients_check %>% group_by(X2) %>% mutate(Freq = n()) %>% unique()
model_coefficients_final <- model_coefficients_Acting[(model_coefficients_Acting$Freq > 5 & 
                                                         model_coefficients_Acting$X2 != "(Intercept)"),]$X2


#Creating a dataset for the final model with consistent features
temporary_2 = temporary_2[,c("Month_Yr","APN","DV","Year",temporary_100[which(temporary_100 %in% model_coefficients_final)]),with = FALSE]

box_xmat = temporary_2[,-c("Month_Yr","APN","DV","Year")]

Low = Low[temporary_100 %in% colnames(box_xmat)]
High = High[temporary_100 %in% colnames(box_xmat)]
temporary_101 <- colnames(box_xmat)


penalty = rep(1,length(Low))
penalty[1]=0
#penalty[2]=0

se = 123
set.seed(se)
temp = cv.glmnet(data.matrix(box_xmat),
                 temporary_2$DV, 
                 alpha = 1,
                 lower.limits = c(Low),
                 upper.limits = c(High),
                 penalty.factor=penalty
                 #,weights=wght
                 
)

model <- temp
model_lambda <- model$lambda.1se
model_coefficients <- as.matrix(predict(model, s = model_lambda,type = "coefficients"))
model_coefficients = as.matrix(model_coefficients[which(abs(model_coefficients[,1]) > 0),])
model_coefficients1 <- data.frame(model_coefficients) 
model_coefficients1$names <- rownames(model_coefficients1)
model_final <- rbind(model_final,model_coefficients1)

#R Square
model_RSq <- model$glmnet.fit$dev.ratio[which(model$glmnet.fit$lambda == model_lambda)]
box_xmat = data.matrix(box_xmat)
pred_vol <- exp(predict(model,box_xmat,s = model_lambda))


write.csv(box_xmat,paste0("../Model_Results/","sku","_data.csv"))
saveRDS(model,paste0("../Model_Results/","sku","_Model",".RData"))

model_base_data <- model_base_data[Month_Yr %in% temporary_2$Month_Yr]
model_base_data$pred_vol <- exp(predict(model,box_xmat,s = model_lambda))
wmape <- sum(abs(model_base_data$wk_sold_qty_bysku - model_base_data$pred_vol)*model_base_data$wk_sold_qty_bysku)/sum(model_base_data$wk_sold_qty_bysku * model_base_data$wk_sold_qty_bysku )   
model_base_data$WMAPE <- wmape
train_pred_vol <- data.frame(Pred = exp(predict(model,box_xmat,s = model_lambda)))
train_pred_vol$Month_Yr = temporary_2$Month_Yr
#train_pred_vol$PPG_Item_No = i
pred_vol = train_pred_vol
#Dataset with Predicted and Actual Values
pred_vol1 = rbind(pred_vol,pred_vol1)
train_MAPE = regr.eval(exp(temporary_2$DV),train_pred_vol$X1)[4]
#model_base_data$Trian_mape <- train_MAPE
model_base_data$Rsq <- model_RSq
box_xmat_new <- data.frame(box_xmat)
box_xmat_new$Date <- NULL
base_vol <- exp(predict(model,data.matrix(box_xmat_new),s = model_lambda))
model_base_data$base_vol <- base_vol

validation <- model_base_data
validation1 <- model_base_data


##validation by Year and APN wise
validation = validation[(Year>=2017),
                        .(
                          avg_actual = mean(wk_sold_qty_bysku),
                          avg_pred = mean(pred_vol),
                          count_freq = length(Month_Yr)
                        ),.(by =Year,APN)
                        ]
validation[,error_perc_actVSpred := (avg_pred - avg_actual)/avg_actual]
setorderv(validation,cols = c("by","APN"))

##validation by overall APN wise
validation1 = validation1[(Year>=2017),
                          .(
                            avg_actual = mean(wk_sold_qty_bysku),
                            avg_pred = mean(pred_vol),
                            count_freq = length(Month_Yr)
                          ),.(by = APN)
                          ]
validation1[,error_perc_actVSpred := (avg_pred - avg_actual)/avg_actual]
setorderv(validation1,cols = c("by"))

###overall yearwise
yearwise = copy(model_base_data)
yearwise = yearwise[(Year>=2017),
                    .(
                      actual = sum(wk_sold_qty_bysku),
                      pred = sum(pred_vol),
                      weeks = length(Month_Yr)
                      
                    ),.(by = Year)
                    ]

yearwise[,error_perc_actVSpred := abs(pred - actual)/actual]

list_of_datasets <- list("Base_incremental" = model_base_data,
                         "Full_Data"=full_data_1,
                         "Coefficients" = model_final,
                         "Validation" = validation,
                         "Validation_APN_Wise" = validation1,
                         "yearwise" = yearwise
)
path=paste0("../Model_Results/sku_Results.xlsx")
openxlsx::write.xlsx(list_of_datasets, file = path)


print(wmape)
print(model_coefficients)



