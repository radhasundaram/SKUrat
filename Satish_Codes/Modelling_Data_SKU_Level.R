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
setwd("../Data/")

Total_SKUWeek_data <- read.csv("Aus_SKUWeek_Data.csv")


Total_SKUWeek_data$Group <- ifelse(Total_SKUWeek_data$APN %in% c(9300613005067,9345625001645,9300605245204,
                                                                 9300613108638,9300613103541,9300613051057),
                                   "ModelGroup_6","Nothing")

Model_data <- Total_SKUWeek_data[Total_SKUWeek_data$Group == "ModelGroup_6",]
Model_data$Date <- as.Date(Model_data$Date,format = "%d-%m-%Y")


#Log Transform of DV- Qty. Sold and IDV- Price features
model_data_set_1 <- copy(Model_data)
model_data_set_1[,":="(wk_sold_qty_bysku_log = log1p(wk_sold_qty_bysku),
                       wk_sold_avg_price_bysku_log = log(wk_sold_avg_price_bysku),
                       wk_sold_median_base_price_bysku_log = log(median_baseprice))]
setDT(model_data_set_1)
setorderv(model_data_set_1,c("APN","Date"))


#Pantry Loading feature
model_data_set_1[, tpr_discount_bysku_lag1 := shift(tpr_discount_bysku, n = 1, fill = NA, type = "lag"),by = list(APN)]
model_data_set_1[is.na(tpr_discount_bysku_lag1), tpr_discount_bysku_lag1 := 0]
model_data_set_1[, tpr_discount_bysku_lag2 := shift(tpr_discount_bysku, n = 2, fill = NA, type = "lag"),by = list(APN)]
model_data_set_1[is.na(tpr_discount_bysku_lag2), tpr_discount_bysku_lag2 := 0]
model_data_set_1[, wk_sold_qty_bysku_log_lag1 := shift(wk_sold_qty_bysku_log, n = 1, fill = NA, type = "lag"),by = list(APN)]
#If using the wk_sold_qty_bysku_log_lag1 in the model,uncomment the following line 
#model_data_set_1[is.na(wk_sold_qty_bysku_log_lag1), tpr_discount_bysku_lag1 := 0]

setorderv(model_data_set_1,c("APN","Date"))

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

model_data_set_1$Date <- as.character(model_data_set_1$Date)
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
                                        "Date",
                                        "wk_sold_doll_bysku",
                                        "wk_sold_qty_bysku",
                                        "wk_sold_avg_price_bysku",
                                        "median_baseprice",
                                        "Final_BasePrice",
                                        "tpr_discount_bysku",
                                        # "ACV_Selling",
                                        "SI",
                                        "tpr_discount_bysku_lag1",
                                        "tpr_discount_bysku_lag2"
                                        
), with = FALSE]
model_ppg <- model_data_set_1[, c("APN",
                                  "Date",
                                  "wk_sold_qty_bysku_log",
                                  "wk_sold_median_base_price_bysku_log",
                                  "tpr_discount_bysku",
                                  # "ACV_Selling",
                                  "SI",
                                  "tpr_discount_bysku_lag1",
                                  "tpr_discount_bysku_lag2"
                                  
), with = FALSE]

setnames(model_ppg,"wk_sold_qty_bysku_log","DV")

model_ppg$Date <- as.character(model_ppg$Date)

temporary_2 <- model_ppg
temporary_2[,"Extra Pprmnt Chwng Gum Fmly 5pk 10's 70g"] <- ifelse(temporary_2$APN == 9300613005067,1,0)
temporary_2[,"Irsstbl Sue Shphrds Gltn Fre Snks 1x150g"] <- ifelse(temporary_2$APN == 9345625001645,1,0)
temporary_2[,"Wonka Red Skins Bag 1x220g"] <- ifelse(temporary_2$APN == 9300605245204,1,0)
temporary_2[,"Extra Peppermint Bottle 1x64g"] <- ifelse(temporary_2$APN == 9300613108638,1,0)
temporary_2[,"Extra White Peppermint Bottle 1x64g"] <- ifelse(temporary_2$APN == 9300613103541,1,0)
temporary_2[,"P.K. Gold Roll 5x14g"] <- ifelse(temporary_2$APN == 9300613051057,1,0)


setDT(temporary_2)

temporary_df = data.frame(DV = temporary_2$DV, tpr_temp = temporary_2$tpr_discount_bysku)

temporary_100 <- colnames(temporary_2[,-c("Date","APN","DV"),with = FALSE])
box_xmat <- temporary_2[,-c("Date","APN","DV"),with = FALSE]

flag_dummy <- sum(colnames(temporary_2) %like% "Flag")
tpr_count = sum(colnames(temporary_2) %like% "tpr_discount_bysku")
tpr_lag_count = sum(colnames(temporary_2) %like% "tpr_discount_bysku_lag")
SI_dummy = sum(colnames(temporary_2) %like% "SI")
ACV_dummy = sum(colnames(temporary_2) %like% "ACV_Selling")

Low = c(-Inf,rep(0,tpr_count-2)  ,rep(0,ACV_dummy)  ,rep(0,SI_dummy),rep(-Inf,tpr_lag_count),rep(-Inf,flag_dummy),rep(-Inf,6))
High = c(0  ,rep(Inf,tpr_count-2),rep(Inf,ACV_dummy),rep(Inf,SI_dummy) ,rep(0,tpr_lag_count),rep(Inf,flag_dummy),rep(Inf,6))
ranges_df = data.frame(col=colnames(box_xmat), low=Low, high = High)


# wght <- ifelse(temporary_2$Date %in% c("2019-03-03","2019-03-17"),0,1)

penalty <- rep(1,length(Low))
penalty[1]=0
#penalty[2]=0



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
model_coefficients_final <- model_coefficients_Acting[(model_coefficients_Acting$Freq > 10 & 
                                                         model_coefficients_Acting$X2 != "(Intercept)"),]$X2


#Creating a dataset for the final model with consistent features
temporary_2 = temporary_2[,c("Date","APN","DV",temporary_100[which(temporary_100 %in% model_coefficients_final)]),with = FALSE]

box_xmat = temporary_2[,-c("Date","APN","DV")]

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
                 # weight=wght
                 
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

model_base_data <- model_base_data[Date %in% temporary_2$Date]
model_base_data$pred_vol <- exp(predict(model,box_xmat,s = model_lambda))
wmape <- sum(abs(model_base_data$wk_sold_qty_bysku - model_base_data$pred_vol)*model_base_data$wk_sold_qty_bysku)/sum(model_base_data$wk_sold_qty_bysku * model_base_data$wk_sold_qty_bysku )   
model_base_data$WMAPE <- wmape
train_pred_vol <- data.frame(Pred = exp(predict(model,box_xmat,s = model_lambda)))
train_pred_vol$Date = temporary_2$Date
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
validation = validation[(year(Date)>=2017),
                        .(
                          avg_actual = mean(wk_sold_qty_bysku),
                          avg_pred = mean(pred_vol),
                          count_freq = length(Date)
                        ),.(by = year(Date),APN)
                        ]
validation[,error_perc_actVSpred := (avg_pred - avg_actual)/avg_actual]
setorderv(validation,cols = c("by","APN"))

##validation by overall APN wise
validation1 = validation1[(year(Date)>=2017),
                          .(
                            avg_actual = mean(wk_sold_qty_bysku),
                            avg_pred = mean(pred_vol),
                            count_freq = length(Date)
                          ),.(by = APN)
                          ]
validation1[,error_perc_actVSpred := (avg_pred - avg_actual)/avg_actual]
setorderv(validation1,cols = c("by"))

###overall yearwise
yearwise = copy(model_base_data)
yearwise = yearwise[(year(Date)>=2017),
                    .(
                      actual = sum(wk_sold_qty_bysku),
                      pred = sum(pred_vol),
                      weeks = length(Date)
                      
                    ),.(by = year(Date))
                    ]

yearwise[,error_perc_actVSpred := abs(pred - actual)/actual]

list_of_datasets <- list("Base_incremental" = model_base_data,
                         "Coefficients" = model_final,
                         "Validation" = validation,
                         "Validation_APN_Wise" = validation1,
                         "yearwise" = yearwise
)
path=paste0("../Output/sku_Results.xlsx")
openxlsx::write.xlsx(list_of_datasets, file = path)


print(wmape)
print(model_coefficients) 

