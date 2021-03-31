#Global Options
options("scipen"=100)
rm(list = ls())
gc(reset = TRUE)
#------------------------------------------------------------------------------------

jan31_feb5 <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)

#remove(feb10_feb15)
library(gtools)

data <- smartbind(feb5_feb10,feb10_feb15,jan31_feb5)


names(data)

lab <- c("username",
         "tradeStatus",
         "tradeTime",
         "currency",
         "payoutAmount",
         "returnRate",
         "investAmount")

raw1 <- data[which(data$tradeStatus == 2),lab]
raw <- raw1[which((raw1$payoutAmount >= (raw1$investAmount * raw1$returnRate)) | (raw1$payoutAmount == 0)),lab]

remove(raw1,data)

library(dplyr)

#currency coonversion
#prop.table(table(raw$currency))*100

'AUD 1 - 82.61 Japanese Yen
EUR 1 - 130.48 Japanese Yen
GBP 1 - 148.17 Japanese Yen
IDR 1- 0.0078 Japanese Yen
USD 1- 109.29 Japanese Yen'

countries <- c("AUD","EUR","GBP","IDR","USD","JPY","CNY")
rates <- c(1,1.58,1.79,0.000095,1.32,0.012,0.21)


for(i in 1:length(countries)) {
  raw[which(raw$currency==countries[i]),8]  <- (raw$investAmount[which(raw$currency==countries[i])]) * rates[i]
  raw[which(raw$currency==countries[i]),9]  <- (raw$payoutAmount[which(raw$currency==countries[i])]) * rates[i]
}

colnames(raw)[c(8,9)] <- c("der_investAmount","der_payoutAmount")


#creating flag for Winning Trades
raw$ideal_profit <- (raw$investAmount * raw$returnRate)
raw$total_win <- ifelse(raw$payoutAmount >= raw$ideal_profit,1,0)

#converting Trade time into date format
y=raw$tradeTime 
y=as.character(y)
X=as.POSIXct(strptime(y, "%Y-%m-%d"))
raw$tradeTime <- X

remove(X,y)

##Aggregating data
test <- raw %>% 
  group_by(username) %>% 
  summarise(Total_investAmount = sum(der_investAmount),
            Total_payoutAmount = sum(der_payoutAmount),
            Total_Transaction = n(),
            Total_profit = (Total_payoutAmount - Total_investAmount),
            Age_Transaction_days = n_distinct(tradeTime),
            Total_Tran_Win = sum(total_win),
            Total_Tran_Loss = Total_Transaction - Total_Tran_Win)  %>% 
  mutate(Return_ratio = (Total_payoutAmount/Total_investAmount),
         Win_ratio = (Total_Tran_Win/Total_Transaction),
         Loss_ratio = (Total_Tran_Loss/Total_Transaction),
         Transaction_ratio = (Total_Transaction/Age_Transaction_days),
         Flag_Win_loss = ifelse(Total_profit > 0,2,-2),
         Profit_per = (Total_profit / Total_investAmount) * 100
         )


lab <- c("Age_Transaction_days",
         "Return_ratio",
         "Win_ratio",
         "Loss_ratio",
         #"Flag_Win_loss",
         "Transaction_ratio",
         "Profit_per")

dx <- as.data.frame(test[,lab])

# splitting data into matrix for ridge
x <- as.matrix(dx[,-c(6)])
y <- dx[,"Profit_per"] + 101

#best lambda
library(glmnet)

set.seed(113)
lasso.mod <- glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001)

set.seed(112)
cv.out <- cv.glmnet(x, y,alpha=1, nlambda=100, lambda.min.ratio=0.0001)

best.lambda <- cv.out$lambda.min
remove(dx)


save(lasso.mod, file='Hilow_lasso.mod.RData')
remove(lasso.mod,cv.out,coeff)


