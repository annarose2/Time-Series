library("lubridate")

setwd("C:\\Users\\sinsi\\Downloads")
ADW<-read.csv("Month_Value_1.csv", sep=',',header=TRUE)

data.ts <- ts(ADW,start=decimal_date(ymd("2015-01-01")), frequency = 12)
data.ts


# plotting the graph
plot(data.ts, xlab ="Monthly Data",
     ylab ="Company Sales",
     main ="Average cost of production",
     col.main ="darkgreen")

# saving the file
dev.off()
data.Average_cost=ts(ADW$Average_cost, start = decimal_date(ymd("2015-01-01")), frequency = 12)
data.Average_cost

# MELAKUKAN UJI STASIONERITAS
library(tseries) 
plot.ts(data.Average_cost,main ="Uji Stasioner Data Awal",col = "orange")
  ##Uji Statistik Stasioner
  adf.test(data.Average_cost)


#Transformasi data
Average_costdiff1 <- diff(data.Average_cost, diff = 1)
plot.ts(Average_costdiff1,main ="Data dgn Diff 1",col = "red")


#Diff Log Order 1
data.log.diff1 = diff(log(data.Average_cost), diff = 1)
plot(data.log.diff1,main ="Data di log dgn Diff 1",col = "purple")

  #UJI STASIONERITAS DATA LOG DIFF 1
  library(tseries) 
  adf.test(data.log.diff1)

 
#identifikasi model
par(mfrow=c(1,2))
acf(data.log.diff1, lag.max = 20,main = "ACF Data Transformasi", col = "brown")
pacf(data.log.diff1, lag.max = 20,main = "PACF Data Transformasi", col = "orange")

    #Underfitting 
    #ARIMA(0,1,0)
    model1 <- arima(data.log.diff1, order=c(0,1,0))
    model1

    #ARIMA(1,1,0)
    model2 <- arima(data.log.diff1, order=c(1,1,0))
    model2
    
    #ARIMA(0,1,1)
    model3 <- arima(data.log.diff1, order=c(0,1,1))
    model3
    
    #ARIMA(0,1,2)
    model4 <- arima(data.log.diff1, order=c(0,1,2))
    model4
    
    #ARIMA(1,1,1)
    model5 <- arima(data.log.diff1, order=c(1,1,1))
    model5
    
    #ARIMA(1,1,2)
    model6 <- arima(data.log.diff1, order=c(1,1,2))
    model6
    
    
  
#DIAGNOSTIK RESIDUAL#
library(TSA)
#PLOT RESIDUAL
plot(rstandard(model4), main = "Plot Residual Model Terpilih ARIMA(0,1,2)", col = "red")

#KENORMALAN RESIDUAL
qqnorm(rstandard(model4)); qqline(rstandard(model4))
shapiro.test(rstandard(model4))

#AUTOKORELASI RESIDUAL
acf(residuals(model4),main = "Plot ACF Model Terpilih ARIMA(0,1,2)", col = "red")



####
#Diagnostic Checking
residual=model4$residuals  

#PLOT RESIDUAL
plot(residual, main = "Plot Residual Model Terpilih ARIMA(0,1,2)", col = "red")

#Normalitas Residual
qqnorm(residual); qqline(residual)
  #Uji statistik Jarquebera (Normalitas)
  jarque.bera.test(residual)
  
#No Autokorelasi
acf(residual,main = "ACF Residual", col = "brown")
  #Uji statistik ljung-box (Autokorelasi)
  library(lmtest)
  ljung_box_test <- Box.test(residual, lag = 20, type = "Ljung-Box")
  print(ljung_box_test)
  
#Homoskedastisitas
acf(residual,main = "ACF Residual", col = "brown")
  #Uji statistik white (Homokedastisitas)
  library(lmtest)
  white_test <- white.test(residual)
  print(white_test)


#Persamaan Model Terbaik ARIMA(0,1,2)
modelterbaik <- Arima(data.Average_cost, order=c(0,1,2))
modelterbaik


#Forcasting Model Terbaik
library(forecast)
fcast=forecast(modelterbaik)
fcast
plot(fcast)
