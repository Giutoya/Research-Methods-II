#SIM Model, R version 1, calibrated

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 3, figures 3.2 and 3.3.

#Created by Mrco Veronese Passarella on 2 October 2018, Modified By GTY for EVEAAL 2025

#Clear
rm(list=ls(all=TRUE))




#En alternativa, cargue datos desde FRED
#install.packages("fredr") #if fredr no está instalado
library(fredr)
fredr_set_key("7515440bdb26ef51d98764cfbccff0b9")
Data0 <-fredr(
  series_id = "GDP",# Producto Interno Bruto  (https://fred.stlouisfed.org/series/GDP)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data1 <-fredr(
  series_id = "W068RCQ027SBEA",# Gastos totales del gobierno  (https://fred.stlouisfed.org/series/W068RCQ027SBEA)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data2 <-fredr(
  series_id = "PCE",# Gastos de Consumo Personal  (https://fred.stlouisfed.org/series/PCE)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data3 <-fredr(
  series_id = "BOGMBASE",# Base Monetaria; Total (https://fred.stlouisfed.org/series/BOGMBASE)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data4 <-fredr(
  series_id = "IITTRLB",# Impuesto sobre la Renta Individual de los Estados Unidos: Tasas impositivas para impuestos regulares: Tramo más bajo  (https://fred.stlouisfed.org/series/IITTRLB)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data5 <-fredr(
  series_id = "W006RC1Q027SBEA",# Recibos de impuestos corrientes del gobierno federal  (https://fred.stlouisfed.org/series/W006RC1Q027SBEA) 
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data6 <-fredr(
  series_id = "A576RC1",#  Remuneración de empleados, recibida: Desembolsos salariales  (https://fred.stlouisfed.org/series/A576RC1)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data7 <-fredr(
  series_id = "PAYEMS",# Todos los empleados, Total no agrícola  (https://fred.stlouisfed.org/series/PAYEMS)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data8 <-fredr(
  series_id = "DSPI",# Ingreso Personal Disponible  (https://fred.stlouisfed.org/series/DSPI)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
names(Data0)[names(Data0) == 'value'] <- 'Y'
names(Data1)[names(Data1) == 'value'] <- 'G'
names(Data2)[names(Data2) == 'value'] <- 'CO'
names(Data3)[names(Data3) == 'value'] <- 'H'
names(Data4)[names(Data4) == 'value'] <- 'theta' #In puntos porcentuales, debe transformarse
names(Data5)[names(Data5) == 'value'] <- 'TAX'
names(Data6)[names(Data6) == 'value'] <- 'w' #In puntos porcentuales, debe transformarse
names(Data7)[names(Data7) == 'value'] <- 'N'
names(Data8)[names(Data8) == 'value'] <- 'YD'


Data0$Year<-format(Data0$date, "%Y")


Dataa <- cbind.data.frame(Data0,
                          Data1,
                          Data2,
                          Data3,
                          Data4,
                          Data5,
                          Data6,
                          Data7,
                          Data8
)

Data <- subset(Dataa, select = c(Year,Y,G,CO,H,theta,TAX,w,N,YD))

Data$theta <- Data$theta/100 
Data$w <- Data$w/Data$N  

rm("Data0", 
   "Dataa",
   "Data1",
   "Data2",
   "Data3",
   "Data4",
   "Data5",
   "Data6",
   "Data7",
   "Data8")


#PERIODS (i= 1 to 51)
nPeriods=51  

#SCENARIOS (j= 1 to 2)
nScenarios=2

#PARAMETERS
alpha1=0.5
alpha2=0.1

#VARIABLES
#Income
y=matrix(data=Data$Y,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Tax Rate
theta=matrix(data=Data$theta,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Wage Rate
w=matrix(data=Data$w,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Consumption demand
c_s=matrix(data=Data$CO,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Consumption supply
c_d=matrix(data=Data$CO,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Government expenditures demand
g_d=matrix(data=Data$G,nrow=nScenarios,ncol=nPeriods, byrow="TRUE") 
#Government expenditures supply
g_s=matrix(data=Data$G,nrow=nScenarios,ncol=nPeriods, byrow="TRUE") 
#Taxes demanded
t_d=matrix(data=Data$TAX,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Taxes supplied
t_s=matrix(data=Data$TAX,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Disposable income
yd=matrix(data=Data$YD,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Cash demand
h_h=matrix(data=Data$H,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Cash supply
h_s=matrix(data=Data$H,nrow=nScenarios,ncol=nPeriods, byrow="TRUE")
#Labour demand
n_d=matrix(data=Data$N,nrow=nScenarios,ncol=nPeriods, byrow="TRUE") 
#Labour supply
n_s=matrix(data=Data$N,nrow=nScenarios,ncol=nPeriods, byrow="TRUE") 


#MODEL
#Select scenarios
for (j in 1:nScenarios){
  
  #Define time
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:nPeriods){
      
      #Define alternative scenarios
      if (i==15 && j==2){
        g_d[2,i]=g_d[2,i]+25   #Government expenditures passed from 20 to 25 after 15 periods
      }    
      
      
      # SIM model in R
      
      # Equilibrium conditions
      c_s[j,i]  = c_d[j,i]
      g_s[j,i]  = g_d[j,i]
      t_s[j,i]  = t_d[j,i]
      n_s[j,i]  = n_d[j,i]
      
      # Household disposable income
      yd[j,i]   = w[j,i] * n_s[j,i] - t_s[j,i]
      
      # Taxes
      t_d[j,i]  = theta[j,i] * w[j,i] * n_s[j,i]
      
      # Consumption function (SIM)
      c_d[j,i]  = alpha1 * yd[j,i] + alpha2 * h_h[j,i-1]
      
      # Government money supply
      h_s[j,i]  = h_s[j,i-1] + g_d[j,i] - t_d[j,i]
      
      # Household money holdings
      h_h[j,i]  = h_h[j,i-1] + yd[j,i] - c_d[j,i]
      
      # Output determination
      y[j,i]    = c_s[j,i] + g_s[j,i]
      
      # Labour demand (from output)
      n_d[j,i]  = y[j,i] / w[j,i]
      
    }
  }
}

years <- 1960:2010 
#Figure 1. In-Sample simulation
plot(years, y[1,1:51],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 1. In-Sample simulation",ylab = '',xlab = '')
lines(years, Data$Y,type="l",lwd=2,lty=1,col="3")
legend("topleft",c("Simulated Y","Observed Y"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,3), box.lwd=0)

#Figure 2. Effect of an increase of government expenditure
plot(years, y[1,1:51],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 2. Effect of an increase of government expenditure",ylab = '',xlab = '')
lines(years, y[2,1:51],type="l",lwd=2,lty=1,col="3")
legend("topleft",c("Steady state solution Y*","Income Y"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,3), box.lwd=0)
