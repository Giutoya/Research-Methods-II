#SIMEX Model, R version 1

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 3, figures 3.2 and 3.3.

#Created by Mrco Veronese Passarella on 2 October 2018, Modified By GTY for EVEAAL 2025

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 100)
nPeriods=100  

#SCENARIOS (j= 1 to 2)
nScenarios=2

#PARAMETERS
alpha1=0.6
alpha2=0.4
theta=0.2
w=1
#g_d=20
#VARIABLES
#Income
y=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Consumption demand
c_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Consumption supply
c_d=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Government expenditures demand
g_d=matrix(data=20,nrow=nScenarios,ncol=nPeriods) 
#Government expenditures supply
g_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Taxes demanded
t_d=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Taxes supplied
t_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Disposable income
yd=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Expected Disposable income
yde=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Cash demand
h_h=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Expected Cash demand
h_d=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Cash supply
h_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Labour demand
n_d=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 
#Labour supply
n_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods) 


#MODEL
#Select scenarios
for (j in 1:nScenarios){
  
  #Define time
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:nPeriods){
      
      #Define alternative scenarios
      if (i>=15 && j==2){
        g_d[2,i]=25   #Government expenditures passed from 20 to 25 after 15 periods
      }    
      
      
      # SIM model in R
      
      # Equilibrium conditions
      c_s[j,i]  = c_d[j,i]
      g_s[j,i]  = g_d[j,i]
      t_s[j,i]  = t_d[j,i]
      n_s[j,i]  = n_d[j,i]
      
      # Household disposable income
      yd[j,i]   = w * n_s[j,i] - t_s[j,i]
      
      # Taxes
      t_d[j,i]  = theta * w * n_s[j,i]
      
      # Consumption function (SIM)
      c_d[j,i]  = alpha1 * yde[j,i] + alpha2 * h_h[j,i-1]
      
      # Government money supply
      h_s[j,i]  = h_s[j,i-1] + g_d[j,i] - t_d[j,i]
      
      # Household actual money holdings
      h_h[j,i]  = h_h[j,i-1] + yd[j,i] - c_d[j,i]
      
      # Household expected money holdings
      h_d[j,i]  = h_s[j,i-1] + yde[j,i] - c_d[j,i]
      
      # Output determination
      y[j,i]    = c_s[j,i] + g_s[j,i]
      
      # Labour demand (from output)
      n_d[j,i]  = y[j,i] / w
      
      # Expected Disposable income
      yde[j,i]  = yd[j,i-1] 
      
    }
  }
}
#Figure 3.5
plot(yd[1,2:100],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 3.5: YD and YDe starting from scratch",ylab = '',xlab = '',ylim=range(0,130))
lines(yde[1,2:100],type="l",lwd=2,lty=1,col="3")
legend("topleft",c("Disposable Income YD","Expected Disposable Income YDe"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,3), box.lwd=0)
