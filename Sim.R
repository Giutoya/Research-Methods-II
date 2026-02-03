#SIM Model, R version 1

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
#Cash demand
h_h=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
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
    c_d[j,i]  = alpha1 * yd[j,i] + alpha2 * h_h[j,i-1]
    
    # Government money supply
    h_s[j,i]  = h_s[j,i-1] + g_d[j,i] - t_d[j,i]
    
    # Household money holdings
    h_h[j,i]  = h_h[j,i-1] + yd[j,i] - c_d[j,i]
    
    # Output determination
    y[j,i]    = c_s[j,i] + g_s[j,i]
    
    # Labour demand (from output)
    n_d[j,i]  = y[j,i] / w
    
   }
 }
}
#Figure 3.1
plot(y[1,2:100],type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 3.1: Impact of Y and Y* of a permanent increase in G",ylab = '',xlab = '',ylim=range(0,130))
lines(y[2,2:100],type="l",lwd=2,lty=1,col="3")
legend("topleft",c("Steady state solution Y*","Income Y"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,3), box.lwd=0)

#Figure 3.2
plot(yd[2,2:100],type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 3.2: YD and C starting from scratch",ylab = '',xlab = '',ylim=range(0,130))
lines(c_d[2,2:100],type="l",lwd=2,lty=1,col="3")
#abline(h=80,col=1,lty=1,lwd=1)
segments(x0=-3, # Value from x (initial)
         x1=100, # Value to x (final)
         y0=100, # Value from y (initial)
         y1=100, # Value to y (final)
         col=1,lty=2,lwd=1)
legend("topleft",c("Income YD","Consumption C"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(2,3), box.lwd=0)

#Figure 3.3
plot(h_h[2,2:100],type="l",lwd=2,lty=1,col="4",font.main=1,cex.main=0.75,main="Figure 3.3: Wealth level and wealth change, starting from scratch",ylab = '',xlab = '')
par(new="TRUE")
plot(diff(h_h[2,2:100]),type="l",lwd=2,lty=1,col="2",xlab = '',ylab = '',xaxt='n',yaxt='n')
axis(side=4)
legend("topleft",c("Wealth level H (money stock)","Household saving (the change in H)"),  bty = 1, cex = 0.8, lty=c(1,1), lwd=c(2,2), col = c(4,2), box.lwd=0)