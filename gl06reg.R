#REG Model: two regions (N and S) sharing a common currency and a common central bank

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 6, figures 6.1 to 6.7.
#It is an R translation of the EViews program gl06reg.prg, restructured in the style of Sim_a.R

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 66, i.e. years 1945 to 2010). Period 1 holds the starting (steady-state) stocks
nPeriods=66

#SCENARIOS (j= 1 to 5)
#1 = baseline
#2 = increase in the import propensity of the S Region (figures 6.1 and 6.2)
#3 = increase in government expenditure of the S Region (figures 6.3 and 6.4)
#4 = increase in the saving propensity of the S Region (figures 6.5 and 6.6)
#5 = increase in the liquidity preference of the S Region (figure 6.7)
nScenarios=5

#ITERATIONS per period (Gauss-Seidel)
nIterations=100

#PARAMETERS
#Propensity to consume out of income (alpha1_s is a matrix: it changes in scenario 4)
alpha1_n=0.6
#Propensity to consume out of wealth
alpha2_n=0.4
alpha2_s=0.3
#Parameters of the portfolio equations (lambda0_s is a matrix: it changes in scenario 5)
lambda0_n=0.635
lambda1_n=5
lambda1_s=6
lambda2_n=0.01
lambda2_s=0.07
#Tax rate
theta=0.2
#Interest rate on bills
r_bar=0.025
#Government expenditure of region N
g_n=20
#Starting values of the stocks (steady state)
v0=86.487      #Households' wealth, each region
bh0=64.865     #Bills held by the households, each region
bcb0=43.244    #Bills held by the central bank

#VARIABLES
#Import propensity of region S (exogenous: changes in scenario 2)
mu_s=matrix(data=0.18781,nrow=nScenarios,ncol=nPeriods)
#Import propensity of region N
mu_n=matrix(data=0.18781,nrow=nScenarios,ncol=nPeriods)
#Government expenditure of region S (exogenous: changes in scenario 3)
g_s=matrix(data=20,nrow=nScenarios,ncol=nPeriods)
#Propensity to consume out of income of region S (exogenous: changes in scenario 4)
alpha1_s=matrix(data=0.7,nrow=nScenarios,ncol=nPeriods)
#Liquidity preference parameter of region S (exogenous: changes in scenario 5)
lambda0_s=matrix(data=0.67,nrow=nScenarios,ncol=nPeriods)
#GDP
y_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
y_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Consumption
cons_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
cons_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Imports
im_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
im_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Exports
x_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
x_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Disposable income
yd_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
yd_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Taxes
t_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
t_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Households' wealth
v_n=matrix(data=v0,nrow=nScenarios,ncol=nPeriods)
v_s=matrix(data=v0,nrow=nScenarios,ncol=nPeriods)
#Bills held by the households
b_h_n=matrix(data=bh0,nrow=nScenarios,ncol=nPeriods)
b_h_s=matrix(data=bh0,nrow=nScenarios,ncol=nPeriods)
#Cash held by the households
h_h_n=matrix(data=v0-bh0,nrow=nScenarios,ncol=nPeriods)
h_h_s=matrix(data=v0-bh0,nrow=nScenarios,ncol=nPeriods)
#Bills held by the households of both regions
b_h=matrix(data=2*bh0,nrow=nScenarios,ncol=nPeriods)
#Cash held by the households of both regions
h_h=matrix(data=2*(v0-bh0),nrow=nScenarios,ncol=nPeriods)
#Cash supply
h_s=matrix(data=2*(v0-bh0),nrow=nScenarios,ncol=nPeriods)
#Bills supplied by the government
b_s=matrix(data=2*bh0+bcb0,nrow=nScenarios,ncol=nPeriods)
#Bills held by the central bank
b_cb=matrix(data=bcb0,nrow=nScenarios,ncol=nPeriods)
#Interest rate on bills
r=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)
#Variables used in the figures (S Region)
#Change in households' wealth
dv_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Trade balance
tb_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Government balance with the S Region
gb_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Redundant equation: cash demand minus cash supply (should be zero)
check=matrix(data=0,nrow=nScenarios,ncol=nPeriods)


#MODEL
#Select scenarios
for (j in 1:nScenarios){

  #Define time
 for (i in 2:nPeriods){

   #Define iterations
   for (iterations in 1:nIterations){

     #Define alternative scenarios (all shocks start in 1960, i.e. i=16)
     if (i>=16 && j==2){
       mu_s[2,i]=0.20781     #Import propensity of S passes from 0.18781 to 0.20781
     }
     if (i>=16 && j==3){
       g_s[3,i]=25           #Government expenditure of S passes from 20 to 25
     }
     if (i>=16 && j==4){
       alpha1_s[4,i]=0.6     #Propensity to consume of S passes from 0.7 to 0.6
     }
     if (i>=16 && j==5){
       lambda0_s[5,i]=0.75   #Liquidity preference of S passes from 0.67 to 0.75
     }


     # REG model in R

     # Region N
     # GDP
     y_n[j,i]     = cons_n[j,i] + g_n + x_n[j,i] - im_n[j,i]
     # Imports
     im_n[j,i]    = mu_n[j,i] * y_n[j,i]
     # Exports of N are the imports of S
     x_n[j,i]     = im_s[j,i]
     # Disposable income
     yd_n[j,i]    = y_n[j,i] - t_n[j,i] + r[j,i-1] * b_h_n[j,i-1]
     # Taxes
     t_n[j,i]     = theta * (y_n[j,i] + r[j,i-1] * b_h_n[j,i-1])
     # Households' wealth
     v_n[j,i]     = v_n[j,i-1] + yd_n[j,i] - cons_n[j,i]
     # Consumption function
     cons_n[j,i]  = alpha1_n * yd_n[j,i] + alpha2_n * v_n[j,i-1]
     # Cash held by the households
     h_h_n[j,i]   = v_n[j,i] - b_h_n[j,i]
     # Demand for bills
     b_h_n[j,i]   = v_n[j,i] * (lambda0_n + lambda1_n * r[j,i] - lambda2_n * (yd_n[j,i] / v_n[j,i]))

     # Region S
     # GDP
     y_s[j,i]     = cons_s[j,i] + g_s[j,i] + x_s[j,i] - im_s[j,i]
     # Imports
     im_s[j,i]    = mu_s[j,i] * y_s[j,i]
     # Exports of S are the imports of N
     x_s[j,i]     = im_n[j,i]
     # Disposable income
     yd_s[j,i]    = y_s[j,i] - t_s[j,i] + r[j,i-1] * b_h_s[j,i-1]
     # Taxes
     t_s[j,i]     = theta * (y_s[j,i] + r[j,i-1] * b_h_s[j,i-1])
     # Households' wealth
     v_s[j,i]     = v_s[j,i-1] + yd_s[j,i] - cons_s[j,i]
     # Consumption function
     cons_s[j,i]  = alpha1_s[j,i] * yd_s[j,i] + alpha2_s * v_s[j,i-1]
     # Cash held by the households
     h_h_s[j,i]   = v_s[j,i] - b_h_s[j,i]
     # Demand for bills
     b_h_s[j,i]   = v_s[j,i] * (lambda0_s[j,i] + lambda1_s * r[j,i] - lambda2_s * (yd_s[j,i] / v_s[j,i]))

     # Government and central bank (common to both regions)
     # Bills held by the households of both regions
     b_h[j,i]     = b_h_n[j,i] + b_h_s[j,i]
     # Cash held by the households of both regions
     h_h[j,i]     = h_h_n[j,i] + h_h_s[j,i]
     # Supply of bills (government budget constraint, central bank profits go to the government)
     b_s[j,i]     = b_s[j,i-1] + g_n + g_s[j,i] + r[j,i-1] * b_s[j,i-1] - t_n[j,i] - t_s[j,i] - r[j,i-1] * b_cb[j,i-1]
     # Bills held by the central bank
     b_cb[j,i]    = b_s[j,i] - b_h[j,i]
     # Supply of cash
     h_s[j,i]     = h_s[j,i-1] + b_cb[j,i] - b_cb[j,i-1]
     # Interest rate set by the central bank
     r[j,i]       = r_bar

     # Variables used in the figures (S Region)
     dv_s[j,i]    = v_s[j,i] - v_s[j,i-1]
     tb_s[j,i]    = x_s[j,i] - im_s[j,i]
     gb_s[j,i]    = t_s[j,i] - g_s[j,i] - r[j,i-1] * b_h_s[j,i-1]
     check[j,i]   = h_h[j,i] - h_s[j,i]

   }
 }
}

#FIGURES (years 1950 to 2000: i= 6 to 56)
x=c("1950":"2000")
at_x=seq(1,51,by=5)

#Figure 6.1
plot(dv_s[2,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.1: Evolution of balances in the S Region with an increase in import propensity",ylab = '',xlab = '',ylim=range(dv_s[2,6:56],gb_s[2,6:56],tb_s[2,6:56]))
lines(gb_s[2,6:56],type="l",lwd=2,lty=2,col="3")
lines(tb_s[2,6:56],type="l",lwd=2,lty=5,col="4")
legend("bottomright",c("Change in households wealth - S Region","Government balance with the S Region","Trade balance - S Region"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.2
plot(y_n[2,6:56],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.2: Evolution of GDP in the N and S Region following an increase in the import propensity of the S Region",ylab = '',xlab = '',ylim=range(y_n[2,6:56],y_s[2,6:56]))
lines(y_s[2,6:56],type="l",lwd=2,lty=2,col="2")
legend("right",c("North Region GDP","South Region GDP"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,2), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.3
plot(y_n[3,6:56],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.3: Evolution of GDP in the N and S Region following an increase in govt. expenditure of the S Region",ylab = '',xlab = '',ylim=range(y_n[3,6:56],y_s[3,6:56]))
lines(y_s[3,6:56],type="l",lwd=2,lty=2,col="2")
legend("bottomright",c("North Region GDP","South Region GDP"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,2), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.4
plot(dv_s[3,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.4: Evolution of balances in the S Region following an increase in govt. expenditure in the S Region",ylab = '',xlab = '',ylim=range(dv_s[3,6:56],gb_s[3,6:56],tb_s[3,6:56]))
lines(gb_s[3,6:56],type="l",lwd=2,lty=2,col="3")
lines(tb_s[3,6:56],type="l",lwd=2,lty=5,col="4")
legend("bottomright",c("Change in households wealth - S Region","Government balance with the S Region","Trade balance - S Region"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.5
plot(y_n[4,6:56],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.5: Evolution of GDP in the N and S Region following an increase in the saving propensity of the S Region",ylab = '',xlab = '',ylim=range(y_n[4,6:56],y_s[4,6:56]))
lines(y_s[4,6:56],type="l",lwd=2,lty=2,col="2")
legend("bottomright",c("North Region GDP","South Region GDP"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,2), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.6
plot(dv_s[4,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.6: Evolution of balances in the S Region following an increase in the saving propensity of the S Region",ylab = '',xlab = '',ylim=range(dv_s[4,6:56],gb_s[4,6:56],tb_s[4,6:56]))
lines(gb_s[4,6:56],type="l",lwd=2,lty=2,col="3")
lines(tb_s[4,6:56],type="l",lwd=2,lty=5,col="4")
legend("topright",c("Change in households wealth - S Region","Government balance with the S Region","Trade balance - S Region"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.7
plot(dv_s[5,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.7: Evolution of balances in the S Region following an increase in the liquidity preference of the S Region",ylab = '',xlab = '',ylim=range(dv_s[5,6:56],gb_s[5,6:56],tb_s[5,6:56]))
lines(gb_s[5,6:56],type="l",lwd=2,lty=2,col="3")
lines(tb_s[5,6:56],type="l",lwd=2,lty=5,col="4")
legend("bottomright",c("Change in households wealth - S Region","Government balance with the S Region","Trade balance - S Region"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)
