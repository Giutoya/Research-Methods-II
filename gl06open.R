#OPEN Model: two countries (N and S) with their own currencies, central banks and governments,
#a fixed exchange rate and gold reserves as the means of settling the trade balance

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 6, figures 6.8 to 6.11.
#It is an R translation of the EViews program gl06open.prg, restructured in the style of Sim_a.R.
#The equations follow the EViews program, in the same order and with the same equation numbers.

#Based on the EViews program gl06open.prg (MODEL OPEN, Chapter 6 of Godley and Lavoie, Monetary Economics)
#Copyright (c) 2006 Gennaro Zezza
#Permission is hereby granted, free of charge, to any person obtaining a
#copy of this software and associated documentation files (the "Software"),
#to deal in the Software without restriction, including without limitation
#the rights to use, copy, modify, merge, publish, distribute, sublicense,
#and/or sell copies of the Software, and to permit persons to whom the
#Software is furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
#IN THE SOFTWARE.

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 66, i.e. years 1945 to 2010). Period 1 holds the starting (steady-state) stocks
nPeriods=66

#SCENARIOS (j= 1 to 3)
#1 = baseline
#2 = increase in the import propensity of the S Country (figures 6.8, 6.9 and 6.10)
#3 = increase in the saving propensity of the S Country (figure 6.11)
nScenarios=3

#ITERATIONS per period (Gauss-Seidel)
nIterations=100

#PARAMETERS
#Propensity to consume out of income (alpha1_s is a matrix: it changes in scenario 3)
alpha1_n=0.6
#Propensity to consume out of wealth
alpha2_n=0.4
alpha2_s=0.3
#Parameters of the portfolio equations
lambda0_n=0.635
lambda0_s=0.67
lambda1_n=5
lambda1_s=6
lambda2_n=0.01
lambda2_s=0.07
#Tax rates
theta_n=0.2
theta_s=0.2
#Interest rates set by the central banks
r_bar_n=0.025
r_bar_s=0.025
#Government expenditure
g_n=20
g_s=20
#Exchange rate (price of the currency of N in terms of the currency of S), fixed by the central banks
xr_bar=1
#Price of gold in the currency of N
p_g_bar=1
#Starting values of the stocks (steady state)
v0=86.487      #Households' wealth, each country
bh0=64.865     #Bills held by the households, each country
bs0=76.486     #Bills supplied by the government, each country
bcb0=11.622    #Bills held by the central bank, each country
or0=10         #Gold reserves, each country

#VARIABLES
#Import propensity of country S (exogenous: changes in scenario 2)
mu_s=matrix(data=0.18781,nrow=nScenarios,ncol=nPeriods)
#Import propensity of country N
mu_n=matrix(data=0.18781,nrow=nScenarios,ncol=nPeriods)
#Propensity to consume out of income of country S (exogenous: changes in scenario 3)
alpha1_s=matrix(data=0.7,nrow=nScenarios,ncol=nPeriods)
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
#Cash supply
h_s_n=matrix(data=v0-bh0,nrow=nScenarios,ncol=nPeriods)
h_s_s=matrix(data=v0-bh0,nrow=nScenarios,ncol=nPeriods)
#Bills supplied by the government
b_s_n=matrix(data=bs0,nrow=nScenarios,ncol=nPeriods)
b_s_s=matrix(data=bs0,nrow=nScenarios,ncol=nPeriods)
#Bills held by the central bank
b_cb_n=matrix(data=bcb0,nrow=nScenarios,ncol=nPeriods)
b_cb_s=matrix(data=bcb0,nrow=nScenarios,ncol=nPeriods)
#Gold reserves of the central bank
or_n=matrix(data=or0,nrow=nScenarios,ncol=nPeriods)
or_s=matrix(data=or0,nrow=nScenarios,ncol=nPeriods)
#Price of gold in each currency
p_g_n=matrix(data=p_g_bar,nrow=nScenarios,ncol=nPeriods)
p_g_s=matrix(data=p_g_bar*xr_bar,nrow=nScenarios,ncol=nPeriods)
#Interest rates on bills
r_n=matrix(data=r_bar_n,nrow=nScenarios,ncol=nPeriods)
r_s=matrix(data=r_bar_s,nrow=nScenarios,ncol=nPeriods)
#Exchange rate
xr=matrix(data=xr_bar,nrow=nScenarios,ncol=nPeriods)
#Variables used in the figures (S Country)
#Change in households' wealth
dv_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Trade balance
tb_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Government balance with the S Country
gb_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Change in the stock of bills held by the central bank
d_bcb_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Change in the stock of money (cash held by the households)
d_hs_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Value of the change in gold reserves
d_or_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Redundant equations: change in the value of gold minus the trade balance (should be zero, up to the rounding of the starting values)
check_n=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
check_s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)


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
       alpha1_s[3,i]=0.6     #Propensity to consume of S passes from 0.7 to 0.6
     }


     # OPEN model in R (equation numbers as in the EViews program)

     # Determination of national income in Country N - eq. 6.O.1
     y_n[j,i]     = cons_n[j,i] + g_n + x_n[j,i] - im_n[j,i]
     # Determination of national income in Country S - eq. 6.O.2
     y_s[j,i]     = cons_s[j,i] + g_s + x_s[j,i] - im_s[j,i]
     # Imports in Country N - eq. 6.O.3
     im_n[j,i]    = mu_n[j,i] * y_n[j,i]
     # Imports in Country S - eq. 6.O.4
     im_s[j,i]    = mu_s[j,i] * y_s[j,i]
     # Exports of Country N - eq. 6.O.5
     x_n[j,i]     = im_s[j,i] / xr[j,i]
     # Exports of Country S - eq. 6.O.6
     x_s[j,i]     = im_n[j,i] * xr[j,i]
     # Disposable income in Country N - eq. 6.O.7
     yd_n[j,i]    = y_n[j,i] - t_n[j,i] + r_n[j,i-1] * b_h_n[j,i-1]
     # Disposable income in Country S - eq. 6.O.8
     yd_s[j,i]    = y_s[j,i] - t_s[j,i] + r_s[j,i-1] * b_h_s[j,i-1]
     # Tax payments in Country N - eq. 6.O.9
     t_n[j,i]     = theta_n * (y_n[j,i] + r_n[j,i-1] * b_h_n[j,i-1])
     # Tax payments in Country S - eq. 6.O.10
     t_s[j,i]     = theta_s * (y_s[j,i] + r_s[j,i-1] * b_h_s[j,i-1])
     # Wealth accumulation in Country N - eq. 6.O.11
     v_n[j,i]     = v_n[j,i-1] + (yd_n[j,i] - cons_n[j,i])
     # Wealth accumulation in Country S - eq. 6.O.12
     v_s[j,i]     = v_s[j,i-1] + (yd_s[j,i] - cons_s[j,i])
     # Consumption function in Country N - eq. 6.O.13
     cons_n[j,i]  = alpha1_n * yd_n[j,i] + alpha2_n * v_n[j,i-1]
     # Consumption function in Country S - eq. 6.O.14
     cons_s[j,i]  = alpha1_s[j,i] * yd_s[j,i] + alpha2_s * v_s[j,i-1]
     # Cash money held in Country N - eq. 6.O.15
     h_h_n[j,i]   = v_n[j,i] - b_h_n[j,i]
     # Cash money held in Country S - eq. 6.O.16
     h_h_s[j,i]   = v_s[j,i] - b_h_s[j,i]
     # Demand for government bills in Country N - eq. 6.O.17
     b_h_n[j,i]   = v_n[j,i] * (lambda0_n + lambda1_n * r_n[j,i] - lambda2_n * (yd_n[j,i] / v_n[j,i]))
     # Demand for government bills in Country S - eq. 6.O.18
     b_h_s[j,i]   = v_s[j,i] * (lambda0_s + lambda1_s * r_s[j,i] - lambda2_s * (yd_s[j,i] / v_s[j,i]))
     # Supply of government bills in Country N - eq. 6.O.19
     b_s_n[j,i]   = b_s_n[j,i-1] + (g_n + r_n[j,i-1] * b_s_n[j,i-1]) - (t_n[j,i] + r_n[j,i-1] * b_cb_n[j,i-1])
     # Supply of government bills in Country S - eq. 6.O.20
     b_s_s[j,i]   = b_s_s[j,i-1] + (g_s + r_s[j,i-1] * b_s_s[j,i-1]) - (t_s[j,i] + r_s[j,i-1] * b_cb_s[j,i-1])
     # Bills held by Central bank in Country N - eq. 6.O.21
     b_cb_n[j,i]  = b_s_n[j,i] - b_h_n[j,i]
     # Bills held by Central bank in Country S - eq. 6.O.22
     b_cb_s[j,i]  = b_s_s[j,i] - b_h_s[j,i]
     # Gold holding by Central bank in Country N - eq. 6.O.23
     or_n[j,i]    = or_n[j,i-1] + (h_s_n[j,i] - h_s_n[j,i-1] - (b_cb_n[j,i] - b_cb_n[j,i-1])) / p_g_n[j,i]
     # Gold holding by Central bank in Country S - eq. 6.O.24
     or_s[j,i]    = or_s[j,i-1] + (h_s_s[j,i] - h_s_s[j,i-1] - (b_cb_s[j,i] - b_cb_s[j,i-1])) / p_g_s[j,i]
     # Supply of cash in Country N - eq. 6.O.25
     h_s_n[j,i]   = h_h_n[j,i]
     # Supply of cash in Country S - eq. 6.O.26
     h_s_s[j,i]   = h_h_s[j,i]
     # Price of gold in Country N - eq. 6.O.27
     p_g_n[j,i]   = p_g_bar
     # Price of gold in Country S - eq. 6.O.28
     p_g_s[j,i]   = p_g_n[j,i] * xr[j,i]
     # Exchange rate - eq. 6.O.29
     xr[j,i]      = xr_bar
     # Interest rate in Country N - eq. 6.O.30
     r_n[j,i]     = r_bar_n
     # Interest rate in Country S - eq. 6.O.31
     r_s[j,i]     = r_bar_s

     # Variables used in the figures (S Country)
     dv_s[j,i]    = v_s[j,i] - v_s[j,i-1]
     tb_s[j,i]    = x_s[j,i] - im_s[j,i]
     gb_s[j,i]    = t_s[j,i] - (g_s + r_s[j,i] * b_h_s[j,i-1])
     d_bcb_s[j,i] = b_cb_s[j,i] - b_cb_s[j,i-1]
     d_hs_s[j,i]  = h_h_s[j,i] - h_h_s[j,i-1]
     d_or_s[j,i]  = p_g_s[j,i] * (or_s[j,i] - or_s[j,i-1])
     check_n[j,i] = p_g_n[j,i] * (or_n[j,i] - or_n[j,i-1]) - (x_n[j,i] - im_n[j,i])
     check_s[j,i] = p_g_s[j,i] * (or_s[j,i] - or_s[j,i-1]) - (x_s[j,i] - im_s[j,i])

   }
 }
}

#FIGURES
#Figure 6.8 (years 1945 to 2010: i= 1 to 66) modified to 1946 as y_i are 0 in baseline
x=c("1946":"2010")
plot(y_n[2,2:66],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.8: Evolution of GDP in the N and S Country following an increase in the import propensity of the S Country",ylab = '',xlab = '',ylim=range(y_n[2,2:66],y_s[2,2:66]))
lines(y_s[2,2:66],type="l",lwd=2,lty=2,col="2")
legend("right",c("North Country GDP","South Country GDP"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,2), box.lwd=0)
axis(side=1,at=seq(1,66,by=5),labels=x[seq(1,66,by=5)],tck=-0.02)

#Figures 6.9 to 6.11 (years 1950 to 2000: i= 6 to 56)
x=c("1950":"2000")
at_x=seq(1,51,by=5)

#Figure 6.9
plot(dv_s[2,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.9: Evolution of balances in the S Country following an increase in its import propensity",ylab = '',xlab = '',ylim=range(dv_s[2,6:56],gb_s[2,6:56],tb_s[2,6:56]))
lines(gb_s[2,6:56],type="l",lwd=2,lty=2,col="3")
lines(tb_s[2,6:56],type="l",lwd=2,lty=5,col="4")
legend("right",c("Change in households wealth - S Country","Government balance with the S Country","Trade balance - S Country"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.10
plot(d_bcb_s[2,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.10: Evolution of 3 components of the CB balance sheet in the S Country following an increase in its import propensity",ylab = '',xlab = '',ylim=range(d_bcb_s[2,6:56],d_hs_s[2,6:56],d_or_s[2,6:56]))
lines(d_hs_s[2,6:56],type="l",lwd=2,lty=2,col="3")
lines(d_or_s[2,6:56],type="l",lwd=2,lty=5,col="4")
legend("right",c("Change in the stock of bills held by the Central bank - S Country","Change in the S Country stock of money","Value of the change in gold reserves of the Central bank - S Country"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 6.11
plot(d_bcb_s[3,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 6.11: Evolution of the 3 components of the CB balance sheet in the S Country following an increase in its propensity to save",ylab = '',xlab = '',ylim=range(d_bcb_s[3,6:56],d_hs_s[3,6:56],d_or_s[3,6:56]))
lines(d_hs_s[3,6:56],type="l",lwd=2,lty=2,col="3")
lines(d_or_s[3,6:56],type="l",lwd=2,lty=5,col="4")
legend("bottomright",c("Change in the stock of bills held by the Central bank - S Country","Change in the S Country stock of money","Value of the change in gold reserves of the Central bank - S Country"),  bty = 1, cex = 0.8, lty=c(1,2,5), lwd=c(2,2,2), col = c(2,3,4), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)
