#BMW Model: Bank Money, with Walras-like fixed capital financed by bank loans

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 7, figures 7.1 to 7.5.
#It is an R translation of the EViews program gl07bmw.prg, restructured in the style of Sim_a.R.
#The equations follow the EViews program, in the same order and with the same equation numbers.

#Based on the EViews program gl07bmw.prg (MODEL BMW, Chapter 7 of Godley and Lavoie, Monetary Economics)
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

#PERIODS (i= 1 to 66, i.e. years 1945 to 2010). Period 1 holds the starting (steady-state) values
nPeriods=66

#SCENARIOS (j= 1 to 3)
#1 = baseline
#2 = increase in autonomous consumption alpha0, from 25 to 28 (figures 7.1 and 7.2)
#3 = increase in the propensity to save: alpha1 falls from 0.75 to 0.74 (figures 7.3, 7.4 and 7.5)
nScenarios=3

#ITERATIONS per period (Gauss-Seidel)
nIterations=300

#PARAMETERS
#Propensity to consume out of wealth
alpha2=0.1
#Depreciation rate
delta=0.1
#Speed of adjustment of capital to its target value
gamma=0.15
#Capital-output ratio
kappa=1
#Labour productivity
pr=1
#Rate of interest on bank loans (exogenously set)
r_l_bar=0.04

#STARTING VALUES (steady state at the baseline parameters: alpha0=25, alpha1=0.75)
k0=200       #Stock of capital
l0=200       #Bank loans
m0=200       #Bank deposits
y0=200       #GDP
af0=delta*k0                       #Depreciation allowances
wb0=y0-r_l_bar*l0-af0              #Wage bill
yd0=wb0+r_l_bar*m0                 #Disposable income
c0=y0-af0                          #Consumption
w0=wb0/(y0/pr)                     #Wage rate

#VARIABLES
#Autonomous consumption (exogenous: changes in scenario 2)
alpha0=matrix(data=25,nrow=nScenarios,ncol=nPeriods)
#Propensity to consume out of income (exogenous: changes in scenario 3)
alpha1=matrix(data=0.75,nrow=nScenarios,ncol=nPeriods)
#Consumption goods demand and supply
c_d=matrix(data=c0,nrow=nScenarios,ncol=nPeriods)
c_s=matrix(data=c0,nrow=nScenarios,ncol=nPeriods)
#Investment goods demand and supply
i_d=matrix(data=af0,nrow=nScenarios,ncol=nPeriods)
i_s=matrix(data=af0,nrow=nScenarios,ncol=nPeriods)
#Labour demand and supply
n_d=matrix(data=y0/pr,nrow=nScenarios,ncol=nPeriods)
n_s=matrix(data=y0/pr,nrow=nScenarios,ncol=nPeriods)
#Loans demanded and supplied
l_d=matrix(data=l0,nrow=nScenarios,ncol=nPeriods)
l_s=matrix(data=l0,nrow=nScenarios,ncol=nPeriods)
#GDP
y=matrix(data=y0,nrow=nScenarios,ncol=nPeriods)
#Wage bill, demand and supply
wb_d=matrix(data=wb0,nrow=nScenarios,ncol=nPeriods)
wb_s=matrix(data=wb0,nrow=nScenarios,ncol=nPeriods)
#Amortization funds and depreciation allowances
af=matrix(data=af0,nrow=nScenarios,ncol=nPeriods)
da=matrix(data=af0,nrow=nScenarios,ncol=nPeriods)
#Disposable income
yd=matrix(data=yd0,nrow=nScenarios,ncol=nPeriods)
#Deposits held by households and supplied by banks
m_h=matrix(data=m0,nrow=nScenarios,ncol=nPeriods)
m_s=matrix(data=m0,nrow=nScenarios,ncol=nPeriods)
#Interest rates on loans and on deposits
r_l=matrix(data=r_l_bar,nrow=nScenarios,ncol=nPeriods)
r_m=matrix(data=r_l_bar,nrow=nScenarios,ncol=nPeriods)
#Wage rate
w=matrix(data=w0,nrow=nScenarios,ncol=nPeriods)
#Stock of capital and target stock of capital
k=matrix(data=k0,nrow=nScenarios,ncol=nPeriods)
k_t=matrix(data=kappa*y0,nrow=nScenarios,ncol=nPeriods)
#Variable used in figure 7.4: output to capital ratio
yk=matrix(data=y0/k0,nrow=nScenarios,ncol=nPeriods)
#Redundant equation: deposits held minus deposits supplied (should be zero)
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
       alpha0[2,i]=28        #Autonomous consumption passes from 25 to 28
     }
     if (i>=16 && j==3){
       alpha1[3,i]=0.74      #Propensity to consume out of income passes from 0.75 to 0.74
     }


     # BMW model in R (equation numbers as in the EViews program)

     # Basic behavioural equations
     # Supply of consumption goods - eq. 7.1
     c_s[j,i]  = c_d[j,i]
     # Supply of investment goods - eq. 7.2
     i_s[j,i]  = i_d[j,i]
     # Supply of labour - eq. 7.3
     n_s[j,i]  = n_d[j,i]
     # Supply of loans - eq. 7.4
     l_s[j,i]  = l_s[j,i-1] + (l_d[j,i] - l_d[j,i-1])

     # Transactions of the firms
     # GDP - eq. 7.5
     y[j,i]    = c_s[j,i] + i_s[j,i]
     # Wage bill - eq. 7.6
     wb_d[j,i] = y[j,i] - r_l[j,i-1] * l_d[j,i-1] - af[j,i]
     # Depreciation allowances - eq. 7.7
     af[j,i]   = delta * k[j,i-1]
     # Demand for bank loans - eq. 7.8
     l_d[j,i]  = l_d[j,i-1] + i_d[j,i] - af[j,i]

     # Transactions of households
     # Disposable income - eq. 7.9
     yd[j,i]   = wb_s[j,i] + r_m[j,i-1] * m_h[j,i-1]
     # Bank deposits held by households - eq. 7.10
     m_h[j,i]  = m_h[j,i-1] + yd[j,i] - c_d[j,i]

     # Transactions of the banks
     # Supply of deposits - eq. 7.11
     m_s[j,i]  = m_s[j,i-1] + (l_s[j,i] - l_s[j,i-1])
     # Rate of interest on deposits - eq. 7.12
     r_m[j,i]  = r_l[j,i]

     # The wage bill
     # "Supply" of wages - eq. 7.13
     wb_s[j,i] = w[j,i] * n_s[j,i]
     # Labour demand - eq. 7.14
     n_d[j,i]  = y[j,i] / pr
     # Wage rate - eq. 7.15
     w[j,i]    = wb_d[j,i] / n_d[j,i]

     # Household behaviour
     # Demand for consumption goods - eq. 7.16
     c_d[j,i]  = alpha0[j,i] + alpha1[j,i] * yd[j,i] + alpha2 * m_h[j,i-1]

     # The investment behaviour
     # Accumulation of capital - eq. 7.17
     k[j,i]    = k[j,i-1] + i_d[j,i] - da[j,i]
     # Depreciation allowances - eq. 7.18
     da[j,i]   = delta * k[j,i-1]
     # Capital stock target - eq. 7.19
     k_t[j,i]  = kappa * y[j,i-1]
     # Demand for investment goods - eq. 7.20
     i_d[j,i]  = gamma * (k_t[j,i] - k[j,i-1]) + da[j,i]

     # The behaviour of banks
     # Interest rate on loans - eq. 7.21
     r_l[j,i]  = r_l_bar

     # Variables used in the figures and checks
     yk[j,i]    = y[j,i] / k[j,i-1]
     check[j,i] = m_h[j,i] - m_s[j,i]

   }
 }
}

#FIGURES (years 1950 to 2000: i= 6 to 56)
x=c("1950":"2000")
at_x=seq(1,51,by=5)

#Figure 7.1
plot(yd[2,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.1: Evolution of C and YD following an increase in autonomous consumption",ylab = '',xlab = '',ylim=range(yd[2,6:56],c_d[2,6:56]))
lines(c_d[2,6:56],type="l",lwd=2,lty=2,col="3")
legend("bottomright",c("Disposable income","Consumption"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 7.2
plot(i_d[2,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.2: Evolution of Id and DA following an increase in autonomus consumption",ylab = '',xlab = '',ylim=range(i_d[2,6:56],da[2,6:56]))
lines(da[2,6:56],type="l",lwd=2,lty=2,col="3")
legend("bottomright",c("Gross investment","Capital depreciation"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 7.3
plot(yd[3,6:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.3: Evolution of C and YD following an increase in the propensity to save",ylab = '',xlab = '',ylim=range(yd[3,6:56],c_d[3,6:56]))
lines(c_d[3,6:56],type="l",lwd=2,lty=2,col="3")
legend("bottomright",c("Disposable income","Consumption"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 7.4 (years 1950 to 1995: i= 6 to 51)
x=c("1950":"1995")
at_x=seq(1,46,by=5)
plot(yk[3,6:51],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.4: Evolution of the output to capital ratio following an increase in the propensity to save",ylab = '',xlab = '',ylim=c(0.9375,1.0025))
legend("bottomright",c("Output to capital ratio"),  bty = 1, cex = 0.8, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 7.5
plot(w[3,6:51],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 7.5: Evolution of the real wage rate following an increase in the propensity to save",ylab = '',xlab = '',ylim=range(w[3,6:51]))
legend("bottomright",c("Real wage rate"),  bty = 1, cex = 0.8, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)
