#DIS Model: disequilibrium, inventories and the costing margin

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 9, figures 9.1 to 9.3.
#It is an R translation of the EViews program gl09dis.prg, restructured in the style of Sim_a.R.
#The equations follow the EViews program, in the same order and with the same equation numbers.

#Based on the EViews program gl09dis.prg (MODEL DIS, Chapter 9 of Godley and Lavoie, Monetary Economics)
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
#2 = increase in the costing margin phi, from 0.25 to 0.30 (figure 9.1)
#3 = increase in the target inventory to sales ratio sigmat, from 0.15 to 0.25 (figures 9.2 and 9.3)
nScenarios=3

#ITERATIONS per period (Gauss-Seidel)
nIterations=50

#PARAMETERS
#Propensities to consume (autonomous, out of expected income, out of wealth)
alpha0=15
alpha1=0.8
alpha2=0.1
#Expectations: weight of last period sales in sales expectations
beta=0.75
#Expectations: weight of last period income in income expectations
eps=0.75
#Speed of adjustment of inventories towards their target
gamma=0.25
#Spread between the rate on loans and the rate on deposits
add=0.02
#Interest rate on loans
r_l_bar=0.04
#Nominal wage rate and labour productivity
w=0.86
pr=1

#STARTING VALUES (steady state at the baseline parameters: phi=0.25, sigmat=0.15)
phi0=0.25
sigmat0=0.15
#Unit cost, normal historic unit cost and price
uc0=w/pr
nhuc0=(1+sigmat0*r_l_bar)*uc0
p0=(1+phi0)*nhuc0
#Real sales (= consumption) in the steady state: c = alpha0 + alpha1*c + alpha2*m, with m = sigmat*c*uc/p
s_k0=alpha0/(1-alpha1-alpha2*sigmat0*uc0/p0)
#Real inventories, inventories at current unit cost, loans and deposits
in_k0=sigmat0*s_k0
in0=in_k0*uc0
m0=in0
#Nominal flows
wb0=w*s_k0/pr
s0=p0*s_k0
f0=s0-wb0-r_l_bar*in0
fb0=add*in0
yd0=wb0+f0+fb0+(r_l_bar-add)*m0

#VARIABLES
#Costing margin (exogenous: changes in scenario 2)
phi=matrix(data=phi0,nrow=nScenarios,ncol=nPeriods)
#Target inventory to sales ratio (exogenous: changes in scenario 3)
sigmat=matrix(data=sigmat0,nrow=nScenarios,ncol=nPeriods)
#Real consumption
c_k=matrix(data=s_k0,nrow=nScenarios,ncol=nPeriods)
#Nominal consumption
cons=matrix(data=p0*s_k0,nrow=nScenarios,ncol=nPeriods)
#Expected real Haig-Simons disposable income
yd_k_hse=matrix(data=s_k0,nrow=nScenarios,ncol=nPeriods)
#Real Haig-Simons disposable income
yd_k_hs=matrix(data=s_k0,nrow=nScenarios,ncol=nPeriods)
#Real sales
s_k=matrix(data=s_k0,nrow=nScenarios,ncol=nPeriods)
#Expected real sales
s_k_e=matrix(data=s_k0,nrow=nScenarios,ncol=nPeriods)
#Target real inventories
in_k_t=matrix(data=in_k0,nrow=nScenarios,ncol=nPeriods)
#Expected real inventories
in_k_e=matrix(data=in_k0,nrow=nScenarios,ncol=nPeriods)
#Real output
y_k=matrix(data=s_k0,nrow=nScenarios,ncol=nPeriods)
#Realised real inventories
in_k=matrix(data=in_k0,nrow=nScenarios,ncol=nPeriods)
#Employment
n=matrix(data=s_k0/pr,nrow=nScenarios,ncol=nPeriods)
#Wage bill
wb=matrix(data=wb0,nrow=nScenarios,ncol=nPeriods)
#Unit cost
uc=matrix(data=uc0,nrow=nScenarios,ncol=nPeriods)
#Normal historic unit cost
nhuc=matrix(data=nhuc0,nrow=nScenarios,ncol=nPeriods)
#Price
p=matrix(data=p0,nrow=nScenarios,ncol=nPeriods)
#Nominal sales
s=matrix(data=s0,nrow=nScenarios,ncol=nPeriods)
#Inventories at current unit cost (variable IN in the EViews program)
inv=matrix(data=in0,nrow=nScenarios,ncol=nPeriods)
#Profits of firms
f=matrix(data=f0,nrow=nScenarios,ncol=nPeriods)
#Loans demanded and supplied
l_d=matrix(data=in0,nrow=nScenarios,ncol=nPeriods)
l_s=matrix(data=in0,nrow=nScenarios,ncol=nPeriods)
#Interest rates on loans and on deposits
r_l=matrix(data=r_l_bar,nrow=nScenarios,ncol=nPeriods)
r_m=matrix(data=r_l_bar-add,nrow=nScenarios,ncol=nPeriods)
#Profits of banks
f_b=matrix(data=fb0,nrow=nScenarios,ncol=nPeriods)
#Nominal disposable income
yd=matrix(data=yd0,nrow=nScenarios,ncol=nPeriods)
#Deposits held by households and supplied by banks
m_h=matrix(data=m0,nrow=nScenarios,ncol=nPeriods)
m_s=matrix(data=m0,nrow=nScenarios,ncol=nPeriods)
#Real deposits
m_h_k=matrix(data=m0/p0,nrow=nScenarios,ncol=nPeriods)
#Variables used in the figures
#Change in realised inventories
d_in_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Expected change in inventories (change in the expected level of inventories)
d_in_k_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
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
       phi[2,i]=0.30         #Costing margin passes from 0.25 to 0.30
     }
     if (i>=16 && j==3){
       sigmat[3,i]=0.25      #Target inventory to sales ratio passes from 0.15 to 0.25
     }


     # DIS model in R (equation numbers as in the EViews program)

     # The production decision
     # Real output - eq. 9.1
     y_k[j,i]      = s_k_e[j,i] + in_k_e[j,i] - in_k[j,i-1]
     # Target level of real inventories - eq. 9.2
     in_k_t[j,i]   = sigmat[j,i] * s_k_e[j,i]
     # Expected real inventories - eq. 9.3
     in_k_e[j,i]   = in_k[j,i-1] + gamma * (in_k_t[j,i] - in_k[j,i-1])
     # Real inventories - eq. 9.4
     in_k[j,i]     = in_k[j,i-1] + y_k[j,i] - s_k[j,i]
     # Expected real sales - eq. 9.5
     s_k_e[j,i]    = beta * s_k[j,i-1] + (1 - beta) * s_k_e[j,i-1]
     # Real sales - eq. 9.6
     s_k[j,i]      = c_k[j,i]
     # Employment - eq. 9.7
     n[j,i]        = y_k[j,i] / pr
     # The wage bill - eq. 9.8
     wb[j,i]       = n[j,i] * w
     # Unit costs - eq. 9.9
     uc[j,i]       = wb[j,i] / y_k[j,i]
     # Stock of inventories (variable IN in the EViews program) - eq. 9.10
     inv[j,i]      = in_k[j,i] * uc[j,i]

     # The pricing decision
     # Sales at current prices - eq. 9.11
     s[j,i]        = p[j,i] * s_k[j,i]
     # Price level - eq. 9.12
     p[j,i]        = (1 + phi[j,i]) * nhuc[j,i]
     # Normal historic unit costs - eq. 9.13
     nhuc[j,i]     = (1 - sigmat[j,i]) * uc[j,i] + sigmat[j,i] * (1 + r_l[j,i-1]) * uc[j,i-1]
     # Firms profits - eq. 9.14
     f[j,i]        = s[j,i] - wb[j,i] + inv[j,i] - inv[j,i-1] - r_l[j,i-1] * inv[j,i-1]

     # The banking system
     # Demand for loans - eq. 9.15
     l_d[j,i]      = inv[j,i]
     # Supply of loans - eq. 9.16
     l_s[j,i]      = l_d[j,i]
     # Supply of deposits - eq. 9.17
     m_s[j,i]      = l_s[j,i]
     # Interest rate on loans - eq. 9.18
     r_l[j,i]      = r_l_bar
     # Interest rate on deposits - eq. 9.19
     r_m[j,i]      = r_l[j,i] - add
     # Realized banks profits - eq. 9.20
     f_b[j,i]      = r_l[j,i-1] * l_d[j,i-1] - r_m[j,i-1] * m_h[j,i-1]

     # The consumption decision
     # Disposable income - eq. 9.21
     yd[j,i]       = wb[j,i] + f[j,i] + f_b[j,i] + r_m[j,i-1] * m_h[j,i-1]
     # Holding of banks deposit - eq. 9.22
     m_h[j,i]      = m_h[j,i-1] + yd[j,i] - cons[j,i]
     # Haig-Simons measure of real disposable income - eq. 9.23
     yd_k_hs[j,i]  = c_k[j,i] + (m_h_k[j,i] - m_h_k[j,i-1])
     # Consumption at current prices - eq. 9.24
     cons[j,i]     = c_k[j,i] * p[j,i]
     # Real value of holding of banks deposit - eq. 9.25
     m_h_k[j,i]    = m_h[j,i] / p[j,i]
     # Consumption decision - eq. 9.26
     c_k[j,i]      = alpha0 + alpha1 * yd_k_hse[j,i] + alpha2 * m_h_k[j,i-1]
     # Expectation on Real HS disposable income - eq. 9.27
     yd_k_hse[j,i] = eps * yd_k_hs[j,i-1] + (1 - eps) * yd_k_hse[j,i-1]

     # Variables used in the figures
     d_in_k[j,i]   = in_k[j,i] - in_k[j,i-1]
     d_in_k_e[j,i] = in_k_e[j,i] - in_k_e[j,i-1]
     check[j,i]    = m_h[j,i] - m_s[j,i]

   }
 }
}

#FIGURES (years 1955 to 2000: i= 11 to 56)
x=c("1955":"2000")
at_x=seq(1,46,by=5)

#Figure 9.1
plot(yd_k_hs[2,11:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 9.1: Evolution of c and ydhs following an increase in the costing margin",ylab = '',xlab = '',ylim=range(yd_k_hs[2,11:56],c_k[2,11:56]))
lines(c_k[2,11:56],type="l",lwd=2,lty=2,col="3")
legend("bottomright",c("Haig-Simons real disposable income","Real consumption"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 9.2
plot(yd_k_hs[3,11:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 9.2: Evolution of c and ydhs following an increase in the target inventory to sales ratio",ylab = '',xlab = '',ylim=range(yd_k_hs[3,11:56],c_k[3,11:56]))
lines(c_k[3,11:56],type="l",lwd=2,lty=2,col="3")
legend("bottomright",c("Haig-Simons real disposable income","Real consumption"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 9.3
plot(d_in_k[3,11:56],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 9.3: Evolution of the changes in inventories - realized and expected - following an increase in the target inventory to sales ratio",ylab = '',xlab = '',ylim=range(d_in_k[3,11:56],d_in_k_e[3,11:56]))
lines(d_in_k_e[3,11:56],type="l",lwd=2,lty=2,col="3")
legend("topright",c("Change in realized inventories","Expected change in inventories"),  bty = 1, cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2,3), box.lwd=0)
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)
