#OPENFIX Model: two countries (UK and US) with their own currencies, a fixed (but adjustable)
#exchange rate, capital mobility and central banks that hold foreign bills

#This code replicates results in the book Monetary Economics:
#An Integrated Approach to Credit, Money, Income, Production and Wealth,
#by Wynne Godley and Marc Lavoie, chapter 12, figures 12.1A to 12.1C.
#It is an R translation of the EViews program gl12openfix.prg, restructured in the style of Sim_a.R.
#The equations are those of the EViews program, with the same equation numbers. They are listed in the order in which
#they can be computed (each equation after the variables it depends on), as the EViews solver does internally;
#this makes the Gauss-Seidel iterations converge.

#Based on the EViews program gl12openfix.prg (MODEL OPENFIX, Chapter 12 of Godley and Lavoie, Monetary Economics)
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

#PERIODS (i= 1 to 101, i.e. years 1950 to 2050). Periods 1 and 2 hold the starting values: the model is solved from 1952 (i=3)
nPeriods=101

#SCENARIOS (j= 1 to 2)
#1 = baseline
#2 = increase in the US propensity to import: eps0 passes from -2.1 to -2 in 1960 (figures 12.1A, 12.1B and 12.1C)
nScenarios=2

#ITERATIONS: maximum number of Gauss-Seidel iterations per period
nIterations=5000
#CONVERGENCE: iterations stop when no endogenous variable changes by more than tol (relative change)
tol=1e-8

#PARAMETERS
#Propensities to consume
alpha1_uk=0.75
alpha1_us=0.75
alpha2_uk=0.13333
alpha2_us=0.13333
#Parameters in the real export and import functions (eps0 is a matrix: it changes in scenario 2)
eps1=0.7
eps2=1
mu0=-2.1
mu1=0.7
mu2=1
#Parameters in the asset demand functions
lambda10=0.7
lambda11=5
lambda12=5
lambda20=0.25
lambda21=5
lambda22=5
lambda40=0.7
lambda41=5
lambda42=5
lambda50=0.25
lambda51=5
lambda52=5
#Parameters in the price equations
nu0m=-0.00001
nu0x=-0.00001
nu1m=0.7
nu1x=0.5
#Mark-ups and tax rates
phi_uk=0.2381
phi_us=0.2381
theta_uk=0.2
theta_us=0.2

#VARIABLES (matrices: scenario x period, filled with their starting values)
#Exogenous variables
eps0=matrix(data=-2.1,nrow=nScenarios,ncol=nPeriods)   #Parameter in the real exports of the US imports (exogenous: changes in scenario 2)
dxre_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)   #exogenous
dxre_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)   #exogenous
g_k_uk=matrix(data=16,nrow=nScenarios,ncol=nPeriods)   #exogenous
g_k_us=matrix(data=16,nrow=nScenarios,ncol=nPeriods)   #exogenous
or_uk=matrix(data=7,nrow=nScenarios,ncol=nPeriods)   #exogenous
or_us=matrix(data=7,nrow=nScenarios,ncol=nPeriods)   #exogenous
pg_us=matrix(data=1,nrow=nScenarios,ncol=nPeriods)   #exogenous
pr_uk=matrix(data=1.3333,nrow=nScenarios,ncol=nPeriods)   #exogenous
pr_us=matrix(data=1.3333,nrow=nScenarios,ncol=nPeriods)   #exogenous
r_uk=matrix(data=0.03,nrow=nScenarios,ncol=nPeriods)   #exogenous
r_us=matrix(data=0.03,nrow=nScenarios,ncol=nPeriods)   #exogenous
w_uk=matrix(data=1,nrow=nScenarios,ncol=nPeriods)   #exogenous
w_us=matrix(data=1,nrow=nScenarios,ncol=nPeriods)   #exogenous
xr_uk=matrix(data=1.0003,nrow=nScenarios,ncol=nPeriods)   #exogenous
#Endogenous variables
f_cb_us=matrix(data=0.00895,nrow=nScenarios,ncol=nPeriods)
pg_uk=matrix(data=0.99971,nrow=nScenarios,ncol=nPeriods)
xr_us=matrix(data=0.99971,nrow=nScenarios,ncol=nPeriods)
f_cb_uk=matrix(data=0.00869,nrow=nScenarios,ncol=nPeriods)
yd_uk=matrix(data=77.851,nrow=nScenarios,ncol=nPeriods)
yd_hs_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
v_uk=matrix(data=145.97,nrow=nScenarios,ncol=nPeriods)
b_ukuk_d=matrix(data=102.18,nrow=nScenarios,ncol=nPeriods)
b_ukus_d=matrix(data=36.493,nrow=nScenarios,ncol=nPeriods)
h_uk_d=matrix(data=7.2987,nrow=nScenarios,ncol=nPeriods)
h_uk_s=matrix(data=7.2987,nrow=nScenarios,ncol=nPeriods)
b_ukuk_s=matrix(data=102.18,nrow=nScenarios,ncol=nPeriods)
b_ukus_s=matrix(data=36.504,nrow=nScenarios,ncol=nPeriods)
yd_us=matrix(data=77.86,nrow=nScenarios,ncol=nPeriods)
yd_hs_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
v_us=matrix(data=145.99001,nrow=nScenarios,ncol=nPeriods)
b_usus_d=matrix(data=102.19,nrow=nScenarios,ncol=nPeriods)
b_usuk_d=matrix(data=36.497,nrow=nScenarios,ncol=nPeriods)
h_us_d=matrix(data=7.2995,nrow=nScenarios,ncol=nPeriods)
h_us_s=matrix(data=7.2995,nrow=nScenarios,ncol=nPeriods)
b_usus_s=matrix(data=102.19,nrow=nScenarios,ncol=nPeriods)
b_cb_usus_d=matrix(data=0.29843,nrow=nScenarios,ncol=nPeriods)
b_cb_usus_s=matrix(data=0.29843,nrow=nScenarios,ncol=nPeriods)
b_usuk_s=matrix(data=36.487,nrow=nScenarios,ncol=nPeriods)
kabp_uk=matrix(data=0.00002,nrow=nScenarios,ncol=nPeriods)
kabp_us=matrix(data=-0.00002,nrow=nScenarios,ncol=nPeriods)
t_uk=matrix(data=19.463,nrow=nScenarios,ncol=nPeriods)
t_us=matrix(data=19.465,nrow=nScenarios,ncol=nPeriods)
b_uk_s=matrix(data=138.94,nrow=nScenarios,ncol=nPeriods)
b_us_s=matrix(data=139.02,nrow=nScenarios,ncol=nPeriods)
b_cb_ukus_s=matrix(data=0.02031,nrow=nScenarios,ncol=nPeriods)
kab_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
kab_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
b_cb_ukuk_d=matrix(data=0.27984,nrow=nScenarios,ncol=nPeriods)
b_cb_ukuk_s=matrix(data=0.27984,nrow=nScenarios,ncol=nPeriods)
b_cb_ukus_d=matrix(data=0.0203,nrow=nScenarios,ncol=nPeriods)
px_us=matrix(data=0.95656,nrow=nScenarios,ncol=nPeriods)
pm_us=matrix(data=0.95661,nrow=nScenarios,ncol=nPeriods)
im_k_uk=matrix(data=11.928,nrow=nScenarios,ncol=nPeriods)
x_k_us=matrix(data=11.928,nrow=nScenarios,ncol=nPeriods)
x_us=matrix(data=11.41,nrow=nScenarios,ncol=nPeriods)
cab_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
im_k_us=matrix(data=11.926,nrow=nScenarios,ncol=nPeriods)
im_us=matrix(data=11.409,nrow=nScenarios,ncol=nPeriods)
im_uk=matrix(data=11.407,nrow=nScenarios,ncol=nPeriods)
cab_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
v_k_uk=matrix(data=152.62,nrow=nScenarios,ncol=nPeriods)
v_k_us=matrix(data=152.63,nrow=nScenarios,ncol=nPeriods)
ydhs_k_uk=matrix(data=81.394,nrow=nScenarios,ncol=nPeriods)
ydhse_k_uk=matrix(data=81.394,nrow=nScenarios,ncol=nPeriods)
c_k_uk=matrix(data=81.393,nrow=nScenarios,ncol=nPeriods)
ds_k_uk=matrix(data=97.393,nrow=nScenarios,ncol=nPeriods)
ydhs_k_us=matrix(data=81.402,nrow=nScenarios,ncol=nPeriods)
ydhse_k_us=matrix(data=81.402,nrow=nScenarios,ncol=nPeriods)
c_k_us=matrix(data=81.401,nrow=nScenarios,ncol=nPeriods)
s_k_us=matrix(data=109.33,nrow=nScenarios,ncol=nPeriods)
ds_k_us=matrix(data=97.401,nrow=nScenarios,ncol=nPeriods)
y_k_us=matrix(data=97.403,nrow=nScenarios,ncol=nPeriods)
n_us=matrix(data=73.054,nrow=nScenarios,ncol=nPeriods)
ps_us=matrix(data=0.9565,nrow=nScenarios,ncol=nPeriods)
s_us=matrix(data=104.57,nrow=nScenarios,ncol=nPeriods)
pds_us=matrix(data=0.95649,nrow=nScenarios,ncol=nPeriods)
ds_us=matrix(data=93.164,nrow=nScenarios,ncol=nPeriods)
y_us=matrix(data=93.164,nrow=nScenarios,ncol=nPeriods)
py_us=matrix(data=0.95649,nrow=nScenarios,ncol=nPeriods)
x_k_uk=matrix(data=11.926,nrow=nScenarios,ncol=nPeriods)
s_k_uk=matrix(data=109.32,nrow=nScenarios,ncol=nPeriods)
y_k_uk=matrix(data=97.392,nrow=nScenarios,ncol=nPeriods)
cons_us=matrix(data=77.86,nrow=nScenarios,ncol=nPeriods)
g_us=matrix(data=15.304,nrow=nScenarios,ncol=nPeriods)
n_uk=matrix(data=73.046,nrow=nScenarios,ncol=nPeriods)
ps_uk=matrix(data=0.95646,nrow=nScenarios,ncol=nPeriods)
s_uk=matrix(data=104.56,nrow=nScenarios,ncol=nPeriods)
y_uk=matrix(data=93.154,nrow=nScenarios,ncol=nPeriods)
py_uk=matrix(data=0.95648,nrow=nScenarios,ncol=nPeriods)
pm_uk=matrix(data=0.95628,nrow=nScenarios,ncol=nPeriods)
px_uk=matrix(data=0.95634,nrow=nScenarios,ncol=nPeriods)
x_uk=matrix(data=11.406,nrow=nScenarios,ncol=nPeriods)
pds_uk=matrix(data=0.95648,nrow=nScenarios,ncol=nPeriods)
ds_uk=matrix(data=93.154,nrow=nScenarios,ncol=nPeriods)
cons_uk=matrix(data=77.851,nrow=nScenarios,ncol=nPeriods)
g_uk=matrix(data=15.304,nrow=nScenarios,ncol=nPeriods)
psbr_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
psbr_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
nafa_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
nafa_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
#Variables used in the figures
tb_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
gb_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
dh_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
dbcb_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
dbcbus_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
debt_us=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
debt_uk=matrix(data=0,nrow=nScenarios,ncol=nPeriods)

#CONVERGENCE CHECK
#Endogenous variables whose change between two iterations is monitored
endog=c(
     "f_cb_us","pg_uk","xr_us","f_cb_uk","yd_uk","yd_hs_uk",
     "v_uk","b_ukuk_d","b_ukus_d","h_uk_d","h_uk_s","b_ukuk_s",
     "b_ukus_s","yd_us","yd_hs_us","v_us","b_usus_d","b_usuk_d",
     "h_us_d","h_us_s","b_usus_s","b_cb_usus_d","b_cb_usus_s","b_usuk_s",
     "kabp_uk","kabp_us","t_uk","t_us","b_uk_s","b_us_s",
     "b_cb_ukus_s","kab_uk","kab_us","b_cb_ukuk_d","b_cb_ukuk_s","b_cb_ukus_d",
     "px_us","pm_us","im_k_uk","x_k_us","x_us","cab_us",
     "im_k_us","im_us","im_uk","cab_uk","v_k_uk","v_k_us",
     "ydhs_k_uk","ydhse_k_uk","c_k_uk","ds_k_uk","ydhs_k_us","ydhse_k_us",
     "c_k_us","s_k_us","ds_k_us","y_k_us","n_us","ps_us",
     "s_us","pds_us","ds_us","y_us","py_us","x_k_uk",
     "s_k_uk","y_k_uk","cons_us","g_us","n_uk","ps_uk",
     "s_uk","y_uk","py_uk","pm_uk","px_uk","x_uk",
     "pds_uk","ds_uk","cons_uk","g_uk","psbr_uk","psbr_us",
     "nafa_uk","nafa_us","tb_uk","gb_uk","dh_uk","dbcb_uk",
     "dbcbus_uk","debt_us","debt_uk")
#Iterations used in each period, and largest change in the last iteration
iterUsed=matrix(data=0,nrow=nScenarios,ncol=nPeriods)
lastDiff=matrix(data=0,nrow=nScenarios,ncol=nPeriods)


#MODEL
#Select scenarios
for (j in 1:nScenarios){

  #Define time
 for (i in 3:nPeriods){

   #Define iterations
   for (iterations in 1:nIterations){

     #Values at the beginning of the iteration
     old=sapply(endog,function(v) get(v)[j,i])

     #Define alternative scenarios (the shock starts in 1960, i.e. i=11)
     if (i>=11 && j==2){
       eps0[2,i]=-2          #Parameter passes from -2.1 to -2: the US imports more
     }

     # OPENFIX model in R (equation numbers as in the EViews program, listed in order of computation)

     # Profits of Central Bank in US - eq. 12.12
     f_cb_us[j,i] = r_us[j,i-1]*b_cb_usus_d[j,i-1]
     # Price of gold is equal in the two countries - eq. 12.85
     pg_uk[j,i] = pg_us[j,i]/xr_uk[j,i]
     # US exchange rate - eq. 12.86
     xr_us[j,i] = 1/xr_uk[j,i]
     # Profits of Central Bank in UK - eq. 12.11 - typo in the book for r_us
     f_cb_uk[j,i] = r_uk[j,i-1]*b_cb_ukuk_d[j,i-1] + r_us[j,i-1]*b_cb_ukus_s[j,i-1]*xr_us[j,i]
     # Disposable income in UK - eq. 12.1
     yd_uk[j,i] = (y_uk[j,i] + r_uk[j,i-1]*b_ukuk_d[j,i-1] + xr_us[j,i]*r_us[j,i-1]*b_ukus_s[j,i-1])*(1 - theta_uk) + (xr_us[j,i] - xr_us[j,i-1])*b_ukus_s[j,i-1]
     # Haig-Simons disposable income in UK - eq. 12.2
     yd_hs_uk[j,i] = yd_uk[j,i] + (xr_us[j,i] - xr_us[j,i-1])*b_ukus_s[j,i-1]
     # Wealth accumulation in UK - eq. 12.3
     v_uk[j,i] = v_uk[j,i-1] + yd_uk[j,i] - cons_uk[j,i]
     # Demand for UK bills in UK - eq. 12.67
     b_ukuk_d[j,i] = v_uk[j,i]*(lambda10 + lambda11*r_uk[j,i] - lambda12*(r_us[j,i] + dxre_us[j,i]))
     # Demand for US bills in UK - eq. 12.68
     b_ukus_d[j,i] = v_uk[j,i]*(lambda20 - lambda21*r_uk[j,i] + lambda22*(r_us[j,i] + dxre_us[j,i]))
     # Demand for money in UK - eq. 12.69
     h_uk_d[j,i] = v_uk[j,i] - b_ukuk_d[j,i] - b_ukus_d[j,i]
     # Suply of cash in UK - eq. 12.80
     h_uk_s[j,i] = h_uk_d[j,i]
     # Bills issued by US acquired by US - eq. 12.81
     b_ukuk_s[j,i] = b_ukuk_d[j,i]
     # Bills supply from UK to US - eq. 12.89F
     b_ukus_s[j,i] = xr_uk[j,i]*b_ukus_d[j,i]
     # Disposable income in US - eq. 12.4
     yd_us[j,i] = (y_us[j,i] + r_us[j,i-1]*b_usus_d[j,i-1] + xr_uk[j,i]*r_uk[j,i-1]*b_usuk_s[j,i-1])*(1 - theta_us) + (xr_uk[j,i] - xr_uk[j,i-1])*b_usuk_s[j,i-1]
     # Haig-Simons disposable income in US - eq. 12.5
     yd_hs_us[j,i] = yd_us[j,i] + (xr_uk[j,i] - xr_uk[j,i-1])*b_usuk_s[j,i-1]
     # Wealth accumulation in US - eq. 12.6
     v_us[j,i] = v_us[j,i-1] + yd_us[j,i] - cons_us[j,i]
     # Demand for US	bills in US - eq. 12.70
     b_usus_d[j,i] = v_us[j,i]*(lambda40 + lambda41*r_us[j,i] - lambda42*(r_uk[j,i] + dxre_uk[j,i]))
     # Demand for UK bills in US - eq. 12.71
     b_usuk_d[j,i] = v_us[j,i]*(lambda50 - lambda51*r_us[j,i] + lambda52*(r_uk[j,i] + dxre_uk[j,i]))
     # Demand for money in US - eq. 12.72
     h_us_d[j,i] = v_us[j,i] - b_usus_d[j,i] - b_usuk_d[j,i]
     # Suply of cash in US - eq. 12.77
     h_us_s[j,i] = h_us_d[j,i]
     # Supply of US bills to CountryN - eq. 12.78
     b_usus_s[j,i] = b_usus_d[j,i]
     # Balance sheet of US Central bank - eq. 12.83 - expressed as changes
     b_cb_usus_d[j,i] = b_cb_usus_d[j,i-1] + (h_us_s[j,i] - h_us_s[j,i-1]) - (or_us[j,i] - or_us[j,i-1])*pg_us[j,i]
     # Supply of US bills to US Central bank - eq. 12.79
     b_cb_usus_s[j,i] = b_cb_usus_d[j,i]
     # Equilibrium condition for bills issued by UK acquired by US - eq. 12.87
     b_usuk_s[j,i] = b_usuk_d[j,i]*xr_us[j,i]
     # Capital account balance in UK, net of official transactions - eq. 12.19
     kabp_uk[j,i] = - (b_ukus_s[j,i] - b_ukus_s[j,i-1])*xr_us[j,i] + (b_usuk_s[j,i] - b_usuk_s[j,i-1])
     # Capital account balance in US, net of official transactions - eq. 12.20
     kabp_us[j,i] = - (b_usuk_s[j,i] - b_usuk_s[j,i-1])*xr_uk[j,i] + (b_ukus_s[j,i] - b_ukus_s[j,i-1])
     # Taxes in UK - eq. 12.7
     t_uk[j,i] = theta_uk*(y_uk[j,i] + r_uk[j,i-1]*b_ukuk_d[j,i-1] + xr_us[j,i]*r_us[j,i-1]*b_ukus_s[j,i-1])
     # Taxes in US - eq. 12.8
     t_us[j,i] = theta_us*(y_us[j,i] + r_us[j,i-1]*b_usus_d[j,i-1] + xr_uk[j,i]*r_uk[j,i-1]*b_usuk_s[j,i-1])
     # Government budget constraint - UK - eq. 12.13
     b_uk_s[j,i] = b_uk_s[j,i-1] + g_uk[j,i] + r_uk[j,i-1]*b_uk_s[j,i-1] - t_uk[j,i] - f_cb_uk[j,i]
     # Government budget constraint - US - eq. 12.14
     b_us_s[j,i] = b_us_s[j,i-1] + g_us[j,i] + r_us[j,i-1]*b_us_s[j,i-1] - t_us[j,i] - f_cb_us[j,i]
     # Supply of UK bills to CountryS, now solved for b_cb_ukus_s - eq. 12.90F
     b_cb_ukus_s[j,i] = b_us_s[j,i] - b_usus_s[j,i] - b_cb_usus_d[j,i] - b_ukus_s[j,i]
     # Capital account balance in UK - eq. 12.16
     kab_uk[j,i] = kabp_uk[j,i] - (xr_us[j,i]*(b_cb_ukus_s[j,i] - b_cb_ukus_s[j,i-1]) + pg_uk[j,i]*(or_uk[j,i] - or_uk[j,i-1]))
     # Capital account balance in US - eq. 12.18
     kab_us[j,i] = kabp_us[j,i] + (b_cb_ukus_s[j,i] - b_cb_ukus_s[j,i-1]) - pg_us[j,i]*(or_us[j,i] - or_us[j,i-1])
     # Balance sheet of UK Central bank - eq. 12.84
     b_cb_ukuk_d[j,i] = b_cb_ukuk_d[j,i-1] + (h_uk_s[j,i] - h_uk_s[j,i-1]) - (b_cb_ukus_s[j,i] - b_cb_ukus_s[j,i-1])*xr_us[j,i] - (or_uk[j,i] - or_uk[j,i-1])*pg_uk[j,i]
     # Supply of UK bills to UK Central bank - eq. 12.82
     b_cb_ukuk_s[j,i] = b_cb_ukuk_d[j,i]
     # Equilibrium condition for bills issued by US acquired by UK Central bank - eq. 12.88
     b_cb_ukus_d[j,i] = b_cb_ukus_s[j,i]*xr_us[j,i]
     # Export prices in US - eq. 12.23
     px_us[j,i] = pm_uk[j,i]*xr_uk[j,i]
     # Import prices in US - eq. 12.24
     pm_us[j,i] = px_uk[j,i]*xr_uk[j,i]
     # Real imports of UK - eq. 12.26
     im_k_uk[j,i] = exp(mu0 - mu1*log(pm_uk[j,i-1]/py_uk[j,i-1]) + mu2*log(y_k_uk[j,i]))
     # Real exports from US - eq. 12.27
     x_k_us[j,i] = im_k_uk[j,i]
     # Exports of US - eq. 12.30
     x_us[j,i] = x_k_us[j,i]*px_us[j,i]
     # Current account balance in US - eq. 12.17
     cab_us[j,i] = x_us[j,i] - im_us[j,i] + xr_uk[j,i]*r_uk[j,i-1]*b_usuk_s[j,i-1] - r_us[j,i-1]*b_ukus_s[j,i-1] - r_us[j,i-1]*b_cb_ukus_s[j,i-1]
     # Real imports of US - eq. 12.28
     im_k_us[j,i] = x_k_uk[j,i]
     # Imports of US - eq. 12.32
     im_us[j,i] = im_k_us[j,i]*pm_us[j,i]
     # Imports of UK - eq. 12.31
     im_uk[j,i] = im_k_uk[j,i]*pm_uk[j,i]
     # Current account balance - UK - eq. 12.15
     cab_uk[j,i] = x_uk[j,i] - im_uk[j,i] + xr_us[j,i]*r_us[j,i-1]*b_ukus_s[j,i-1] - r_uk[j,i-1]*b_usuk_s[j,i-1] + r_us[j,i-1]*b_cb_ukus_s[j,i-1]*xr_us[j,i]
     # Real wealth in UK - eq. 12.33
     v_k_uk[j,i] = v_uk[j,i]/pds_uk[j,i]
     # Real wealth in US - eq. 12.34
     v_k_us[j,i] = v_us[j,i]/pds_us[j,i]
     # Real Haig-Simons disposable income in UK - eq. 12.35
     ydhs_k_uk[j,i] = yd_uk[j,i]/pds_uk[j,i] - v_k_uk[j,i-1]*(pds_uk[j,i] - pds_uk[j,i-1])/pds_uk[j,i]
     # Expected real Haig-Simons disposable income in UK - eq. 12.39
     ydhse_k_uk[j,i] = (ydhs_k_uk[j,i] + ydhs_k_uk[j,i-1])/2
     # Real consumption in UK - eq. 12.37
     c_k_uk[j,i] = alpha1_uk*ydhse_k_uk[j,i] + alpha2_uk*v_k_uk[j,i-1]
     # Real domestic sales in UK - eq. 12.51
     ds_k_uk[j,i] = c_k_uk[j,i] + g_k_uk[j,i]
     # Real Haig-Simons disposable income in US - eq. 12.36
     ydhs_k_us[j,i] = yd_us[j,i]/pds_us[j,i] - v_k_us[j,i-1]*(pds_us[j,i] - pds_us[j,i-1])/pds_us[j,i]
     # Expected real Haig-Simons disposable income in US - eq. 12.40
     ydhse_k_us[j,i] = (ydhs_k_us[j,i] + ydhs_k_us[j,i-1])/2
     # Real consumption in US - eq. 12.38
     c_k_us[j,i] = alpha1_us*ydhse_k_us[j,i] + alpha2_us*v_k_us[j,i-1]
     # Real sales in US - eq. 12.42
     s_k_us[j,i] = c_k_us[j,i] + g_k_us[j,i] + x_k_us[j,i]
     # Real domestic sales in US - eq. 12.52
     ds_k_us[j,i] = c_k_us[j,i] + g_k_us[j,i]
     # Value of real output in US - eq. 12.56
     y_k_us[j,i] = s_k_us[j,i] - im_k_us[j,i]
     # Employment in US - eq. 12.66
     n_us[j,i] = y_k_us[j,i]/pr_us[j,i]
     # Price of sales in US - eq. 12.46
     ps_us[j,i] = (1 + phi_us)*(w_us[j,i]*n_us[j,i] + im_us[j,i])/s_k_us[j,i]
     # Value of sales in US - eq. 12.44
     s_us[j,i] = s_k_us[j,i]*ps_us[j,i]
     # Price of domestic sales in US - eq. 12.48
     pds_us[j,i] = (s_us[j,i] - x_us[j,i])/(s_k_us[j,i] - x_k_us[j,i])
     # Domestic sales in US - eq. 12.50
     ds_us[j,i] = s_us[j,i] - x_us[j,i]
     # Value of output in US - eq. 12.54
     y_us[j,i] = s_us[j,i] - im_us[j,i]
     # Price of output in US - eq. 12.58
     py_us[j,i] = y_us[j,i]/y_k_us[j,i]
     # Real exports from UK - eq. 12.25 - depends on current relative price
     x_k_uk[j,i] = exp(eps0[j,i] - eps1*log(pm_us[j,i]/py_us[j,i]) + eps2*log(y_k_us[j,i]))
     # Real sales in UK - eq. 12.41
     s_k_uk[j,i] = c_k_uk[j,i] + g_k_uk[j,i] + x_k_uk[j,i]
     # Value of real output in UK - eq. 12.55
     y_k_uk[j,i] = s_k_uk[j,i] - im_k_uk[j,i]
     # Consumption in US - eq. 12.60
     cons_us[j,i] = c_k_us[j,i]*pds_us[j,i]
     # Government expenditure in US - eq. 12.62
     g_us[j,i] = g_k_us[j,i]*pds_us[j,i]
     # Employment in UK - eq. 12.65
     n_uk[j,i] = y_k_uk[j,i]/pr_uk[j,i]
     # Price of sales in UK - eq. 12.45
     ps_uk[j,i] = (1 + phi_uk)*(w_uk[j,i]*n_uk[j,i] + im_uk[j,i])/s_k_uk[j,i]
     # Value of sales in UK - eq. 12.43
     s_uk[j,i] = s_k_uk[j,i]*ps_uk[j,i]
     # Value of output in UK - eq. 12.53
     y_uk[j,i] = s_uk[j,i] - im_uk[j,i]
     # Price of output in UK - eq. 12.57
     py_uk[j,i] = y_uk[j,i]/y_k_uk[j,i]
     # Import prices in UK - eq. 12.21
     pm_uk[j,i] = exp(nu0m + nu1m*log(py_us[j,i]) + (1 - nu1m)*log(py_uk[j,i]) - nu1m*log(xr_uk[j,i]))
     # Export prices in UK - eq. 12.22
     px_uk[j,i] = exp(nu0x + nu1x*log(py_us[j,i]) + (1 - nu1x)*log(py_uk[j,i]) - nu1x*log(xr_uk[j,i]))
     # Exports of UK - eq. 12.29
     x_uk[j,i] = x_k_uk[j,i]*px_uk[j,i]
     # Price of domestic sales in UK - eq. 12.47
     pds_uk[j,i] = (s_uk[j,i] - x_uk[j,i])/(s_k_uk[j,i] - x_k_uk[j,i])
     # Domestic sales in UK - eq. 12.49
     ds_uk[j,i] = s_uk[j,i] - x_uk[j,i]
     # Consumption in UK - eq. 12.59
     cons_uk[j,i] = c_k_uk[j,i]*pds_uk[j,i]
     # Government expenditure in UK - eq. 12.61
     g_uk[j,i] = g_k_uk[j,i]*pds_uk[j,i]
     psbr_uk[j,i] = g_uk[j,i] + r_uk[j,i-1]*b_uk_s[j,i-1] - t_uk[j,i] - f_cb_uk[j,i]
     psbr_us[j,i] = g_us[j,i] + r_us[j,i-1]*b_us_s[j,i-1] - t_us[j,i] - f_cb_us[j,i]
     nafa_uk[j,i] = psbr_uk[j,i] + cab_uk[j,i]
     nafa_us[j,i] = psbr_us[j,i] + cab_us[j,i]

     # Variables used in the figures
     tb_uk[j,i]     = x_uk[j,i] - im_uk[j,i]
     gb_uk[j,i]     = - psbr_uk[j,i]
     dh_uk[j,i]     = h_uk_d[j,i] - h_uk_d[j,i-1]
     dbcb_uk[j,i]   = b_cb_ukuk_d[j,i] - b_cb_ukuk_d[j,i-1]
     dbcbus_uk[j,i] = b_cb_ukus_d[j,i] - b_cb_ukus_d[j,i-1]
     debt_us[j,i]   = b_us_s[j,i] / y_us[j,i]
     debt_uk[j,i]   = b_uk_s[j,i] / y_uk[j,i]

     #Convergence check: largest relative change of the endogenous variables in this iteration
     new=sapply(endog,function(v) get(v)[j,i])
     lastDiff[j,i]=max(abs(new-old)/pmax(1,abs(old)))
     iterUsed[j,i]=iterations
     if (!is.na(lastDiff[j,i]) && lastDiff[j,i]<tol){
       break
     }

   }
 }
}

#CONVERGENCE REPORT
solved=iterUsed[,3:nPeriods]
change=lastDiff[,3:nPeriods]
cat("Convergence report\n")
cat("Tolerance (relative change between two iterations):",tol,"\n")
cat("Iterations used per period: minimum",min(solved),"- maximum",max(solved),"(limit:",nIterations,")\n")
cat("Largest change of any endogenous variable in the last iteration:",max(change),"\n")
if (any(is.na(change)) || any(change>=tol)){
  warning("Convergence not reached in some periods: increase nIterations")
} else {
  cat("All periods have converged\n")
}

#FIGURES (years 1955 to 2000: i= 6 to 51)
x=c("1955":"2000")
at_x=seq(1,46,by=5)

#Figure 12.1A
plot(cab_uk[2,6:51],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 12.1A: Evolution of CAB, NAFA, the balance of trade and government budget for the UK following an increase in the US propensity to import",ylab = '',xlab = '',ylim=range(cab_uk[2,6:51],tb_uk[2,6:51],gb_uk[2,6:51],nafa_uk[2,6:51]))
lines(tb_uk[2,6:51],type="l",lwd=2,lty=2,col="3")
lines(gb_uk[2,6:51],type="l",lwd=2,lty=5,col="2")
lines(nafa_uk[2,6:51],type="l",lwd=2,lty=4,col="4")
legend("right",c("UK current account balance","UK trade balance","UK government budget balance","UK NAFA"),  bty = "n", cex = 0.8, lty=c(1,2,5,4), lwd=c(2,2,2,2), col = c(4,3,2,4))
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 12.1B
plot(cab_uk[2,6:51],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 12.1B: Evolution of the UK CAB and of the Bank of England balance sheet components following an increase in the US propensity to import",ylab = '',xlab = '',ylim=range(cab_uk[2,6:51],dbcb_uk[2,6:51],dh_uk[2,6:51],dbcbus_uk[2,6:51]))
lines(dbcb_uk[2,6:51],type="l",lwd=2,lty=2,col="3")
lines(dh_uk[2,6:51],type="l",lwd=2,lty=5,col="4")
lines(dbcbus_uk[2,6:51],type="l",lwd=2,lty=4,col="2")
legend("center",c("UK current account balance","Change in the Bank of England holding of domestic bills","Change in the UK stock of money","Change in the Bank of England foreign reserves"),  bty = "n", cex = 0.8, lty=c(1,2,5,4), lwd=c(2,2,2,2), col = c(4,3,4,2))
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Figure 12.1C
plot(debt_us[2,6:51],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 12.1C: Evolution of the US and UK debt to GDP ratios following an increase in the US propensity to import",ylab = '',xlab = '',ylim=range(debt_us[2,6:51],debt_uk[2,6:51]))
lines(debt_uk[2,6:51],type="l",lwd=2,lty=2,col="3")
legend("right",c("US debt to GDP ratio","UK debt to GDP ratio"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,3))
axis(side=1,at=at_x,labels=x[at_x],tck=-0.02)

#Convergence chart: Gauss-Seidel iterations used in each period
plot(iterUsed[1,3:nPeriods],type="l",col="1",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Convergence: iterations used in each period",ylab = 'Iterations',xlab = 'Period',ylim=range(0,iterUsed))
for (sc in 2:nScenarios){
  lines(iterUsed[sc,3:nPeriods],type="l",lwd=2,lty=1,col=as.character(sc))
}
legend("topright",paste("Scenario",1:nScenarios),  bty = "n", cex = 0.8, lty=rep(1,nScenarios), lwd=rep(2,nScenarios), col = 1:nScenarios)
