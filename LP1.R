# LP1 model – R translation 

#Created by GTY for ECON 530 2026

#Clear
rm(list=ls(all=TRUE))

#PERIODS (i= 1 to 100)
nPeriods=100  

#SCENARIOS (j= 1 to 2)
nScenarios=3



# ----------------------------------------------------------
# Parameters
# ----------------------------------------------------------
alpha1   = 0.8
alpha2   = 0.2
chi      = 0.1
lambda20 = 0.44196
lambda22 = 1.1
lambda23 = 1
lambda24 = 0.03
lambda30 = 0.3997
lambda32 = 1
lambda33 = 1.1
lambda34 = 0.03
theta    = 0.1938


# ----------------------------------------------------------
# Endogenous variables
# ----------------------------------------------------------


# Demand for government bills
b_d  = matrix(0, nScenarios, nPeriods)
# Government bills held by households
b_h  = matrix(37.839, nScenarios, nPeriods)
# Government bills supplied by government
b_s  = matrix(57.964, nScenarios, nPeriods)
# Government bills held by Central Bank
b_cb = matrix(b_s - b_h, nScenarios, nPeriods)
# Demand for government bonds
bl_d = matrix(0, nScenarios, nPeriods)
# Government bonds held by households
bl_h = matrix(1.892, nScenarios, nPeriods)
# Supply for government bonds
bl_s = matrix(bl_h, nScenarios, nPeriods)
# Capital gains on bonds
cg   = matrix(0, nScenarios, nPeriods)
# Expected capital gains on bonds
cg_e = matrix(0, nScenarios, nPeriods)
# Consumption goods
cons = matrix(0, nScenarios, nPeriods)
# Expected rate of return on bonds
er_rbl = matrix(0, nScenarios, nPeriods)
# Government goods (exogenous)
g = matrix(20, nScenarios, nPeriods)
# Demand for cash
h_d = matrix(0, nScenarios, nPeriods)
# Cash money held by households
h_h = matrix(0, nScenarios, nPeriods)
# Cash money supplied by central bank
h_s = matrix(20.125, nScenarios, nPeriods)
# Expected price of bonds
p_bl_e = matrix(0, nScenarios, nPeriods)
# Exogenously set price of bonds
p_bl_bar = matrix(20, nScenarios, nPeriods)
# Exogenously set interest rate on government bills
r_bar = matrix(0.03, nScenarios, nPeriods)
# Interest rate on government bills
r_b = matrix(r_bar, nScenarios, nPeriods)
# Interest rate on government bonds
r_bl = matrix(0, nScenarios, nPeriods)
# Price of bonds
p_bl = matrix(p_bl_bar, nScenarios, nPeriods)
# Taxes
t = matrix(0, nScenarios, nPeriods)
# Households wealth
v = matrix(95.803, nScenarios, nPeriods)
# Expected households wealth
v_e = matrix(0, nScenarios, nPeriods)
# Income = GDP
y = matrix(0, nScenarios, nPeriods)
# Regular disposable income of households
yd_r = matrix(95.803, nScenarios, nPeriods)
# Expected regular disposable income of households
yd_r_e = matrix(0, nScenarios, nPeriods)

# ----------------------------------------------------------
# Simulation
# ----------------------------------------------------------
for (j in 1:nScenarios){
  
  for (i in 2:nPeriods){
    
    for (iterations in 1:nPeriods){
      
      # -----------------------------
      # Exogenous shock (Baseline)
      # -----------------------------
      if (j == 2 && i >= 15){
        r_bar[2,i]    = 0.04
        p_bl_bar[2,i] = 15
        
      }
      
      # -----------------------------
      # Exogenous shock (Scenario 1)
      # -----------------------------
      if (j == 3 && i >= 15){
        alpha1 = 0.7
        
      }
      
      # -----------------------------
      # Model equations
      # -----------------------------
      
      y[j,i]       = cons[j,i] + g[j,i]
      yd_r[j,i]    = y[j,i] - t[j,i] + r_b[j,i-1]*b_h[j,i-1] + bl_h[j,i-1]
      t[j,i]       = theta*(y[j,i] + r_b[j,i-1]*b_h[j,i-1] + bl_h[j,i-1])
      cg[j,i]      = (p_bl[j,i] - p_bl[j,i-1])*bl_h[j,i-1]
      v[j,i]       = v[j,i-1] + (yd_r[j,i] - cons[j,i]) + cg[j,i]
      cons[j,i]    = alpha1*yd_r_e[j,i] + alpha2*v[j,i-1]
      v_e[j,i]     = v[j,i-1] + (yd_r_e[j,i] - cons[j,i]) + cg[j,i]
      h_h[j,i]     = v[j,i] - b_h[j,i] - p_bl[j,i]*bl_h[j,i]
      h_d[j,i]     = v_e[j,i] - b_d[j,i] - p_bl[j,i]*bl_d[j,i]
      er_rbl[j,i]  = r_bl[j,i] + chi*(p_bl_e[j,i] - p_bl[j,i])/p_bl[j,i]
      b_d[j,i]     = v_e[j,i]*(lambda20 + lambda22*r_b[j,i] - lambda23*er_rbl[j,i] - lambda24*(yd_r_e[j,i]/v_e[j,i]))
      bl_d[j,i]    = v_e[j,i]*(lambda30 - lambda32*r_b[j,i] + lambda33*er_rbl[j,i] - lambda34*(yd_r_e[j,i]/v_e[j,i]))/p_bl[j,i]
      b_h[j,i]     = b_d[j,i]
      bl_h[j,i]    = bl_d[j,i]
      b_s[j,i]     = b_s[j,i-1] + (g[j,i] + r_b[j,i-1]*b_s[j,i-1] + bl_s[j,i-1]) - (t[j,i] + r_b[j,i-1]*b_cb[j,i-1]) - p_bl[j,i]*(bl_s[j,i] - bl_s[j,i-1])
      h_s[j,i]     = h_s[j,i-1] + b_cb[j,i] - b_cb[j,i-1]
      b_cb[j,i]    = b_s[j,i] - b_h[j,i]
      bl_s[j,i]    = bl_h[j,i]
      r_bl[j,i]    = 1/p_bl[j,i]
      p_bl_e[j,i]  = p_bl[j,i]
      cg_e[j,i]    = chi*(p_bl_e[j,i] - p_bl[j,i])*bl_h[j,i]
      yd_r_e[j,i]  = yd_r[j,i-1]
      r_b[j,i]     = r_bar[j,i]
      p_bl[j,i]    = p_bl_bar[j,i]
      
    }
  }
}



x=c("1958":"2001")

#Figure 5.2
plot(v[2,12:55]/yd_r[2,12:55],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 5.2 Evolution of V/Yd following an increase in interest rates",ylab = NA,xlab = NA,ylim=range(0.9,1.001))
par(new=T)
legend("center",c("Wealth to disposable income ratio"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = "2", box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)

#Figure 5.3
plot(yd_r[2,12:55],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 5.3 Evolution of Yd and C following an increase in interest rates",ylab = NA,xlab = NA,ylim=range(93,102))
lines(cons[2,12:55],xaxt='n',type="l",lwd=2,lty=2,col="2", ylab=NA, xlab=NA,cex.main=0.75,font.main=1)
legend("center",c("Disposable income","Consumption"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(4,2), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)

#Figure 5.4
plot(b_h[2,12:55]/v[2,12:55],xaxt='n',type="l",col="4",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 5.4 Evolution of B/V and BL/V following an increase in interest rates",ylab = NA,xlab = NA,ylim=range(.384,.408))
lines(p_bl[2,12:55]*bl_h[2,12:55]/v[2,12:55],type="l",lwd=2,lty=2,col="2")
legend("bottomright",c("Bills to wealth ratio","Bonds to wealth ratio"),  bty = "n", cex = 0.8, lty=c(4,2), lwd=c(2,2), col = c(4,2), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)


#Figure 5.10
plot(y[3,12:55],xaxt='n',type="l",col="2",lwd=2,lty=1,font.main=1,cex.main=0.75,main="Figure 5.10 Evolution of GDP following a sharp decrease\rin the propensity to consume out of income",ylab = NA,xlab = NA,ylim=range(95,125))
legend("bottomright",c("Gross domestic product"),  bty = "n", cex = 0.8, lty=c(1,2), lwd=c(2,2), col = c(2), box.lwd=0)
axis(side=1,at=1:44,labels=x,tck=-0.07)





# ----------------------------------------------------------
# APPENDIX: Alternative way to define variables (with loop)
# ----------------------------------------------------------
# 
# 
# # ----------------------------------------------------------
# # Variables
# # ----------------------------------------------------------
# vars = c(
#   "b_cb","b_d","b_h","b_s","bl_d","bl_h","bl_s","cg","cg_e",
#   "cons","er_rbl","g","h_d","h_h","h_s","p_bl","p_bl_e",
#   "p_bl_bar","r_b","r_bar","r_bl","t","v","v_e","y",
#   "yd_r","yd_r_e"
# )
# 
# for (vname in vars){
#   assign(vname, matrix(data=0, nrow=nScenarios, ncol=nPeriods))
# }
# 
# # ----------------------------------------------------------
# # Initial values (t = 1)
# # ----------------------------------------------------------
# g[,]        =matrix(20)
# r_bar[,]    = matrix(0.03)
# p_bl_bar[,] = matrix(20)
# 
# v[,]    = matrix(95.803)
# b_h[,]  = matrix(37.839)
# b_s[,]  = matrix(57.964)
# b_cb[,] = b_s[,] - b_h[,]
# bl_h[,] = matrix(1.892)
# bl_s[,] = bl_h[,]
# h_s[,]  = matrix(20.125)
# 
# yd_r[,] = matrix(95.803)
# r_b[,]  = r_bar[,]
# p_bl[,] = p_bl_bar[,]

