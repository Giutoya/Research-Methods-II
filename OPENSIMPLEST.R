# =====================================================================
# OPENSIMPLEST: The Smallest SFC Open Economy Model
# =====================================================================
#
# This code translates into R the OPENSIMPLEST model originally developed in EViews by:
# Francesco Zezza (Sapienza University of Rome and Levy Economics Institute)
#
# Reference Working Paper:
# Zezza, F. (2026), "OPENSIMPLEST: The Smallest SFC Open Economy Model",
# Levy Economics Institute of Bard College, Working Paper No. 1105.
# Link to working paper and original EViews code:
# https://www.levyinstitute.org/wp-content/uploads/2026/01/wp_1105.pdf
#
# Structure follows the conventions of SFC models in R (Sim_a.R, Sim_b.R, PC.R, LP1.R)
# Created for ECON 530 / Research Methods II
# =====================================================================

# STEP 1: Clear the workspace
rm(list=ls(all=TRUE))

# PERIODS (i = 1 to 150)
nPeriods = 150

# SCENARIOS (j = 1 to 5)
# Scenario 1: Baseline (Steady State)
# Scenario 2: Step-fall in UK propensity to export (eps0 = -2.2 from period 5)
# Scenario 3: Increase in UK government spending (g = 18 from period 5)
# Scenario 4: Reduction in domestic interest rate (r = 0.029 from period 5)
# Scenario 5: Shift in liquidity preference towards foreign bills (lambda10 = 0.65, lambda20 = 0.3 from period 5)
nScenarios = 5

# =====================================================================
# STEP 2: PARAMETERS
# =====================================================================
alpha1        = 0.75     # Propensity to consume out of disposable income
alpha2        = 0.13333  # Propensity to consume out of wealth
theta         = 0.2      # Average tax rate on income
eps0_base     = -2.1     # Export equation constant term (baseline)
eps1          = 0.7      # Export elasticity with respect to exchange rate
eps2          = 1.2      # Export elasticity with respect to foreign income
mu0           = -2.1     # Import equation constant term
mu1           = 0.7      # Import elasticity with respect to exchange rate
mu2           = 1.2      # Import elasticity with respect to domestic income
lambda10_base = 0.7      # Portfolio share parameter: domestic bills (baseline)
lambda11      = 5.0      # Portfolio sensitivity to domestic interest rate (domestic bills)
lambda12      = 5.0      # Portfolio sensitivity to foreign interest rate (domestic bills)
lambda20_base = 0.25     # Portfolio share parameter: foreign bills (baseline)
lambda21      = 5.0      # Portfolio sensitivity to domestic interest rate (foreign bills)
lambda22      = 5.0      # Portfolio sensitivity to foreign interest rate (foreign bills)
phi           = 0.01     # Exchange rate adjustment speed parameter (CA-adjustment rule)

# =====================================================================
# STEP 3: VARIABLES AND MATRICES
# =====================================================================

# --- Exogenous Variables ---
# Government expenditure on goods and services
g = matrix(data=16.0, nrow=nScenarios, ncol=nPeriods)
# Domestic interest rate on government bills
r = matrix(data=0.03, nrow=nScenarios, ncol=nPeriods)
# Foreign interest rate
r_f = matrix(data=0.03, nrow=nScenarios, ncol=nPeriods)
# Foreign income (output of Rest of World)
y_f = matrix(data=80.0, nrow=nScenarios, ncol=nPeriods)

# --- Shock-Dependent Parameters (as matrices across scenarios and time) ---
eps0     = matrix(data=eps0_base, nrow=nScenarios, ncol=nPeriods)
lambda10 = matrix(data=lambda10_base, nrow=nScenarios, ncol=nPeriods)
lambda20 = matrix(data=lambda20_base, nrow=nScenarios, ncol=nPeriods)

# --- Endogenous Variables ---
# Output / GDP (income)
y = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Household consumption (private expenditure)
pe = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Regular disposable income (excluding capital gains)
yd_r = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Haig-Simons disposable income (including exchange-rate capital gains)
yd_hs = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Household net financial wealth
v = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Tax revenue
t = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Exports of goods and services
x = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Imports of goods and services
im = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Trade balance (exports - imports)
tb = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Current account balance
ca = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Nominal exchange rate (domestic currency per unit of foreign currency, GBP/USD)
xr = matrix(data=1.141, nrow=nScenarios, ncol=nPeriods)
# Net foreign asset position (NETFA / NIIP)
netfa = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Domestic government bills held by households
b_p = matrix(data=87.100, nrow=nScenarios, ncol=nPeriods)
# Foreign bills held by domestic households (in foreign currency)
bw_p = matrix(data=21.775, nrow=nScenarios, ncol=nPeriods)
# Total supply of domestic government bills
b_s = matrix(data=144.977, nrow=nScenarios, ncol=nPeriods)
# Foreign holdings of domestic government bills
b_w = matrix(data=24.668, nrow=nScenarios, ncol=nPeriods)
# Domestic bills held by the central bank
b_cb = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Money balances held by households (cash demand)
h_p = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Money supply (high-powered money)
h_s = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Central bank profits
f_cb = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Public Sector Borrowing Requirement (government deficit)
psbr = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Net Acquisition of Financial Assets by private sector (NAFA)
nafa = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)
# Sectoral balance consistency check (NAFA - PSBR - CA = 0)
sectoral_check = matrix(data=0.0, nrow=nScenarios, ncol=nPeriods)

# =====================================================================
# STEP 4: INITIAL CONDITIONS (PERIOD 1 CALIBRATION)
# =====================================================================
for (j in 1:nScenarios){
  xr[j,1]    = 1.141
  b_p[j,1]   = 87.100
  bw_p[j,1]  = 21.775
  b_s[j,1]   = 144.977
  b_w[j,1]   = 24.668
  # Net Foreign Assets (NETFA) identity
  netfa[j,1] = bw_p[j,1] * xr[j,1] - b_w[j,1]
  # Wealth identity: v = b_s + netfa ensures h_p = h_s
  v[j,1]     = b_s[j,1] + netfa[j,1]
  # Household money from balance sheet identity
  h_p[j,1]   = v[j,1] - b_p[j,1] - bw_p[j,1] * xr[j,1]
  # Central bank holdings and money supply from its balance sheet
  b_cb[j,1]  = b_s[j,1] - b_p[j,1] - b_w[j,1]
  h_s[j,1]   = b_cb[j,1]
  f_cb[j,1]  = 0
  psbr[j,1]  = 0
  ca[j,1]    = 0
  nafa[j,1]  = 0
  sectoral_check[j,1] = 0
}

# =====================================================================
# STEP 5: MODEL SIMULATION (SCENARIOS & GAUSS-SEIDEL ITERATIONS)
# =====================================================================
for (j in 1:nScenarios){

  # Define time loop
  for (i in 2:nPeriods){

    # -----------------------------------------------------------------
    # Define alternative scenarios (shocks applied from period 5 onwards)
    # -----------------------------------------------------------------
    # Scenario 2: Step-fall in export competitiveness (residual parameter eps0)
    if (j == 2 && i >= 5){
      eps0[2,i] = -2.2
    }
    
    # Scenario 3: Permanent increase in government spending (+2 units)
    if (j == 3 && i >= 5){
      g[3,i] = 18.0
    }
    
    # Scenario 4: Reduction in domestic interest rate (-0.1 percentage point)
    if (j == 4 && i >= 5){
      r[4,i] = 0.029
    }
    
    # Scenario 5: Portfolio reallocation towards foreign bills
    if (j == 5 && i >= 5){
      lambda10[5,i] = 0.65
      lambda20[5,i] = 0.30
    }

    # -----------------------------------------------------------------
    # Exchange rate predetermined adjustment rule (Eq. 22)
    # The nominal exchange rate adjusts gradually to lagged CA imbalances
    # -----------------------------------------------------------------
    xr[j,i] = xr[j,i-1] + phi * (-ca[j,i-1])
    d_xr    = xr[j,i] - xr[j,i-1]

    # Initial guess for income/output
    y[j,i] = if (y[j,i-1] > 0) y[j,i-1] else 80.0

    # -----------------------------------------------------------------
    # Gauss-Seidel iterations for within-period convergence
    # -----------------------------------------------------------------
    for (iterations in 1:100){

      # --- External Trade Block ---
      # 8. Exports of goods and services
      x[j,i] = exp(eps0[j,i] + eps1 * log(xr[j,i-1]) + eps2 * log(y_f[j,i]))
      
      # 9. Imports of goods and services
      im[j,i] = exp(mu0 - mu1 * log(xr[j,i-1]) + mu2 * log(pmax(y[j,i], 1e-6)))
      
      # 10. Trade balance
      tb[j,i] = x[j,i] - im[j,i]
      
      # 11. Current account balance (trade balance + net cross-border interest flows)
      ca[j,i] = tb[j,i] + (r_f[j,i-1] * bw_p[j,i-1] * xr[j,i] - r[j,i-1] * b_w[j,i-1])
      
      # 12. External position (Net Foreign Assets, NETFA, from CA + valuation gains)
      netfa[j,i] = netfa[j,i-1] + ca[j,i] + d_xr * bw_p[j,i-1]

      # --- Income, Wealth, and Fiscal Block ---
      # 1. Regular disposable income (excluding capital gains)
      yd_r[j,i] = y[j,i] + (r[j,i-1] * b_p[j,i-1] + r_f[j,i-1] * bw_p[j,i-1] * xr[j,i]) - t[j,i]
      
      # 2. Haig-Simons disposable income (including valuation gains on foreign assets)
      yd_hs[j,i] = yd_r[j,i] + d_xr * bw_p[j,i-1]
      
      # 6. Household consumption (private expenditure)
      pe[j,i] = alpha1 * yd_hs[j,i] + alpha2 * v[j,i-1]
      
      # 5. Income / Output determination (GDP)
      y[j,i] = pe[j,i] + g[j,i] + x[j,i] - im[j,i]
      
      # 4. Tax revenue
      t[j,i] = theta * (y[j,i] + r[j,i-1] * b_p[j,i-1] + r_f[j,i-1] * bw_p[j,i-1] * xr[j,i])
      
      # 3. Household wealth accumulation
      v[j,i] = v[j,i-1] + yd_hs[j,i] - pe[j,i]

      # --- Portfolio Allocation Block (Tobin Asset Demands) ---
      # 14. Demand for domestic bills
      b_p[j,i] = v[j,i] * (lambda10[j,i] + lambda11 * r[j,i] - lambda12 * r_f[j,i])
      
      # 15. Demand for foreign bills (in foreign currency units)
      bw_p[j,i] = v[j,i] * (lambda20[j,i] - lambda21 * r[j,i] + lambda22 * r_f[j,i]) / xr[j,i]
      
      # 13. Foreign demand for domestic government bills (from Balance Sheet Matrix)
      b_w[j,i] = bw_p[j,i] * xr[j,i] - netfa[j,i]
      
      # 16. Demand for money (determined residually to close household portfolio)
      h_p[j,i] = v[j,i] - b_p[j,i] - bw_p[j,i] * xr[j,i]

      # --- Central Bank and Government Block ---
      # 17. Central bank profits (interest received on bill holdings, rebated to gov)
      f_cb[j,i] = r[j,i-1] * b_cb[j,i-1]
      
      # 20. Public Sector Borrowing Requirement (PSBR, government deficit)
      psbr[j,i] = g[j,i] + r[j,i-1] * b_s[j,i-1] - t[j,i] - f_cb[j,i]
      
      # 21. Total supply of domestic government bills
      b_s[j,i] = b_s[j,i-1] + psbr[j,i]
      
      # 18. Central bank holding of domestic bills (residual purchaser of public debt)
      b_cb[j,i] = b_s[j,i] - b_p[j,i] - b_w[j,i]
      
      # 19. Money supply (high-powered money, closes CB balance sheet)
      h_s[j,i] = b_cb[j,i]

      # --- Identities & Consistency Checks ---
      # 7. Private sector Net Acquisition of Financial Assets (NAFA)
      nafa[j,i] = psbr[j,i] + ca[j,i]
      
      # Sectoral balance consistency check (should identically equal zero)
      sectoral_check[j,i] = nafa[j,i] - psbr[j,i] - ca[j,i]

    } # End iterations
  }   # End time
}     # End scenarios

# =====================================================================
# STEP 6: CONSISTENCY CHECKS
# =====================================================================
# Check 1: Sectoral Balances identity (NAFA - PSBR - CA = 0)
max_sec_err = max(abs(sectoral_check))
cat("Maximum Sectoral Balance error (NAFA - PSBR - CA):", max_sec_err, "\n")

# Check 2: Balance Sheet / Money Market identity (H_p - H_s = 0)
max_h_err = max(abs(h_p - h_s))
cat("Maximum Money Market error (H_p - H_s):", max_h_err, "\n")

# =====================================================================
# STEP 7: GRAPHICAL OUTPUT (REPLICATING FIGURES FROM WORKING PAPER)
# =====================================================================

# Time index for plotting (periods 1 to 150)
time_axis = 1:nPeriods

# ---------------------------------------------------------------------
# Figure 2: Effect of a Step-Fall in UK Propensity to Export (Scenario 2 vs Baseline)
# Replicating Figure 2 in paper (and fig1 in EViews code)
# ---------------------------------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,3,2))

# Panel A: Current Account and Trade Balance
plot(time_axis, ca[2,] - ca[1,], type="l", col="blue", lty=2, lwd=2,
     main="A: Current Account and Trade Balance", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(ca[2,] - ca[1,], tb[2,] - tb[1,])))
lines(time_axis, tb[2,] - tb[1,], col="forestgreen", lty=1, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Current Account (CA)", "Trade Balance (TB)"),
       col=c("blue", "forestgreen"), lty=c(2,1), lwd=2, bty="n", cex=0.75)

# Panel B: Sterling Exchange Rate ($ per GBP)
plot(time_axis, (1/xr[2,]) - (1/xr[1,]), type="l", col="blue", lty=1, lwd=2,
     main="B: Sterling Exchange Rate ($ per GBP)", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Exchange rate ($ per GBP)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel C: Real GDP
plot(time_axis, y[2,] - y[1,], type="l", col="blue", lty=1, lwd=2,
     main="C: Real GDP", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Real GDP (Y)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel D: Sectoral Balances
plot(time_axis, nafa[2,] - nafa[1,], type="l", col="blue", lty=2, lwd=2,
     main="D: Sectoral Balances", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(nafa[2,] - nafa[1,], ca[2,] - ca[1,], psbr[2,] - psbr[1,])))
lines(time_axis, ca[2,] - ca[1,], col="forestgreen", lty=1, lwd=2)
lines(time_axis, psbr[2,] - psbr[1,], col="red", lty=3, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Private NAFA", "Current Account (CA)", "Government Deficit (PSBR)"),
       col=c("blue", "forestgreen", "red"), lty=c(2,1,3), lwd=2, bty="n", cex=0.7)

# Reset graphics layout
par(mfrow=c(1,1))

# ---------------------------------------------------------------------
# Figure 3: Effect of an Increase in UK Government Spending (Scenario 3 vs Baseline)
# Replicating Figure 3 in paper (and fig2 in EViews code)
# ---------------------------------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,3,2))

# Panel A: Current Account and Trade Balance
plot(time_axis, ca[3,] - ca[1,], type="l", col="blue", lty=2, lwd=2,
     main="A: Current Account and Trade Balance", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(ca[3,] - ca[1,], tb[3,] - tb[1,])))
lines(time_axis, tb[3,] - tb[1,], col="forestgreen", lty=1, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Current Account (CA)", "Trade Balance (TB)"),
       col=c("blue", "forestgreen"), lty=c(2,1), lwd=2, bty="n", cex=0.75)

# Panel B: Sterling Exchange Rate ($ per GBP)
plot(time_axis, (1/xr[3,]) - (1/xr[1,]), type="l", col="blue", lty=1, lwd=2,
     main="B: Sterling Exchange Rate ($ per GBP)", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Exchange rate ($ per GBP)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel C: Real GDP
plot(time_axis, y[3,] - y[1,], type="l", col="blue", lty=1, lwd=2,
     main="C: Real GDP", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Real GDP (Y)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel D: Sectoral Balances
plot(time_axis, nafa[3,] - nafa[1,], type="l", col="blue", lty=2, lwd=2,
     main="D: Sectoral Balances", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(nafa[3,] - nafa[1,], ca[3,] - ca[1,], psbr[3,] - psbr[1,])))
lines(time_axis, ca[3,] - ca[1,], col="forestgreen", lty=1, lwd=2)
lines(time_axis, psbr[3,] - psbr[1,], col="red", lty=3, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Private NAFA", "Current Account (CA)", "Government Deficit (PSBR)"),
       col=c("blue", "forestgreen", "red"), lty=c(2,1,3), lwd=2, bty="n", cex=0.7)

# Reset graphics layout
par(mfrow=c(1,1))

# ---------------------------------------------------------------------
# Figure 4: Effect of a Reduction in UK Interest Rate of 0.1% (Scenario 4 vs Baseline)
# Replicating Figure 4 in paper (and fig3 in EViews code)
# ---------------------------------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,3,2))

# Panel A: Current Account and Trade Balance
plot(time_axis, ca[4,] - ca[1,], type="l", col="blue", lty=2, lwd=2,
     main="A: Current Account and Trade Balance", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(ca[4,] - ca[1,], tb[4,] - tb[1,])))
lines(time_axis, tb[4,] - tb[1,], col="forestgreen", lty=1, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("topright", legend=c("Current Account (CA)", "Trade Balance (TB)"),
       col=c("blue", "forestgreen"), lty=c(2,1), lwd=2, bty="n", cex=0.75)

# Panel B: Sterling Exchange Rate ($ per GBP)
plot(time_axis, (1/xr[4,]) - (1/xr[1,]), type="l", col="blue", lty=1, lwd=2,
     main="B: Sterling Exchange Rate ($ per GBP)", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Exchange rate ($ per GBP)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel C: Real GDP
plot(time_axis, y[4,] - y[1,], type="l", col="blue", lty=1, lwd=2,
     main="C: Real GDP", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Real GDP (Y)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel D: Sectoral Balances
plot(time_axis, nafa[4,] - nafa[1,], type="l", col="blue", lty=2, lwd=2,
     main="D: Sectoral Balances", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(nafa[4,] - nafa[1,], ca[4,] - ca[1,], psbr[4,] - psbr[1,])))
lines(time_axis, ca[4,] - ca[1,], col="forestgreen", lty=1, lwd=2)
lines(time_axis, psbr[4,] - psbr[1,], col="red", lty=3, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("topright", legend=c("Private NAFA", "Current Account (CA)", "Government Deficit (PSBR)"),
       col=c("blue", "forestgreen", "red"), lty=c(2,1,3), lwd=2, bty="n", cex=0.7)

# Reset graphics layout
par(mfrow=c(1,1))

# ---------------------------------------------------------------------
# Figure 5: Effect of a Change in UK Private Sector Preference for Foreign Bills
# (Scenario 5 vs Baseline, replicating Figure 5 in paper / fig4 in EViews)
# ---------------------------------------------------------------------
par(mfrow=c(2,2), mar=c(4,4,3,2))

# Panel A: Current Account and Trade Balance
plot(time_axis, ca[5,] - ca[1,], type="l", col="blue", lty=2, lwd=2,
     main="A: Current Account and Trade Balance", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(ca[5,] - ca[1,], tb[5,] - tb[1,])))
lines(time_axis, tb[5,] - tb[1,], col="forestgreen", lty=1, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("topright", legend=c("Current Account (CA)", "Trade Balance (TB)"),
       col=c("blue", "forestgreen"), lty=c(2,1), lwd=2, bty="n", cex=0.75)

# Panel B: Sterling Exchange Rate ($ per GBP)
plot(time_axis, (1/xr[5,]) - (1/xr[1,]), type="l", col="blue", lty=1, lwd=2,
     main="B: Sterling Exchange Rate ($ per GBP)", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Exchange rate ($ per GBP)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel C: Real GDP
plot(time_axis, y[5,] - y[1,], type="l", col="blue", lty=1, lwd=2,
     main="C: Real GDP", xlab="Periods", ylab="Deviation from Baseline")
abline(h=0, col="gray60", lty=3)
legend("bottomright", legend=c("Real GDP (Y)"),
       col="blue", lty=1, lwd=2, bty="n", cex=0.75)

# Panel D: Sectoral Balances
plot(time_axis, nafa[5,] - nafa[1,], type="l", col="blue", lty=2, lwd=2,
     main="D: Sectoral Balances", xlab="Periods", ylab="Deviation from Baseline",
     ylim=range(c(nafa[5,] - nafa[1,], ca[5,] - ca[1,], psbr[5,] - psbr[1,])))
lines(time_axis, ca[5,] - ca[1,], col="forestgreen", lty=1, lwd=2)
lines(time_axis, psbr[5,] - psbr[1,], col="red", lty=3, lwd=2)
abline(h=0, col="gray60", lty=3)
legend("topright", legend=c("Private NAFA", "Current Account (CA)", "Government Deficit (PSBR)"),
       col=c("blue", "forestgreen", "red"), lty=c(2,1,3), lwd=2, bty="n", cex=0.7)

# Reset graphics layout
par(mfrow=c(1,1))

# =====================================================================
# STEADY-STATE CONDITIONS CHECK (NAFA = 0) - BASELINE SCENARIO (_0)
# =====================================================================

# 1. Extract Baseline Scenario (Row 1) variables directly from the matrices
g_0 <- g[1, ]
r_0 <- r[1, ]
r_f_0 <- r_f[1, ]
b_p_0 <- b_p[1, ]
b_w_0 <- b_w[1, ]
bw_p_0 <- bw_p[1, ]
xr_0 <- xr[1, ]
y_f_0 <- y_f[1, ]
v_0 <- v[1, ]
netfa_0 <- netfa[1, ]
eps0_0 <- eps0[1, ]
lambda10_0 <- lambda10[1, ]
lambda20_0 <- lambda20[1, ]
y_0 <- y[1, ]
ca_0 <- ca[1, ]
psbr_0 <- psbr[1, ]

# 2. Compute the steady-state analytical conditions using base R vector arithmetic
# Steady-state output derived from the Fiscal Stance (PSBR = 0)
y_star_fiscal <- (g_0 + r_0*(b_p_0 + b_w_0))/theta - r_0*b_p_0 - r_f_0*bw_p_0*xr_0

# Steady-state output derived from the Export Performance Ratio (CA = 0)
y_star_trade <- ((exp(eps0_0 + eps1*log(xr_0) + eps2*log(y_f_0)) + r_f_0*bw_p_0*xr_0 - r_0*b_w_0) / 
                   (exp(mu0) * xr_0^(-mu1))) ^ (1/mu2)

# Steady-state output derived from structural portfolio parameters
lambda_p <- lambda10_0 + lambda11*r_0 - lambda12*r_f_0
lambda_w <- lambda20_0 - lambda21*r_0 + lambda22*r_f_0
y_star_exog <- g_0/theta + v_0 * ((r_0*(lambda_p + lambda_w))/theta - r_0*lambda_p - r_f_0*lambda_w) - (r_0*netfa_0)/theta

# Phase Diagram Scaling Variables (% of GDP)
ca_pct <- (ca_0 / y_0) * 100
pub_budget_pct <- (-psbr_0 / y_0) * 100
nafa_zero_line <- ca_pct # 45-degree boundary line (Y = X)

time_axis <- 1:nPeriods

# ---------------------------------------------------------------------
# Plot 1: Steady-State Convergence (Output) 
# ---------------------------------------------------------------------
plot(time_axis, y_0, type="l", col="black", lty=1, lwd=2,
     main="Steady-State Output Conditions (NAFA=0)",
     xlab="Periods", ylab="Output",
     ylim=c(93, max(c(y_0, y_star_fiscal, y_star_trade, y_star_exog), na.rm=TRUE)))

lines(time_axis, y_star_fiscal, col="blue", lty=2, lwd=2)
lines(time_axis, y_star_trade, col="red", lty=3, lwd=2)
lines(time_axis, y_star_exog, col="green", lty=4, lwd=2)

legend("bottomright", 
       legend=c("Actual GDP (y_0)", "SS from Fiscal Stance", 
                "SS from Trade Balance", "SS from Structural Params"),
       col=c("black", "blue", "red", "green"), 
       lty=c(1, 2, 3, 4), lwd=2, bty="n", cex=0.8)

# ---------------------------------------------------------------------
# Plot 2: XY Phase Diagram (CA vs Public Budget Scaled as % of GDP)
# ---------------------------------------------------------------------
# Calculate dynamic text coordinates for NAFA zones
text_x_pos_min <- min(ca_pct, na.rm = TRUE) * 0.5
text_y_pos_min <- min(pub_budget_pct, na.rm = TRUE) #* 0.5
text_x_pos_max <- text_x_pos_min#max(ca_pct, na.rm = TRUE) * #-0.5
text_y_pos_max <- max(pub_budget_pct, na.rm = TRUE) #* 0.75

# Initialize plot with trajectory path and circles
plot(ca_pct, pub_budget_pct, type="b", pch=1, col="blue", lwd=1.5,
     main="8A: Phase Diagram (CA vs Public Budget as % of GDP)",
     xlab="Current Account Balance (% of GDP)",
     ylab="Public Budget Balance (% of GDP)",
     xlim=range(c(ca_pct, -max(abs(ca_pct))), na.rm=TRUE),
     ylim=range(c(pub_budget_pct, nafa_zero_line, -max(abs(pub_budget_pct))), na.rm=TRUE))

# Add 0-lines for X and Y axes
abline(h=0, col="black", lwd=1)
abline(v=0, col="black", lwd=1)

# Add 45-degree NAFA=0 Boundary line (Y = X)
lines(ca_pct, nafa_zero_line, col="black", lty=2, lwd=2)

# Floating text annotations for NAFA areas
text(x = text_x_pos_max, y = text_y_pos_max, labels = "NAFA < 0 Area", font = 3)
text(x = text_x_pos_min, y = text_y_pos_min, labels = "NAFA > 0 Area", font = 3)

legend("bottomright", 
       legend=c("Economy Trajectory", "NAFA = 0 Boundary"),
       col=c("blue", "black"), 
       lty=c(1, 2), pch=c(1, NA), lwd=1.5, bty="n", cex=0.8)