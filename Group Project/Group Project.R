# --------------------------------------------------------------------------------------------
# Fixed Income Securities | Group Project | Data Science for Finance 2025
# Afonso Casanova 20240795 | Francisco Perestrello 20241560 | João Pardal 20240796 | Nuno Vieira 20241111
# --------------------------------------------------------------------------------------------

rm(list=ls(all.names = TRUE))
graphics.off()
close.screen(all.screens = TRUE)
erase.screen()
windows.options(record=TRUE)
options(scipen = 999)
#setwd('C:/Users/20241560/Desktop/Pós-Graduação/T2/Fixed Income Securities/Project') # Adapt to your directory

library(pacman)
if (!require("pacman")) install.packages("pacman")

p_load(YieldCurve)
p_load(readxl)
p_load(tidyr)
p_load(dplyr)
p_load(ggplot2)

# --------------------------------------------------------------------------------------------
# 1. Government Bonds
# --------------------------------------------------------------------------------------------

bonds <- data.frame(maturity = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                    coupon = c(0.5, 0.75, 1, 1.25, 1.5, 1.7, 1.8, 2, 2.25, 2.5),
                    price = c(98, 98.5, 100, 99.5, 99, 100.5, 101, 101, 101.5, 100.5),
                    yield_vol = c(3.5, 3.4, 3.3, 3.15, 3, 2.8, 2.6, 2.4, 2.2, 2)
                    ) 

# a) Bootstrapping the Spot Rates 

get_zero_coupon <- function(coupons = bonds$coupon,
                            BondPrices = bonds$price,
                            nominal_value = 100){
  
  # Both Coupons and Bond Prices vectors are arranged to 0.5 years increasing maturity
  cashflow_matrix <- matrix(0, nrow=length(coupons), ncol=length(coupons))
  
  # Assign the coupons for each semester
  for(i in 1:length(coupons)){
    cashflow_matrix[i,1:i] <- nominal_value * (coupons[i]/100) / 2 # Semi-annual coupons
  }
  
  # Add the maturity nominal value
  diag(cashflow_matrix) <- diag(cashflow_matrix) + nominal_value
  cashflow_matrix <- cashflow_matrix
    
  # Solve the system of equations to get the discount factors
  zero_coupon_prices <- solve(cashflow_matrix, BondPrices)
  
  # Calculate the spot rates (assuming continuous compounding)
  zero_coupon_yields <- -log(zero_coupon_prices)/(1:length(coupons)/2)
  
  return(list(spot_rates = zero_coupon_yields, cashflow_matrix = cashflow_matrix, zero_coupon_prices = zero_coupon_prices))
}

# Store relevant variables
zero_coupon_results <- get_zero_coupon()
zero_coupon_prices <- zero_coupon_results$zero_coupon_prices
spot_rates <- zero_coupon_results$spot_rates
spot_rates_pct <- round(spot_rates*100, 2)

spot_curve <- data.frame(
  Maturity        = bonds$maturity,
  DiscountFactor  = zero_coupon_prices,
  SpotRateDecimal = spot_rates,
  SpotRatePercent = spot_rates_pct
)

spot_curve

# Plotting 
plot(
  spot_curve$Maturity, 
  spot_curve$SpotRatePercent, 
  type = "n",                              
  main = "Bootstrapped Spot Curve (Continuous Compounding)",
  xlab = "Maturity (Years)",
  ylab = "Spot Rate (%)",
  ylim = c(0, max(spot_rates_pct)*1.1)     
)
grid(col="gray80")
lines(spot_curve$Maturity, spot_curve$SpotRatePercent, 
      col = "black", lwd=2)
points(spot_curve$Maturity, spot_curve$SpotRatePercent, 
       col = "red", pch=19)

# Observations on the results:

# The short-end of the curve is very steepened compared to the belly and low-end of the curve. 


# b) Bond Volatilities 

# Converting percentages to decimals
couponRates <- bonds$coupon / 100   
yieldVols <- bonds$yield_vol / 100   

# Function to find the price of bonds
priceBond <- function(ytm, cpnRate, maturity, face, marketPrice=NA) {
  n   <- as.integer(maturity * 2)      
  cpn <- cpnRate * face / 2           
  cfTimes <- 1:n                      
  df <- exp(-ytm*cfTimes) # continuous compounding
  pvcoupons <- cpn * sum(df)
  pvredemp  <- face * df[length(df)]
  return( pvcoupons + pvredemp )
}

# Function to find yield to maturity
getYTM <- function(mktPrice, cpnRate, maturity, face=100) {
  f <- function(y) priceBond(y, cpnRate, maturity, face) - mktPrice
  root <- uniroot(f, interval=c(-0.0001, 0.20))
  return(root$root)
}

# Using Macaulay (and Modified) duration
bondDuration <- function(ytm, cpnRate, maturity, face=100) {
  n   <- as.integer(maturity * 2)
  cpn <- cpnRate * face / 2
  tHalfYears <- 1:n
  tYears <- 0.5 * tHalfYears
  df <- exp(-ytm*tHalfYears) # continuous compounding
  
  # Cashflows
  cf <- rep(cpn, n)
  cf[n] <- cf[n] + face  
  
  # Present value of each cashflow
  pvcf <- cf * df
  
  # Macaulay duration = sum( t * PV(CF) ) / total PV  (where t is in years)
  wts   <- pvcf / sum(pvcf)
  D_mac <- sum(tYears * wts)
  D_mod <- D_mac # under continuous compounding, Macaulay Duration = Modified Duration
  
  return(list(D_mac = D_mac, D_mod = D_mod))
}

nBonds <- length(bonds$maturity)
results <- data.frame(
  Bond      = 1:nBonds,
  Maturity  = bonds$maturity,
  Price     = bonds$price,
  YTM_pct   = numeric(nBonds),
  ModDur    = numeric(nBonds),
  YieldVol_pct = bonds$yield_vol,
  BondVol_pct  = numeric(nBonds)  
)

for(i in seq_len(nBonds)) {
  y <- getYTM(
    mktPrice = bonds$price[i], 
    cpnRate  = couponRates[i],
    maturity = bonds$maturity[i])
  
  dur <- bondDuration(
    ytm       = y,
    cpnRate   = couponRates[i],
    maturity  = bonds$maturity[i])
  
  bondVol <- dur$D_mod * yieldVols[i] * 100  
  
  results$YTM_pct[i]       <- y * 100
  results$ModDur[i]        <- dur$D_mod
  results$BondVol_pct[i]   <- bondVol
}

round(results, 2)

# Plotting
plot(
  results$Maturity,
  results$BondVol_pct,
  type = "b",             
  pch  = 19,
  col  = "red",
  lwd  = 2,
  main = "Bond Price Volatility by Maturity",
  xlab = "Maturity (Years)",
  ylab = "Annualized Price Volatility (%)",
  ylim = c(0, max(results$BondVol_pct) * 1.1)
)
grid()


# c) Corporate Bond

faceValue    <- 100000
annualCoupon <- 0.03       # 3% semi-annual coupon payments         
coupon       <- annualCoupon/2 * faceValue  
spread       <- 0.01       # 1% flat credit spread
tVector      <- spot_curve$Maturity   
spotRates    <- spot_curve$SpotRateDecimal   

bondPrice <- 0
n <- length(tVector)  
for (k in 1:n) {
  t <- tVector[k]
  r <- spotRates[k] + spread         
  df <- exp(-r * t) # continuous compounding            
  cf <- if (k < n) coupon else (coupon + faceValue)
  
  bondPrice <- bondPrice + cf*df
}

timeWeightedPV <- 0
for (k in 1:n) {
  t  <- tVector[k]
  r  <- spotRates[k] + spread
  df <- exp(-r * t) # continuous compounding
  cf <- if (k < n) coupon else (coupon + faceValue)
  
  timeWeightedPV <- timeWeightedPV + t * cf * df
}

# Calculate Durations
macaulayDuration <- timeWeightedPV / bondPrice
modifiedDuration <- macaulayDuration  # continuous compounding

# Results
cat(sprintf("Corporate Bond Price: $%0.2f\n", bondPrice))
cat(sprintf("Macaulay/Modified Duration: %0.3f years\n", modifiedDuration))

timeSquaredPV <- 0
for (k in 1:n) {
  t  <- tVector[k]
  r  <- spotRates[k] + spread
  df <- exp(-r * t) # continuous compounding
  cf <- if (k < n) coupon else (coupon + faceValue)
  
  timeSquaredPV <- timeSquaredPV + (t^2) * cf * df
}
convexity <- timeSquaredPV / bondPrice

cat(sprintf("Convexity: %0.3f\n", convexity))

# --------------------------------------------------------------------------------------------
# 2. Yield Curve
# --------------------------------------------------------------------------------------------

# a) Nelson-Siegel and Mean Squared Error

# Import data and set Date as index
df_raw <- read_excel("Yield_data.xlsx")

# Convert the 'Date' column to Date type
df_raw <- df_raw %>%
  mutate(Date = as.Date(Date))

head(df_raw)

# Reshaping Data from Wide to Long format
# Convert maturity columns into a single "Maturity" column
# Yield values go into the "Yield" column

df_long <- df_raw %>%
  pivot_longer(
    cols = c("0.25","0.5","1","2","3","4","5","6","7","8","9","10","15","20","25","30"),
    names_to = "Maturity",
    values_to = "Yield"
  ) %>%
  
  # Convert Maturity from character (e.g. "0.25") to numeric
  mutate(Maturity = as.numeric(Maturity))

head(df_long)

# Define the Nelson-Siegel Function

# The Nelson-Siegel model:
#  R(tau) = Beta0 + Beta1 * [ (1 - exp(-lambda * tau)) / (lambda * tau) ] 
#                       + Beta2 * [ (1 - exp(-lambda * tau)) / (lambda * tau) - exp(-lambda * tau) ]

nelson_siegel <- function(params, tau) {
  beta0  <- params[1]
  beta1  <- params[2]
  beta2  <- params[3]
  lambda <- params[4]
  
  # Small epsilon to avoid division by zero
  eps <- 1e-12
  factor1 <- (1 - exp(-lambda * tau)) / (lambda * tau + eps)
  factor2 <- factor1 - exp(-lambda * tau)
  
  beta0 + beta1 * factor1 + beta2 * factor2
}

# Objective function: sum of squared errors
sse_function <- function(params, tau, yields) {
  fitted <- nelson_siegel(params, tau)
  sum((yields - fitted)^2)
}

# Fitting the NS Model for Each Date and computing MSE

# Grouping by Date, then run an optimization per date
ns_results <- df_long %>%
  group_by(Date) %>%
  do({
    # Extract maturity & yield vectors 
    tau_i <- .$Maturity
    y_i   <- .$Yield
    n_i   <- length(y_i)  # number of maturities 
    
    # Initial parameter guess
    #  Beta0 ~ average yield
    #  Beta1 ~ slope estimate (difference between last & first yield)
    #  Beta2 ~ 0
    #  lambda ~ 1
    init_params <- c(mean(y_i), y_i[n_i] - y_i[1], 0, 1)
    
    # Constrain lambda > 0 => use method="L-BFGS-B" with lower bound
    lower_bounds <- c(-Inf, -Inf, -Inf, 1e-6)
    upper_bounds <- c( Inf,  Inf,  Inf,  Inf)
    
    # Run optimization (minimize SSE)
    opt <- optim(
      par    = init_params,
      fn     = function(par) sse_function(par, tau_i, y_i),
      method = "L-BFGS-B",
      lower  = lower_bounds,
      upper  = upper_bounds
    )
    
    # Extract parameter estimates
    beta0_est  <- opt$par[1]
    beta1_est  <- opt$par[2]
    beta2_est  <- opt$par[3]
    lambda_est <- opt$par[4]
    
    # Calculate SSE & MSE
    sse_val <- opt$value
    mse_val <- sse_val / n_i
    
    # Return results
    data.frame(
      Beta0  = beta0_est,
      Beta1  = beta1_est,
      Beta2  = beta2_est,
      Lambda = lambda_est,
      MSE    = mse_val
    )
  }) %>%
  ungroup()

head(ns_results)

# Full display of Parameters results
print(ns_results, n = Inf)


# b) Visualization of yield curve dynamics

# Create a dataframe with the the original data + NS spot rates
df_fitted <- df_long %>%
  left_join(ns_results, by = "Date") %>%
  rowwise() %>%
  # Calculate FittedYield using the nelson_siegel function
  mutate(FittedYield = nelson_siegel(c(Beta0, Beta1, Beta2, Lambda), Maturity)) %>%
  
  # Drop the NS parameter columns
  select(-Beta0, -Beta1, -Beta2, -Lambda, -MSE) %>%
  
  ungroup()

head(df_fitted)

# see how the model fits the observed yields for a specific date
# Example date
chosen_date <- as.Date("2007-01-31")

df_chosen <- df_fitted %>%
  filter(Date == chosen_date) %>%
  arrange(Maturity)

ggplot(df_chosen, aes(x = Maturity)) +
  geom_point(aes(y = Yield), color = "blue", linewidth = 2) +
  geom_line(aes(y = FittedYield), color = "red", linewidth = 1) +
  labs(
    title = paste("Observed vs. Fitted Yield Curve on", chosen_date),
    x = "Maturity (years)",
    y = "Yield (continuously compounded)"
  ) +
  theme_minimal()

# Plot observed versus predictions for every semester
all_dates <- unique(df_fitted$Date)
subset_dates <- all_dates[seq(1, length(all_dates), by = 6)]

df_subset <- df_fitted %>%
  filter(Date %in% subset_dates)

ggplot(df_subset, aes(x = Maturity)) +
  geom_point(aes(y = Yield), color = "blue") +
  geom_line(aes(y = FittedYield), color = "red") +
  facet_wrap(~ Date) +
  labs(
    title = "Observed vs. Fitted Yield Curves (Faceted by Date)",
    x = "Maturity (years)",
    y = "Yield"
  ) +
  theme_bw()


# c) Visualization of yield curve parameters dynamics

# Quick statistical overview (min, max, mean, quartiles) of the parameters and MSE across all dates
summary(ns_results)

# Plot the NS parameters over time
ns_results_long <- ns_results %>%
  pivot_longer(cols = c(Beta0, Beta1, Beta2, Lambda, MSE), names_to = "Parameter", values_to = "Value")

ggplot(ns_results_long, aes(x = Date, y = Value, color = Parameter)) +
  geom_line() +
  facet_wrap(~Parameter, scales = "free_y") +
  theme_minimal() +
  labs(title = "Nelson-Siegel Parameters and MSE Over Time",
       x = "Date",
       y = "Value")

# Plot Distribution of each parameter
ns_results_long %>%
  ggplot(aes(x = Value, fill = Parameter)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Nelson-Siegel Parameters & MSE")

# Correlation between parameters
cor_matrix <- cor(ns_results %>% select(-Date)) # Exclude Date column
cor_matrix

# --------------------------------------------------------------------------------------------
# 3. NSS Hedging
# --------------------------------------------------------------------------------------------

# a) Portfolio Parametric Risk Measures

# Define NSS spot rate estimation function
NSS_spot <- function(theta, tau1=5, tau2=0.5, beta0=0.059, beta1=-0.016, beta2=-0.005, beta3=0.01){
  beta0 + beta1*(1-exp(-theta/tau1))/(theta/tau1) + 
    beta2*((1-exp(-theta/tau1))/(theta/tau1) - exp(-theta/tau1)) + 
    beta3*((1-exp(-theta/tau2))/(theta/tau2) - exp(-theta/tau2))
}

# Define a function to compute the NSS interest rate parametric risk measures
NSS_Sens <- function(beta0=0.059, beta1=-0.016, beta2=-0.005, beta3=0.01, tau1=5, tau2=0.5, 
                     par=100, coupon_rate, maturity){
  
  ct <- par*rep(coupon_rate,maturity)   # coupons
  ct[length(ct)] <- ct[length(ct)]+par  # add principal redemption
  theta_i <- 1:maturity                 # time to cash flow payments (in years)
  
  r0t <- 0
  for(i in 1:maturity){
    r0t[i] <- NSS_spot(theta=i,tau1=tau1,tau2=tau2,beta0=beta0,beta1=beta1,beta2=beta2,beta3=beta3)}
  
  # Compute bond's PV
  B0 <- sum(ct*exp(-theta_i*r0t))   # PV of future cash flows discounted using continuous compounding
  
  # dollar-durations
  beta0_sens <- -sum(ct*theta_i*exp(-theta_i*r0t))
  beta1_sens <- -sum(ct*theta_i*(1-exp(-theta_i/tau1))/(theta_i/tau1)*exp(-theta_i*r0t))
  beta2_sens <- -sum(ct*theta_i*((1-exp(-theta_i/tau1))/(theta_i/tau1) - exp(-theta_i/tau1))*exp(-theta_i*r0t))
  beta3_sens <- -sum(ct*theta_i*((1-exp(-theta_i/tau2))/(theta_i/tau2) - exp(-theta_i/tau2))*exp(-theta_i*r0t))
  
  # parametric durations
  D0 <- -(1/B0)*beta0_sens
  D1 <- -(1/B0)*beta1_sens
  D2 <- -(1/B0)*beta2_sens
  D3 <- -(1/B0)*beta3_sens
  
  return(list(B0=B0, Sens=c(Beta0=beta0_sens, Beta1=beta1_sens, Beta2=beta2_sens, Beta3=beta3_sens),
              Duration=c(Beta0=D0, Beta1=D1, Beta2=D2, Beta3=D3)))
}

# Define the portfolio of fixed-rate treasury bonds ($100 FV, Annual Coupon, Continuous Compounding)
portfolio <- data.frame(id = 1:13,
                        maturity_date = as.Date(c("2025-12-01", "2026-12-04", "2027-12-06", "2028-12-10", 
                                                  "2029-12-03", "2030-12-09", "2032-12-06", "2035-12-03", 
                                                  "2030-12-03", "2045-12-04", "2050-12-04", "2051-12-01", 
                                                  "2052-12-07")),
                        coupon = c(4, 7.75, 4, 7, 5.75, 5.5, 4, 4.75, 4.5, 5, 4.5, 4, 5),
                        quantity = c(10000, 250000, 50000, 100000, 10000, 200000, 15000, 10000, 30000, 75000, 100000, 10000, 10000))

# Set Valuation date
valuation_date <- as.Date("2022-02-09")

# Calculate time to maturity (in years)
portfolio$maturity <- as.numeric(difftime(portfolio$maturity_date, valuation_date, units="days")) / 365.25

# Create matrix to record fair value and risk measures
IRRm_portfolio <- matrix(NA, nrow=13, ncol = 9, 
               dimnames = list(bond=c(1:13), c('B0','S0','S1','S2','S3','D0','D1','D2','D3')))

for (i in 1:13){
  IRRm_portfolio[i,1] <- NSS_Sens(coupon_rate=portfolio[i,3]/100, maturity=portfolio[i,5])$B0
  IRRm_portfolio[i,2:5] <- NSS_Sens(coupon_rate=portfolio[i,3]/100, maturity=portfolio[i,5])$Sens  
  IRRm_portfolio[i,6:9] <- NSS_Sens(coupon_rate=portfolio[i,3]/100, maturity=portfolio[i,5])$Duration
}

# Portfolio bonds parametric Durations and $Durations
IRRm_portfolio

# Aggregate Portfolio parametric Durations and $Durations
portfolio$wi <- portfolio$quantity*IRRm_portfolio[,1] / sum(portfolio$quantity*IRRm_portfolio[,1]) # portfolio weights
portfolio_agg_rm <- colSums(portfolio$wi*IRRm_portfolio[,2:9]) # portfolio parametric risk measures
portfolio_agg_rm


# b) Hedging Assets Parametric Risk Measures

# Define the hedging portfolio
hedging_portfolio <- data.frame(id = 1:5,
                     maturity_date = as.Date(c("2026-04-12", "2032-12-28", "2035-05-06", "2040-10-10", "2051-10-10")),
                     coupon = c(4.5, 5, 6, 6, 6.5))

# Calculate time to maturity (in years)
hedging_portfolio$maturity <- as.numeric(difftime(hedging_portfolio$maturity_date, valuation_date, units="days")) / 365.25

# Create matrix to record fair value and risk measures
IRRm_hedging <- matrix(NA, nrow=5, ncol = 9, 
               dimnames = list(bond=c(1:5), c('B0','S0','S1','S2','S3','D0','D1','D2','D3')))

for (i in 1:5){
  IRRm_hedging[i,1] <- NSS_Sens(coupon_rate=hedging_portfolio[i,3]/100, maturity=hedging_portfolio[i,4])$B0
  IRRm_hedging[i,2:5] <- NSS_Sens(coupon_rate=hedging_portfolio[i,3]/100, maturity=hedging_portfolio[i,4])$Sens  
  IRRm_hedging[i,6:9] <- NSS_Sens(coupon_rate=hedging_portfolio[i,3]/100, maturity=hedging_portfolio[i,4])$Duration
}

# Hedging assets parametric Durations and $Durations
IRRm_hedging


# c) Hedging Portfolio with Self-Financing

# Using portfolio durations
# Target portfolio
nb <- length(hedging_portfolio$id) # number of hedging instruments
B0 <- sum(portfolio$quantity*IRRm_portfolio[,1]) # present value of the portfolio (quantity*price)
H <- tail(portfolio$maturity, 1) # bond with largest time to maturity
# hold to maturity strategy, thus the investment horizon is the time to maturity of the longest bond

# Hedging portfolio 
a <- rbind(t(IRRm_hedging[,6:9]), IRRm_hedging[,1])  # risk measures matrix + B0 of hedging assets
b <- c(-portfolio_agg_rm[5:8], -B0) # imposing self-financing with -B0

# weights (quantity of each hedging asset)
w <- solve(a,b); w

# calculate total cost
Dollar <- sum(w*IRRm_hedging[,1])
Dollar; B0 
round(Dollar + B0, 4) # Self financing was achieved


# d) Impact of a shift in NSS parameters

# Set the new NSS parameters (tau1 and tau2 remain the same)
beta0_new <- 0.065; beta1_new <- -0.01; beta2_new <- 0.001; beta3_new <- 0.02

# i.

# Compute the new portfolio value given the new parameters
# Create matrix to record fair value and risk measures
IRRm_portfolio_new <- matrix(NA, nrow=13, ncol = 9, 
                         dimnames = list(bond=c(1:13), c('B0','S0','S1','S2','S3','D0','D1','D2','D3')))

# Using the new parameters
for (i in 1:13){
  IRRm_portfolio_new[i,1] <- NSS_Sens(coupon_rate=portfolio[i,3]/100, maturity=portfolio[i,5], beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new)$B0
  IRRm_portfolio_new[i,2:5] <- NSS_Sens(coupon_rate=portfolio[i,3]/100, maturity=portfolio[i,5], beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new)$Sens  
  IRRm_portfolio_new[i,6:9] <- NSS_Sens(coupon_rate=portfolio[i,3]/100, maturity=portfolio[i,5], beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new)$Duration
}

# New Aggregate Portfolio parametric Durations and $Durations
portfolio$wi <- portfolio$quantity*IRRm_portfolio_new[,1] / sum(portfolio$quantity*IRRm_portfolio_new[,1]) # portfolio weights
portfolio_agg_rm_new <- colSums(portfolio$wi*IRRm_portfolio_new[,2:9]) # portfolio parametric risk measures
portfolio_agg_rm_new

# Calculate new target portfolio value
B0_portfolio_new <- sum(portfolio$quantity*IRRm_portfolio_new[,1])

# Profit/Loss in the No-hedging scenario
PL <- B0_portfolio_new-B0
PL

# In percentage (%)
round(((PL)/B0)*100, 2)


# ii.

# Create matrix to record new fair value and risk measures
IRRm_hedging_new <- matrix(NA, nrow=5, ncol = 9, 
                           dimnames = list(bond=c(1:5), c('B0','S0','S1','S2','S3','D0','D1','D2','D3')))

for (i in 1:5){
  IRRm_hedging_new[i,1] <- NSS_Sens(beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new, coupon_rate=hedging_portfolio[i,3]/100, maturity=hedging_portfolio[i,4])$B0
  IRRm_hedging_new[i,2:5] <- NSS_Sens(beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new, coupon_rate=hedging_portfolio[i,3]/100, maturity=hedging_portfolio[i,4])$Sens  
  IRRm_hedging_new[i,6:9] <- NSS_Sens(beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new, coupon_rate=hedging_portfolio[i,3]/100, maturity=hedging_portfolio[i,4])$Duration
}

# New hedging assets prices
B0_hedging_new <- IRRm_hedging_new[,1]

# Profit/Loss of the hedged portfolio
PL <- sum((B0_hedging_new-IRRm_hedging[,1])*w) + (B0_portfolio_new-B0)
PL

# In percentage (%)
round(PL/B0*100, 2)

