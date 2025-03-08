# ---------------------------------------------------------------------------------------
# Fixed Income Securities Individual Project | DS4F 2025 | Francisco Perestrello 20241560
# ---------------------------------------------------------------------------------------

rm(list = ls(all.names =TRUE))
graphics.off()
close.screen(all.screens = TRUE)
erase.screen()
windows.options(record = TRUE)
options(digits = 10)

#setwd('C:/Users/Francisco/Desktop/Pós-Graduação/T2/Fixed Income Securities/Project')

library(pacman)
p_load(ggplot2)
p_load(tidyverse)  # data manipulation and visualization
p_load(gridExtra)  # plot arrangement
p_load(zoo)
p_load(openxlsx) # xlsx handling

# -----------------------------------------------------------------------------
# Data upload
# -----------------------------------------------------------------------------

# Load the yield curve data
yield_curve <- read.xlsx('Yield_data.xlsx', sheet="PCA", colNames=TRUE, rowNames=TRUE, detectDates=TRUE)

# Take a quick look at the data
head(yield_curve)
dim(yield_curve)

# a)
cor(yield_curve)
# correlation is generally high, which suggests that interest rate changes are influenced by
# common factors, but it is clearly imperfect. Further, we observe that correlations are highest
# for nearby maturities and lowest for the most distant maturities -> this calls the need for PCA


# b)
# Perform PCA Analysis using the prcomp function from the stats R package
n <- dim(yield_curve)[1]
pca <- prcomp(yield_curve[2:n,] - yield_curve[1:(n-1),], scale=TRUE, center=TRUE)
pca

# $center - Means of the spot rates
pca$center

# $scale - Standard deviations of the spot rates
pca$scale

# $sdev - From standard deviations, we can extract the variances of the Principal Components. These are identical to the Eigenvalues
pca$sdev
pca.var <- pca$sdev^2
pca.var

# $rotation - The rotation matrix provides the Principal Component loadings (also known as the eigenvectors)
pca$rotation <- -pca$rotation # inverting the signal so the eigenvectors point in the positive direction
pca$rotation

# $x - Principal Components scores
pca$x <- - pca$x
head(pca$x)

# CHECK: The variances of cov(PC) should be equal to the Eigenvalues and the covariances
# should be 0 (aside from rounding errors) since the Principal Components have to be uncorrelated.
cov(pca$x)
round(diag(cov(pca$x)), 3) == round(pca.var, 3)

# Plot Proportion of Variance Explained by the Principal Components (Simple and Cumulative)

# Proportion of Variance Explained (PVE)
PVE <- pca.var / sum(pca.var)
names(PVE) <- paste("PC", 1:5, sep="")
round(PVE, 3)

# PVE plot
PVEplot <- qplot(c(1:5), PVE) + 
  geom_line(color="blue", size=1) + 
  geom_point(color="blue", size=2) +
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("PVE Plot") +
  ylim(0, 1) +
  scale_x_discrete(name ="Principal Component")

# Cumulative PVE plot
cumPVE <- qplot(c(1:5), cumsum(PVE)) + 
  geom_line(color="blue", size=1) + 
  geom_point(color="blue", size=2) +
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative PVE Plot") +
  ylim(0, 1) +
  scale_x_discrete(name ="Principal Component")

grid.arrange(PVEplot, cumPVE, ncol = 2)


# c)
# Plot factor loadings 
ggplot(data.frame(cbind(term=c(1,2,3,4,5), pca$rotation[,1:5]))) +
  geom_line(aes(x=term,y=PC1,colour="PC1"), size=1) + geom_point(aes(x=term,y=PC1)) +
  geom_line(aes(x=term,y=PC2,colour="PC2"), size=1) + geom_point(aes(x=term,y=PC2)) +
  geom_line(aes(x=term,y=PC3,colour="PC3"), size=1) + geom_point(aes(x=term,y=PC3)) +
  geom_line(aes(x=term,y=PC4,colour="PC4"), size=1) + geom_point(aes(x=term,y=PC4)) +
  geom_line(aes(x=term,y=PC5,colour="PC5"), size=1) + geom_point(aes(x=term,y=PC5)) +
  xlab("Term") +
  ylab("Rate (%)") +
  ggtitle("PCA Factor Loadings") + 
  scale_color_manual(values = c("PC1"="red", "PC2"="green", "PC3"="blue", "PC4"='yellow', 'PC5'='pink')) +
  theme(legend.title = element_blank())

# The first factor is the level factor, since yield changes caused by a shock from this factor 
# are roughly constant across maturities - Parallel Shocks
# The second factor is the steepness factor, as interest rates' sensitivities towards the second 
# factor have an ascending shape. Thus, a positive shock in factor 2 induces a steepening of the 
# curve around maturity 3 - Slope Shocks
# The third factor is the curvature factor, as it shows different impacts on intermediate rates
# as opposed to extreme maturities (both short and long) - Curvature Shocks


# d)
# Plot the factor scores for the three dominant factors over time
ggplot(data.frame(Time = as.Date(rownames(pca$x)), PC1 = pca$x[,1], PC2 = pca$x[,2], PC3 = pca$x[,3]), aes(x=Time)) +
  geom_line(aes(x=Time, y=PC1, color="PC1")) +
  geom_line(aes(x=Time, y=PC2, color="PC2")) +
  geom_line(aes(x=Time, y=PC3, color="PC3")) +
  labs(title = "Dominant Principal Component Scores Over Time", x = "Date", y = "Scores") +
  scale_color_manual(values = c("PC1" = "red", "PC2" = "darkgreen", "PC3" = "blue")) +
  theme(legend.title = element_blank())

# they resemble returns


# e) PCA hedging

# Assumed annual coupons, face value 100, continuous compounding
hedging_bonds <- data.frame(id = 2:4,
                            maturity = c(2, 3, 4),
                            coupon = c(5, 4.5, 4))

# Define NSS spot rate estimation function
NSS_spot <- function(theta, tau1=5, tau2=0.5, beta0=0.059, beta1=-0.016, beta2=-0.005, beta3=0.01){
  beta0 + beta1*(1-exp(-theta/tau1))/(theta/tau1) + 
    beta2*((1-exp(-theta/tau1))/(theta/tau1) - exp(-theta/tau1)) + 
    beta3*((1-exp(-theta/tau2))/(theta/tau2) - exp(-theta/tau2))
}

# Compute the NSS spot rates for the hedging assets' maturities
yields <- NSS_spot(hedging_bonds$maturity)

# Function to compute duration under continuous compounding
compute_duration <- function(maturity, coupon, yield, face_value=100) {
  
  # Generate cash flow times
  time_points <- seq(1, maturity, by = 1)
  
  # Compute cash flows (coupon payments + face value at maturity)
  cash_flows <- rep(coupon, maturity)
  cash_flows[maturity] <- cash_flows[maturity] + face_value  # Add face value at maturity
  
  # Discounted cash flows using continuous compounding
  discount_factors <- exp(-yield * time_points)
  discounted_cash_flows <- cash_flows * discount_factors
  
  # Compute Macaulay Duration
  duration <- sum(time_points * discounted_cash_flows) / sum(discounted_cash_flows)
  
  return(duration)
}

# Compute Hedging Assets' Durations
hedging_bonds$duration <- mapply(compute_duration, 
                                 maturity = hedging_bonds$maturity, 
                                 coupon = hedging_bonds$coupon, 
                                 yield = yields)

factor_loadings <- pca$rotation

# Let's assume a portfolio with H=3, Coupon=5%, face_value=1000
H <- 3; portfolio_coupon <- 0.05; portfolio_face_value <- 1000

portfolio_duration <- compute_duration(H, portfolio_coupon, NSS_spot(H), portfolio_face_value)


# PC1 Hedge 
# [e12*Dh2]*[x1] = [-e13*Dp]
a <- factor_loadings[2,1]*hedging_bonds$duration[1]
b <- -factor_loadings[3,1]*portfolio_duration

w1 <- solve(a,b); round(w1, 4)

# Net exposure to PC1 of hedged portfolio
net_exposure <- a %*% w1 - b
round(net_exposure, 4)


# PC1+PC2 Hedge 
# [e12*Dh2  e13*Dh3]*[x1] = [-e13*Dp]
# [e22*Dh2  e23*Dh3]*[x2] = [-e23*Dp]
a <- t(rbind(
  factor_loadings[2,1:2]*hedging_bonds$duration[1], 
  factor_loadings[3,1:2]*hedging_bonds$duration[2]))

b <- c(-factor_loadings[3,1]*portfolio_duration, 
       -factor_loadings[3,2]*portfolio_duration)

w2 <- solve(a,b); round(w2, 4)

# Net exposure to PC1 and PC2 of hedged portfolio
net_exposure <- a %*% w2 - b
round(net_exposure, 4)


# PC1+PC2+PC3 Hedge 
# [e12*Dh2  e13*Dh3  e14*Dh4]*[x1] = [-e13*Dp]
# [e22*Dh2  e23*Dh3  e24*Dh4]*[x2] = [-e23*Dp]
# [e32*Dh2  e33*Dh3  e34*Dh4]*[x3] = [-e33*Dp]
a <- t(rbind(
  factor_loadings[2,1:3]*hedging_bonds$duration[1], 
  factor_loadings[3,1:3]*hedging_bonds$duration[2],
  factor_loadings[4,1:3]*hedging_bonds$duration[3]))

b <- c(-factor_loadings[3,1]*portfolio_duration, 
       -factor_loadings[3,2]*portfolio_duration,
       -factor_loadings[3,3]*portfolio_duration)

w3 <- solve(a,b); round(w3, 4)

# Net exposure to PC1, PC2, and PC3 of hedged portfolio
net_exposure <- a %*% w3 - b
round(net_exposure, 4)


# Assess effects of a shift in the yield curve

# Define function to compute price
compute_price <- function(maturity, coupon, yield, face_value=100) {
  
  # Generate cash flow times
  time_points <- seq(1, maturity, by = 1)
  
  # Compute cash flows (coupon payments + face value at maturity)
  cash_flows <- rep(coupon, maturity)
  cash_flows[maturity] <- cash_flows[maturity] + face_value  # Add face value at maturity
  
  # Discounted cash flows using continuous compounding
  discount_factors <- exp(-yield * time_points)
  discounted_cash_flows <- cash_flows * discount_factors
  
  B0 <- sum(discounted_cash_flows)
  
  return(B0)
}

# Original Prices
hedging_bonds$B0 <- mapply(compute_price, 
                           maturity = hedging_bonds$maturity, 
                           coupon = hedging_bonds$coupon, 
                           yield = yields)

portfolio_B0 <- compute_price(H, portfolio_coupon, NSS_spot(H), portfolio_face_value)

# Set the new NSS parameters (tau1 and tau2 remain the same)
beta0_new <- 0.065; beta1_new <- -0.01; beta2_new <- 0.001; beta3_new <- 0.02

# Compute the new NSS spot rates for the hedging assets' maturities
yields_new <- NSS_spot(hedging_bonds$maturity, beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new)

# New Prices
hedging_bonds$B0_new <- mapply(compute_price, 
                           maturity = hedging_bonds$maturity, 
                           coupon = hedging_bonds$coupon, 
                           yield = yields_new)

portfolio_B0_new <- compute_price(H, portfolio_coupon, NSS_spot(H, beta0=beta0_new, beta1=beta1_new, beta2=beta2_new, beta3=beta3_new), portfolio_face_value)

# Profit/Loss of the PC1 hedged portfolio
w1[2] <- 0; w1[3] <- 0
w1 <- w1*10 # to match the $ amount in the target portfolio
PL <- sum((hedging_bonds$B0_new-hedging_bonds$B0)*w1) + (portfolio_B0_new-portfolio_B0)
PL

# In percentage (%)
round(PL/portfolio_B0*100, 2)

# Profit/Loss of the PC1+PC2 hedged portfolio
w2[3] <- 0
w2 <- w2*10 # to match the $ amount in the target portfolio
PL <- sum((hedging_bonds$B0_new-hedging_bonds$B0)*w2) + (portfolio_B0_new-portfolio_B0)
PL

# In percentage (%)
round(PL/portfolio_B0*100, 2)

# Profit/Loss of the PC1+PC2+PC3 hedged portfolio
w3 <- w3*10 # to match the $ amount in the target portfolio
PL <- sum((hedging_bonds$B0_new-hedging_bonds$B0)*w3) + (portfolio_B0_new-portfolio_B0)
PL

# In percentage (%)
round(PL/portfolio_B0*100, 2)

