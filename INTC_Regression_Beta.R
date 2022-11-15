
# Setting up the environment
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(quantmod)

# Loading the data
INTC    <- readRDS("INTC_5yr_monthly_raw")
SP500   <- readRDS("SP500_5yr_monthly_raw")
NASDAQ  <- readRDS("NASDAQ_5yr_monthly_raw")
INTCd   <- readRDS("INTC_2yr_daily_raw")
SP500d  <- readRDS("SP500_2yr_daily_raw")
NASDAQd <- readRDS("NASDAQ_2yr_daily_raw")

# Data cleaning

# Removing unnecessary columns
INTC_Adj    <-  INTC[,6]
SP500_Adj   <- SP500[,6]
NASDAQ_Adj  <- NASDAQ[,6]
INTCd_Adj   <-  INTCd[,6]
SP500d_Adj  <- SP500d[,6]
NASDAQd_Adj <- NASDAQd[,6]


# Converting prices to returns
INTC_Ret <- na.omit(diff(INTC_Adj)/lag(coredata(INTC_Adj)))
colnames(INTC_Ret)[colnames(INTC_Ret) == "INTC.Adjusted"] <- "INTC_Returns"

SP500_Ret <- na.omit(diff(SP500_Adj)/lag(coredata(SP500_Adj)))
colnames(SP500_Ret)[colnames(SP500_Ret) == "GSPC.Adjusted"] <- "GSPC_Returns"

NASDAQ_Ret <- na.omit(diff(NASDAQ_Adj)/lag(coredata(NASDAQ_Adj)))
colnames(NASDAQ_Ret)[colnames(NASDAQ_Ret) == "IXIC.Adjusted"] <- "IXIC_Returns"

INTCd_Ret <- na.omit(diff(INTCd_Adj)/lag(coredata(INTCd_Adj)))
colnames(INTCd_Ret)[colnames(INTCd_Ret) == "INTC.Adjusted"] <- "INTC_Returns"

SP500d_Ret <- na.omit(diff(SP500d_Adj)/lag(coredata(SP500d_Adj)))
colnames(SP500d_Ret)[colnames(SP500d_Ret) == "GSPC.Adjusted"] <- "GSPC_Returns"

NASDAQd_Ret <- na.omit(diff(NASDAQd_Adj)/lag(coredata(NASDAQd_Adj)))
colnames(NASDAQd_Ret)[colnames(NASDAQd_Ret) == "IXIC.Adjusted"] <- "IXIC_Returns"


# Combining dataframes so they can be processed by the regression function
INTC_SP500    <- merge(INTC_Ret, SP500_Ret)
INTC_NASDAQ   <- merge(INTC_Ret, NASDAQ_Ret)
INTC_SP500d   <- merge(INTCd_Ret, SP500d_Ret)
INTC_NASDAQd  <- merge(INTCd_Ret, NASDAQd_Ret)


# Running (and plotting) the Linear Regression Models

# Finding the regression beta against the S&P 500 monthly returns
lmINTC <- lm(INTC_Ret~SP500_Ret, data = INTC_SP500)
lmINTCplot <- ggplot(data = INTC_SP500, aes(x = SP500_Ret, y = INTC_Ret)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  lims(x = c(-.25, .25), y = c(-.25, .25)) +
  labs(title="Regression Beta: Intel vs. S&P 500", subtitle = "Monthly Data: Dec 2016 - Dec 2021", x = "S&P 500 Returns", y = "Intel Returns")+
  stat_regline_equation(label.y = .18, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .12, aes(label = ..rr.label..))


# Finding the regression beta against the NASDAQ 5yr monthly returns

lmINTC2 <- lm(INTC_Ret~NASDAQ_Ret, data = INTC_NASDAQ)
lmINTC2plot <- ggplot(data = INTC_NASDAQ, aes(x = NASDAQ_Ret, y = INTC_Ret)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  lims(x = c(-.25, .25), y = c(-.25, .25)) +
  labs(title="Regression Beta: Intel vs. NASDAQ Composite", subtitle = "Monthly Data: Dec 2016 - Dec 2021", x = "NASDAQ Returns", y = "Intel Returns")+
  stat_regline_equation(label.y = .18, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .12, aes(label = ..rr.label..))


# Finding the regression beta against the S&P500 2yr daily returns

lmINTCd <- lm(INTCd_Ret~SP500d_Ret, data = INTC_SP500d)
lmINTCdplot <- ggplot(data = INTC_SP500d, aes(x = SP500d_Ret, y = INTCd_Ret)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  lims(x = c(-.25, .25), y = c(-.25, .25)) +
  labs(title="Regression Beta: Intel vs. S&P 500", subtitle = "Daily Prices: Dec 2019 - Dec 2021", x = "S&P 500 Returns", y = "Intel Returns")+
  stat_regline_equation(label.y = .18, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .12, aes(label = ..rr.label..))


# Finding the regression beta against the NASDAQ 2yr daily returns

lmINTCd2 <- lm(INTCd_Ret~NASDAQd_Ret, data = INTC_NASDAQd)
lmINTCd2plot <- ggplot(data = INTC_NASDAQd, aes(x = NASDAQd_Ret, y = INTCd_Ret)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  lims(x = c(-.25, .25), y = c(-.25, .25)) +
  labs(title="Regression Beta: Intel vs. NASDAQ Comp", subtitle = "Daily Prices: Dec 2019 - Dec 2021", x = "NASDAQ Returns", y = "Intel Returns")+
  stat_regline_equation(label.y = .18, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .12, aes(label = ..rr.label..))

# And finally, we plot the results
grid.arrange(lmINTCplot, lmINTC2plot, lmINTCdplot, lmINTCd2plot, ncol = 2)
