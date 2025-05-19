# ─────────────────────────────────────────────────────────────────────────────
# COVERED-CALL BACKTEST WITH IM, VM RESERVE, FUNDING COST, DISCOUNTING & METRICS
# ─────────────────────────────────────────────────────────────────────────────
library(data.table); library(purrr); library(zoo)

# 1. PARAMETERS
start_capital  <- 1e6      # $1,000,000
reserve_pct    <- 0.15     # 15% VM reserve
rf_rate        <- 0.03     # 3% annual risk-free rate
days_year      <- 365
im_prem_factor <- 0.15     # IM factor for short call leg

# 2. LOAD & CLEAN DATA
files  <- list.files("SPX2018-2023", "\\.txt$", full.names=TRUE)
dt_all <- map_dfr(files, ~ fread(.x, sep=",", header=TRUE), .id="src")
setnames(dt_all, names(dt_all),
         gsub("\\[|\\]", "", names(dt_all)) %>% trimws())
dt_all[, `:=`(
  QUOTE_DATE  = as.Date(QUOTE_DATE, "%d.%m.%Y"),
  EXPIRE_DATE = as.Date(EXPIRE_DATE, "%d.%m.%Y"),
  DTE         = as.integer(EXPIRE_DATE - QUOTE_DATE)
)]

# 3. ROLL DATES & ENTRY SELECTION (30-DTE ATM)
dt_all[, ym := format(QUOTE_DATE, "%Y-%m")]
roll_dates <- dt_all[DTE>25 & DTE<35,
                     .(roll_date = max(QUOTE_DATE)), by=ym][order(ym)]

entry <- dt_all[roll_dates, on=.(QUOTE_DATE=roll_date)
][, .SD[which.min(abs(STRIKE - UNDERLYING_LAST))],
  by=QUOTE_DATE][
    , .(trade_date = QUOTE_DATE,
        strike     = STRIKE,
        spot_in    = UNDERLYING_LAST,
        mid_in     = (C_BID + C_ASK)/2,
        expiry     = EXPIRE_DATE
    )
  ]

# 4. EXIT PRICES & TRADE P&L
exit <- dt_all[, .(QUOTE_DATE, STRIKE, UNDERLYING_LAST, C_BID, C_ASK)]
setkey(exit, QUOTE_DATE, STRIKE)
cc <- merge(entry, exit, by.x=c("expiry","strike"),
            by.y=c("QUOTE_DATE","STRIKE"), all.x=TRUE)
cc[, `:=`(
  spot_out = UNDERLYING_LAST,
  mid_out  = (C_BID + C_ASK)/2
)]
cc[, pnl := (spot_out - spot_in) + mid_in - pmax(spot_out - strike, 0)]

# 5. DAILY P&L SERIES
all_days <- data.table(QUOTE_DATE=seq(min(dt_all$QUOTE_DATE),
                                      max(dt_all$QUOTE_DATE), by="day"))
daily <- merge(all_days, cc[, .(QUOTE_DATE=trade_date, pnl)], 
               by="QUOTE_DATE", all.x=TRUE)[
                 , pnl := ifelse(is.na(pnl), 0, pnl)
               ]

# 6. INITIAL MARGIN (IM) FOR SHORT CALL LEG
#    approximate SPAN-style: max(premium + 0.2*spot - OTM, premium*im_prem_factor)*100
daily <- merge(daily, entry[, .(QUOTE_DATE=trade_date, spot_in, mid_in, strike)],
               by="QUOTE_DATE", all.x=TRUE)
daily[, `:=`(
  spot_in    = ifelse(is.na(spot_in), 0, spot_in),
  mid_in     = ifelse(is.na(mid_in), 0, mid_in),
  strike     = ifelse(is.na(strike), 0, strike)
)]
daily[, initial_margin := {
  legM <- function(prem, spot, K) pmax(prem + 0.2*spot - abs(K - spot),
                                       prem * im_prem_factor)
  100 * legM(mid_in, spot_in, strike)
}]

# 7. VARIATION MARGIN CALL & RESERVE
daily[, vm_call := pmax(-pnl, 0) * 100]
daily[, reserve := NA_real_]
daily[1, reserve := start_capital * reserve_pct]
for(i in 2:nrow(daily)) {
  daily[i, reserve := pmax(daily[i-1, reserve] - vm_call, 0)]
}

# 8. FUNDING COST & ADJUSTED P&L
daily[, blocked   := initial_margin + vm_call]
daily[, fund_cost := blocked * (rf_rate / days_year)]
daily[, pnl_adj   := pnl - fund_cost]

# 9. CUMULATIVE P&L, DISCOUNT & EFFECTIVE RETURNS
daily[, cum_pnl_adj := cumsum(ifelse(is.na(pnl_adj), 0, pnl_adj))]
h_days    <- as.integer(max(daily$QUOTE_DATE) - min(daily$QUOTE_DATE))
disc_fac  <- (1 + rf_rate)^(h_days / days_year)
pv_final  <- tail(daily$cum_pnl_adj, 1) / disc_fac

daily[, eff_cap := pmax(start_capital - blocked, .Machine$double.eps)]
daily[, ret_eff := pnl_adj / eff_cap]
rets      <- daily[is.finite(ret_eff), ret_eff]

# 10. PERFORMANCE METRICS
ann_sharpe  <- sqrt(252) * mean(rets) / sd(rets)
down_dev    <- sd(rets[rets < 0])
ann_sortino <- sqrt(252) * mean(rets) / down_dev
cum_ret     <- cumprod(1 + rets)
max_dd      <- max(cummax(cum_ret) - cum_ret)

# 11. REPORT
cat(sprintf(
  "Covered Call → Discounted P&L: $%.2f\nSharpe: %.2f  Sortino: %.2f  MaxDD: %.1f%%\n",
  pv_final, ann_sharpe, ann_sortino, max_dd * 100
))


# 1. Compute cumulative net P&L if not already done
daily[, cum_net := cumsum(ifelse(is.na(pnl_adj), 0, pnl_adj))]

# 2. Melt
plot_dt <- melt(
  daily,
  id.vars     = "QUOTE_DATE",
  measure.vars= c("cum_net", "reserve"),
  variable.name = "series",
  value.name    = "value"
)[!is.na(value)]

# 3. Plot
ggplot(plot_dt, aes(x = QUOTE_DATE, y = value, color = series, linetype = series)) +
  geom_line(size = 0.8) +
  scale_color_manual(
    values = c(cum_net = "darkgreen", reserve = "firebrick"),
    labels = c(cum_net = "Net Cum P&L", reserve = "VM Reserve")
  ) +
  scale_linetype_manual(
    values = c(cum_net = "solid", reserve = "dashed")
  ) +
  labs(
    title = "Covered Call: Net Cumulative P&L & VM Reserve",
    x     = "Date",
    y     = "USD",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal()


# 1. Build the trades table
cc_trades <- cc[, .(
  TradeDate  = trade_date,
  Expiry     = expiry,
  Strike     = strike,
  SpotIn     = spot_in,
  PremiumIn  = mid_in,
  SpotOut    = spot_out,
  PremiumOut = mid_out,
  PnL        = pnl
)]

# 2. Round numeric columns for readability
cc_trades[, `:=`(
  SpotIn     = round(SpotIn, 2),
  PremiumIn  = round(PremiumIn,  2),
  SpotOut    = round(SpotOut,  2),
  PremiumOut = round(PremiumOut, 2),
  PnL        = round(PnL,       2)
)]

# 3. Display and/or save
print(cc_trades)
write.csv(cc_trades, "covered_call_trades_updated.csv", row.names = FALSE)




# 1. packages
pkgs <- c("PerformanceAnalytics","tseries","boot","moments")
inst <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(inst)) install.packages(inst)

# 2. Load libraries
library(PerformanceAnalytics)
library(tseries)
library(boot)
library(moments)

# 3. Extract effective returns
rets <- daily$ret_eff

# 4. Jarque–Bera normality test
jb <- jarque.bera.test(rets)
print(jb)

# 5. Bootstrap Sharpe function
boot_sharpe <- function(x, i) {
  r <- x[i]
  sqrt(252) * mean(r) / sd(r)
}

# 6. Run bootstrap (1 000 replications)
set.seed(42)
res <- boot(data=rets, statistic=boot_sharpe, R=1000)

# 7. Confidence interval for Sharpe
ci <- boot.ci(res, type="perc")
print(ci)

# 1. PerformanceAnalytics
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# 2. Compute Omega Ratio (MAR = 0)
omega_0 <- Omega(daily$ret_eff, MAR = 0)

# 3. Compute Rachev Ratio (5% tails)
#    RachevRatio defaults to 5% for both tails
rachev_5 <- RachevRatio(daily$ret_eff, p = 0.05)

# 4. Print
cat(sprintf("Omega (MAR=0): %.2f\nRachev (5%% tails): %.2f\n", omega_0, rachev_5))


# 8. Skewness & kurtosis for quick diagnostics
cat("Skewness:", skewness(rets), "\n")
cat("Excess kurtosis:", kurtosis(rets) - 3, "\n")


# 1. Histogram of effective daily returns
hist(rets,
     breaks = 50,
     main   = "Histogram of Effective Daily Returns",
     xlab   = "Daily Return",
     ylab   = "Frequency")

# 2. QQ-plot against a normal
qqnorm(rets,
       main = "QQ-Plot of Effective Daily Returns")
qqline(rets, col = "red", lwd = 2)



