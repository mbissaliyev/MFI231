# ─────────────────────────────────────────────────────────────────────────────
# LONG STRADDLE BACKTEST WITH CBOE IM, VM RESERVE, FUNDING COST, DISCOUNTING & METRICS
# ─────────────────────────────────────────────────────────────────────────────
library(data.table); library(purrr); library(zoo); library(ggplot2)

# 1. PARAMETERS
start_capital <- 1e6     # $1,000,000
reserve_pct   <- 0.15    # 15% VM reserve
rf_rate       <- 0.03    # 3% annual rf rate
days_year     <- 365

# 2. LOAD & CLEAN
files  <- list.files("SPX2018-2023", "\\.txt$", full.names=TRUE)
dt_all <- map_dfr(files, ~ fread(.x), .id="src")
setnames(dt_all, names(dt_all), gsub("\\[|\\]", "", names(dt_all)) %>% trimws())
dt_all[, `:=`(
  QUOTE_DATE  = as.Date(QUOTE_DATE, "%d.%m.%Y"),
  EXPIRE_DATE = as.Date(EXPIRE_DATE,"%d.%m.%Y"),
  DTE         = as.integer(EXPIRE_DATE - QUOTE_DATE)
)]

# 3. ROLL DATES & ENTRY PREMIUMS
dt_all[, bin2 := ((month(QUOTE_DATE)-1)%/%2)+1]
roll_dates <- dt_all[DTE %between% c(55,65),
                     .(trade_date=max(QUOTE_DATE)),
                     by=.(year=year(QUOTE_DATE), bin2)][order(year,bin2)]
atmPremium <- function(chain, type){
  spot <- chain$UNDERLYING_LAST[1]
  opt  <- chain[, .(STRIKE, bid=get(paste0(type,"_BID")),
                    ask=get(paste0(type,"_ASK")))]
  S    <- opt[which.min(abs(STRIKE-spot)), STRIKE]
  p    <- opt[STRIKE==S, (bid+ask)/2]
  list(strike=S, prem=p)
}
entries <- roll_dates[, {
  ch <- dt_all[QUOTE_DATE==trade_date]
  C  <- atmPremium(ch,"C"); P <- atmPremium(ch,"P")
  .(QUOTE_DATE=trade_date, expiry=ch$EXPIRE_DATE[1],
    strike=C$strike, call_in=C$prem, put_in=P$prem)
}, by=trade_date]

# 4. INITIAL MARGIN per CBOE Rule 10.3 (long <9 mo  full premium)
entries[, initial_margin := 100*(call_in + put_in)]

# 5. EXIT P&L
exit_q <- dt_all[, .(
  call_out=(C_BID+C_ASK)/2,
  put_out =(P_BID+P_ASK)/2
), keyby=.(expiry=EXPIRE_DATE, STRIKE)]
entries[exit_q, on=.(QUOTE_DATE=expiry, strike=STRIKE), `:=`(
  call_out=i.call_out, put_out=i.put_out
)]
entries[, pnl := (call_out-call_in)+(put_out-put_in)]

# 6. DAILY SERIES, VM RESERVE & FUNDING COST
all_days <- data.table(QUOTE_DATE=seq(min(dt_all$QUOTE_DATE),
                                      max(dt_all$QUOTE_DATE), by="day"))
daily <- all_days[entries[, .(QUOTE_DATE, pnl, initial_margin)], on="QUOTE_DATE"][
  , `:=`(
    pnl            = ifelse(is.na(pnl),0,pnl),
    initial_margin = ifelse(is.na(initial_margin),0,initial_margin),
    vm_call        = pmax(-pnl,0)*100,
    reserve        = NA_real_
  )
][order(QUOTE_DATE)]
daily[1, reserve := start_capital*reserve_pct]
for(i in 2:nrow(daily)){
  daily[i, reserve := pmax(daily[i-1,reserve] - vm_call, 0)]
}
daily[, blocked   := initial_margin + vm_call]
daily[, fund_cost := blocked*(rf_rate/days_year)]
daily[, pnl_adj   := pnl - fund_cost]

# 7. DISCOUNT & CORE METRICS
daily[, cum_pnl := cumsum(ifelse(is.na(pnl_adj),0,pnl_adj))]
h_days   <- as.integer(max(daily$QUOTE_DATE) - min(daily$QUOTE_DATE))
disc_fac <- (1+rf_rate)^(h_days/days_year)
pv_final <- tail(daily$cum_pnl,1)/disc_fac

daily[, eff_cap := pmax(start_capital - blocked, .Machine$double.eps)]
daily[, ret_eff := pnl_adj/eff_cap]
rets     <- daily[is.finite(ret_eff), ret_eff]

sharpe  <- sqrt(252)*mean(rets)/sd(rets)
sortino <- sqrt(252)*mean(rets)/sd(rets[rets<0])
cumret  <- cumprod(1+rets)
maxdd   <- max(cummax(cumret)-cumret)

# 8. TAIL-SENSITIVE RATIOS (Omega & Rachev)
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
omega_0  <- Omega(rets, MAR = 0)
rachev_5 <- RachevRatio(rets, p = 0.05)

# 9. REPORT ALL METRICS
cat(sprintf(
  "Straddle Metrics:\n  Discounted P&L: $%.2f\n  Sharpe: %.2f\n  Sortino: %.2f\n  Omega (MAR=0): %.2f\n  Rachev (5%% tails): %.2f\n  Max Drawdown: %.1f%%\n",
  pv_final, sharpe, sortino, omega_0, rachev_5, maxdd*100
))




# ─────────────────────────────────────────────────────────────────────────────
# STRADDLE RETURN DISTRIBUTION & PERFORMANCE DIAGNOSTICS (NO NAs)
# ─────────────────────────────────────────────────────────────────────────────
# 1. Install & load required packages
pkgs <- c("tseries", "moments", "boot", "PerformanceAnalytics")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install)) install.packages(to_install)
library(tseries); library(moments); library(boot); library(PerformanceAnalytics)

# 2. Clean returns (drop NA/Inf)
rets_raw      <- daily$ret_eff
rets          <- rets_raw[is.finite(rets_raw)]

# 3. Jarque–Bera normality test
jb <- jarque.bera.test(rets)

# 4. Skewness & excess kurtosis
skw <- skewness(rets)
ek  <- kurtosis(rets) - 3

# 5. Bootstrap Sharpe CI (1,000 reps)
boot_sharpe <- function(x, i) sqrt(252) * mean(x[i]) / sd(x[i])
set.seed(123)
bs <- boot(data = rets, statistic = boot_sharpe, R = 1000)
ci <- boot.ci(bs, type = "perc")$percent[4:5]

# 6. Omega & Rachev ratios
omega_0  <- Omega(rets, MAR = 0)
rachev_5 <- RachevRatio(rets, p = 0.05)

# 7. Report (corrected sprintf formatting)
cat(sprintf(
  "Straddle Return Diagnostics:\n  Jarque–Bera     = %.2f (df = %d, p = %.3g)\n  Skewness        = %.2f\n  Excess kurtosis = %.2f\n  Sharpe          = %.2f; 95%% CI [%.2f, %.2f]\n  Omega (MAR=0)   = %.2f\n  Rachev (5%%)    = %.2f\n",
  jb$statistic, jb$parameter, jb$p.value,
  skw, ek,
  sqrt(252) * mean(rets) / sd(rets), ci[1], ci[2],
  omega_0, rachev_5
))





# 1. P&L is compute check
daily[, cum_net := cumsum(pnl_adj)]

# 2. Prepare data for plotting
plot_dt <- melt(
  daily,
  id.vars     = "QUOTE_DATE",
  measure.vars= c("cum_net", "reserve"),
  variable.name = "series",
  value.name    = "value"
)[!is.na(value)]

# 3. Plot: Straddle net cumulative P&L vs VM reserve
ggplot(plot_dt, aes(x = QUOTE_DATE, y = value, color = series, linetype = series)) +
  geom_line(size = 0.8) +
  scale_color_manual(
    values = c(cum_net = "steelblue", reserve = "firebrick"),
    labels = c(cum_net = "Net Cum P&L", reserve = "VM Reserve")
  ) +
  scale_linetype_manual(
    values = c(cum_net = "solid", reserve = "dashed")
  ) +
  labs(
    title = "Long SPX 60-DTE ATM Straddle: Net Cum P&L & VM Reserve",
    x     = "Date",
    y     = "USD",
    color = NULL, linetype = NULL
  ) +
  theme_minimal()


# 9. BUILD STRADDLE TRADES TABLE
straddle_trades <- entries[ , .(
  TradeDate       = QUOTE_DATE,
  Expiry          = expiry,
  Strike          = strike,
  CallPremiumIn   = round(call_in,   2),
  PutPremiumIn    = round(put_in,    2),
  CallPremiumOut  = round(call_out,  2),
  PutPremiumOut   = round(put_out,   2),
  PnL             = round(pnl,       2)
)]

# 10. DISPLAY AND SAVE
print(straddle_trades)
write.csv(straddle_trades, "straddle_trades_updated.csv", row.names = FALSE)