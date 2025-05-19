# ─────────────────────────────────────────────────────────────────────────────
# COLLAR STRATEGY BACKTEST WITH IM, VM RESERVE, FUNDING COST, DISCOUNTING & METRICS
# ─────────────────────────────────────────────────────────────────────────────
library(data.table); library(purrr); library(zoo); library(ggplot2)
# Install/load required packages
for(pkg in c("PerformanceAnalytics","tseries","moments","boot")){
  if(!require(pkg, character.only=TRUE)) install.packages(pkg)
  library(pkg, character.only=TRUE)
}

# 1. PARAMETERS & DATA LOAD
start_capital <- 1e6      # $1,000,000
reserve_pct   <- 0.15     # 15% VM reserve
rf_rate       <- 0.03     # 3% annual rf rate
days_year     <- 365

files <- list.files("SPX2018-2023","\\.txt$",full.names=TRUE)
dt_all <- map_dfr(files, ~ fread(.x), .id="src")
setnames(dt_all,names(dt_all),gsub("\\[|\\]","",names(dt_all)) %>% trimws())
dt_all[,`:=`(
  QUOTE_DATE  = as.Date(QUOTE_DATE, "%d.%m.%Y"),
  EXPIRE_DATE = as.Date(EXPIRE_DATE, "%d.%m.%Y"),
  DTE         = as.integer(EXPIRE_DATE - QUOTE_DATE)
)]

# 2. ROLL DATES (~90 DTE quarterly), label quarter without collision
dt_all[, qlabel := sprintf("%d-Q%d",
                           year(QUOTE_DATE), ((month(QUOTE_DATE)-1)%/%3)+1)]
roll_dates <- dt_all[DTE %between% c(85,95),
                     .(trade_date = max(QUOTE_DATE)),
                     by=qlabel][order(qlabel)]

# 3. ENTRY: 5% OTM Put & Call premiums & strikes
approxStrike <- function(chain, type, pct){
  spot <- chain$UNDERLYING_LAST[1]
  target <- spot * pct
  opt <- chain[, .(STRIKE, bid=get(paste0(type,"_BID")), ask=get(paste0(type,"_ASK")))]
  row <- opt[which.min(abs(STRIKE-target))]
  (row$bid + row$ask) / 2
}
entries <- roll_dates[, {
  chain <- dt_all[QUOTE_DATE==trade_date]
  spot <- chain$UNDERLYING_LAST[1]
  put_K  <- round(spot*0.95)
  call_K <- round(spot*1.05)
  .(QUOTE_DATE    = trade_date,
    expiry        = chain$EXPIRE_DATE[1],
    spot_in       = spot,
    put_strike    = put_K,
    call_strike   = call_K,
    put_prem_in   = approxStrike(chain,"P",0.95),
    call_prem_in  = approxStrike(chain,"C",1.05)
  )
}, by=trade_date]

# 4. INITIAL MARGIN (IM)
entries[, initial_margin := 100*(put_prem_in + call_prem_in)]

# 5. EXIT QUOTES & TRADE P&L
exit_q <- dt_all[, .(
  expiry   = EXPIRE_DATE,
  STRIKE,
  put_mid  = (P_BID + P_ASK)/2,
  call_mid = (C_BID + C_ASK)/2,
  spot_out = UNDERLYING_LAST
)]
setkey(exit_q, expiry, STRIKE)
entries[exit_q, on=.(QUOTE_DATE=expiry, put_strike=STRIKE),
        `:=`(spot_out=i.spot_out, put_mid_out=i.put_mid)]
entries[exit_q, on=.(QUOTE_DATE=expiry, call_strike=STRIKE),
        call_mid_out := i.call_mid]
entries[, pnl := (
  (spot_out - spot_in) +               # underlying return
    (call_prem_in - put_prem_in) +       # net credit/debit
    pmax(put_strike - spot_out, 0) -     # put payoff
    pmax(spot_out - call_strike, 0)      # call payoff
) * 100]

# 6. DAILY SERIES & MARGIN ACCOUNTS
all_days <- data.table(QUOTE_DATE=seq(min(dt_all$QUOTE_DATE),
                                      max(dt_all$QUOTE_DATE), by="day"))
daily <- merge(all_days, entries[, .(QUOTE_DATE, pnl, initial_margin)],
               by="QUOTE_DATE", all.x=TRUE)[
                 , `:=`(
                   pnl            = ifelse(is.na(pnl), 0, pnl),
                   initial_margin = ifelse(is.na(initial_margin), 0, initial_margin),
                   vm_call        = pmax(-pnl, 0) * 100,
                   reserve        = NA_real_
                 )
               ][order(QUOTE_DATE)]
daily[1, reserve := start_capital * reserve_pct]
for(i in 2:nrow(daily)){
  daily[i, reserve := pmax(daily[i-1, reserve] - vm_call, 0)]
}
daily[, blocked   := initial_margin + vm_call]
daily[, fund_cost := blocked * (rf_rate / days_year)]
daily[, pnl_adj   := pnl - fund_cost]

# 7. HANDLE NAs & DISCOUNTED P&L
daily[is.na(pnl_adj), pnl_adj := 0]
daily[, cum_pnl := cumsum(pnl_adj)]
h_days   <- as.integer(max(daily$QUOTE_DATE) - min(daily$QUOTE_DATE))
pv_final <- tail(daily$cum_pnl,1) / (1 + rf_rate)^(h_days / days_year)

# 8. CORE METRICS
daily[, eff_cap := pmax(start_capital - blocked, .Machine$double.eps)]
daily[, ret_eff := pnl_adj / eff_cap]
rets     <- daily[is.finite(ret_eff), ret_eff]
sharpe   <- sqrt(252) * mean(rets) / sd(rets)
sortino  <- sqrt(252) * mean(rets) / sd(rets[rets < 0])
cumret   <- cumprod(1 + rets); maxdd <- max(cummax(cumret) - cumret)

# 9. TAIL DIAGNOSTICS
jb       <- jarque.bera.test(rets)
skw      <- skewness(rets)
ek       <- kurtosis(rets) - 3
bs       <- boot(rets, function(x,i) sqrt(252)*mean(x[i])/sd(x[i]), R=1000)
ci       <- boot.ci(bs, type="perc")$percent[4:5]
omega_0  <- Omega(rets, MAR=0)
rachev_5 <- RachevRatio(rets, p=0.05)

# 10. REPORT
cat(sprintf(
  "Collar Metrics:\n  Discounted P&L: $%.2f\n  Sharpe: %.2f (95%% CI [%.2f,%.2f])\n  Sortino: %.2f\n  Omega (0%%): %.2f\n  Rachev (5%%): %.2f\n  Max Drawdown: %.1f%%\n  JB: %.2f (df=%d, p=%.3g)\n  Skewness: %.2f\n  Excess kurtosis: %.2f\n",
  pv_final, sharpe, ci[1], ci[2], sortino, omega_0, rachev_5,
  maxdd * 100, jb$statistic, jb$parameter, jb$p.value, skw, ek
))

# 11. PLOT
daily[, cum_net := cumsum(pnl_adj)]
plot_dt <- melt(daily, id="QUOTE_DATE", measure.vars=c("cum_net","reserve"),
                variable.name="series", value.name="value")[!is.na(value)]
ggplot(plot_dt, aes(QUOTE_DATE, value, color=series, linetype=series)) +
  geom_line(size=0.8) +
  scale_color_manual(values=c(cum_net="darkgreen",reserve="firebrick")) +
  scale_linetype_manual(values=c(cum_net="solid",reserve="dashed")) +
  labs(
    title = "Quarterly 5% OTM SPX Collar: Net Cum P&L & VM Reserve",
    x = "Date", y = "USD"
  ) +
  theme_minimal()

# 12. TRADE LOG
collar_trades <- entries[, .(
  TradeDate     = QUOTE_DATE,
  Expiry        = expiry,
  SpotIn        = spot_in,
  SpotOut       = spot_out,
  PutStrike     = put_strike,
  CallStrike    = call_strike,
  PutPremiumIn  = round(put_prem_in,  2),
  CallPremiumIn = round(call_prem_in, 2),
  PutPremiumOut = round(put_mid_out,  2),
  CallPremiumOut= round(call_mid_out, 2),
  PnL           = round(pnl,          2)
)]
print(collar_trades)
write.csv(collar_trades, "collar_trades_updated.csv", row.names=FALSE)





# 1) Gross cumulative P&L (no funding cost)
daily[, cum_gross := cumsum(pnl)]

# 2) Net cumulative P&L (with funding cost)
daily[, cum_net   := cumsum(pnl_adj)]

# 3) Prepare for plotting
cmp <- melt(
  daily[, .(QUOTE_DATE, cum_gross, cum_net, reserve)],
  id.vars      = "QUOTE_DATE",
  measure.vars = c("cum_gross","cum_net","reserve"),
  variable.name= "series",
  value.name   = "value"
)[!is.na(value)]

# 4) Plot side by side
ggplot(cmp, aes(QUOTE_DATE, value, color=series, linetype=series)) +
  geom_line(size=0.8) +
  scale_color_manual(
    values = c(cum_gross="blue", cum_net="darkgreen", reserve="firebrick"),
    labels = c("Gross Cum P&L","Net Cum P&L","VM Reserve")
  ) +
  scale_linetype_manual(values = c("solid","solid","dashed")) +
  labs(
    title = "Collar: Gross vs Net P&L and VM Reserve",
    x     = "Date", y = "USD",
    color = NULL, linetype = NULL
  ) +
  theme_minimal()


