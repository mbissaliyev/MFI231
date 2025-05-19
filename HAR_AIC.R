library(data.table)
library(purrr)
library(dplyr)
library(zoo)
library(rugarch)
library(rmgarch)
library(forecast)
library(parallel)

# ─── User controls ───────────────────────────────────────────────────────────
init_n      <- 500   # in‐sample days
W           <-   5   # realized window for realized vol
refit_every <-   5   # DCC refit frequency
# ─────────────────────────────────────────────────────────────────────────────

# 1) Load & clean data
data_dir <- "SPX2018-2023"
files    <- list.files(data_dir, "\\.txt$", full.names = TRUE)
dt_all   <- map_dfr(files, ~ fread(.x, sep = ",", header = TRUE), .id = "src")
setnames(dt_all, names(dt_all),
         names(dt_all) %>% gsub("\\[|\\]", "", .) %>% trimws())
dt_all[, QUOTE_DATE := as.Date(QUOTE_DATE, "%d.%m.%Y")]
dt_all[, QUOTE_TIME_HOURS := NULL]

# 2) Build daily returns & ΔIV
daily <- dt_all %>%
  group_by(QUOTE_DATE) %>%
  summarize(
    underlying = UNDERLYING_LAST[1],
    iv_mean    = mean(c(C_IV, P_IV), na.rm = TRUE)
  ) %>%
  arrange(QUOTE_DATE) %>%
  mutate(
    ret = c(NA, diff(log(underlying))),
    dIV = c(NA, diff(iv_mean))
  ) %>%
  filter(!is.na(ret), !is.na(dIV))

r_zoo <- zoo(daily[, c("ret", "dIV")], order.by = daily$QUOTE_DATE)

# 3) Define multiple EGARCH‐DCC specifications
uspec_eg_std <- ugarchspec(
  variance.model     = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder  = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)
uspec_eg_norm <- ugarchspec(
  variance.model     = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder  = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

dcc_variants <- list(
  DCC11_mvt  = dccspec(uspec = multispec(list(uspec_eg_std, uspec_eg_std)),
                       dccOrder = c(1, 1), distribution = "mvt"),
  DCC11_norm = dccspec(uspec = multispec(list(uspec_eg_norm, uspec_eg_norm)),
                       dccOrder = c(1, 1), distribution = "mvnorm")
)

# Fit each DCC and collect AIC/BIC/HQ
dcc_ic <- map_dfr(names(dcc_variants), function(name) {
  spec <- dcc_variants[[name]]
  fit  <- dccfit(spec, data = r_zoo[1:init_n, ], fit.control = list(eval.se = FALSE))
  ic   <- infocriteria(fit)[c("Akaike", "Bayes", "Hannan-Quinn"), 1]
  tibble(
    Model = name,
    AIC   = ic["Akaike"],
    BIC   = ic["Bayes"],
    HQC   = ic["Hannan-Quinn"]
  )
})
print(dcc_ic)

# Choose best DCC spec
best_dcc_name <- dcc_ic$Model[which.min(dcc_ic$AIC)]
best_dcc_spec <- dcc_variants[[best_dcc_name]]

# 4) Fit best‐spec DCC roll‐forward
dcc_init <- dccfit(best_dcc_spec, data = r_zoo[1:init_n, ], fit.control = list(eval.se = FALSE))

cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, { library(rugarch); library(rmgarch) })
clusterExport(cl, c("best_dcc_spec", "r_zoo", "dcc_init", "init_n", "refit_every"))

n_fc     <- nrow(r_zoo) - init_n
dcc_roll <- dccroll(
  spec            = best_dcc_spec,
  data            = r_zoo,
  n.ahead         = 1,
  forecast.length = n_fc,
  refit.every     = refit_every,
  refit.window    = "moving",
  window.size     = init_n,
  mfit            = dcc_init@mfit,
  cluster         = cl
)
stopCluster(cl)

# 5) Extract 1‐step return vol forecasts from DCC
dates_oos <- index(r_zoo)[(init_n + 1):nrow(r_zoo)]
rcovar    <- rcov(dcc_roll)
f1        <- zoo(sqrt(rcovar[1, 1, ]), dates_oos)

# 6) Compute realized volatility series (rolling sum of squared returns)
rv1 <- rollapply(r_zoo[,1]^2, W,
                 function(x) sqrt(sum(x)), fill = NA, align = "right")
y1  <- rv1[dates_oos]

# 7) Fit multiple HAR(p,q,r) models & collect AIC
rv_vec <- coredata(rv1)

fit_har <- function(p, q, r) {
  max_lag <- max(c(p, q, r))
  idx     <- (max_lag + 1):init_n
  df      <- data.frame(
    RV    = rv_vec[idx],
    lag1  = rv_vec[idx - p],
    lag5  = zoo::rollapply(rv_vec, q, mean,    fill = NA, align = "right")[idx],
    lag22 = zoo::rollapply(rv_vec, r, mean,    fill = NA, align = "right")[idx]
  )
  mod     <- lm(RV ~ lag1 + lag5 + lag22, data = df)
  tibble(
    Model = sprintf("HAR(%d,%d,%d)", p, q, r),
    AIC   = AIC(mod),
    Mod   = list(mod)
  )
}

har_specs <- list(c(1,5,22), c(1,10,22), c(1,5,30))
har_ic    <- map_dfr(har_specs, ~ fit_har(.x[1], .x[2], .x[3]))
print(har_ic)

# Choose best HAR by lowest AIC
best_har_row <- har_ic[which.min(har_ic$AIC), ]
best_har_mod <- best_har_row$Mod[[1]]

# 8) Generate out‐of‐sample HAR forecasts
max_lag  <- max(har_specs[[which.min(har_ic$AIC)]])
oos_idx  <- (init_n + 1):(nrow(r_zoo))
lag1_o   <- rv_vec[oos_idx - 1]
lag5_o   <- zoo::rollapply(rv_vec, har_specs[[which.min(har_ic$AIC)]][2], mean,
                           fill = NA, align = "right")[oos_idx]
lag22_o  <- zoo::rollapply(rv_vec, har_specs[[which.min(har_ic$AIC)]][3], mean,
                           fill = NA, align = "right")[oos_idx]
new_har  <- data.frame(lag1 = lag1_o, lag5 = lag5_o, lag22 = lag22_o)
har_f1   <- zoo(predict(best_har_mod, newdata = new_har), dates_oos)

# 9) Optimize α for blending DCC & HAR
b1   <- rollapply(r_zoo[,1]^2, W,
                  function(x) sqrt(mean(x)), fill = NA, align = "right")[dates_oos]
opt  <- optimize(
  function(a) mean((a * coredata(f1) + (1 - a) * coredata(har_f1) - coredata(y1))^2, na.rm = TRUE),
  interval = c(0, 1)
)
alpha   <- opt$minimum
blend_f1 <- alpha * coredata(f1) + (1 - alpha) * coredata(har_f1)
blend_z  <- zoo(blend_f1, dates_oos)

# 10) Diebold‐Mariano test
loss_blend  <- (blend_f1 - coredata(y1))^2
loss_bench  <- (coredata(b1) - coredata(y1))^2
ok_blend    <- complete.cases(loss_blend, loss_bench)
if (sum(ok_blend) > 1) {
  dm_res <- dm.test(loss_blend[ok_blend], loss_bench[ok_blend], h = 1, power = 2)
  print(dm_res)
  cat("Optimal alpha:", round(alpha, 3), "\n")
}

# 11) Plot comparison
df_long <- rbind(
  data.frame(date = dates_oos, series = "DCC",     vol = coredata(f1)),
  data.frame(date = dates_oos, series = "HAR",     vol = coredata(har_f1)),
  data.frame(date = dates_oos, series = "Blend",   vol = blend_f1),
  data.frame(date = dates_oos, series = "Realized",vol = coredata(y1))
)

ggplot(df_long, aes(x = date, y = vol, color = series)) +
  geom_line(size = 0.8) +
  labs(
    title = sprintf("DCC vs HAR vs Blend vs Realized (α = %.2f)", alpha),
    x     = "Date", y = "Volatility", color = ""
  ) +
  theme_minimal()


#Ljung–Box
resid_init <- residuals(dcc_init)          # T×2 matrix of raw residuals
sigma_init <- sigma(dcc_init)              # T×2 matrix of conditional σ_t
std_init   <- resid_init / sigma_init      # standardized residuals
dates_init <- index(r_zoo)[1:init_n]       # in‐sample dates
z1_init    <- zoo(std_init[,1], dates_init)

# Out‐of‐Sample Forecast Errors
err_dcc <- coredata(y1) - coredata(f1)     # one‐step DCC errors
dates_oos <- index(r_zoo)[(init_n+1):nrow(r_zoo)]
e1_dcc  <- zoo(err_dcc, dates_oos)

# Define lags to test
lags <- c(5, 10, 20)

# Ljung–Box on in‐sample residuals and their squares
lb_z1     <- sapply(lags, function(L)
  Box.test(coredata(z1_init),    lag = L, type = "Ljung")$p.value)
lb_z1_sq  <- sapply(lags, function(L)
  Box.test(coredata(z1_init)^2,  lag = L, type = "Ljung")$p.value)

# Ljung–Box on OOS forecast errors and their squares
lb_e1    <- sapply(lags, function(L)
  Box.test(coredata(e1_dcc),     lag = L, type = "Ljung")$p.value)
lb_e1_sq <- sapply(lags, function(L)
  Box.test(coredata(e1_dcc)^2,   lag = L, type = "Ljung")$p.value)

# Print results
cat("Ljung–Box p-values for z_t (lags 5,10,20):",    round(lb_z1,3),   "\n")
cat("Ljung–Box p-values for z_t^2 (lags 5,10,20):",  round(lb_z1_sq,3),"\n")
cat("Ljung–Box p-values for e_t (lags 5,10,20):",    round(lb_e1,3),   "\n")
cat("Ljung–Box p-values for e_t^2 (lags 5,10,20):",  round(lb_e1_sq,3),"\n")
