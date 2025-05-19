library(data.table)
library(purrr)

data_dir <- "SPX2018-2023"
files <- list.files(data_dir, pattern = "\\.txt$", full.names = TRUE)

# read + rbind all into one big data.table
dt_all <- map_dfr(files, ~ fread(.x, sep=",", header=TRUE), .id="source_file")


setnames(dt_all, 
         old = names(dt_all),
         new = names(dt_all) %>%
           gsub("\\[|\\]", "", .) %>%    # remove square brackets
           trimws()                       # trim whitespace
)


# Convert the DATE columns using as.Date + the proper format
dt_all[ , QUOTE_DATE  := as.Date(QUOTE_DATE,  "%d.%m.%Y")]
dt_all[ , EXPIRE_DATE := as.Date(EXPIRE_DATE, "%d.%m.%Y")]

dt_all[ , QUOTE_DATETIME := as.POSIXct(
  paste(QUOTE_DATE, sprintf("%02.0f:00:00", QUOTE_TIME_HOURS)),
  format = "%Y-%m-%d %H:%M:%S",
  tz     = "UTC"    # or your local TZ
)]

# Drop the old hour‐column if you like:
dt_all[ , QUOTE_TIME_HOURS := NULL]
dt_all[, .N, by=EXPIRE_DATE][order(EXPIRE_DATE)]


#headers.
head(dt_all)


# 2. Read VIX CSV (must contain Date & Close columns)
df_iv <- dt_all[, .(mean_iv = mean(P_IV, na.rm = TRUE)), by = QUOTE_DATE]
df_iv <- dt_all[, .(
  avg_call_iv = mean(C_IV, na.rm = TRUE),
  avg_put_iv  = mean(P_IV, na.rm = TRUE)
), by = QUOTE_DATE]

ggplot(df_iv, aes(x = QUOTE_DATE, y = mean_iv)) +
  geom_line() +
  labs(
    title = "Daily Avg. Put Implied Volatility, SPX 2018",
    x     = "Date",
    y     = "Implied Volatility"
  ) +
  theme_minimal()
vix <- fread("VIX.csv")
vix[, Date := as.Date(Date, format = "%m/%d/%Y")]
setnames(vix, "Close", "VIX")

# 3. Merge VIX into your IV data
df_iv2 <- merge(
  df_iv, 
  vix[, .(Date, VIX)], 
  by.x = "QUOTE_DATE", 
  by.y = "Date", 
  all.x = TRUE
)

# 4. Define volatility regimes
df_iv2[, regime := fifelse(
  VIX > 25, "High-Vol",
  fifelse(VIX < 15, "Low-Vol", "Mid-Vol")
)]

# 5. Build contiguous regime periods
regimes <- df_iv2[,
                  .(start = min(QUOTE_DATE), end = max(QUOTE_DATE)),
                  by = .(grp = rleid(regime), regime)
][, grp := NULL]

# 6. Plot average call-IV with regime shading
ggplot(df_iv2, aes(x = QUOTE_DATE, y = avg_call_iv)) +
  geom_rect(
    data = regimes,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = regime),
    inherit.aes = FALSE, alpha = 0.1
  ) +
  geom_line() +
  scale_fill_manual(
    values = c("Low-Vol"="navy", "Mid-Vol"="gray70", "High-Vol"="firebrick")
  ) +
  labs(
    title = "Avg Call IV with VIX Regime Shading",
    x     = "Date",
    y     = "Avg Call Implied Volatility",
    fill  = "VIX Regime"
  ) +
  theme_minimal()

