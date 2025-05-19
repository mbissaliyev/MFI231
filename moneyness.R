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


# 1. How many observations by expiration?
dt_all[, .N, by=EXPIRE_DATE][order(EXPIRE_DATE)]

# 2. Distribution of implied volatilities
dt_all[, summary(P_IV)]

# 3. ATM vs OTM volumes
dt_all[, abs(STRIKE_DISTANCE_PCT) < 0.01, by=.(STRIKE_DISTANCE_PCT)][
  , .(count=.N), by=V1]

# 4. Time-series of average IV
# install if you haven’t yet
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

df_iv <- dt_all[, .(mean_iv = mean(P_IV, na.rm = TRUE)), by = QUOTE_DATE]

ggplot(df_iv, aes(x = QUOTE_DATE, y = mean_iv)) +
  geom_line() +
  labs(
    title = "Daily Avg. Put Implied Volatility, SPX 2018",
    x     = "Date",
    y     = "Implied Volatility"
  ) +
  theme_minimal()


# ─────────────────────────────────────────────────────────────────────────────
# 5. Moneyness distribution
# ─────────────────────────────────────────────────────────────────────────────
# create a moneyness column
dt_all[, moneyness := STRIKE / UNDERLYING_LAST]


# pick a representative year (2020 here – adjust as you like)
df_mon <- dt_all[QUOTE_DATE >= as.Date("2018-01-01") & QUOTE_DATE < as.Date("2019-01-01")]
# histogram of moneyness 2019
ggplot(df_mon, aes(x = moneyness)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(
    title = "Moneyness Distribution for SPX Options (2018)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal()


# pick a representative year (2020)
df_mon <- dt_all[QUOTE_DATE >= as.Date("2019-01-01") & QUOTE_DATE < as.Date("2020-01-01")]
# histogram of moneyness 2019
ggplot(df_mon, aes(x = moneyness)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(
    title = "Moneyness Distribution for SPX Options (2019)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal()


df_mon <- dt_all[QUOTE_DATE >= as.Date("2020-01-01") & QUOTE_DATE < as.Date("2021-01-01")]
# histogram of moneyness 2020
ggplot(df_mon, aes(x = moneyness)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(
    title = "Moneyness Distribution for SPX Options (2020)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal()


df_mon <- dt_all[QUOTE_DATE >= as.Date("2021-01-01") & QUOTE_DATE < as.Date("2022-01-01")]
# histogram of moneyness 2021
ggplot(df_mon, aes(x = moneyness)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(
    title = "Moneyness Distribution for SPX Options (2021)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal()

df_mon <- dt_all[QUOTE_DATE >= as.Date("2022-01-01") & QUOTE_DATE < as.Date("2023-01-01")]
# histogram of moneyness 2022
ggplot(df_mon, aes(x = moneyness)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(
    title = "Moneyness Distribution for SPX Options (2022)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal()

df_mon <- dt_all[QUOTE_DATE >= as.Date("2023-01-01") & QUOTE_DATE < as.Date("2024-01-01")]
# histogram of moneyness 2023
ggplot(df_mon, aes(x = moneyness)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(
    title = "Moneyness Distribution for SPX Options (2023)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal()


# 1) add a “year” column
dt_all[, year := format(QUOTE_DATE, "%Y")]

# 2) filter to the range you care about
dt_plot <- dt_all[year %in% as.character(2018:2023)]

# 3) plot with facets
ggplot(dt_plot, aes(x = moneyness)) +
  geom_histogram(bins = 100, fill = "gray40", color = "white", alpha = 0.8) +
  facet_wrap(~ year, ncol = 2, scales = "free_y") +
  labs(
    title = "Moneyness Distribution for SPX Options (2018–2023)",
    x     = "Strike / Spot",
    y     = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )