# ============================================================
# TCS (TATA CONSULTANCY SERVICES) STOCK ANALYSIS PROJECT
# PART 1: Setup, Data Download & Feature Engineering
# ============================================================
# Stock : TCS (Tata Consultancy Services Ltd.)
# Ticker: TCS.NS (NSE)
# Period: 01-Jan-2006 to 30-Apr-2026
# ============================================================

# --- 1.1 Install & Load Required Packages -------------------

packages <- c(
  "quantmod",               # Stock data download
  "tidyverse",              # Data wrangling + ggplot2
  "forecast",               # ARIMA & forecasting
  "tseries",                # ADF test, time-series tools
  "TTR",                    # Technical indicators (SMA, RSI, MACD)
  "PerformanceAnalytics",   # Financial performance metrics
  "lubridate",              # Date manipulation
  "scales",                 # Axis formatting
  "gridExtra",              # Arrange multiple plots
  "ggthemes",               # Extra ggplot2 themes
  "plotly",                 # Interactive plots
  "zoo",                    # Time-series zoo objects
  "xts"                     # Extensible time-series
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}
sapply(packages, install_if_missing)

cat("\n✅ All packages loaded successfully!\n\n")

# --- 1.2 Download TCS Stock Data from Yahoo Finance ---------

cat("📥 Downloading TCS stock data from Yahoo Finance...\n")
cat("   Ticker : TCS.NS\n")
cat("   From   : 2006-01-01\n")
cat("   To     : 2026-04-30\n\n")

getSymbols("TCS.NS",
           src  = "yahoo",
           from = "2006-01-01",
           to   = "2026-04-30",
           auto.assign = TRUE)

# Store raw xts object
tcs_xts <- `TCS.NS`

cat("✅ Data downloaded successfully!\n")
cat("   Total trading days:", nrow(tcs_xts), "\n")
cat("   Date range:", as.character(index(tcs_xts)[1]),
    "to", as.character(tail(index(tcs_xts), 1)), "\n\n")

# --- 1.3 Convert to Data Frame for ggplot2 ------------------

tcs_df <- data.frame(
  Date     = index(tcs_xts),
  Open     = as.numeric(Op(tcs_xts)),
  High     = as.numeric(Hi(tcs_xts)),
  Low      = as.numeric(Lo(tcs_xts)),
  Close    = as.numeric(Cl(tcs_xts)),
  Volume   = as.numeric(Vo(tcs_xts)),
  Adjusted = as.numeric(Ad(tcs_xts))
)

# Remove rows with NA values
tcs_df <- na.omit(tcs_df)
cat("   Observations after cleaning:", nrow(tcs_df), "\n\n")

# --- 1.4 Feature Engineering ---------------------------------

tcs_df <- tcs_df %>%
  mutate(
    # ---- Date Components ----
    Year       = year(Date),
    Month      = month(Date, label = TRUE),
    MonthNum   = month(Date),
    Quarter    = quarter(Date),
    DayOfWeek  = wday(Date, label = TRUE),
    WeekNum    = isoweek(Date),
    
    # ---- Returns ----
    Daily_Return = (Close - lag(Close)) / lag(Close) * 100,
    Log_Return   = log(Close / lag(Close)) * 100,
    
    # ---- Price Metrics ----
    Price_Range    = High - Low,
    Typical_Price  = (High + Low + Close) / 3,
    VWAP_Approx    = (High + Low + Close) / 3,   # simplified VWAP
    
    # ---- Moving Averages ----
    SMA_20  = SMA(Close, n = 20),
    SMA_50  = SMA(Close, n = 50),
    SMA_100 = SMA(Close, n = 100),
    SMA_200 = SMA(Close, n = 200),
    EMA_12  = EMA(Close, n = 12),
    EMA_26  = EMA(Close, n = 26),
    
    # ---- Technical Indicators ----
    RSI_14       = RSI(Close, n = 14),
    Volatility_20 = runSD(Daily_Return, n = 20)
  )

# ---- MACD ----
macd_vals <- MACD(tcs_df$Close, nFast = 12, nSlow = 26, nSig = 9)
tcs_df$MACD           <- macd_vals[, 1]
tcs_df$MACD_Signal    <- macd_vals[, 2]
tcs_df$MACD_Histogram <- macd_vals[, 1] - macd_vals[, 2]

# ---- Bollinger Bands ----
bb <- BBands(tcs_df$Close, n = 20, sd = 2)
tcs_df$BB_Upper  <- bb[, "up"]
tcs_df$BB_Lower  <- bb[, "dn"]
tcs_df$BB_Middle <- bb[, "mavg"]

# ---- Cumulative Return ----
tcs_df$Cum_Return <- cumprod(1 + coalesce(tcs_df$Daily_Return / 100, 0)) - 1

# --- 1.5 Summary Statistics ---------------------------------

cat("=" , rep("=", 50), "\n")
cat("  📊 TCS STOCK - SUMMARY STATISTICS\n")
cat("=" , rep("=", 50), "\n\n")

cat("Total observations :", nrow(tcs_df), "\n")
cat("Date range         :", as.character(min(tcs_df$Date)),
    "to", as.character(max(tcs_df$Date)), "\n")
cat("Min Close Price    : ₹", round(min(tcs_df$Close, na.rm = TRUE), 2), "\n")
cat("Max Close Price    : ₹", round(max(tcs_df$Close, na.rm = TRUE), 2), "\n")
cat("Mean Close Price   : ₹", round(mean(tcs_df$Close, na.rm = TRUE), 2), "\n")
cat("Median Close       : ₹", round(median(tcs_df$Close, na.rm = TRUE), 2), "\n")
cat("Std Deviation      : ₹", round(sd(tcs_df$Close, na.rm = TRUE), 2), "\n")
cat("Mean Daily Return  :", round(mean(tcs_df$Daily_Return, na.rm = TRUE), 4), "%\n")
cat("Total Cum. Return  :", round(tail(tcs_df$Cum_Return, 1) * 100, 2), "%\n\n")

cat("--- Detailed Summary ---\n")
print(summary(tcs_df[, c("Open", "High", "Low", "Close", "Volume", "Adjusted")]))

cat("\n✅ Part 1 complete! Data is stored in 'tcs_df' and 'tcs_xts'.\n")
cat("   Proceed to Part 2 for Visualizations.\n")
