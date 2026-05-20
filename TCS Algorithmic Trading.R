# ============================================================
# PART 4: ALGORITHMIC TRADING – TCS (TATA CONSULTANCY SERVICES)
# ============================================================
# Stock : TCS (Tata Consultancy Services Ltd.)
# Ticker: TCS.NS (NSE)
# NOTE : Run part1_setup_download.R first to load 'tcs_df'
# ============================================================

cat("\n")
cat("============================================================\n")
cat("  PART 4: ALGORITHMIC TRADING – TCS\n")
cat("============================================================\n\n")

# ============================================================
# 4.1  USE TCS DATA FROM PART 1 (already loaded as tcs_df)
# ============================================================

cat("✅ Using TCS data from Part 1 (tcs_df)\n")
cat("   Observations:", nrow(tcs_df), "\n")
cat("   Range:", as.character(min(tcs_df$Date)),
    "to", as.character(max(tcs_df$Date)), "\n\n")

# ============================================================
# 4.2  STRATEGY 1: SMA CROSSOVER (Golden Cross / Death Cross)
# ============================================================

cat("--- Strategy 1: SMA Crossover (50/200) ---\n\n")

tcs_sma <- tcs_df %>%
  filter(!is.na(SMA_200)) %>%
  mutate(
    Signal_SMA = ifelse(SMA_50 > SMA_200, 1, -1),
    Prev_Signal = lag(Signal_SMA),
    Trade = case_when(
      Signal_SMA == 1 & Prev_Signal == -1 ~ "BUY (Golden Cross)",
      Signal_SMA == -1 & Prev_Signal == 1 ~ "SELL (Death Cross)",
      TRUE ~ "HOLD"
    ),
    Strategy_Return = Signal_SMA * (Close - lag(Close)) / lag(Close) * 100
  )

trades_sma <- tcs_sma %>% filter(Trade != "HOLD")
cat("Trade Signals (SMA 50/200 Crossover):\n")
print(trades_sma %>% select(Date, Close, SMA_50, SMA_200, Trade))
cat("\n")

p_sma_strat <- ggplot(tcs_sma, aes(x = Date)) +
  geom_line(aes(y = Close), color = "#1E88E5", linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = SMA_50), color = "#FF9800", linewidth = 0.7) +
  geom_line(aes(y = SMA_200), color = "#F44336", linewidth = 0.7) +
  geom_point(data = tcs_sma %>% filter(Trade == "BUY (Golden Cross)"),
             aes(y = Close), color = "#00C853", shape = 24, size = 4, fill = "#00C853") +
  geom_point(data = tcs_sma %>% filter(Trade == "SELL (Death Cross)"),
             aes(y = Close), color = "#FF1744", shape = 25, size = 4, fill = "#FF1744") +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(title = "TCS – SMA Crossover Strategy",
       subtitle = "SMA-50 (Orange) vs SMA-200 (Red) | ▲ Buy | ▼ Sell",
       x = "Date", y = "Price (₹)",
       caption = "Golden Cross = BUY | Death Cross = SELL") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"))
print(p_sma_strat)
cat("✅ SMA Crossover Strategy Chart - Done\n\n")

# ============================================================
# 4.3  STRATEGY 2: RSI-BASED TRADING
# ============================================================

cat("--- Strategy 2: RSI Trading (Overbought/Oversold) ---\n\n")

tcs_rsi <- tcs_df %>%
  filter(!is.na(RSI_14)) %>%
  mutate(
    Signal_RSI = case_when(RSI_14 < 30 ~ 1, RSI_14 > 70 ~ -1, TRUE ~ 0),
    Trade_RSI = case_when(
      Signal_RSI == 1  & lag(Signal_RSI) != 1  ~ "BUY (Oversold)",
      Signal_RSI == -1 & lag(Signal_RSI) != -1 ~ "SELL (Overbought)",
      TRUE ~ "HOLD"
    )
  )

trades_rsi <- tcs_rsi %>% filter(Trade_RSI != "HOLD")
cat("RSI Trade Signals:\n")
if(nrow(trades_rsi) > 0) {
  print(trades_rsi %>% select(Date, Close, RSI_14, Trade_RSI) %>% head(20))
} else { cat("  No RSI trade signals detected.\n") }
cat("\n")

p_rsi_strat <- ggplot(tcs_rsi, aes(x = Date)) +
  geom_line(aes(y = Close), color = "#1E88E5", linewidth = 0.5) +
  geom_point(data = tcs_rsi %>% filter(Trade_RSI == "BUY (Oversold)"),
             aes(y = Close), color = "#00C853", shape = 24, size = 3, fill = "#00C853") +
  geom_point(data = tcs_rsi %>% filter(Trade_RSI == "SELL (Overbought)"),
             aes(y = Close), color = "#FF1744", shape = 25, size = 3, fill = "#FF1744") +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(title = "TCS – RSI Trading Strategy",
       subtitle = "Buy when RSI < 30 (Oversold) | Sell when RSI > 70 (Overbought)",
       x = "Date", y = "Price (₹)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"))
print(p_rsi_strat)
cat("✅ RSI Strategy Chart - Done\n\n")

# ============================================================
# 4.4  STRATEGY 3: MACD CROSSOVER
# ============================================================

cat("--- Strategy 3: MACD Crossover ---\n\n")

tcs_macd <- tcs_df %>%
  filter(!is.na(MACD_Signal)) %>%
  mutate(
    Signal_MACD = ifelse(MACD > MACD_Signal, 1, -1),
    Prev_MACD   = lag(Signal_MACD),
    Trade_MACD = case_when(
      Signal_MACD == 1  & Prev_MACD == -1 ~ "BUY",
      Signal_MACD == -1 & Prev_MACD == 1  ~ "SELL",
      TRUE ~ "HOLD"
    )
  )

trades_macd <- tcs_macd %>% filter(Trade_MACD != "HOLD")
cat("MACD Trade Signals:\n")
print(trades_macd %>% select(Date, Close, MACD, MACD_Signal, Trade_MACD) %>% head(20))
cat("\n")

p_macd_strat <- ggplot(tcs_macd, aes(x = Date)) +
  geom_line(aes(y = Close), color = "#1E88E5", linewidth = 0.5) +
  geom_point(data = tcs_macd %>% filter(Trade_MACD == "BUY"),
             aes(y = Close), color = "#00C853", shape = 24, size = 3, fill = "#00C853") +
  geom_point(data = tcs_macd %>% filter(Trade_MACD == "SELL"),
             aes(y = Close), color = "#FF1744", shape = 25, size = 3, fill = "#FF1744") +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(title = "TCS – MACD Crossover Strategy",
       subtitle = "▲ BUY when MACD crosses above Signal | ▼ SELL when below",
       x = "Date", y = "Price (₹)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"))
print(p_macd_strat)
cat("✅ MACD Strategy Chart - Done\n\n")

# ============================================================
# 4.5  STRATEGY 4: BOLLINGER BAND BREAKOUT
# ============================================================

cat("--- Strategy 4: Bollinger Band Breakout ---\n\n")

tcs_bb <- tcs_df %>%
  filter(!is.na(BB_Upper)) %>%
  mutate(
    Signal_BB = case_when(Close <= BB_Lower ~ 1, Close >= BB_Upper ~ -1, TRUE ~ 0),
    Trade_BB = case_when(
      Signal_BB == 1  & lag(Signal_BB) != 1  ~ "BUY (Lower Band)",
      Signal_BB == -1 & lag(Signal_BB) != -1 ~ "SELL (Upper Band)",
      TRUE ~ "HOLD"
    )
  )

trades_bb <- tcs_bb %>% filter(Trade_BB != "HOLD")
cat("Bollinger Band Trade Signals:\n")
if(nrow(trades_bb) > 0) {
  print(trades_bb %>% select(Date, Close, BB_Lower, BB_Upper, Trade_BB) %>% head(20))
} else { cat("  No Bollinger Band signals detected.\n") }
cat("\n")

p_bb_strat <- ggplot(tcs_bb, aes(x = Date)) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "#E3F2FD", alpha = 0.5) +
  geom_line(aes(y = Close), color = "#1E88E5", linewidth = 0.5) +
  geom_line(aes(y = BB_Upper), color = "#EF5350", linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = BB_Lower), color = "#66BB6A", linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = BB_Middle), color = "#FFA726", linewidth = 0.4, linetype = "dotted") +
  geom_point(data = tcs_bb %>% filter(Trade_BB == "BUY (Lower Band)"),
             aes(y = Close), color = "#00C853", shape = 24, size = 3, fill = "#00C853") +
  geom_point(data = tcs_bb %>% filter(Trade_BB == "SELL (Upper Band)"),
             aes(y = Close), color = "#FF1744", shape = 25, size = 3, fill = "#FF1744") +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(title = "TCS – Bollinger Band Strategy",
       subtitle = "Buy at Lower Band | Sell at Upper Band",
       x = "Date", y = "Price (₹)", caption = "Bands: 20-day SMA ± 2 Std Dev") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"))
print(p_bb_strat)
cat("✅ Bollinger Band Strategy Chart - Done\n\n")

# ============================================================
# 4.6  STRATEGY 5: COMBINED MULTI-INDICATOR STRATEGY
# ============================================================

cat("--- Strategy 5: Combined Multi-Indicator Strategy ---\n\n")

tcs_combined <- tcs_df %>%
  filter(!is.na(SMA_200) & !is.na(MACD_Signal) & !is.na(BB_Upper)) %>%
  mutate(
    Score_SMA  = ifelse(SMA_50 > SMA_200, 1, -1),
    Score_RSI  = case_when(RSI_14 < 30 ~ 1, RSI_14 > 70 ~ -1, TRUE ~ 0),
    Score_MACD = ifelse(MACD > MACD_Signal, 1, -1),
    Score_BB   = case_when(Close <= BB_Lower ~ 1, Close >= BB_Upper ~ -1, TRUE ~ 0),
    Total_Score = Score_SMA + Score_RSI + Score_MACD + Score_BB,
    Combined_Signal = case_when(
      Total_Score >= 3  ~ "STRONG BUY",
      Total_Score == 2  ~ "BUY",
      Total_Score <= -3 ~ "STRONG SELL",
      Total_Score == -2 ~ "SELL",
      TRUE              ~ "HOLD"
    )
  )

cat("Signal Distribution:\n")
print(table(tcs_combined$Combined_Signal))
cat("\n")

p_combined <- ggplot(tcs_combined, aes(x = Date)) +
  geom_line(aes(y = Close), color = "#455A64", linewidth = 0.5) +
  geom_point(data = tcs_combined %>% filter(Combined_Signal %in% c("STRONG BUY", "BUY")),
             aes(y = Close, color = Combined_Signal), shape = 24, size = 2.5, fill = "#00C853") +
  geom_point(data = tcs_combined %>% filter(Combined_Signal %in% c("STRONG SELL", "SELL")),
             aes(y = Close, color = Combined_Signal), shape = 25, size = 2.5, fill = "#FF1744") +
  scale_color_manual(values = c("STRONG BUY" = "#00C853", "BUY" = "#69F0AE",
                                "STRONG SELL" = "#FF1744", "SELL" = "#FF8A80")) +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(title = "TCS – Multi-Indicator Combined Strategy",
       subtitle = "Score = SMA + RSI + MACD + Bollinger (range: -4 to +4)",
       x = "Date", y = "Price (₹)", color = "Signal") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"),
        legend.position = "bottom")
print(p_combined)
cat("✅ Combined Strategy Chart - Done\n\n")

# ============================================================
# 4.7  BACKTESTING & PERFORMANCE METRICS
# ============================================================

cat("--- 4.7  Backtesting Performance ---\n\n")

tcs_backtest <- tcs_sma %>%
  filter(!is.na(Strategy_Return)) %>%
  mutate(
    Cum_Strategy = cumprod(1 + coalesce(Strategy_Return / 100, 0)),
    Cum_BuyHold  = cumprod(1 + coalesce(Daily_Return / 100, 0))
  )

p_backtest <- ggplot(tcs_backtest, aes(x = Date)) +
  geom_line(aes(y = Cum_BuyHold, color = "Buy & Hold"), linewidth = 0.7) +
  geom_line(aes(y = Cum_Strategy, color = "SMA Crossover Strategy"), linewidth = 0.7) +
  scale_color_manual(values = c("Buy & Hold" = "#1E88E5",
                                "SMA Crossover Strategy" = "#FF6F00")) +
  labs(title = "TCS – Strategy Backtest: SMA Crossover vs Buy & Hold",
       subtitle = "Cumulative returns comparison",
       x = "Date", y = "Cumulative Return (₹1 base)", color = "Strategy") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"),
        legend.position = "bottom")
print(p_backtest)

total_days <- nrow(tcs_backtest)
strategy_total_return <- (tail(tcs_backtest$Cum_Strategy, 1) - 1) * 100
buyhold_total_return  <- (tail(tcs_backtest$Cum_BuyHold, 1) - 1) * 100

strategy_daily_ret <- tcs_backtest$Strategy_Return / 100
buyhold_daily_ret  <- tcs_backtest$Daily_Return / 100
sharpe_strategy <- sqrt(252) * mean(strategy_daily_ret, na.rm = TRUE) / sd(strategy_daily_ret, na.rm = TRUE)
sharpe_buyhold  <- sqrt(252) * mean(buyhold_daily_ret, na.rm = TRUE) / sd(buyhold_daily_ret, na.rm = TRUE)

cum_max_strat <- cummax(tcs_backtest$Cum_Strategy)
drawdown_strat <- (tcs_backtest$Cum_Strategy - cum_max_strat) / cum_max_strat
max_dd_strat <- min(drawdown_strat, na.rm = TRUE) * 100

cum_max_bh <- cummax(tcs_backtest$Cum_BuyHold)
drawdown_bh <- (tcs_backtest$Cum_BuyHold - cum_max_bh) / cum_max_bh
max_dd_bh <- min(drawdown_bh, na.rm = TRUE) * 100

wins <- sum(tcs_backtest$Strategy_Return > 0, na.rm = TRUE)
losses <- sum(tcs_backtest$Strategy_Return < 0, na.rm = TRUE)
win_rate <- wins / (wins + losses) * 100

cat("\n📊 PERFORMANCE METRICS\n")
cat("=", rep("=", 45), "\n")
cat("Total Trading Days        :", total_days, "\n")
cat("Strategy Total Return     :", round(strategy_total_return, 2), "%\n")
cat("Buy & Hold Total Return   :", round(buyhold_total_return, 2), "%\n")
cat("Strategy Outperformance   :", round(strategy_total_return - buyhold_total_return, 2), "%\n")
cat("Sharpe Ratio (Strategy)   :", round(sharpe_strategy, 4), "\n")
cat("Sharpe Ratio (Buy & Hold) :", round(sharpe_buyhold, 4), "\n")
cat("Max Drawdown (Strategy)   :", round(max_dd_strat, 2), "%\n")
cat("Max Drawdown (Buy & Hold) :", round(max_dd_bh, 2), "%\n")
cat("Win Rate (Strategy)       :", round(win_rate, 2), "%\n")
cat("Number of Trades (SMA)    :", nrow(trades_sma), "\n")
cat("=", rep("=", 45), "\n\n")

# ============================================================
# 4.8  DRAWDOWN VISUALIZATION
# ============================================================

tcs_backtest$Drawdown <- drawdown_strat * 100

p_drawdown <- ggplot(tcs_backtest, aes(x = Date, y = Drawdown)) +
  geom_area(fill = "#FFCDD2", alpha = 0.6) +
  geom_line(color = "#D32F2F", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(title = "TCS – Strategy Drawdown",
       subtitle = "Maximum peak-to-trough decline in cumulative returns",
       x = "Date", y = "Drawdown (%)",
       caption = paste0("Max Drawdown: ", round(max_dd_strat, 2), "%")) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#B71C1C"),
        plot.subtitle = element_text(hjust = 0.5, color = "#455A64"))
print(p_drawdown)
cat("✅ Drawdown Chart - Done\n\n")

# ============================================================
# 4.9  LIVE TRADING MONITOR (SIMULATION FRAMEWORK)
# ============================================================

cat("--- 4.9  Live Trading Monitor (Framework) ---\n\n")

latest <- tail(tcs_df, 1)

cat("╔════════════════════════════════════════════════════════╗\n")
cat("║           TCS – LIVE TRADING DASHBOARD                ║\n")
cat("╠════════════════════════════════════════════════════════╣\n")
cat("║  Date       :", format(latest$Date, "%d-%b-%Y"), "                    ║\n")
cat("║  Close      : ₹", sprintf("%-10s", round(latest$Close, 2)), "                   ║\n")
cat("║  SMA-20     : ₹", sprintf("%-10s", round(latest$SMA_20, 2)), "                   ║\n")
cat("║  SMA-50     : ₹", sprintf("%-10s", round(latest$SMA_50, 2)), "                   ║\n")
cat("║  RSI (14)   :", sprintf("%-10s", round(latest$RSI_14, 2)), "                    ║\n")
cat("║  MACD       :", sprintf("%-10s", round(latest$MACD, 2)), "                    ║\n")
cat("╠════════════════════════════════════════════════════════╣\n")

rsi_signal  <- ifelse(latest$RSI_14 < 30, "BUY", ifelse(latest$RSI_14 > 70, "SELL", "NEUTRAL"))
sma_signal  <- ifelse(latest$SMA_50 > latest$SMA_200, "BULLISH", "BEARISH")
macd_signal <- ifelse(latest$MACD > latest$MACD_Signal, "BULLISH", "BEARISH")

cat("║  RSI Signal : ", sprintf("%-10s", rsi_signal), "                   ║\n")
cat("║  SMA Signal : ", sprintf("%-10s", sma_signal), "                   ║\n")
cat("║  MACD Signal: ", sprintf("%-10s", macd_signal), "                   ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# ============================================================
# FINAL SUMMARY
# ============================================================

cat("╔════════════════════════════════════════════════════════╗\n")
cat("║              PROJECT COMPLETE ✅                       ║\n")
cat("╠════════════════════════════════════════════════════════╣\n")
cat("║  Part 1: TCS Data Download & Feature Engineering      ║\n")
cat("║  Part 2: 13 ggplot2 Visualizations                    ║\n")
cat("║  Part 3: ARIMA Models & Seasonal Decomposition        ║\n")
cat("║  Part 4: TCS Algorithmic Trading                      ║\n")
cat("║          (5 Strategies + Backtesting + Dashboard)      ║\n")
cat("╚════════════════════════════════════════════════════════╝\n")
