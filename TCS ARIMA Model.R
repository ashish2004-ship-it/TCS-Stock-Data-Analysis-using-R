# ============================================================
# TCS STOCK ANALYSIS - PART 3
# ARIMA Modeling & Seasonal Decomposition
# ============================================================
# NOTE: Run part1_setup_download.R first to load 'tcs_df'
# ============================================================

cat("\n")
cat("============================================================\n")
cat("  PART 3: ARIMA MODELING & SEASONAL DECOMPOSITION\n")
cat("============================================================\n\n")

# ============================================================
# 3.1  PREPARE TIME SERIES DATA
# ============================================================

# Create a monthly time series of average closing prices
monthly_data <- tcs_df %>%
  group_by(Year, MonthNum) %>%
  summarise(
    Avg_Close  = mean(Close, na.rm = TRUE),
    Avg_Volume = mean(Volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, MonthNum)

# Determine start year and month
start_year  <- min(monthly_data$Year)
start_month <- min(monthly_data$MonthNum[monthly_data$Year == start_year])

# Create ts object (monthly frequency = 12)
tcs_ts <- ts(monthly_data$Avg_Close,
             start = c(start_year, start_month),
             frequency = 12)

cat("✅ Monthly time series created\n")
cat("   Start:", start_year, "/", start_month, "\n")
cat("   Length:", length(tcs_ts), "months\n\n")

# Plot raw time series
p_ts <- ggplot(monthly_data, aes(x = as.Date(paste(Year, MonthNum, 1, sep = "-")),
                                 y = Avg_Close)) +
  geom_line(color = "#1E88E5", linewidth = 0.7) +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "TCS – Monthly Average Closing Price",
    subtitle = "Time series used for decomposition and ARIMA modeling",
    x = "Date", y = "Avg. Closing Price (₹)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
    plot.subtitle = element_text(hjust = 0.5, color = "#455A64"),
    panel.grid.minor = element_blank()
  )

print(p_ts)

# ============================================================
# 3.2  SEASONAL DECOMPOSITION (STL)
# ============================================================

cat("--- 3.2  Seasonal Decomposition (STL) ---\n\n")

# STL Decomposition (robust to outliers)
stl_decomp <- stl(tcs_ts, s.window = "periodic", robust = TRUE)

cat("✅ STL decomposition completed\n")
cat("   Components: Trend, Seasonal, Remainder (Residual)\n\n")

# Base R plot of decomposition
plot(stl_decomp,
     main = "TCS Stock – STL Decomposition",
     col  = "#1E88E5")

# ---- Extract components into a data frame for ggplot2 ------

decomp_df <- data.frame(
  Date     = seq(as.Date(paste(start_year, start_month, 1, sep = "-")),
                 by = "month", length.out = length(tcs_ts)),
  Observed = as.numeric(tcs_ts),
  Trend    = as.numeric(stl_decomp$time.series[, "trend"]),
  Seasonal = as.numeric(stl_decomp$time.series[, "seasonal"]),
  Residual = as.numeric(stl_decomp$time.series[, "remainder"])
)

# ---- 3.2.1 Trend Component ----
p_trend <- ggplot(decomp_df, aes(x = Date)) +
  geom_line(aes(y = Observed), color = "#90CAF9", linewidth = 0.4,
            alpha = 0.6) +
  geom_line(aes(y = Trend), color = "#1565C0", linewidth = 1) +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "TCS – Trend Component",
    subtitle = "Underlying long-term movement extracted by STL",
    x = "Date", y = "Price (₹)",
    caption  = "Light blue = Observed | Dark blue = Trend"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#1A237E"),
    plot.subtitle = element_text(hjust = 0.5, color = "#455A64")
  )

print(p_trend)
cat("✅ Chart: Trend Component - Done\n")

# ---- 3.2.2 Seasonal Component ----
p_seasonal <- ggplot(decomp_df, aes(x = Date, y = Seasonal)) +
  geom_line(color = "#FF6F00", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_area(fill = "#FFE0B2", alpha = 0.4) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "TCS – Seasonal Component",
    subtitle = "Repeating patterns within each year",
    x = "Date", y = "Seasonal Effect (₹)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#E65100"),
    plot.subtitle = element_text(hjust = 0.5, color = "#455A64")
  )

print(p_seasonal)
cat("✅ Chart: Seasonal Component - Done\n")

# ---- 3.2.3 Residual Component ----
p_residual <- ggplot(decomp_df, aes(x = Date, y = Residual)) +
  geom_segment(aes(xend = Date, y = 0, yend = Residual),
               color = ifelse(decomp_df$Residual >= 0, "#4CAF50", "#F44336"),
               linewidth = 0.3, alpha = 0.6) +
  geom_point(aes(color = ifelse(Residual >= 0, "Positive", "Negative")),
             size = 0.8, show.legend = FALSE) +
  scale_color_manual(values = c("Positive" = "#4CAF50", "Negative" = "#F44336")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "TCS – Residual (Remainder) Component",
    subtitle = "Random noise after removing trend and seasonality",
    x = "Date", y = "Residual (₹)",
    caption  = "Large residuals indicate unexpected price movements"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#B71C1C"),
    plot.subtitle = element_text(hjust = 0.5, color = "#455A64")
  )

print(p_residual)
cat("✅ Chart: Residual Component - Done\n")

# ---- 3.2.4 All Components Combined ----
decomp_long <- decomp_df %>%
  pivot_longer(cols = c(Observed, Trend, Seasonal, Residual),
               names_to = "Component", values_to = "Value") %>%
  mutate(Component = factor(Component,
                            levels = c("Observed", "Trend", "Seasonal", "Residual")))

p_decomp_all <- ggplot(decomp_long, aes(x = Date, y = Value)) +
  geom_line(aes(color = Component), linewidth = 0.5) +
  facet_wrap(~ Component, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Observed" = "#1E88E5", "Trend" = "#1565C0",
                                "Seasonal" = "#FF6F00", "Residual" = "#F44336")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "TCS – Complete STL Decomposition",
    subtitle = "Observed = Trend + Seasonal + Residual",
    x = "Date", y = "Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
    strip.text    = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

print(p_decomp_all)
cat("✅ Chart: Combined Decomposition - Done\n\n")

# ============================================================
# 3.3  STATIONARITY TESTING
# ============================================================

cat("--- 3.3  Stationarity Tests ---\n\n")

# Augmented Dickey-Fuller Test on raw series
adf_raw <- adf.test(tcs_ts, alternative = "stationary")
cat("ADF Test on Raw Series:\n")
cat("  Test Statistic:", round(adf_raw$statistic, 4), "\n")
cat("  P-value       :", round(adf_raw$p.value, 4), "\n")
cat("  Conclusion    :", ifelse(adf_raw$p.value < 0.05,
                                "Stationary ✅",
                                "Non-stationary ❌ (differencing needed)"), "\n\n")

# Differencing
tcs_diff <- diff(tcs_ts)
adf_diff <- adf.test(tcs_diff, alternative = "stationary")
cat("ADF Test on First-Differenced Series:\n")
cat("  Test Statistic:", round(adf_diff$statistic, 4), "\n")
cat("  P-value       :", round(adf_diff$p.value, 4), "\n")
cat("  Conclusion    :", ifelse(adf_diff$p.value < 0.05,
                                "Stationary ✅",
                                "Non-stationary ❌"), "\n\n")

# Number of differences needed
ndiffs_val <- ndiffs(tcs_ts)
cat("Recommended differencing order (ndiffs):", ndiffs_val, "\n\n")

# ============================================================
# 3.4  ACF & PACF PLOTS
# ============================================================

cat("--- 3.4  ACF & PACF Plots ---\n\n")

par(mfrow = c(2, 2))

acf(as.numeric(tcs_ts), lag.max = 48,
    main = "ACF – TCS Raw Monthly Prices", col = "#1E88E5")
pacf(as.numeric(tcs_ts), lag.max = 48,
     main = "PACF – TCS Raw Monthly Prices", col = "#FF6F00")
acf(as.numeric(tcs_diff), lag.max = 48,
    main = "ACF – First Differenced", col = "#4CAF50")
pacf(as.numeric(tcs_diff), lag.max = 48,
     main = "PACF – First Differenced", col = "#F44336")

par(mfrow = c(1, 1))
cat("✅ ACF/PACF plots generated\n\n")

# ============================================================
# 3.5  ARIMA MODELING
# ============================================================

cat("--- 3.5  ARIMA Modeling ---\n\n")

# ---- 3.5.1 Auto ARIMA (automated model selection) ----
cat("Running auto.arima() for optimal model selection...\n")
arima_auto <- auto.arima(tcs_ts,
                         seasonal     = TRUE,
                         stepwise     = FALSE,
                         approximation = FALSE,
                         trace        = TRUE)

cat("\n✅ Best ARIMA model selected:\n")
print(summary(arima_auto))

# ---- 3.5.2 Manual ARIMA models for comparison ----
cat("\n--- Manual ARIMA Models ---\n")

# ARIMA(1,1,1)
arima_111 <- Arima(tcs_ts, order = c(1, 1, 1))
cat("ARIMA(1,1,1) AIC:", round(arima_111$aic, 2),
    " BIC:", round(arima_111$bic, 2), "\n")

# ARIMA(2,1,2)
arima_212 <- Arima(tcs_ts, order = c(2, 1, 2))
cat("ARIMA(2,1,2) AIC:", round(arima_212$aic, 2),
    " BIC:", round(arima_212$bic, 2), "\n")

# ARIMA(1,1,0)
arima_110 <- Arima(tcs_ts, order = c(1, 1, 0))
cat("ARIMA(1,1,0) AIC:", round(arima_110$aic, 2),
    " BIC:", round(arima_110$bic, 2), "\n")

# ARIMA(0,1,1)
arima_011 <- Arima(tcs_ts, order = c(0, 1, 1))
cat("ARIMA(0,1,1) AIC:", round(arima_011$aic, 2),
    " BIC:", round(arima_011$bic, 2), "\n\n")

# Model comparison table
model_comparison <- data.frame(
  Model = c("Auto ARIMA", "ARIMA(1,1,1)", "ARIMA(2,1,2)",
            "ARIMA(1,1,0)", "ARIMA(0,1,1)"),
  AIC = c(arima_auto$aic, arima_111$aic, arima_212$aic,
          arima_110$aic, arima_011$aic),
  BIC = c(arima_auto$bic, arima_111$bic, arima_212$bic,
          arima_110$bic, arima_011$bic)
)
model_comparison <- model_comparison %>%
  mutate(AIC = round(AIC, 2), BIC = round(BIC, 2)) %>%
  arrange(AIC)

cat("--- Model Comparison (sorted by AIC) ---\n")
print(model_comparison)
cat("\n")

# ---- 3.5.3 Residual Diagnostics ----
cat("--- Residual Diagnostics ---\n")
checkresiduals(arima_auto)

# Ljung-Box test
lb_test <- Box.test(residuals(arima_auto), lag = 20, type = "Ljung-Box")
cat("\nLjung-Box Test:\n")
cat("  Statistic:", round(lb_test$statistic, 4), "\n")
cat("  P-value  :", round(lb_test$p.value, 4), "\n")
cat("  Conclusion:", ifelse(lb_test$p.value > 0.05,
                            "Residuals are white noise ✅ (good fit)",
                            "Residuals have autocorrelation ❌"), "\n\n")

# ============================================================
# 3.6  FORECASTING
# ============================================================

cat("--- 3.6  ARIMA Forecast ---\n\n")

# Forecast next 12 months
forecast_horizon <- 12
tcs_forecast <- forecast(arima_auto, h = forecast_horizon, level = c(80, 95))

cat("12-Month Forecast Summary:\n")
print(tcs_forecast)

# ---- ggplot2 Forecast Visualization ----
forecast_dates <- seq(max(decomp_df$Date) + 30, by = "month",
                      length.out = forecast_horizon)

forecast_df <- data.frame(
  Date      = forecast_dates,
  Forecast  = as.numeric(tcs_forecast$mean),
  Lo80      = as.numeric(tcs_forecast$lower[, 1]),
  Hi80      = as.numeric(tcs_forecast$upper[, 1]),
  Lo95      = as.numeric(tcs_forecast$lower[, 2]),
  Hi95      = as.numeric(tcs_forecast$upper[, 2])
)

# Take last 60 months of historical for context
hist_tail <- tail(decomp_df, 60)

p_forecast <- ggplot() +
  # Historical
  geom_line(data = hist_tail, aes(x = Date, y = Observed),
            color = "#1E88E5", linewidth = 0.7) +
  # 95% CI
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo95, ymax = Hi95),
              fill = "#BBDEFB", alpha = 0.4) +
  # 80% CI
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo80, ymax = Hi80),
              fill = "#64B5F6", alpha = 0.5) +
  # Forecast line
  geom_line(data = forecast_df, aes(x = Date, y = Forecast),
            color = "#D32F2F", linewidth = 1, linetype = "dashed") +
  geom_point(data = forecast_df, aes(x = Date, y = Forecast),
             color = "#D32F2F", size = 2) +
  # Vertical line at forecast start
  geom_vline(xintercept = max(hist_tail$Date), linetype = "dotted",
             color = "grey40") +
  annotate("text", x = max(hist_tail$Date), y = Inf, vjust = 2,
           label = "Forecast →", fontface = "italic", color = "grey40") +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title    = "TCS – 12-Month ARIMA Forecast",
    subtitle = paste0("Model: ", arima_auto),
    x = "Date", y = "Predicted Price (₹)",
    caption  = "Shaded regions: 80% (dark) and 95% (light) confidence intervals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15,
                              color = "#1A237E"),
    plot.subtitle = element_text(hjust = 0.5, color = "#455A64"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_forecast)
cat("✅ 12-Month Forecast Chart - Done\n\n")

# ---- Forecast accuracy (train/test split) ----
cat("--- Cross-Validation Accuracy ---\n")
train_end <- length(tcs_ts) - 12
tcs_train <- window(tcs_ts, end = c(start_year + (train_end - 1) %/% 12,
                                    start_month + (train_end - 1) %% 12))
tcs_test  <- window(tcs_ts, start = c(start_year + train_end %/% 12,
                                      start_month + train_end %% 12))

fit_train <- auto.arima(tcs_train)
pred_test <- forecast(fit_train, h = length(tcs_test))

cat("\nForecast Accuracy Metrics:\n")
print(accuracy(pred_test, tcs_test))

cat("\n✅ Part 3 complete! ARIMA & Decomposition analysis done.\n")
cat("   Proceed to Part 4 for Algorithmic Trading (PolicyBazaar).\n")
