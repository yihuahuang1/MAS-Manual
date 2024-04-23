## ----304-98, message=FALSE, warning=FALSE, include=FALSE----------------------
# Define function to read another rmd file
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')

  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)

  envir <- globalenv()
  source(tempR, local = envir, ...)
}


## ----304-99, message=FALSE, warning=FALSE, include=FALSE----------------------
# Read in base model construction rmd to continue with 3.4
source_rmd("03_c_MACD.Rmd")


## ----304-1, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# Define a function to compute the value for RSI using the adjusted close price with parameter settings of n days 

RSI_comput <- function (price,n){
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  rsi <- rep(NA,N)
  Lprice <- Lag(price,1)
  for (i in 2:N){
    if (price[i]>=Lprice[i]){
      U[i] <- price[i]- Lprice[i]
    } else{
      D[i] <- Lprice[i]- price[i]
    }
    if (i>n){
      AvgUp <- mean(U[(i-n+1):i])
      AvgDn <- mean(D[(i-n+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
      }
    }
  rsi <- reclass(rsi, price)
  return(rsi)
}


## ----304-2, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Compute RSI for all stocks, Full Data
mega7data_modified <-
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(RSI9 = RSI_comput(adjusted, 9),
         RSI14 = RSI_comput(adjusted, 14),
         RSI25 = RSI_comput(adjusted, 25))


## ----304-3, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Alternative way: 
# Below utilizes the `TTR` package directly for RSI computations: RSI()
# AAPL <- xts(rsi_value_result_pdf$adjusted,rsi_value_result_pdf$date)
# rsi <- RSI(AAPL,SMA, n = 14) 


## ----304-4, message=FALSE, warning=FALSE, include=FALSE-----------------------
rsi_value_result <- 
  mega7data_modified %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted, RSI9, RSI14, RSI25, volume) %>%
  arrange(desc(date)) %>%
  head(5000)

rsi_value_result_pdf <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted, RSI9, RSI14, RSI25, volume) %>%
  arrange(desc(date))


## ----304-5, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## 
## # NOT FINISHED
## 
## # ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}
## 
## # ##### [Individual stock outputs are suppressed, only `AAPL.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`苹果公司(AAPL.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}
## 
## # pdf output
## # Display AAPL output (EMA values)
## kbl(head(ema_value_result_pdf,10), booktabs = T, linesep = '', digits = 1,
##     caption = "EMA Computations for AAPL.US",
##     format.args = list(big.mark = ",", scientific = FALSE),
##     col.names = c("Symbol","Date","Adj Close","EMA5","EMA8",
##                   "EMA13","EMA20","EMA50","EMA200","Volume")) %>%
##   kable_styling(latex_options = c("striped", "scale_down",
##                                   "HOLD_position"),
##                 position = "center") %>%
##   column_spec(c(1,4:9), bold = T) %>%
##   kableExtra::footnote(general = "Output 3.2.a: Latest 10 rows of data are shown.")


## ----304-6, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# html output
# Display RSI values
DT::datatable(rsi_value_result, 
              colnames = c('Symbol' = 2, 'Date' = 3, 'Adjusted' = 4, 
                           'RSI(9)' = 5, 'RSI(14)' = 6, 'RSI(25)' = 7, 
                           'Volume' = 8),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'RSI Computations'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), 
              extensions = 'Buttons') %>%
  formatRound(columns = c('Adjusted','RSI(9)', 'RSI(14)', 'RSI(25)'), 
              digits = 2)  %>%
  formatRound(columns = 'Volume', digits = 0)


## ----304-7, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Data manipulation for RSI plot AAPL.US
rsi_aapl_xts <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.POSIXct(date)) %>%
  select(date, open, high, low, close, volume, adjusted) %>%
  na.omit()

open <- rsi_aapl_xts$open
high <- rsi_aapl_xts$high
low <- rsi_aapl_xts$low
close <- rsi_aapl_xts$close
adjusted <- rsi_aapl_xts$adjusted
volume <- as.numeric(rsi_aapl_xts$volume)

rsi_aapl_xts <- xts(cbind(open, high, low, close, volume, adjusted),
                   as.POSIXct(rsi_aapl_xts$date))
rsi_aapl_xts <- last(rsi_aapl_xts,'365 days')


## ----304-8, message=FALSE, warning=FALSE, include=FALSE-----------------------
##### AAPL Plot include RSI(14) using `TTR` package {-}
# pdf plot
# RSI plot TLSA.US RSI(14)
chartSeries(rsi_aapl_xts,
            type = "candlesticks",
            theme = chartTheme('black'))
addRSI(n = 14,maType = "SMA")


## ----eval=FALSE, include=FALSE------------------------------------------------
## RSI（Relative Strength Index，相对强弱指数）是一种动量振荡器，用于评估一项资产的过度买入或过度卖出状态。RSI 的值范围为0到100，通常用14天周期计算。
## RSI 指标的计算公式如下：
## 
## 其中：
## pos_avg是价格上升时的价格变化的滚动平均值，反映了近期的平均正向价格变化。
## neg_avg是价格下降时的价格变化的滚动平均值，反映了近期的平均负向价格变化。
## RSI 的使用方法：
## 1）超买或超卖：当 RSI 从高于70的区域下降，通常被认为是过度买入状态，可能预示着未来价格将下跌。相反，当 RSI 从低于30的区域上升，通常被认为是过度卖出状态，可能预示着未来价格将上涨。
## 2）背离：当价格创新高或新低，而 RSI 没有相应地创新高或新低，可能出现所谓的“背离”，此时可能预示着价格的反转。
## 3）中线交叉：如果 RSI 从下方向上穿过 50，可能预示着市场情绪从悲观转向乐观，可能是买入信号。反之，如果 RSI 从上方向下穿过 50，可能预示着市场情绪从乐观转向悲观，可能是卖出信号。
## 
## 
## 
## The Relative Strength Index (RSI) is a popular momentum oscillator in technical analysis, and there are several common trading rules or signals associated with its movements. These rules are based on the RSI crossing certain threshold levels, divergences with price, and specific patterns within the RSI itself. Here's a breakdown of some widely used RSI trading rules:
## 
## ### 1. Overbought and Oversold Signals
## - **Overbought Signal**: When the RSI crosses above 70, it suggests that the asset may be getting overbought and could be due for a correction or pullback. Traders might consider this a signal to sell or take profits.
## - **Oversold Signal**: Conversely, when the RSI crosses below 30, it indicates that the asset may be oversold and potentially undervalued, which could lead to a rebound. This is often taken as a buy signal by traders.
## 
## ### 2. Centerline Crossover
## - **Bullish Signal**: An RSI moving from below 50 to above 50 indicates increasing bullish momentum. This is considered a bullish signal, suggesting that the price might continue to rise.
## - **Bearish Signal**: When the RSI moves from above 50 to below 50, it indicates increasing bearish momentum. Traders might take this as a signal that the price could continue to fall.
## 
## ### 3. RSI Divergence
## - **Bullish Divergence**: Occurs when the price records a lower low, but the RSI forms a higher low. This indicates weakening downward momentum and a potential bullish reversal.
## - **Bearish Divergence**: Happens when the price records a higher high, but the RSI forms a lower high. This signals weakening upward momentum and a potential bearish reversal.
## 
## ### 4. Swing Rejections
## - **Bullish Swing Rejection**: It has four parts: (a) the RSI falls into the oversold area (below 30), (b) the RSI crosses back above 30, (c) a pullback within the oversold area without crossing below 30 again, and (d) the RSI then breaks its most recent high. This sequence suggests a strong bullish reversal.
## - **Bearish Swing Rejection**: This signal includes: (a) the RSI rises into the overbought area (above 70), (b) the RSI crosses back below 70, (c) a rally back into the overbought area without crossing above 70 again, and (d) the RSI then breaks its most recent low. It suggests a bearish reversal.
## 
## ### 5. Trendline Breaks within the RSI
## - **Bullish Signal**: Drawing trendlines directly on the RSI chart and seeing the RSI break above a downtrend line can signal a bullish reversal.
## - **Bearish Signal**: Conversely, if the RSI breaks below an uptrend line drawn on the RSI chart, it can be a bearish reversal signal.
## 
## It's important for traders to use these RSI signals in conjunction with other technical analysis tools and indicators to confirm trading signals and strategies. Also, considering the asset's volatility, market conditions, and adjusting RSI settings (like the period length) can help tailor the use of RSI to individual trading preferences and strategies.


## ----304-9, message=FALSE, warning=FALSE, include=FALSE-----------------------
# RSI Trading Signal 10: RSI crossover 30, 70 
# RSI Trading Rule setup and Generate Signals 

mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal10 = case_when(
    lag(RSI14) < 30 & RSI14 >= 30 ~ 'BUY',
    lag(RSI14) > 70 & RSI14 <= 70 ~ 'SELL',
    TRUE ~ 'HOLD'))


## ----304-10, message=FALSE, warning=FALSE, include=FALSE----------------------
# RSI Trading Signal 11: RSI Swing Rejections on 30, 70  
# RSI Trading Rule setup and Generate Signals 

mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(
    initial_bullish = lag(RSI14) < 30 & RSI14 >= 30,
    initial_bearish = lag(RSI14) > 70 & RSI14 <= 70,
    retest_bullish = ifelse(initial_bullish, lag(RSI14, 5) >= 30, FALSE),
    retest_bearish = ifelse(initial_bearish, lag(RSI14, 5) <= 70, FALSE),
    tsignal11 = 
      case_when(
      initial_bullish & retest_bullish ~ 'BUY',
      initial_bearish & retest_bearish ~ 'SELL',
      TRUE ~ 'HOLD')
    ) %>% 
  select(-initial_bullish, -initial_bearish, -retest_bullish, -retest_bearish)
  


## ----304-11, message=FALSE, warning=FALSE, include=FALSE----------------------
# RSI Trading Signal 12: RSI crossover 50
# RSI Trading Rule setup and Generate Signals 

mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal12 = case_when(
    lag(RSI14) < 50 & RSI14 > 50 ~ 'BUY',
    lag(RSI14) >= 50 & RSI14 < 50 ~ 'SELL',
    TRUE ~ 'HOLD')) 


## ----304-12, message=FALSE, warning=FALSE, include=FALSE, paged.print=TRUE----
# Prepare data frame RSI signal results
rsi_signal_result <-
  mega7data_modified %>%
  select(date, symbol, adjusted, tsignal10, 
         tsignal11, tsignal12) %>%
  arrange(desc(date)) %>%
  head(5000)

# For AAPL signals results (RSI)
rsi_signal_result_pdf <-
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  ungroup() %>%
  select(date, symbol, adjusted, tsignal10, 
         tsignal11, tsignal12) %>%
  arrange(desc(date))


## ----304-13, eval=FALSE, fig.pos='H', message=FALSE, warning=FALSE, include=FALSE----
## 
## # NOT FINISHED
## 
## # ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}
## 
## # ##### [Individual stock outputs are suppressed, only `AAPL.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`苹果公司(AAPL.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}
## 
## # pdf output
## # Display AAPL signal results (EMA signals)
## kbl(head(ema_signal_result_pdf,10), booktabs = T, linesep = "", longtable = T,
##     digits = 1, caption = "EMA Trading Signals for AAPL.US",
##     format.args = list(big.mark = ",", scientific = FALSE),
##     col.names = c("Date","Symbol","Adj Close","tsignal4","tsignal5",
##                   "tsignal6","tsignal6a","tsignal6b")) %>%
##   kable_styling(latex_options = c("striped", "scale_up",
##                                   "HOLD_position"),
##                 full_width = F) %>%
##   column_spec(c(2,4:6), bold = T) %>%
##   column_spec(1, width = "1.9cm") %>%
##   kableExtra::footnote(general = "Output 3.2.b: latest 10 rows of data are shown.",
##               number = "Includes tsignal4, tsignal5, tsignal6, tsignal6a and tsignal6b.")
## #图表3.2.b: EMA交易信号，展示最新的10行数据。包括`tsignal4`, `tsignal5`, `tsignal6`, #`tsignal6a`和`tsignal6b`。


## ----304-14, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# Display RSI signal results 
DT::datatable(rsi_signal_result, 
              colnames = c('Date' = 2, 'Symbol' = 3, 'Adjusted' = 4,
                           'Signal 10' = 5, 'Signal 11' = 6, 'Signal 12' = 7),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'RSI Trading Signals'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), 
              extensions = 'Buttons') %>%
    formatRound(columns = 'Adjusted', digits = 2)  


## ----304-15, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----------
## 
## # NOT FINISHED
## 
## # pdf output
## # EMA Candlestick+Volume Plot for AAPL with trading signals
## # Prepare the dataset for candlestick components
## ema_signal_plot_pdf<-
##   mega7data_modified %>%
##   slice(which(symbol == "AAPL")) %>%
##   select(date, symbol, adjusted, open, close, high, low, volume, EMA5,
##          EMA8, EMA13, EMA20, EMA50, EMA200, tsignal4, tsignal5, tsignal6) %>%
##   mutate(date = as.Date(date, format = "%Y-%m-%d"),
##          day_trend_color = ifelse(open - adjusted > 0, "red", "green")) %>%
##   filter(as.Date(date) > as.Date(Sys.Date()-months(12)))


## ----304-16, eval=FALSE, fig.height=7, fig.width=10, message=FALSE, warning=FALSE, include=FALSE----
## 
## # NOT FINISHED
## 
## # pdf output
## # EMA Candlestick+Volume Plot for AAPL with trading signals
## 
## # Candlestick Plot with EMA Trading Signals
## color_values <- c(
##   "EMA5" = "black",
##   "EMA8" = "yellow",
##   "EMA13" = "purple",
##   "EMA20" = "red",
##   "EMA50" = "grey",
##   "EMA200" = "blue",
##   "green" = "darkgreen",
##   "red" = "red",
##   "Signal4 BUY " = "lightgreen",
##   "Signal4 SELL" = "deeppink",
##   "Signal5 BUY" = "lightgreen",
##   "Signal5 SELL" = "deeppink",
##   "Signal6 BUY" = "lightgreen",
##   "Signal6 SELL" = "deeppink"
## )
## 
## candlestick <-
##   ggplot(ema_signal_plot_pdf, aes(x = date)) +
##   geom_segment(aes(xend = date, y = open,
##                    yend = adjusted, colour = day_trend_color), size = 5) +
##   geom_segment(aes(xend = date, y = high,
##                    yend = low, colour = day_trend_color)) +
##   geom_line(aes(y = EMA5, color = "EMA5"), size = 0.5) +
##   geom_line(aes(y = EMA8, color = "EMA8"), size = 0.5) +
##   geom_line(aes(y = EMA13, color = "EMA13"), size = 0.5) +
##   geom_line(aes(y = EMA20, color = "EMA20"), size = 0.5) +
##   geom_line(aes(y = EMA50, color = "EMA50"), size = 0.5) +
##   geom_line(aes(y = EMA200, color = "EMA200"), size = 0.5) +
## 
##   # Adding trading signals with different shapes and colors
##   geom_point(data = filter(ema_signal_plot_pdf, tsignal4 == "BUY"),
##              aes(x = date, y = high, color = "Signal4 BUY",
##                  shape = "Signal4 BUY"), size = 2) +
##   geom_point(data = filter(ema_signal_plot_pdf, tsignal4 == "SELL"),
##              aes(x = date, y = low, color = "Signal4 SELL",
##                  shape = "Signal4 SELL"), size = 2) +
##   geom_point(data = filter(ema_signal_plot_pdf, tsignal5 == "BUY"),
##              aes(x = date, y = high, color = "Signal5 BUY",
##                  shape = "Signal5 BUY"), size = 2) +
##   geom_point(data = filter(ema_signal_plot_pdf, tsignal5 == "SELL"),
##              aes(x = date, y = low, color = "Signal5 SELL",
##                  shape = "Signal5 SELL"), size = 2) +
##   geom_point(data = filter(ema_signal_plot_pdf, tsignal6 == "BUY"),
##              aes(x = date, y = high, color = "Signal6 BUY",
##                  shape = "Signal6 BUY"), size = 2) +
##   geom_point(data = filter(ema_signal_plot_pdf, tsignal6 == "SELL"),
##              aes(x = date, y = low, color = "Signal6 SELL",
##                  shape = "Signal6 SELL"), size = 2) +
##   theme(legend.position = "top",
##             guides(color = guide_legend(nrow = 4, byrow = TRUE,
##                                 title = "Indicators & Signals"),
##     shape = guide_legend(nrow = 4, byrow = TRUE, title = "Trade Signals")
##   )) +
##   scale_color_manual(name = "Indicators & Signals", values = color_values) +
##   scale_shape_manual(name = "Signal Shapes",
##                      values = c("Signal4 BUY" = 17, "Signal4 SELL" = 4,
##                                 "Signal5 BUY" = 18, "Signal5 SELL" = 3,
##                                 "Signal6 BUY" = 15, "Signal6 SELL" = 1)) +
##   labs(
##     y = "Stock Price, in USD", x = "Date") +
##     labs(title = paste0("Price Volume Candlestick Plot for TLSA.US \
##        with EMA Lines and Trading Signals")) +
##   theme_bw() +
##   scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")
## 
## # Volume Plot
## volume_plot <-
##   ggplot(ema_signal_plot_pdf, aes(x = date, y = volume, fill = day_trend_color)) +
##   geom_bar(stat = "identity") +
##   scale_fill_manual(values = c("green" = "darkgreen", "red" = "red")) +
##   labs(y = "Daily Trading Volume", x = "Date") +
##   theme_bw() +
##   theme(legend.position = "none",
##             guides(color = guide_legend(nrow = 4, byrow = TRUE,
##                                 title = "Indicators & Signals"),
##     shape = guide_legend(nrow = 4, byrow = TRUE, title = "Trade Signals")
##   )) +
##   scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
##   scale_y_continuous(labels = scales::comma)
## 
## 
## # Combine 2 plots
## ggarrange(candlestick, volume_plot, ncol = 1, nrow = 2,
##     heights =c(9,3), widths = 10, align = "v")
## 
## # Output 3.1.c: EMA plot with trading signals on Price and Volume, duration setting TTM.


## ----304-17, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# plotly interactive plot
# RSI Candlestick Plot + RSI subplot for AAPL with trading signals
# Prepare the dataset 

rsi_signal_plot_html<- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  select(date, symbol, adjusted, open, close, high, low, volume, RSI9, 
         RSI14, RSI25, tsignal10, tsignal11, tsignal12) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day_trend_color =  ifelse(open < adjusted, "lightgreen", "red"),
         prev_volume = lag(volume),
         prev_adjusted = lag(adjusted),
         prev_RSI9 = lag(RSI9),
         prev_RSI14 = lag(RSI14),
         prev_RSI25 = lag(RSI25),
         volume_color = ifelse(volume > prev_volume, "orange", "lightblue"),
         hover_text_volume = paste(
      "<b>Symbol:</b>", symbol, "<br>",
      "<b>Date:</b>", format(date, '%Y-%m-%d'), "<br>",
      "<b>Daily Volume:</b>", formatC(volume, big.mark = ","),
      "<span style='color:", 
      ifelse(volume > prev_volume, "darkgreen", "red"), ";'>",
      ifelse(volume > prev_volume, "▲", "▼"), "</span>", "<br>")) %>% 
  filter(as.Date(date) > as.Date(Sys.Date()-months(12)))

# Define shapes for each signal
signal_shapes <- list(
  "Signal10 BUY" = "circle",
  "Signal10 SELL" = "circle-open",
  "Signal11 BUY" = "triangle-up",
  "Signal11 SELL" = "triangle-down-open",
  "Signal12 BUY" = "diamond",
  "Signal12 SELL" = "diamond-open"
)

rsi_signal_plot_html <- 
  rsi_signal_plot_html %>%
  mutate(hover_text = paste(
    "<b>Symbol:</b>", symbol, "<br>",
    "<b>Date:</b>", format(date, '%Y-%m-%d'), "<br>",
    "<b>Adjusted Close Price:</b> $", round(adjusted, 2), "<br>",
    "<span style='color:", 
    ifelse(adjusted > prev_adjusted, "darkgreen", "pink"), ";'>",
      ifelse(adjusted > prev_adjusted, "▲", "▼"), "</span>", "<br>",
    "<b>Open:</b> $", round(open, 2), "<br>",
    "<b>High:</b> $", round(high, 2), "<br>",
    "<b>Low:</b> $", round(low, 2), "<br>",
    "<b>Volume:</b>", formatC(volume, format = "f", big.mark = ","),
     "<span style='color:", 
      ifelse(volume > prev_volume, "darkgreen", "red"), ";'>",
      ifelse(volume > prev_volume, "▲", "▼"), "</span>", "<br>"
  )) 

# Hover for RSI subplot
rsi_signal_plot_html <- 
  rsi_signal_plot_html %>%
  mutate(hover_text_rsi = paste(
      "<b>Symbol:</b>", symbol, "<br>", 
      "<b>Date:</b>", format(date, '%Y-%m-%d'), "<br>",
      "<b>Adjusted Close Price:</b> $", round(adjusted, 2), 
      "<span style='color:", 
      ifelse(adjusted > prev_adjusted, "darkgreen", "red"), ";'>",
      ifelse(adjusted > prev_adjusted, "▲", "▼"), "</span>", "<br>",
      "<b>RSI(9):</b>", round(RSI9, 2),
      "<span style='color:", 
      ifelse(RSI9 > prev_RSI9, "darkgreen", "deeppink"), ";'>",
      ifelse(RSI9 > prev_RSI9, "▲", "▼"), "</span>", "<br>",
      "<b>RSI(14):</b>", round(RSI14, 2),
      "<span style='color:", 
      ifelse(RSI14 > prev_RSI14, "darkgreen", "deeppink"), ";'>",
      ifelse(RSI14 > prev_RSI14, "▲", "▼"), "</span>", "<br>",
      "<b>RSI(25)</b>", round(RSI25, 2),
      "<span style='color:", 
      ifelse(RSI25 > prev_RSI25, "darkgreen", "deeppink"), ";'>",
      ifelse(RSI25 > prev_RSI25, "▲", "▼"), "</span>", "<br>"
    )
  )


## ----304-18, echo=FALSE, fig.height=7, fig.width=12, message=FALSE, warning=FALSE----
# html output
# plotly interactive plot
# RSI Candlestick Plot + RSI subplot for AAPL with trading signals

# Candlestick Plot with RSI Trading Signals
fig <- plot_ly(data = rsi_signal_plot_html, 
               type = "candlestick", 
               x = ~date, open = ~open, close = ~adjusted, 
               high = ~high, low = ~low,
               increasing = list(line = list(color = "darkgreen", width = 10), 
                                 fillcolor = "darkgreen"),
               decreasing = list(line = list(color = "red", width = 10), 
                                 fillcolor = "red"), 
               hoverinfo = "text", text = ~hover_text, showlegend = FALSE) %>%
  add_markers(data = filter(rsi_signal_plot_html, tsignal10 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal10 BUY"]]), 
              name = "Signal10 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal10 BUY</b><extra></extra>") %>%
  add_markers(data = filter(rsi_signal_plot_html, tsignal10 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal10 SELL"]]), 
              name = "Signal10 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal10 SELL</b><extra></extra>") %>%
  add_markers(data = filter(rsi_signal_plot_html, tsignal11 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal11 BUY"]]), 
              name = "Signal11 BUY",  showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal11 BUY</b><extra></extra>") %>%
  add_markers(data = filter(rsi_signal_plot_html, tsignal11 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal11 SELL"]]), 
              name = "Signal11 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal11 SELL</b><extra></extra>") %>%
  add_markers(data = filter(rsi_signal_plot_html, tsignal12 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal12 BUY"]]), 
              name = "Signal12 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal12 BUY</b><extra></extra>") %>%
  add_markers(data = filter(rsi_signal_plot_html, tsignal12 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal12 SELL"]]), 
              name = "Signal12 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal12 SELL</b><extra></extra>")

# Add the RSI9, RSI14, RSI25 lines on the same subplot as the histogram
fig <- fig %>%
  add_trace(
    data = rsi_signal_plot_html,
    x = ~date,
    y = ~RSI9,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'darkgrey'),
    name = 'RSI(9)',
    yaxis = 'y2', 
    showlegend = TRUE,
    hover_info = 'text',
    text = ~hover_text_rsi) %>%
  add_trace(
    data = rsi_signal_plot_html,
    x = ~date,
    y = ~RSI14,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'red'),
    name = 'RSI(14)',
    yaxis = 'y2',
    showlegend = TRUE,
    hover_info = 'text',
    text = ~hover_text_rsi) %>% 
  add_trace(
    data = rsi_signal_plot_html,
    x = ~date,
    y = ~RSI25,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'lightblue'),
    name = 'RSI(25)',
    yaxis = 'y2',
    showlegend = TRUE,
    hover_info = 'text',
    text = ~hover_text_rsi)
  

# Update the layout for the secondary y-axis without using 'overlaying' and set the domain for both y-axes
fig <- fig %>%
  layout(
    title = list(text = "Interactive Price Candlestick Plot for AAPL.US \
                      with RSI Subplot and Trading Signals",
                      x = 0.5,  
                      y = 1.2,  
                      xanchor = 'center',
                      yanchor = 'top'),
    yaxis = list(domain = c(0.4, 1), title = "Stock Price, in USD", fixedrange = FALSE),
    yaxis2 = list(domain = c(0, 0.25), title = "RSI", anchor = 'x', 
                  side = 'left', overlaying = NULL, fixedrange = FALSE),
    xaxis = list(domain = c(0, 1), title = "Date", anchor = 'y',
                      type = "date", 
                      rangeslider = list(visible = TRUE),
                      rangeselector = list(
                        buttons = list(
                          list(count = 1, label = "1w", 
                               step = "week", stepmode = "backward"),
                          list(count = 2, label = "2w", 
                               step = "week", stepmode = "backward"),
                          list(count = 1, label = "1m", 
                               step = "month", stepmode = "backward"),
                          list(count = 6, label = "6m", 
                               step = "month", stepmode = "backward"),
                          list(count = 12, label = "TTM", 
                               step = "month", stepmode = "backward"),
                          list(count = 1, label = "YTD",
                               step = "year", stepmode = "todate"),
                          list(count = 1, label = "1y", 
                               step = "year", stepmode = "backward"),
                          list(step = "all")
                      )
                    )),
    legend = list(orientation = "h",x = 0, y = -0.5, 
                       xanchor = "left", yanchor = "top", traceorder = "normal"),
    margin = list(t = 100, b = 200),
    width = 900, height = 750
  )

fig


## ----304-19, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----------
## 
## # NOT FINISHED
## 
## # html output
## # plotly interactive plot
## # RSI Volume Plot for AAPL with trading signals
## # Define signal colors and shapes for trading signals
## 
## volume_plot <- plot_ly(data = ema_signal_plot_html,
##                        x = ~date, y = ~volume, type = 'bar',
##                        marker = list(color = ~volume_color),
##                        hoverinfo = 'text', text = ~hover_text_volume,
##                        name = "Trading Volume") %>%
##   layout(title = list(text = "Interactive Daily Trading Volume Plot for TLSA.US \
##                       with EMA Lines and Trading Signals",
##                       x = 0.5, y = 0.95,
##                       xanchor = 'center', yanchor = 'top'),
##          xaxis = list(title = "Date", type = "date", rangeslider = list(visible = TRUE),
##                       rangeselector = list(
##                         buttons = list(
##                           list(count = 1, label = "1w",
##                                step = "week", stepmode = "backward"),
##                           list(count = 2, label = "2w",
##                                step = "week", stepmode = "backward"),
##                           list(count = 1, label = "1m",
##                                step = "month", stepmode = "backward"),
##                           list(count = 6, label = "6m",
##                                step = "month", stepmode = "backward"),
##                           list(count = 12, label = "TTM",
##                                step = "month", stepmode = "backward"),
##                           list(count = 1, label = "YTD",
##                                step = "year", stepmode = "todate"),
##                           list(count = 1, label = "1y",
##                                step = "year", stepmode = "backward"),
##                           list(step = "all")
##                       )
##                     )),
##          yaxis = list(title = "Daily Trading Volume", fixedrange = FALSE, side = "left"),
##          yaxis2 = list(title = "Daily EMA Values",
##                        overlaying = "y", side = "right",
##                        showgrid = FALSE,titlefont = list(size = 14)),
##          barmode = 'group',
##          legend = list(orientation = "h", x = 0.5, y = -0.4,
##                        xanchor = "center", yanchor = "top"),
##          margin = list(l = 40, r = 40,b = 100, t = 100, pad = 4), height = 450)
## 
## vol_signal_colors <- c("BUY" = "lightgreen", "SELL" = "pink")
## signals <- c("tsignal4", "tsignal5", "tsignal6")
## 
## for (sig in signals) {
##   volume_plot <- volume_plot %>%
##     add_markers(data = ema_signal_plot_html %>%
##                   filter(get(sig) == "BUY"),
##                 x = ~date, y = ~volume,
##                 marker = list(color = vol_signal_colors[["BUY"]],
##                               size = 10, symbol = "circle"),
##                 name = paste(sig, "BUY"),
##                 hoverinfo = "text",
##                 hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>",
##                                       sig, "BUY</b><extra></extra>")) %>%
##     add_markers(data = ema_signal_plot_html %>% filter(get(sig) == "SELL"),
##                 x = ~date, y = ~volume,
##                 marker = list(color = vol_signal_colors[["SELL"]],
##                               size = 10, symbol = "x"),
##                 name = paste(sig, "SELL"),
##                 hoverinfo = "text",
##                 hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>",
##                                       sig, "SELL</b><extra></extra>"))
## }
## 
## ema_lines <- c("EMA5", "EMA8", "EMA13", "EMA20", "EMA50", "EMA200")
## 
## ema_colors <- c("EMA5" = "black", "EMA8" = "yellow",
##                 "EMA13" = "purple", "EMA20" = "red",
##                 "EMA50" = "grey", "EMA200" = "blue")
## 
## for (ema in ema_lines) {
##   volume_plot <-
##     volume_plot %>%
##     add_lines(data = ema_signal_plot_html,
##           x = ~date, y = as.formula(paste0("~", ema)), name = ema,
##               line = list(color = ema_colors[ema]), yaxis = "y2",
##               hoverinfo = "y+text",
##               hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>",
##                                     ema, ":</b> %{y:.2f}<extra></extra>"))
## }
## 
## volume_plot

