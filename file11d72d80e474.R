## ----305-98, message=FALSE, warning=FALSE, include=FALSE----------------------
# Define function to read another rmd file
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')

  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)

  envir <- globalenv()
  source(tempR, local = envir, ...)
}


## ----305-99, message=FALSE, warning=FALSE, include=FALSE----------------------
# Read in base model construction rmd to continue with 3.4
source_rmd("03_d_RSI.Rmd")


## ----305-1, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# Define a function to compute the value for SMA using the adjusted close price with parameter settings of n days 
# Same as 3.1
SMA_comput <- function(price, n){
  sma <- rep(NA, length(price)) 
  if (length(price) >= n) {  # Only compute SMA if enough data points are available
    for (i in n:length(price)){
      sma[i] <- mean(price[(i-n+1):i])
    }
  }
  return(sma)
}


## ----305-2, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# Define functions to compute the values for BBands (all components) using the adjusted close price with parameter settings of n days 

BBands_up_comput <- function(price, n, sd){
  mavg <- SMA_comput(price, n)
  sdev <- rep(NA, length(price))
  N <- length(price)
  if (N >= n) { # Only compute if enough data points are available
    for (i in (n+1):N){
      sdev[i] <- sd(price[(i-n+1):i])
    }
    sdev <- sqrt((n-1)/n) * sdev
    up <- mavg + sd * sdev
    return(up)
  } else {
    return(rep(NA, N))
  }
}


BBands_down_comput <- function(price, n, sd){
  mavg <- SMA_comput(price, n)
  sdev <- rep(NA, length(price))
  N <- length(price)
  if (N >= n) {
    for (i in (n+1):N){
      sdev[i] <- sd(price[(i-n+1):i])
    }
    sdev <- sqrt((n-1)/n) * sdev
    dn <- mavg - sd * sdev
    return(dn)
  } else {
    return(rep(NA, N))
  }
}


BBands_mavg_comput <- function(price, n){
  mavg <- SMA_comput(price, n)
  return(mavg)
}


BBands_pctB_comput <- function(price, n, sd){
  mavg <- SMA_comput(price, n)
  sdev <- rep(NA, length(price))
  N <- length(price)
  if (N >= n) {
    for (i in (n+1):N){
      sdev[i] <- sd(price[(i-n+1):i])
    }
    sdev <- sqrt((n-1)/n) * sdev
    up <- mavg + sd * sdev
    dn <- mavg - sd * sdev
    # Prevent division by zero
    range <- up - dn
    range[range == 0] <- NA  # replace 0s with NA to avoid division by zero
    pctB <- (price - dn) / range
    return(pctB)
  } else {
    return(rep(NA, N))
  }
}



## ----305-3, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Compute BBands for all stocks, Full Data
mega7data_modified <-
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(BBands_20_up = BBands_up_comput(adjusted, n = 20, sd = 2),
         BBands_20_down = BBands_down_comput(adjusted, n = 20, sd = 2),
         BBands_20_mavg = BBands_mavg_comput(adjusted, n = 20),
         BBands_20_pctB = BBands_pctB_comput(adjusted, n = 20, sd = 2))


## ----305-4, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Alternative way: 
# Below utilizes the `TTR` package directly for BBands computations: BBands()
# AAPL <- xts(bbands_value_result_pdf$adjusted,bbands_value_result_pdf$date)
# bbands <- BBands(AAPL,n = 20, sd = 2) 


## ----305-5, message=FALSE, warning=FALSE, include=FALSE-----------------------
bbands_value_result <- 
  mega7data_modified %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted,BBands_20_down, BBands_20_mavg, 
         BBands_20_up,BBands_20_pctB, volume) %>%
  arrange(desc(date)) %>%
  head(5000)

bbands_value_result_pdf <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted,BBands_20_down, BBands_20_mavg, 
         BBands_20_up,BBands_20_pctB, volume) %>%
  arrange(desc(date))


## ----305-6, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
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


## ----305-7, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# html output
# Display BBands values
DT::datatable(bbands_value_result, 
              colnames = c('Symbol' = 2, 'Date' = 3, 'Adjusted' = 4, 
                           'BBands(20) Down' = 5,'BBands(20) Trend Line' = 6,
                           'BBands(20) Up' = 7,
                           'BBands(20) Price Location' = 8,
                           'Volume' = 9),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'Bollinger Bands (BBands) Computations'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, scrollX = TRUE,
              style = list(width = '100%')), extensions = 'Buttons') %>%
  formatRound(columns = c('Adjusted','BBands(20) Down','BBands(20) Trend Line',
                          'BBands(20) Up',
                          'BBands(20) Price Location'), digits = 2)  %>%
  formatRound(columns = 'Volume', digits = 0)


## ----305-8, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Data manipulation for BBands plot AAPL.US
bbands_aapl_xts <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.POSIXct(date)) %>%
  select(date, open, high, low, close, volume, adjusted) %>%
  na.omit()

open <- bbands_aapl_xts$open
high <- bbands_aapl_xts$high
low <- bbands_aapl_xts$low
close <- bbands_aapl_xts$close
adjusted <- bbands_aapl_xts$adjusted
volume <- as.numeric(bbands_aapl_xts$volume)

bbands_aapl_xts <- xts(cbind(open, high, low, close, volume, adjusted),
                   as.POSIXct(bbands_aapl_xts$date))
bbands_aapl_xts <- last(bbands_aapl_xts,'365 days')


## ----305-9, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## ##### AAPL Plot include BBands components using `TTR` package {-}
## # pdf plot
## # BBands plot AAPL.US n = 20, k = 2
## chartSeries(bbands_aapl_xts,
##             theme = chartTheme('black'))
## 
## addBBands(n = 20, sd = 2, maType = "SMA", on = 1)


## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE------------------
## 布林带 (Bollinger Bands，简称 BBANDS) 是由 John Bollinger 提出的一种流行的波动性指标，主要用来衡量价格的高低程度以及波动性。布林带由三条线组成：中线是移动平均线，上线和下线则是在中线的基础上加减标准差得到的。
## 布林带的计算公式如下：
## 1）计算收盘价序列的移动平均线（MID）。
## 2）计算收盘价序列的标准差（stdev）：
## 3）计算布林带的上线和下线：
## 4）计算布林带的宽度和价格在布林带中的位置：
## 以下是布林带一些主要的应用方法：
## 1）波动性度量：当布林带的宽度（上线与下线之间的距离）扩大时，表示市场波动性增大，可能预示着价格大幅波动的可能性增加；当布林带的宽度收窄时，表示市场波动性减小，可能预示着市场即将进入平静期。
## 2）超买超卖判断：布林带的上线和下线可被视为价格的相对高点和低点，通常，当价格触及上线时，市场可能处于超买状态，可能存在回落的可能性；反之，当价格触及下线时，市场可能处于超卖状态，可能存在反弹的可能性。
## 3）趋势判断：价格在布林带的中线上方运动，通常被认为是上升趋势；在中线下方运动，则可能表示下降趋势。
## 4）突破信号：当价格突破上线或者下线，特别是伴随着交易量的放大，可能预示着价格趋势的改变。但这种信号需要谨慎处理，因为有可能是假突破。
## Buy signal arises when price is above the band.
## 
## Sell signal arises when price is below the band.


## ----305-10, message=FALSE, warning=FALSE, include=FALSE----------------------
# BBands Trading Signal 13: BBands mavg <= adjusted < BBands up
# BBands Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal13 = case_when(
    BBands_20_mavg <= adjusted & adjusted < BBands_20_up ~ 'BUY',
    BBands_20_down < adjusted & adjusted < BBands_20_mavg ~ 'SELL',
    TRUE ~ 'HOLD'))


## ----305-11, message=FALSE, warning=FALSE, include=FALSE----------------------
# BBands Trading Signal 14: adjusted crossover below BBands down
# BBands Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal14 = case_when(
    lag(adjusted) >= lag(BBands_20_down) & adjusted <= BBands_20_down ~ 'BUY',
    lag(adjusted) <= lag(BBands_20_up) & adjusted >= BBands_20_up ~ 'SELL',
    TRUE ~ 'HOLD'))


## ----305-12, message=FALSE, warning=FALSE, include=FALSE----------------------
# BBands Trading Signal 15a: adjusted <= BBands down 
# BBands Trading Signal 15: adjusted <= BBands down & trading volume increased
# BBands Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal15a = case_when(
    adjusted <= BBands_20_down ~ 'BUY',
    adjusted >= BBands_20_up ~ 'SELL',
    TRUE ~ 'HOLD')) %>%
  mutate(tsignal15 = case_when(
    adjusted <= BBands_20_down & lag(volume) <= volume ~ 'BUY',
    adjusted >= BBands_20_up & lag(volume) <= volume ~ 'SELL',
    TRUE ~ 'HOLD'))


## ----305-13, message=FALSE, warning=FALSE, include=FALSE, paged.print=TRUE----
# Prepare data frame BBands signal results
bbands_signal_result <-
  mega7data_modified %>%
  select(date, symbol, adjusted, tsignal13, 
         tsignal14, tsignal15a, tsignal15) %>%
  arrange(desc(date)) %>%
  head(5000)

# For AAPL signals results (BBands)
bbands_signal_result_pdf <-
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  ungroup() %>%
  select(date, symbol, adjusted, tsignal13, 
         tsignal14, tsignal15a, tsignal15) %>%
  arrange(desc(date)) %>%
  head(10)


## ----305-14, eval=FALSE, fig.pos='H', message=FALSE, warning=FALSE, include=FALSE----
## 
## # NOT FINISHED
## 
## # ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}
## 
## # ##### [Individual stock outputs are suppressed, only `AAPL.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`苹果公司(AAPL.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}
## 
## # pdf output
## # Display AAPL signal results (EMA signals)
## kbl(ema_signal_result_pdf, booktabs = T, linesep = "", longtable = T,
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


## ----305-15, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# Display BBands signal results 
DT::datatable(bbands_signal_result, 
              colnames = c('Date' = 2, 'Symbol' = 3, 'Adjusted' = 4,
                           'Signal 13' = 5, 'Signal 14' = 6, 
                           'Signal 15a' = 7, 'Signal 15' = 8),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'Bollinger Bands (BBands) Trading Signals'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), 
              extensions = 'Buttons') %>%
    formatRound(columns = 'Adjusted', digits = 2)  


## ----305-16, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----------
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


## ----305-17, eval=FALSE, fig.height=7, fig.width=10, message=FALSE, warning=FALSE, include=FALSE----
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


## ----305-18, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# plotly interactive plot
# BBands Candlestick+Volume Plot for AAPL with trading signals
# Prepare the dataset 

bbands_signal_plot_html<- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  select(date, symbol, adjusted, open, close, high, low, volume, BBands_20_down, 
         BBands_20_mavg, BBands_20_up, BBands_20_pctB, tsignal13, tsignal14,
         tsignal15a, tsignal15) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day_trend_color =  ifelse(open < adjusted, "lightgreen", "red"),
         prev_volume = lag(volume),
         prev_adjusted = lag(adjusted),
         prev_BBands_20_down = lag(BBands_20_down),
         prev_BBands_20_mavg = lag(BBands_20_mavg),
         prev_BBands_20_up = lag(BBands_20_up),
         prev_BBands_20_pctB = lag(BBands_20_pctB),
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
  "Signal13 BUY" = "circle",
  "Signal13 SELL" = "circle-open",
  "Signal14 BUY" = "triangle-up",
  "Signal14 SELL" = "triangle-down-open",
  "Signal15a BUY" = "square",
  "Signal15a SELL" = "square-open",
  "Signal15 BUY" = "diamond",
  "Signal15 SELL" = "diamond-open"
)

bbands_signal_plot_html <- 
  bbands_signal_plot_html %>%
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

# Hover for BBands subplot
bbands_signal_plot_html <- 
  bbands_signal_plot_html %>%
  mutate(hover_text_bbands = paste(
      "<b>Symbol:</b>", symbol, "<br>", 
      "<b>Date:</b>", format(date, '%Y-%m-%d'), "<br>",
      "<b>Adjusted Close Price:</b> $", round(adjusted, 2), 
      "<span style='color:", 
      ifelse(adjusted > prev_adjusted, "darkgreen", "red"), ";'>",
      ifelse(adjusted > prev_adjusted, "▲", "▼"), "</span>", "<br>",
      "<b>BBands Down:</b>", round(BBands_20_down, 2),
      "<span style='color:", 
      ifelse(BBands_20_down > prev_BBands_20_down, "darkgreen", "deeppink"), ";'>",
      ifelse(BBands_20_down > prev_BBands_20_down, "▲", "▼"), "</span>", "<br>",
      "<b>BBands Trend:</b>", round(BBands_20_mavg, 2),
      "<span style='color:", 
      ifelse(BBands_20_mavg > prev_BBands_20_mavg, "darkgreen", "deeppink"), ";'>",
      ifelse(BBands_20_mavg > prev_BBands_20_mavg, "▲", "▼"), "</span>", "<br>",
      "<b>BBands Up</b>", round(BBands_20_up, 2),
      "<span style='color:", 
      ifelse(BBands_20_up > prev_BBands_20_up, "darkgreen", "deeppink"), ";'>",
      ifelse(BBands_20_up > prev_BBands_20_up, "▲", "▼"), "</span>", "<br>",
      "<b>BBands Price Location</b>", round(BBands_20_pctB, 2),
      "<span style='color:", 
      ifelse(BBands_20_pctB > prev_BBands_20_pctB, "darkgreen", "deeppink"), ";'>",
      ifelse(BBands_20_pctB > prev_BBands_20_pctB, "▲", "▼"), "</span>", "<br>"
    )
  )


## ----305-19, echo=FALSE, fig.height=7, fig.width=12, message=FALSE, warning=FALSE----
# html output
# plotly interactive plot
# BBands Price Candlestick Plot for AAPL with trading signals

# Candlestick Plot with BBands Trading Signals
fig <- plot_ly(data = bbands_signal_plot_html, type = "candlestick", 
               x = ~date, open = ~open, close = ~adjusted, high = ~high, low = ~low,
               increasing = list(line = list(color = "darkgreen", width = 10), 
                                 fillcolor = "darkgreen"),
               decreasing = list(line = list(color = "red", width = 10), 
                                 fillcolor = "red"), 
               hoverinfo = "text", text = ~hover_text, showlegend = FALSE) %>%
  add_lines(y = ~BBands_20_up, name = "BBands Up", 
            line = list(color = "red", dash = "dash", width = 3), showlegend = TRUE,
            hoverinfo = "y+text", 
            text = ~hover_text_bbands) %>%
  add_lines(y = ~BBands_20_mavg, name = "BBands Trend Line", 
            line = list(color = "darkgrey", width = 3),  showlegend = TRUE,
            hoverinfo = "y+text", 
            text = ~hover_text_bbands ) %>%
  add_lines(y = ~BBands_20_down, name = "BBands Down", 
            line = list(color = "red", dash = "dash", width = 3), showlegend = TRUE,
            hoverinfo = "y+text", 
            text = ~hover_text_bbands) %>% 
  add_markers(data = filter(bbands_signal_plot_html, tsignal13 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal13 BUY"]]), 
              name = "Signal13 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal13 BUY</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal13 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal13 SELL"]]), 
              name = "Signal13 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal13 SELL</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal14 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal14 BUY"]]), 
              name = "Signal14 BUY",  showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal14 BUY</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal14 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal14 SELL"]]), 
              name = "Signal14 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal14 SELL</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal15a == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal15a BUY"]]), 
              name = "Signal15a BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal15a BUY</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal15a == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal15a SELL"]]), 
              name = "Signal15a SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
              "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal15a SELL</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal15 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal15 BUY"]]), 
              name = "Signal15 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal15 BUY</b><extra></extra>") %>%
  add_markers(data = filter(bbands_signal_plot_html, tsignal15 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal15 SELL"]]), 
              name = "Signal15 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
              "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal15 SELL</b><extra></extra>") %>%
  
  layout(title = list(text = "Interactive Price Candlestick Plot for AAPL.US \
                      with Bollinger Bands (BBands)  and Trading Signals",
                      x = 0.5,  
                      y = 1.2,  
                      xanchor = 'center',
                      yanchor = 'top'),
         xaxis = list(title = "Date", 
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
         yaxis = list(title = "Stock Price, in USD", fixedrange = FALSE),
         legend = list(orientation = "h",x = 0, y = -0.6, 
                       xanchor = "left", yanchor = "top", traceorder = "normal"),
         margin = list(t = 100, b = 200),
         width = 800, height = 650)

fig


## ----305-20, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----------
## 
## # NOT FINISHED
## 
## # html output
## # plotly interactive plot
## # EMA Volume Plot for AAPL with trading signals
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

