## ----eval=FALSE, include=FALSE------------------------------------------------
## <span style="color:red;font-weight:700;font-size:28px">DEMO ONLY: Internal Version</span>
## <span style="color:red;font-weight:700;font-size:28px">内部版本 请勿外传</span>


## ----300-1, echo=FALSE, message=FALSE, warning=FALSE--------------------------
knitr::include_graphics("tech_indicator_list.png")


## ----300-2, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Define function to read another rmd file
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')

  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)

  envir <- globalenv()
  source(tempR, local = envir, ...)
}


## ----300-3, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Read in base model construction rmd to continue with 3.1
source_rmd("02_Base-Model-Construction.Rmd")


## ----301-1, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Define a function to compute the value for SMA using the adjusted close price with parameter settings of n days 

SMA_comput <- function(price, n){
  sma <- rep(NA, length(price)) 
  if (length(price) >= n) {  # Only compute SMA if enough data points are available
    for (i in n:length(price)){
      sma[i] <- mean(price[(i-n+1):i])
    }
  }
  return(sma)
}


## ----301-2, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Compute SMA for all stocks
mega7data_modified <-
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(SMA5 = SMA_comput(adjusted, 5), 
         SMA8 = SMA_comput(adjusted,8),
         SMA13 = SMA_comput(adjusted,13),
         SMA20 = SMA_comput(adjusted,20),
         SMA50 = SMA_comput(adjusted,50),
         SMA200 = SMA_comput(adjusted,200))


## ----301-3, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Alternative way: 
# Below utilizes the `TTR` package directly for SMA computations: SMA()
# AAPL <- xts(sma_value_result_pdf$adjusted,sma_value_result_pdf$date)
# SMA20 <- SMA(AAPL,n = 20) 


## ----301-4, message=FALSE, warning=FALSE, include=FALSE-----------------------
sma_value_result <- 
  mega7data_modified %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted, SMA5, SMA8, SMA13, 
         SMA20, SMA50, SMA200, volume) %>%
  arrange(desc(date)) %>%
  head(30000)

sma_value_result_pdf <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted, SMA5, SMA8, SMA13, 
         SMA20, SMA50, SMA200, volume) %>%
  arrange(desc(date))


## ----301-5, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## # ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}
## 
## # ##### [Individual stock outputs are suppressed, only `TSLA.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`特斯拉(TSLA.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}
## 
## # pdf output
## # Display TSLA output (SMA values)
## kbl(head(sma_value_result_pdf,10), booktabs = T, linesep = '', digits = 1,
##     caption = "SMA Computations for AAPL.US",
##     format.args = list(big.mark = ",", scientific = FALSE),
##     col.names = c("Symbol","Date","Adj Close","SMA5","SMA8",
##                   "SMA13","SMA20","SMA50","SMA200","Volume")) %>%
##   kable_styling(latex_options = c("striped", "scale_down",
##                                   "HOLD_position"),
##                 position = "center") %>%
##   column_spec(c(1,4:9), bold = T) %>%
##   kableExtra::footnote(general = "Output 3.1.a: Latest 10 rows of data are shown.")


## ----301-6, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# html output
# Display SMA values
DT::datatable(sma_value_result, 
              colnames = c('Symbol' = 2, 'Date' = 3, 'Adjusted' = 4, 'Volume' = 11),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'SMA Computations'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), extensions = 'Buttons') %>%
  formatRound(columns = c('Adjusted','SMA5','SMA8','SMA13','SMA20','SMA50','SMA200'),
              digits = 2)  %>%
  formatRound(columns = 'Volume', digits = 0)


## ----301-7, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Data manipulation for SMA plot AAPL.US
sma_aapl_xts <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.POSIXct(date)) %>%
  select(date, open, high, low, close, volume, adjusted) 

open <- sma_aapl_xts$open
high <- sma_aapl_xts$high
low <- sma_aapl_xts$low
close <- sma_aapl_xts$close
adjusted <- sma_aapl_xts$adjusted
volume <- as.numeric(sma_aapl_xts$volume)

sma_aapl_xts <- xts(cbind(open, high, low, close, volume, adjusted),
                   as.POSIXct(sma_aapl_xts$date))
sma_aapl_xts <- last(sma_aapl_xts,'365 days')



## ----301-8, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## ##### AAPL Plot include SMA5, SMA8, and SMA13 using `TTR` package {-}
## # pdf plot
## # SMA plot AAPL.US p.1 SMA5 SMA8 SMA13
## chartSeries(sma_aapl_xts,
##             type = "candlesticks",
##             theme = chartTheme('black'))
## 
## addSMA(n = c(5, 8, 13), on = 1,
##        col = c("white","yellow", "purple"), overlay = TRUE)


## ----301-9, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## ##### AAPL Plot include SMA50, and SMA200
## # pdf plot
## # SMA plot AAPL.US p.2 SMA50 SMA200
## chartSeries(sma_aapl_xts,
##             type="candlesticks",
##             theme = chartTheme('black'))
## 
## addSMA(n = c(50,200), on = 1,
##        col = c("grey","blue"), overlay = TRUE)


## ----301-10, message=FALSE, warning=FALSE, include=FALSE----------------------
# SMA Trading Signal 1: Price > SMA20
# SMA Trading Rule setup and Generate Signals 
mega7data_modified <-
  mega7data_modified %>%
  mutate(tsignal1 = case_when(SMA20 < adjusted ~ 'BUY',
                              SMA20 >= adjusted ~ 'SELL'))


## ----301-11, message=FALSE, warning=FALSE, include=FALSE----------------------
# SMA Trading Signal 2: SMA50 > SMA200
# SMA Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal2 = case_when(
    lag(SMA50) < lag(SMA200) & SMA50 > SMA200 ~ 'BUY',
    lag(SMA50) >= lag(SMA200) & SMA50 < SMA200 ~ 'SELL',
    lag(SMA50) < lag(SMA200) & SMA50 <= SMA200 ~ 'HOLD',
    lag(SMA50) >= lag(SMA200) & SMA50 >= SMA200 ~ 'HOLD'))


## ----301-12, message=FALSE, warning=FALSE, include=FALSE----------------------
# SMA Trading Signal 3a: SMA5 > SMA8
# SMA Trading Signal 3b: SMA8 > SMA13
# SMA Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal3a = case_when(
    lag(SMA5) < lag(SMA8) & SMA5 > SMA8 ~ 'BUY',
    lag(SMA5) >= lag(SMA8) & SMA5 < SMA8 ~ 'SELL',
    lag(SMA5) < lag(SMA8) & SMA5 <= SMA8 ~ 'HOLD',
    lag(SMA5) >= lag(SMA8) & SMA5 >= SMA8 ~ 'HOLD')) %>%
  mutate(tsignal3b = case_when(
    lag(SMA8) < lag(SMA13) & SMA8 > SMA13 ~ 'BUY',
    lag(SMA8) >= lag(SMA13) & SMA8 < SMA13 ~ 'SELL',
    lag(SMA8) < lag(SMA13) & SMA8 <= SMA13 ~ 'HOLD',
    lag(SMA8) >= lag(SMA13) & SMA8 >= SMA13 ~ 'HOLD'))


## ----301-13, message=FALSE, warning=FALSE, include=FALSE----------------------
# SMA Trading Signal 3: only when 3a & 3b are the SAME (simultaneously)
# SMA Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  mutate(tsignal3 = case_when(
    tsignal3a == 'BUY' & tsignal3b == 'BUY' ~ 'BUY',
    tsignal3a == 'BUY' & tsignal3b == 'SELL' ~ 'HOLD',
    tsignal3a == 'BUY' & tsignal3b == 'HOLD' ~ 'HOLD',
    tsignal3a == 'HOLD' & tsignal3b == 'HOLD' ~ 'HOLD',
    tsignal3a == 'HOLD' & tsignal3b == 'BUY' ~ 'HOLD',
    tsignal3a == 'HOLD' & tsignal3b == 'SELL' ~ 'HOLD',
    tsignal3a == 'SELL' & tsignal3b == 'BUY' ~ 'HOLD',
    tsignal3a == 'SELL' & tsignal3b == 'SELL' ~ 'SELL',
    tsignal3a == 'SELL' & tsignal3b == 'HOLD' ~ 'HOLD')) 


## ----301-14, message=FALSE, warning=FALSE, include=FALSE, paged.print=TRUE----
# Prepare data frame SMA signal results
sma_signal_result <-
  mega7data_modified %>%
  select(date, symbol, adjusted, tsignal1, tsignal2, 
         tsignal3, tsignal3a, tsignal3b) %>%
  arrange(desc(date))

# For AAPL signals results (SMA)
sma_signal_result_pdf <-
  mega7data_modified %>%
  slice(which(symbol == "TSLA")) %>%
  ungroup() %>%
  select(date, symbol, adjusted, tsignal1, tsignal2, 
         tsignal3, tsignal3a, tsignal3b) %>%
  arrange(desc(date)) %>%
  head(10)


## ----301-15, eval=FALSE, fig.pos='H', message=FALSE, warning=FALSE, include=FALSE----
## # ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}
## 
## # ##### [Individual stock outputs are suppressed, only `TSLA.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`特斯拉(TSLA.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}
## 
## # pdf output
## # Display TSLA signal results (SMA signals)
## kbl(sma_signal_result_pdf, booktabs = T, linesep = "", longtable = T,
##     digits = 1, caption = "SMA Trading Signals for TSLA.US",
##     format.args = list(big.mark = ",", scientific = FALSE),
##     col.names = c("Date","Symbol","Adj Close","tsignal1","tsignal2",
##                   "tsignal3","tsignal3a","tsignal3b")) %>%
##   kable_styling(latex_options = c("striped", "scale_up",
##                                   "HOLD_position"),
##                 full_width = F) %>%
##   column_spec(c(2,4:6), bold = T) %>%
##   column_spec(1, width = "1.9cm") %>%
##   kableExtra::footnote(general = "Output 3.1.b: latest 10 rows of data are shown.",
##               number = "Includes tsignal1, tsignal2, tsignal3, tsignal3a and tsignal3b.")
## #图表3.1.b: SMA交易信号，展示最新的10行数据。包括`tsignal1`, `tsignal2`, `tsignal3`, #`tsignal3a`和`tsignal3b`。


## ----301-16, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# Display SMA signal results 
DT::datatable(head(sma_signal_result,30000), 
              colnames = c('Date' = 2, 'Symbol' = 3, 'Adjusted' = 4),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'SMA Trading Signals'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), extensions = 'Buttons') %>%
    formatRound(columns = 'Adjusted', digits = 2)  


## ----301-17, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# pdf output
# SMA Candlestick+Volume Plot for AAPL with trading signals
# Prepare the dataset for candlestick components
sma_signal_plot_pdf<- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  select(date, symbol, adjusted, open, close, high, low, volume, SMA5, 
         SMA8, SMA13, SMA20, SMA50, SMA200, tsignal1, tsignal2, tsignal3) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day_trend_color = ifelse(open - adjusted > 0, "red", "green")) %>%
  filter(as.Date(date) > as.Date(Sys.Date()-months(12)))


## ----301-18, eval=FALSE, fig.height=7, fig.width=10, message=FALSE, warning=FALSE, include=FALSE----
## # pdf output
## # SMA Candlestick+Volume Plot for AAPL with trading signals
## 
## # Candlestick Plot with SMA Trading Signals
## color_values <- c(
##   "SMA5" = "black",
##   "SMA8" = "yellow",
##   "SMA13" = "purple",
##   "SMA20" = "red",
##   "SMA50" = "grey",
##   "SMA200" = "blue",
##   "green" = "darkgreen",
##   "red" = "red",
##   "Signal1 BUY " = "lightgreen",
##   "Signal1 SELL" = "deeppink",
##   "Signal2 BUY" = "lightgreen",
##   "Signal2 SELL" = "deeppink",
##   "Signal3 BUY" = "lightgreen",
##   "Signal3 SELL" = "deeppink"
## )
## 
## candlestick <-
##   ggplot(sma_signal_plot_pdf, aes(x = date)) +
##   geom_segment(aes(xend = date, y = open,
##                    yend = adjusted, colour = day_trend_color), size = 5) +
##   geom_segment(aes(xend = date, y = high,
##                    yend = low, colour = day_trend_color)) +
##   geom_line(aes(y = SMA5, color = "SMA5"), size = 0.5) +
##   geom_line(aes(y = SMA8, color = "SMA8"), size = 0.5) +
##   geom_line(aes(y = SMA13, color = "SMA13"), size = 0.5) +
##   geom_line(aes(y = SMA20, color = "SMA20"), size = 0.5) +
##   geom_line(aes(y = SMA50, color = "SMA50"), size = 0.5) +
##   geom_line(aes(y = SMA200, color = "SMA200"), size = 0.5) +
## 
##   # Adding trading signals with different shapes and colors
##   geom_point(data = filter(sma_signal_plot_pdf, tsignal1 == "BUY"),
##              aes(x = date, y = high, color = "Signal1 BUY",
##                  shape = "Signal1 BUY"), size = 2) +
##   geom_point(data = filter(sma_signal_plot_pdf, tsignal1 == "SELL"),
##              aes(x = date, y = low, color = "Signal1 SELL",
##                  shape = "Signal1 SELL"), size = 2) +
##   geom_point(data = filter(sma_signal_plot_pdf, tsignal2 == "BUY"),
##              aes(x = date, y = high, color = "Signal2 BUY",
##                  shape = "Signal2 BUY"), size = 2) +
##   geom_point(data = filter(sma_signal_plot_pdf, tsignal2 == "SELL"),
##              aes(x = date, y = low, color = "Signal2 SELL",
##                  shape = "Signal2 SELL"), size = 2) +
##   geom_point(data = filter(sma_signal_plot_pdf, tsignal3 == "BUY"),
##              aes(x = date, y = high, color = "Signal3 BUY",
##                  shape = "Signal3 BUY"), size = 2) +
##   geom_point(data = filter(sma_signal_plot_pdf, tsignal3 == "SELL"),
##              aes(x = date, y = low, color = "Signal3 SELL",
##                  shape = "Signal3 SELL"), size = 2) +
##   theme(legend.position = "top",
##             guides(color = guide_legend(nrow = 4, byrow = TRUE,
##                                 title = "Indicators & Signals"),
##     shape = guide_legend(nrow = 4, byrow = TRUE, title = "Trade Signals")
##   )) +
##   scale_color_manual(name = "Indicators & Signals", values = color_values) +
##   scale_shape_manual(name = "Signal Shapes",
##                      values = c("Signal1 BUY" = 17, "Signal1 SELL" = 4,
##                                 "Signal2 BUY" = 18, "Signal2 SELL" = 3,
##                                 "Signal3 BUY" = 15, "Signal3 SELL" = 1)) +
##   labs(
##     y = "Stock Price, in USD", x = "Date") +
##     labs(title = paste0("Price Volume Candlestick Plot for AAPL.US \
##        with SMA Lines and Trading Signals")) +
##   theme_bw() +
##   scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")
## 
## # Volume Plot
## volume_plot <-
##   ggplot(sma_signal_plot_pdf, aes(x = date, y = volume, fill = day_trend_color)) +
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
## # Output 3.1.c: SMA plot with trading signals on Price and Volume, duration setting TTM.


## ----301-19, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# plotly interactive plot
# SMA Candlestick+Volume Plot for AAPL with trading signals
# Prepare the dataset 

sma_signal_plot_html<- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  select(date, symbol, adjusted, open, close, high, low, volume, SMA5, 
         SMA8, SMA13, SMA20, SMA50, SMA200, tsignal1, tsignal2, tsignal3) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day_trend_color =  ifelse(open < adjusted, "lightgreen", "red"),
         prev_adjusted = lag(adjusted),
         prev_volume = lag(volume),
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
  "Signal1 BUY" = "circle",
  "Signal1 SELL" = "circle-open",
  "Signal2 BUY" = "triangle-up",
  "Signal2 SELL" = "triangle-down-open",
  "Signal3 BUY" = "diamond",
  "Signal3 SELL" = "diamond-open"
)

sma_signal_plot_html <- sma_signal_plot_html %>%
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


## ----301-20, echo=FALSE, fig.height=7, fig.width=12, message=FALSE, warning=FALSE----
# html output
# plotly interactive plot
# SMA Price Candlestick Plot for AAPL with trading signals

# Candlestick Plot with SMA Trading Signals
fig <- plot_ly(data = sma_signal_plot_html, type = "candlestick", 
               x = ~date, open = ~open, close = ~adjusted, high = ~high, low = ~low,
               increasing = list(line = list(color = "darkgreen", width = 10), 
                                 fillcolor = "darkgreen"),
               decreasing = list(line = list(color = "red", width = 10), 
                                 fillcolor = "red"), 
               hoverinfo = "text", text = ~hover_text, showlegend = FALSE) %>%
  add_lines(y = ~SMA5, name = "SMA5", line = list(color = "black"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>SMA5:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~SMA8, name = "SMA8", line = list(color = "yellow"),  showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>SMA8:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~SMA13, name = "SMA13", line = list(color = "purple"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>SMA13:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~SMA20, name = "SMA20", line = list(color = "red"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>SMA20:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~SMA50, name = "SMA50", line = list(color = "grey"),  showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>SMA50:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~SMA200, name = "SMA200", line = list(color = "blue"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>SMA200:</b> %{y:.2f}<extra></extra>")) %>%
  add_markers(data = filter(sma_signal_plot_html, tsignal1 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal1 BUY"]]), 
              name = "Signal1 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal1 BUY</b><extra></extra>") %>%
  add_markers(data = filter(sma_signal_plot_html, tsignal1 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal1 SELL"]]), 
              name = "Signal1 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal1 SELL</b><extra></extra>") %>%
  add_markers(data = filter(sma_signal_plot_html, tsignal2 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal2 BUY"]]), 
              name = "Signal2 BUY",  showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal2 BUY</b><extra></extra>") %>%
  add_markers(data = filter(sma_signal_plot_html, tsignal2 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal2 SELL"]]), 
              name = "Signal2 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal2 SELL</b><extra></extra>") %>%
  add_markers(data = filter(sma_signal_plot_html, tsignal3 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal3 BUY"]]), 
              name = "Signal3 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal3 BUY</b><extra></extra>") %>%
  add_markers(data = filter(sma_signal_plot_html, tsignal3 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal3 SELL"]]), 
              name = "Signal3 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal3 SELL</b><extra></extra>") %>%
  layout(title = list(text = "Interactive Price Candlestick Plot for AAPL.US with SMA Lines and Trading Signals",
                      x = 0.5,  
                      y = 0.9,  
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
         width = 800, height = 600)

fig


## ----301-21, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# plotly interactive plot
# SMA Volume Plot for AAPL with trading signals
# Define signal colors and shapes for trading signals

volume_plot <- plot_ly(data = sma_signal_plot_html, 
                       x = ~date, y = ~volume, type = 'bar', 
                       marker = list(color = ~volume_color),
                       hoverinfo = 'text', text = ~hover_text_volume,
                       name = "Trading Volume") %>%
  layout(title = list(text = "Interactive Daily Trading Volume Plot for AAPL.US \
                      with SMA Lines and Trading Signals", 
                      x = 0.5, y = 0.95, 
                      xanchor = 'center', yanchor = 'top'),
         xaxis = list(title = "Date", type = "date", rangeslider = list(visible = TRUE),
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
         yaxis = list(title = "Daily Trading Volume", fixedrange = FALSE, side = "left"),
         yaxis2 = list(title = "Daily SMA Values", 
                       overlaying = "y", side = "right", 
                       showgrid = FALSE,titlefont = list(size = 14)),
         barmode = 'group',
         legend = list(orientation = "h", x = 0.5, y = -0.4, 
                       xanchor = "center", yanchor = "top"),
         margin = list(l = 40, r = 40,b = 100, t = 100, pad = 4), height = 450)

vol_signal_colors <- c("BUY" = "lightgreen", "SELL" = "pink")
signals <- c("tsignal1", "tsignal2", "tsignal3")

for (sig in signals) {
  volume_plot <- volume_plot %>%
    add_markers(data = sma_signal_plot_html %>% 
                  filter(get(sig) == "BUY"), 
                x = ~date, y = ~volume, 
                marker = list(color = vol_signal_colors[["BUY"]], 
                              size = 10, symbol = "circle"),
                name = paste(sig, "BUY"),
                hoverinfo = "text",
                hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>", 
                                      sig, "BUY</b><extra></extra>")) %>%
    add_markers(data = sma_signal_plot_html %>% filter(get(sig) == "SELL"), 
                x = ~date, y = ~volume, 
                marker = list(color = vol_signal_colors[["SELL"]], 
                              size = 10, symbol = "x"),
                name = paste(sig, "SELL"),
                hoverinfo = "text",
                hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>", 
                                      sig, "SELL</b><extra></extra>"))
}

sma_lines <- c("SMA5", "SMA8", "SMA13", "SMA20", "SMA50", "SMA200")

sma_colors <- c("SMA5" = "black", "SMA8" = "yellow",
                "SMA13" = "purple", "SMA20" = "red", 
                "SMA50" = "grey", "SMA200" = "blue")

for (sma in sma_lines) {
  volume_plot <- 
    volume_plot %>%
    add_lines(data = sma_signal_plot_html, 
          x = ~date, y = as.formula(paste0("~", sma)), name = sma, 
              line = list(color = sma_colors[sma]), yaxis = "y2",
              hoverinfo = "y", 
              hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>", 
                                    sma, ":</b> %{y:.2f}<extra></extra>"))
}

volume_plot


## ----302-0, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# Read in base model construction rmd to continue with 3.1
source_rmd("03_b_EMA.Rmd")

