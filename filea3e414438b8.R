## ----302-98, message=FALSE, warning=FALSE, include=FALSE----------------------
# Define function to read another rmd file
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')

  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)

  envir <- globalenv()
  source(tempR, local = envir, ...)
}


## ----302-99, message=FALSE, warning=FALSE, include=FALSE----------------------
# Read in base model construction rmd to continue with 3.2
source_rmd("03_Technical-Indicators-Construction.Rmd")


## ----302-1, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# Define a function to compute the value for EMA using the adjusted close price with parameter settings of n days 

EMA_comput <- function(price, n){
  # Ensure ema has the same length as price and initially filled with NAs
  ema <- rep(NA, length(price))

  if (length(price) >= n) {  # Only compute EMA if enough data points are available
    ema[n] <- mean(price[1:n])
    beta <- 2 / (n + 1)

    for (i in (n + 1):length(price)){
      ema[i] <- beta * price[i] + (1 - beta) * ema[i - 1]
    }
  }

  return(ema)
}



## ----302-2, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Compute EMA for all stocks, Full Data
mega7data_modified <-
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(EMA5 = EMA_comput(adjusted, 5), 
         EMA8 = EMA_comput(adjusted,8),
         EMA13 = EMA_comput(adjusted,13),
         EMA20 = EMA_comput(adjusted,20),
         EMA50 = EMA_comput(adjusted,50),
         EMA200 = EMA_comput(adjusted,200))


## ----302-3, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Alternative way: 
# Below utilizes the `TTR` package directly for EMA computations: EMA()
# AAPL <- xts(ema_value_result_pdf$adjusted,ema_value_result_pdf$date)
# EMA200 <- EMA(AAPL,n = 200) 


## ----302-4, message=FALSE, warning=FALSE, include=FALSE-----------------------
ema_value_result <- 
  mega7data_modified %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted, EMA5, EMA8, EMA13, 
         EMA20, EMA50, EMA200, volume) %>%
  arrange(desc(date)) %>%
  head(30000)

ema_value_result_pdf <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.Date(date)) %>%
  select(date, adjusted, EMA5, EMA8, EMA13, 
         EMA20, EMA50, EMA200, volume) %>%
  arrange(desc(date))


## ----302-5, message=FALSE, warning=FALSE, include=FALSE-----------------------
# ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}

# ##### [Individual stock outputs are suppressed, only `TSLA.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`特斯拉(TSLA.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}

# pdf output
# Display TSLA output (EMA values)
kbl(head(ema_value_result_pdf,10), booktabs = T, linesep = '', digits = 1,
    caption = "EMA Computations for TSLA.US",
    format.args = list(big.mark = ",", scientific = FALSE),
    col.names = c("Symbol","Date","Adj Close","EMA5","EMA8",
                  "EMA13","EMA20","EMA50","EMA200","Volume")) %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center") %>%
  column_spec(c(1,4:9), bold = T) %>%
  kableExtra::footnote(general = "Output 3.2.a: Latest 10 rows of data are shown.")


## ----302-6, echo=FALSE, message=FALSE, warning=FALSE--------------------------
# html output
# Display EMA values
DT::datatable(ema_value_result, 
              colnames = c('Symbol' = 2, 'Date' = 3, 'Adjusted' = 4, 'Volume' = 11),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'EMA Computations'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), extensions = 'Buttons') %>%
  formatRound(columns = c('Adjusted','EMA5','EMA8','EMA13','EMA20','EMA50','EMA200'),
              digits = 2)  %>%
  formatRound(columns = 'Volume', digits = 0)


## ----302-7, message=FALSE, warning=FALSE, include=FALSE-----------------------
# Data manipulation for EMA plot AAPL.US
ema_aapl_xts <- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  mutate(date = as.POSIXct(date)) %>%
  select(date, open, high, low, close, volume, adjusted) 

open <- ema_aapl_xts$open
high <- ema_aapl_xts$high
low <- ema_aapl_xts$low
close <- ema_aapl_xts$close
adjusted <- ema_aapl_xts$adjusted
volume <- as.numeric(ema_aapl_xts$volume)

ema_aapl_xts <- xts(cbind(open, high, low, close, volume, adjusted),
                   as.POSIXct(ema_aapl_xts$date))
ema_aapl_xts <- last(ema_aapl_xts,'365 days')


## ----302-8, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## ##### AAPL Plot include EMA5, EMA8, and EMA13 using `TTR` package {-}
## # pdf plot
## # EMA plot AAPL.US p.1 EMA5 EMA8 EMA13
## chartSeries(ema_aapl_xts,
##             type = "candlesticks",
##             theme = chartTheme('black'))
## 
## addEMA(n = c(5, 8, 13), on = 1,
##        col = c("white","yellow", "purple"), overlay = TRUE)


## ----302-9, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-----------
## ##### AAPL Plot include EMA50, and EMA200
## # pdf plot
## # EMA plot TLSA.US p.2 SMA50 SMA200
## chartSeries(ema_aapl_xts,
##             type="candlesticks",
##             theme = chartTheme('black'))
## 
## addEMA(n = c(50,200), on = 1,
##        col = c("grey","blue"), overlay = TRUE)


## ----302-10, message=FALSE, warning=FALSE, include=FALSE----------------------
# EMA Trading Signal 4: Price > EMA20
# EMA Trading Rule setup and Generate Signals 
mega7data_modified <-
  mega7data_modified %>%
  mutate(tsignal4 = case_when(EMA20 < adjusted ~ 'BUY',
                              EMA20 >= adjusted ~ 'SELL'))


## ----302-11, message=FALSE, warning=FALSE, include=FALSE----------------------
# EMA Trading Signal 5: EMA50 > EMA200
# EMA Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal5 = case_when(
    lag(EMA50) < lag(EMA200) & EMA50 > EMA200 ~ 'BUY',
    lag(EMA50) >= lag(EMA200) & EMA50 < EMA200 ~ 'SELL',
    lag(EMA50) < lag(EMA200) & EMA50 <= EMA200 ~ 'HOLD',
    lag(EMA50) >= lag(EMA200) & EMA50 >= EMA200 ~ 'HOLD'))


## ----302-12, message=FALSE, warning=FALSE, include=FALSE----------------------
# EMA Trading Signal 6a: EMA5 > EMA8
# EMA Trading Signal 6b: EMA8 > EMA13
# EMA Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  group_by(symbol) %>%
  mutate(tsignal6a = case_when(
    lag(EMA5) < lag(EMA8) & EMA5 > EMA8 ~ 'BUY',
    lag(EMA5) >= lag(EMA8) & EMA5 < EMA8 ~ 'SELL',
    lag(EMA5) < lag(EMA8) & EMA5 <= EMA8 ~ 'HOLD',
    lag(EMA5) >= lag(EMA8) & EMA5 >= EMA8 ~ 'HOLD')) %>%
  mutate(tsignal6b = case_when(
    lag(EMA8) < lag(EMA13) & EMA8 > EMA13 ~ 'BUY',
    lag(EMA8) >= lag(EMA13) & EMA8 < EMA13 ~ 'SELL',
    lag(EMA8) < lag(EMA13) & EMA8 <= EMA13 ~ 'HOLD',
    lag(EMA8) >= lag(EMA13) & EMA8 >= EMA13 ~ 'HOLD'))


## ----302-13, message=FALSE, warning=FALSE, include=FALSE----------------------
# EMA Trading Signal 6: only when 6a & 6b are the SAME (simultaneously)
# EMA Trading Rule setup and Generate Signals 
mega7data_modified <- 
  mega7data_modified %>%
  mutate(tsignal6 = case_when(
    tsignal6a == 'BUY' & tsignal6b == 'BUY' ~ 'BUY',
    tsignal6a == 'BUY' & tsignal6b == 'SELL' ~ 'HOLD',
    tsignal6a == 'BUY' & tsignal6b == 'HOLD' ~ 'HOLD',
    tsignal6a == 'HOLD' & tsignal6b == 'HOLD' ~ 'HOLD',
    tsignal6a == 'HOLD' & tsignal6b == 'BUY' ~ 'HOLD',
    tsignal6a == 'HOLD' & tsignal6b == 'SELL' ~ 'HOLD',
    tsignal6a == 'SELL' & tsignal6b == 'BUY' ~ 'HOLD',
    tsignal6a == 'SELL' & tsignal6b == 'SELL' ~ 'SELL',
    tsignal6a == 'SELL' & tsignal6b == 'HOLD' ~ 'HOLD')) 


## ----302-14, message=FALSE, warning=FALSE, include=FALSE, paged.print=TRUE----
# Prepare data frame EMA signal results
ema_signal_result <-
  mega7data_modified %>%
  select(date, symbol, adjusted, tsignal4, tsignal5, 
         tsignal6, tsignal6a, tsignal6b) %>%
  arrange(desc(date)) %>%
  head(30000)

# For TSLA signals results (EMA)
ema_signal_result_pdf <-
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  ungroup() %>%
  select(date, symbol, adjusted, tsignal4, tsignal5, 
         tsignal6, tsignal6a, tsignal6b) %>%
  arrange(desc(date)) %>%
  head(10)


## ----302-15, eval=FALSE, fig.pos='H', message=FALSE, warning=FALSE, include=FALSE----
## # ##### [Code Hidden 代码已隐藏]{style="color:red"} {-}
## 
## # ##### [Individual stock outputs are suppressed, only `TSLA.US` related results are shown for illustration purposes. Refer to the *webapp* or the `html` version of the *e-Manual* for complete outputs. 个股输出数据及图表已隐藏，报告仅展示`特斯拉(TSLA.US)`的相关结果,完整图表可参阅*webapp* 或网页版*Global MAS 量化模型搭建说明*。]{style="color:red"} {-}
## 
## # pdf output
## # Display TSLA signal results (EMA signals)
## kbl(ema_signal_result_pdf, booktabs = T, linesep = "", longtable = T,
##     digits = 1, caption = "EMA Trading Signals for TSLA.US",
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


## ----302-16, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# Display EMA signal results 
DT::datatable(ema_signal_result, 
              colnames = c('Date' = 2, 'Symbol' = 3, 'Adjusted' = 4),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'EMA Trading Signals'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), extensions = 'Buttons') %>%
    formatRound(columns = 'Adjusted', digits = 2)  


## ----302-17, message=FALSE, warning=FALSE, include=FALSE----------------------
# pdf output
# EMA Candlestick+Volume Plot for AAPL with trading signals
# Prepare the dataset for candlestick components
ema_signal_plot_pdf<- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  select(date, symbol, adjusted, open, close, high, low, volume, EMA5, 
         EMA8, EMA13, EMA20, EMA50, EMA200, tsignal4, tsignal5, tsignal6) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day_trend_color = ifelse(open - adjusted > 0, "red", "green")) %>%
  filter(as.Date(date) > as.Date(Sys.Date()-months(12)))


## ----302-18, eval=FALSE, fig.height=7, fig.width=10, message=FALSE, warning=FALSE, include=FALSE----
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
##     labs(title = paste0("Price Volume Candlestick Plot for AAPL.US \
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


## ----302-19, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# plotly interactive plot
# EMA Candlestick+Volume Plot for AAPL with trading signals
# Prepare the dataset 

ema_signal_plot_html<- 
  mega7data_modified %>%
  slice(which(symbol == "AAPL")) %>%
  select(date, symbol, adjusted, open, close, high, low, volume, EMA5, 
         EMA8, EMA13, EMA20, EMA50, EMA200, tsignal4, tsignal5, tsignal6) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day_trend_color =  ifelse(open < adjusted, "lightgreen", "red"),
         prev_volume = lag(volume),
         prev_adjusted = lag(adjusted),
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
  "Signal4 BUY" = "circle",
  "Signal4 SELL" = "circle-open",
  "Signal5 BUY" = "triangle-up",
  "Signal5 SELL" = "triangle-down-open",
  "Signal6 BUY" = "diamond",
  "Signal6 SELL" = "diamond-open"
)

ema_signal_plot_html <- ema_signal_plot_html %>%
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


## ----302-20, echo=FALSE, fig.height=7, fig.width=12, message=FALSE, warning=FALSE----
# html output
# plotly interactive plot
# EMA Price Candlestick Plot for AAPL with trading signals

# Candlestick Plot with EMA Trading Signals
fig <- plot_ly(data = ema_signal_plot_html, type = "candlestick", 
               x = ~date, open = ~open, close = ~adjusted, high = ~high, low = ~low,
               increasing = list(line = list(color = "darkgreen", width = 10), 
                                 fillcolor = "darkgreen"),
               decreasing = list(line = list(color = "red", width = 10), 
                                 fillcolor = "red"), 
               hoverinfo = "text", text = ~hover_text, showlegend = FALSE) %>%
  add_lines(y = ~EMA5, name = "EMA5", line = list(color = "black"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>EMA5:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~EMA8, name = "EMA8", line = list(color = "yellow"),  showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>EMA8:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~EMA13, name = "EMA13", line = list(color = "purple"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>EMA13:</b> %{y:.2f}<extra></extra>")) %>% 
  add_lines(y = ~EMA20, name = "EMA20", line = list(color = "red"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>EMA20:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~EMA50, name = "EMA50", line = list(color = "grey"),  showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>EMA50:</b> %{y:.2f}<extra></extra>")) %>%
  add_lines(y = ~EMA200, name = "EMA200", line = list(color = "blue"), showlegend = TRUE,
            hoverinfo = "y+text", 
            hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>EMA200:</b> %{y:.2f}<extra></extra>")) %>%
  add_markers(data = filter(ema_signal_plot_html, tsignal4 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal4 BUY"]]), 
              name = "Signal4 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal4 BUY</b><extra></extra>") %>%
  add_markers(data = filter(ema_signal_plot_html, tsignal4 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal4 SELL"]]), 
              name = "Signal4 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal4 SELL</b><extra></extra>") %>%
  add_markers(data = filter(ema_signal_plot_html, tsignal5 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal5 BUY"]]), 
              name = "Signal5 BUY",  showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal5 BUY</b><extra></extra>") %>%
  add_markers(data = filter(ema_signal_plot_html, tsignal5 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal5 SELL"]]), 
              name = "Signal5 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal5 SELL</b><extra></extra>") %>%
  add_markers(data = filter(ema_signal_plot_html, tsignal6 == "BUY"), 
              x = ~date, y = ~high, 
              marker = list(color = "lightgreen", size = 6,
                            symbol = signal_shapes[["Signal6 BUY"]]), 
              name = "Signal6 BUY", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal6 BUY</b><extra></extra>") %>%
  add_markers(data = filter(ema_signal_plot_html, tsignal6 == "SELL"), 
              x = ~date, y = ~low, 
              marker = list(color = "pink", size = 6,
                            symbol = signal_shapes[["Signal6 SELL"]]), 
              name = "Signal6 SELL", showlegend = TRUE,
              hoverinfo = "text",
              hovertemplate = 
                "<b>Date:</b> %{x|%Y-%m-%d}<br><b>Signal6 SELL</b><extra></extra>") %>%
  layout(title = list(text = "Interactive Price Candlestick Plot for AAPL.US with EMA Lines and Trading Signals",
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


## ----302-21, echo=FALSE, message=FALSE, warning=FALSE-------------------------
# html output
# plotly interactive plot
# EMA Volume Plot for AAPL with trading signals
# Define signal colors and shapes for trading signals

volume_plot <- plot_ly(data = ema_signal_plot_html, 
                       x = ~date, y = ~volume, type = 'bar', 
                       marker = list(color = ~volume_color),
                       hoverinfo = 'text', text = ~hover_text_volume,
                       name = "Trading Volume") %>%
  layout(title = list(text = "Interactive Daily Trading Volume Plot for AAPL.US \
                      with EMA Lines and Trading Signals", 
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
         yaxis2 = list(title = "Daily EMA Values", 
                       overlaying = "y", side = "right", 
                       showgrid = FALSE,titlefont = list(size = 14)),
         barmode = 'group',
         legend = list(orientation = "h", x = 0.5, y = -0.4, 
                       xanchor = "center", yanchor = "top"),
         margin = list(l = 40, r = 40,b = 100, t = 100, pad = 4), height = 450)

vol_signal_colors <- c("BUY" = "lightgreen", "SELL" = "pink")
signals <- c("tsignal4", "tsignal5", "tsignal6")

for (sig in signals) {
  volume_plot <- volume_plot %>%
    add_markers(data = ema_signal_plot_html %>% 
                  filter(get(sig) == "BUY"), 
                x = ~date, y = ~volume, 
                marker = list(color = vol_signal_colors[["BUY"]], 
                              size = 10, symbol = "circle"),
                name = paste(sig, "BUY"),
                hoverinfo = "text",
                hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>", 
                                      sig, "BUY</b><extra></extra>")) %>%
    add_markers(data = ema_signal_plot_html %>% filter(get(sig) == "SELL"), 
                x = ~date, y = ~volume, 
                marker = list(color = vol_signal_colors[["SELL"]], 
                              size = 10, symbol = "x"),
                name = paste(sig, "SELL"),
                hoverinfo = "text",
                hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>", 
                                      sig, "SELL</b><extra></extra>"))
}

ema_lines <- c("EMA5", "EMA8", "EMA13", "EMA20", "EMA50", "EMA200")

ema_colors <- c("EMA5" = "black", "EMA8" = "yellow",
                "EMA13" = "purple", "EMA20" = "red", 
                "EMA50" = "grey", "EMA200" = "blue")

for (ema in ema_lines) {
  volume_plot <- 
    volume_plot %>%
    add_lines(data = ema_signal_plot_html, 
          x = ~date, y = as.formula(paste0("~", ema)), name = ema, 
              line = list(color = ema_colors[ema]), yaxis = "y2",
              hoverinfo = "y+text", 
              hovertemplate = paste("<b>Date:</b> %{x|%Y-%m-%d}<br><b>", 
                                    ema, ":</b> %{y:.2f}<extra></extra>"))
}

volume_plot

