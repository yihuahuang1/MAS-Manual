## ----eval=FALSE, include=FALSE------------------------------------------------
## <span style="color:red;font-weight:700;font-size:28px">DEMO ONLY: Internal Version</span>
## <span style="color:red;font-weight:700;font-size:28px">内部版本 请勿外传</span>


## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE------------------
## # Clean the environment
## rm(list = ls())


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
### Load packages
library(tsibble)
library(lubridate)
library(tidyverse)
library(quantmod)
library(rvest) 
library(xts)
library(TTR)
library(blotter)
library(rvest)
library(quantstrat)
library(zoo)
library(knitr)
library(kableExtra)
library(readxl)
library(plotly)
library(ggplot2)
library(shiny)
library(DT)
library(flextable)
library(htmltools)
library(readr)
library(cowplot)
library(ggpubr)
library(htmltools)
library(stringr)
library(reshape2)
library(lubridate)
library(purrr)


## ----02flow, echo=FALSE, message=FALSE, warning=FALSE-------------------------
knitr::include_graphics("stock_model_flow.png")


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
# Data Intake
#mega7tidyurl <- url("https://gitee.com/yihuahuang1/stockdata/raw/master/mega7tidy.csv")
#mega7capurl <- url("https://gitee.com/yihuahuang1/stockdata/raw/master/mega7marketcap.csv")
#mega7data = read.csv(mega7tidyurl)
#mega7mcap = read.csv(mega7capurl)
mega7data = read.csv("sp500ohlc_long_clean.csv")
mega7mcap = read.csv("mega7marketcap.csv")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data <-
  mega7data %>%
  select(date, symbol, adjusted, close, high, low, open, volume) 





## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
## # Full data part data intake for loop
## 
## # Web-scrape SP500 stock list
## sp_500_stock_list <-
##  read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
##   html_node("table.wikitable") %>%
##   html_table() %>%
##   select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
##   as_tibble()
## # Format names
## names(sp_500_stock_list) <-
##   sp_500_stock_list %>%
##   names() %>%
##   str_to_lower() %>%
##   make.names()
## # Show results
## sp_500_stock_list


## ----include=FALSE------------------------------------------------------------
# Full data part data intake for loop 


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
# Data cleanup & add marketcap column to the main dataframe
mega7data_modified <- 
  mega7data %>%
  inner_join(mega7mcap, join_by(symbol == Stock)) %>%
  rename(marketcap = Marketcap) 

mega7data_modified$marketcap <- as.numeric(mega7data_modified$marketcap)

mega7data_modified <-
  mega7data_modified %>%
  select(-X)


## ----echo=FALSE, fig.pos='H', message=FALSE, warning=FALSE--------------------
# pdf output
kbl(tail(mega7data_modified,10), booktabs = T, linesep = "", 
    digits = 1, caption = "Cleaned Data Glance: NYSE and NASDAQ Stocks", 
    format.args = list(big.mark = ",", scientific = FALSE)) %>%
  kable_styling(latex_options = c("striped","scale_down",
                                  "HOLD_position"), 
                position = "center") %>%
  column_spec(c(2,3,8,9), bold = T) %>%
  kableExtra::footnote(general = "Output 2.1.a: Latest 10 rows are shown.")


## ----echo=FALSE, fig.pos='H', message=FALSE, warning=FALSE--------------------
# html output

# Apply comma formatting to "Average Volume" column without change dataset

mega7data_modified_display <- 
  mega7data_modified %>%
  mutate(date = mdy(date)) %>%
  tail(10000)

# mega7data_modified_display$volume <- format(mega7data_modified_display$volume, big.mark = ",")
# mega7data_modified_display$marketcap <- format(mega7data_modified_display$marketcap, big.mark = ",", scientific = FALSE)

# Create the interactive table using DT
DT::datatable(mega7data_modified_display, 
              colnames = c('Date' = 2, 'Symbol' = 3, 'Adjusted Close' = 4, 'Close' = 5,
                           'High' = 6, 'Low' = 7, 'Open' = 8, 'Volume' = 9, 'Market Cap' = 10),
          caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; 
                                            color: black; 
                                            font-size: 200%;', 
                                            'Cleaned Data Glance: NYSE and NASDAQ Stocks'),
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 15, 20),
            searching = TRUE)) %>%
  formatRound(columns = c('Open', 'Close', 'Adjusted Close',
                          'Close','High','Low','Open'), digits = 2) %>%
  formatRound(columns = c('Volume','Market Cap'), digits = 0)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
cap_remove_list_intermed <-
  mega7data_modified %>%
  filter(marketcap < 7000) 

cap_remove_list <- 
  pull(cap_remove_list_intermed, symbol)  %>%
  unique()


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data_modified <- 
  mega7data_modified %>%
  filter(!symbol %in% cap_remove_list)

# print(cap_remove_list)

kbl(cap_remove_list, booktabs = T, linesep = "", 
    col.names = "Symbol" , 
    caption = "Stocks Removed by Market Cap") %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center", full_width = T) %>%
  #column_spec(c(2), bold = T) %>%
  kableExtra::footnote(general = "Output 2.2.1.a: Last 10 rows are shown.",
           symbol = "Stocks with market cap smaller than 7 billion USD")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data_modified <-
  mega7data %>% 
   mutate(date = mdy(date),  
         week = strftime(date, format = "%V"), 
         year = year(date))  


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.pos='H'---------------------
mega7data_modified <-
  mega7data_modified %>%
  group_by(year,week,symbol) %>%
  mutate(average.volume = mean(volume)) %>%
  ungroup() 


## ----include=FALSE------------------------------------------------------------
weekavg_volume <- 
  mega7data_modified %>%
  select(year, week, date, symbol, average.volume) %>%
  arrange(desc(date))


## ----fig.pos='H', message=FALSE, warning=FALSE, include=FALSE-----------------
# pdf output
kbl(head(weekavg_volume,10), booktabs = T, linesep = "",
    digits = 1, caption = "Average Weekly Trading Volume", 
    format.args = list(big.mark= ",",scientific = FALSE), longtable = T,
    col.names = c("Year","Week","Date","Symbol","Average Volume")) %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center") %>%
  column_spec(c(5), bold = T) %>%
  kableExtra::footnote(general = "Output 2.2.2.a: First 10 rows are shown.")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
# html output
# Apply comma formatting to "Average Volume" column without changing the date frame

weekavg_volume_display <- 
  weekavg_volume %>% 
  head(15000)

# weekavg_volume_display$average.volume <- format(weekavg_volume_display$average.volume, big.mark = ",")

# html output
# Create the interactive table using DT
DT::datatable(weekavg_volume_display, 
              colnames = c('Year' = 2, 'Week' = 3, 'Date' = 4, 'Symbol' = 5,
                           'Average Volume' = 6),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'Average Weekly Trading Volume'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), extensions = 'Buttons') %>%
  formatRound(columns = 'Average Volume', digits = 0)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
volume_remove_list_intermed <-
  weekavg_volume %>%
  filter(average.volume < 20000) 

volume_remove_list <- 
  pull(volume_remove_list_intermed, symbol)  %>%
  unique()

volume_remove_list


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data_modified <- 
  mega7data_modified %>%
  filter(!symbol %in% volume_remove_list)

#print(volume_remove_list)

kbl(volume_remove_list, booktabs = T, linesep = "", 
    col.names = "Symbol" , 
    caption = "Stocks Removed by Trading Volume") %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center", full_width = T) %>%
  #column_spec(c(2), bold = T) %>%
  kableExtra::footnote(general = "Output 2.2.2.b: Last 10 rows are shown.",
           symbol = "Stocks with average weekly trading volume smaller than 1 million.")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data_modified <-
  mega7data_modified %>% 
  group_by(symbol) %>%
  mutate(price_pctchange = (adjusted/lag(adjusted)-1)*100)

pctchange_display <-
  mega7data_modified %>% 
  select(symbol,date,adjusted,price_pctchange) %>%
  arrange(desc(date)) %>%
  head(15000)


## ----eval=FALSE, fig.pos='H', message=FALSE, warning=FALSE, include=FALSE-----
## # pdf output
## kbl(head(pctchange_display,10), booktabs = T, linesep = "",
##     digits = 4, caption = "Price Percentage Change (1-day ROC)",
##     format.args = list(big.mark = ",", scientific = FALSE),
##     col.names = c("Symbol","Date","Adj Close","Price Change %")) %>%
##   kable_styling(latex_options = c("striped", "scale_down",
##                                   "HOLD_position"),
##                 position = "center") %>%
##   column_spec(c(3,4), bold = T) %>%
##   kableExtra::footnote(general = "Output 2.2.3.a: First 10 rows are shown.")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
# html output
# Create the interactive table using DT

DT::datatable(pctchange_display, 
              colnames = c('Symbol' = 2, 'Date' = 3, 'Adjusted Close' = 4, 
                           'Price Percent Change (1-day ROC)' = 5),
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color: black; font-size: 200%;', ' Price Percentage Change (1-day ROC)'),
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 15, 20),
            searching = TRUE)) %>% 
  formatRound(columns = 'Adjusted Close', digits = 2) %>%
  formatRound(columns = 'Price Percent Change (1-day ROC)', digits = 4)


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
prcpct_remove_list_intermed <-
  mega7data_modified %>%
  filter(price_pctchange < -20) 

prcpct_remove_list <- 
  pull(prcpct_remove_list_intermed, symbol)  %>%
  unique()
prcpct_remove_list <- c("ADM","AIG","AMAT","AMP","ANET","BA","BKR","BLDR","CAH",  "CFG", "COF", "COP") 


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data_modified <- 
  mega7data_modified %>%
  filter(!symbol %in% prcpct_remove_list)

#print(prcpct_remove_list)

kbl(prcpct_remove_list, booktabs = T, linesep = "", 
    col.names = "Symbol" , 
    caption = "Stocks Removed by Rate of Change (ROC)") %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center", full_width = T) %>%
  #column_spec(c(2), bold = T) %>%
  kableExtra::footnote(general = "Output 2.2.3.b: Last 10 rows are shown.",
           symbol = "Stocks with daily price change (%) more than -15%.")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
mega7data_modified <-
  mega7data_modified %>%
  mutate(annual_volatility = price_pctchange * sqrt(252)) 

annual_volatility_display <-
  mega7data_modified %>%
  select(date, symbol, adjusted, annual_volatility) %>%
  na.omit() %>%
  arrange(desc(date)) %>%
  head(15000)


## ----eval=FALSE, fig.pos='H', message=FALSE, warning=FALSE, include=FALSE-----
## # pdf output
## kbl(head(annual_volatility_display,10), booktabs = T, linesep = "",
##     digits = 4, caption = "Implied Annualized Volatility",
##     format.args = list(big.mark = ",", scientific = FALSE),
##     col.names = c("Date","Adj Close","Symbol","Annual Volatility %")) %>%
##   kable_styling(latex_options = c("striped", "scale_down",
##                                   "HOLD_position"),
##                 position = "center") %>%
##   column_spec(c(2,4), bold = T) %>%
##   kableExtra::footnote(general = "Output 2.2.4.a: First 10 rows are shown.")


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
# html output
# Create the interactive table using DT

DT::datatable(annual_volatility_display,
              colnames = c('Date' = 2, 'Symbol' = 3,'Adjusted Close' = 4,
                           'Annual Volatility' = 5),
          caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; 
                                            color: black; 
                                            font-size: 200%;', 'Implied Annualized Volatility'),
          options = list(
            pageLength = 10,
            lengthMenu = c(5, 10, 15, 20),
            searching = TRUE)) %>% 
  formatRound(columns = 'Adjusted Close', digits = 2) %>%
  formatRound(columns = 'Annual Volatility', digits = 3)


## ----echo=FALSE---------------------------------------------------------------
volat_remove_list_intermed <-
  mega7data_modified %>%
  filter(abs(annual_volatility) > 460) 


volat_remove_list <- 
  pull(volat_remove_list_intermed, symbol)  %>%
  unique()


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
mega7data_modified <- 
  mega7data_modified %>%
  filter(!symbol %in% volat_remove_list)

#print(volat_remove_list)

kbl(volat_remove_list, booktabs = T, linesep = "", 
    col.names = "Symbol" , 
    caption = "Stocks Removed by Implied Volatility") %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center", full_width = T) %>%
  column_spec(c(1), bold = T) %>%
  kableExtra::footnote(general = "Output 2.2.4.b: Last 10 rows are shown.",
           symbol = "Stocks with Implied Annualized Volatility(%) more than 100%.")


## ----echo=FALSE---------------------------------------------------------------
# html output
# Display SMA values
volat_remove_list_display <- data.frame(volat_remove_list)

DT::datatable(volat_remove_list_display, 
              colnames = c('Symbol' = 2),
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'Stocks with Implied Annualized Volatility(%) more than 250%'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '250%')), extensions = 'Buttons')



## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
# Combine the remove stock list from all the subj remove lists
cap_df <- data.frame(stock = cap_remove_list, origin = "Market Cap")
volume_df <- data.frame(stock = volume_remove_list, origin = "Average Volume")
prcpct_df <- data.frame(stock = prcpct_remove_list, origin = "Price ROC")
volat_df <- data.frame(stock = volat_remove_list, origin = "Volatility")

combined_df <- rbind(cap_df, volume_df, prcpct_df, volat_df)

result_df <- combined_df %>%
  group_by(stock) %>%
  summarise(origin = paste(unique(origin), collapse = ", "))

company_info = read.csv("sp500ohlc_long_clean_meta_2021.csv")


## ----include=FALSE------------------------------------------------------------
company_info_unique <- 
  company_info %>%
  group_by(symbol) %>%
  summarise(
    company_name = first(company_name),
    CUSIP = first(CUSIP),
    industry = first(industry),
    sector = first(sector)
  )

remove_stock_df <- 
  result_df %>%
  left_join(company_info_unique, by = c("stock" = "symbol"))

remove_stock_list_df <-
  remove_stock_df %>%
  rename('Symbol' = stock,
         'Remove by' = origin,
         'Company Name' = company_name,
         'Industry' = industry,
         'Sector' = 'sector') %>%
  select(Symbol, 'Remove by', 'Company Name', 'Industry','Sector','CUSIP')


## ----include=FALSE------------------------------------------------------------
# Export list for streamlit
write_csv(remove_stock_list_df,"remove_stock_list.csv")


## ----include=FALSE------------------------------------------------------------
kbl(tail(remove_stock_list_df,10), booktabs = T, linesep = "", 
    caption = "Stocks Removed by the Subjective Indicators") %>%
  kable_styling(latex_options = c("striped", "scale_down",
                                  "HOLD_position"), 
                position = "center", full_width = T) %>%
  column_spec(c(2,3), bold = T) %>%
  kableExtra::footnote(general = "Output 2.2.c: Last 10 rows are shown.",
           symbol = "In no means these removed stocks will bring negative investment returns, some of them may bring substantial (positive) future returns.")


## ----echo=FALSE---------------------------------------------------------------
# html output
# Display SMA values


DT::datatable(remove_stock_list_df, 
              caption = htmltools::tags$caption(style = 'caption-side: top; 
                                            text-align: center; color: black; 
                                            font-size: 200%;',
                                            'Stocks Removed by the Subjective Indicators'),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
              searching = TRUE, style = list(width = '100%')), extensions = 'Buttons')


