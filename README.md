# Automated-trading-robot

**Automated Trading Bot Developed in R Using Shiny**

This trading bot performs automatic buy and sell operations. The user provides the desired stock or asset symbol at the beginning, and the bot monitors the market based on the following conditions:

**Buy Conditions:**

1. Spread is less than 1%
2. RSI (Relative Strength Index) is below 30
3. Bid volume is greater than ask volume
4. Current price is higher than the 10-minute moving average

If all the above conditions are met, the bot automatically places a **buy order**.

**Sell Conditions:**

1. Reaching a **profit target** (minimum of 3.5% gain)
2. Hitting a **stop-loss** (maximum of 1% loss)

The bot operates using data and trading APIs provided by **Nobitex**, and it is fully integrated with their system for real-time price tracking and order execution.

**Required Libraries:** shiny,httr,jsonlite,TTR,dplyr

---

## Helper functions
```R
get_market_data <- function(crypto) {
  tryCatch({
    res <- GET(paste0("https://api.nobitex.ir/v2/orderbook/", crypto, "IRT"))
    data <- fromJSON(content(res, "text"))
    list(
      last_price = mean(c(as.numeric(data$asks[[1]][1]), as.numeric(data$bids[[1]][1]))),
      best_ask = as.numeric(data$asks[[1]][1]),
      best_bid = as.numeric(data$bids[[1]][1])
    ) 
  }, error = function(e) {
    return(NULL)
  })
}

get_order_book <- function(crypto) {
  tryCatch({
    res <- GET(paste0("https://api.nobitex.ir/v2/orderbook/", crypto, "IRT"))
    data <- fromJSON(content(res, "text"))
    list(
      asks = data$asks,
      bids = data$bids
    )
  }, error = function(e) {
    return(NULL)
  })
}

calculate_ma_comparison <- function(crypto) {
  tryCatch({
    # Get recent transactions
    res <- GET(paste0("https://api.nobitex.ir/v2/trades/", crypto, "IRT?limit=1000"))
    data <- fromJSON(content(res, "text"))

    # Convert timestamps and prices
    trades <- data$trades %>%
      mutate(time = as.POSIXct(as.numeric(time), origin="1970-01-01"),
             price = as.numeric(price))

    # Filter transactions from the last 10 minutes
    ten_min_ago <- Sys.time() - 600
    recent_trades <- trades %>% filter(time >= ten_min_ago)
    
    if (nrow(recent_trades) > 0) {
      ma_10min <- mean(recent_trades$price, na.rm = TRUE)
      current_price <- tail(trades$price, 1)

      # Compare current price with the average
      comparison <- if(current_price > ma_10min) {
        "more"
      } else if(current_price < ma_10min) {
        "little"
      } else {
        "equale"
      }
      
      return(list(
        ma_10min = ma_10min,
        current_price = current_price,
        comparison = comparison,
        success = TRUE
      ))
    } else {
      return(list(success = FALSE))
    }
  }, error = function(e) {
    return(list(success = FALSE))
  })
}

calculate_ma_10min <- function(crypto) {
  tryCatch({
    res <- GET(paste0("https://api.nobitex.ir/v2/trades/", crypto, "IRT?limit=60"))
    data <- fromJSON(content(res, "text"))
    prices <- data$trades$price |> as.numeric()
    mean(prices, na.rm = TRUE)
  }, error = function(e) {
    return(NA)
  })
}

# Function to get market statistics from Nobitex
get_market_stats <- function(crypto, base) {
  tryCatch({
    res <- GET("https://api.nobitex.ir/market/stats")
    json <- content(res, "text")
    data <- fromJSON(json)
    
    pair <- paste0(tolower(crypto), "-", ifelse(tolower(base) == "irt", "rls", tolower(base)))
    
    if (!is.null(data$stats[[pair]])) {
      stats <- data$stats[[pair]]
      
      if (tolower(base) == "irt") {
        if (!is.null(stats$volumeDst)) {
          stats$volumeDst <- as.numeric(stats$volumeDst) / 10  
        }
      }
      
      return(stats)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}


# Function to get price from Nobitex (fallback to CryptoCompare if it fails)
get_price <- function(crypto, base) {
  tryCatch({
    pair <- paste0(toupper(crypto), toupper(base))
    res <- GET(paste0("https://api.nobitex.ir/v2/orderbook/", pair))
    data <- fromJSON(content(res, "text"))
    
    if (!is.null(data$asks) && !is.null(data$bids)) {
      best_ask <- as.numeric(data$asks[[1]][1]) 
      best_bid <- as.numeric(data$bids[[1]][1])  
      price <- (best_ask + best_bid) / 2  
      
      return(list(
        price = price,
        best_ask = best_ask,
        best_bid = best_bid,
        update_time = Sys.time()
      ))
    } else {
      stop("Invalid from Nobitex.")
    }
  }, error = function(e) {
    tryCatch({
      fsym <- toupper(crypto)
      tsym <- ifelse(toupper(base) == "irt", "IRR", toupper(base))  
      res <- GET(paste0("https://min-api.cryptocompare.com/data/price?fsym=", fsym, "&tsyms=", tsym))
      data <- fromJSON(content(res, "text"))
      
      if (!is.null(data[[tsym]])) {
        price <- as.numeric(data[[tsym]])
        if (toupper(base) == "irt") price <- price * 10  
        
        return(list(
          price = price,
          best_ask = price * 1.01,  
          best_bid = price * 0.99,   
          update_time = Sys.time()
        ))
      } else {
        stop("داده از CryptoCompare نامعتبر است.")
      }
    }, error = function(e) {
      return(NULL)  
    })
  })
}

# Function to get user wallet balance from Nobitex
get_user_wallet <- function(token) {
  tryCatch({
    res <- GET(
      "https://api.nobitex.ir/users/wallets/list",
      add_headers(Authorization = paste("Token", token))
    )
    data <- fromJSON(content(res, "text"))
    if (!is.null(data$wallets)) {
      return(data.frame(
        currency = data$wallets$currency,
        balance = data$wallets$balance
      ))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Function to get active wallets from Nobitex
get_active_wallets <- function(token) {
  tryCatch({
    res <- GET(
      "https://api.nobitex.ir/users/wallets/list",
      add_headers(Authorization = paste("Token", token))
    )
    data <- fromJSON(content(res, "text"))
    if (!is.null(data$wallets)) {
      return(data.frame(
        wallet = data$wallets$currency,
        status = ifelse(data$wallets$balance > 0, "active", "inactive")
      ))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

```
## User Interface
```R
ui <- fluidPage(
  titlePanel("Created by: Sajjad Gholinia"),
  titlePanel("Nobitex – Real-time Cryptocurrency Prices"),
  sidebarLayout(
    sidebarPanel(
      passwordInput("user_token", "Your Private API Token:", "", placeholder = "e.g. eyJhbGciOiJIUzI1NiIs..."),
      textInput("crypto", "Currency Name (English):", value = "BTC", placeholder = "e.g. BTC, ETH, DOGE..."),
      radioButtons("base", "Price Base:", choices = c("Toman (IRT)" = "irt", "Tether (USDT)" = "usdt"), selected = "irt"),
      numericInput("buy_percent", "Buy Percentage (e.g. 1 = 100%):", value = 0.8, min = 0.01, max = 1, step = 0.01),
      numericInput("sell_percent", "Sell Percentage (e.g. 1 = 100%):", value = 1, min = 0.01, max = 1, step = 0.01),
      radioButtons("show_wallet", "Would you like to see your wallet balance?",choices = c("No" = "no", "Yes" = "yes"), selected = "no"),
      checkboxInput("auto_trade", "Enable Auto-Trading", value = FALSE),
      actionButton("stop_trading", "Stop Trading", class = "btn-danger"),
      actionButton("refresh", "Update Price",class = "btn-primary")
    ),
    mainPanel(
      h3(textOutput("price_header")),
      verbatimTextOutput("price_output")
    )
  )
)
```
## Server
```R
server <- function(input, output) {
  price_data <- reactiveVal()
  market_stats <- reactiveVal()
  output_log <- reactiveVal("")
  refresh_count <- reactiveVal(0)
  last_key <- reactiveVal("")
  active_wallets_data <- reactiveVal(NULL)
  
  get_rsi_status <- function() {
    tryCatch({
      trades <- GET(paste0("https://api.nobitex.ir/v2/trades/", toupper(input$crypto), 
                           toupper(input$base), "?limit=100")) |> 
        content("text") |> 
        fromJSON()
      prices <- trades$trades$price |> as.numeric()
      rsi <- RSI(prices, n = 14)
      last_rsi <- tail(rsi, 1)
      rsi_text <- paste0("Latest RSI value:", round(last_rsi, 2))
      if (is.na(last_rsi)) {
        rsi_text <- paste0(rsi_text, "\nStatus: Calculation failed")
      } else if (last_rsi > 70) {
        rsi_text <- paste0(rsi_text, "\nStatus: Overbought – possible price drop")
      } else if (last_rsi < 30) {
        rsi_text <- paste0(rsi_text, "\nStatus: Oversold – possible price increase")
      } else {
        rsi_text <- paste0(rsi_text, "\nStatus: Normal")
      }
      return(rsi_text)
    }, error = function(e) {
      return("خطا در محاسبه RSI")
    })
  }
  
  observe({
    if (isTRUE(input$auto_trade)) {
      invalidateLater(30000)
      
      crypto <- input$crypto
      buy_percent <- input$buy_percent
      sell_percent <- input$sell_percent
      
      # Market data retrieval  
      market_data <- get_market_data(crypto)
      if (is.null(market_data)) return()
      
      order_book <- get_order_book(crypto)
      if (is.null(order_book)) return()
      
      rsi <- calculate_rsi(crypto)
      if (is.na(rsi)) return()
      
      ma_10min <- calculate_ma_10min(crypto)
      if (is.na(ma_10min)) return()
      
      
      # Demand and supply volume calculation  
      bids_volume <- if (!is.null(order_book$bids)) sum(as.numeric(order_book$bids[,2])) else 0
      asks_volume <- if (!is.null(order_book$asks)) sum(as.numeric(order_book$asks[,2])) else 0
      
      # Spread calculation  
      if (!is.null(market_data$best_ask) && !is.null(market_data$best_bid) && market_data$best_bid != 0) {
        spread <- (market_data$best_ask - market_data$best_bid) / market_data$best_bid * 100
      } else {
        spread <- NA
      }
      
      
      # Check all buy conditions  
      if (!is.na(spread) && spread < 1 && 
          !is.na(rsi) && rsi < 30 && 
          bids_volume > asks_volume && 
          !is.na(ma_10min) && ma_10min < market_data$last_price) {
        
        # Retrieve user balance using token  
        if (nzchar(input$user_token)) {
          wallet <- get_user_wallet(input$user_token)
          if (!is.null(wallet)) {
            irt_balance <- as.numeric(wallet$balance[wallet$currency == "IRT"])
            if (length(irt_balance) > 0 && !is.na(irt_balance)) {
              amount_to_buy <- (irt_balance * buy_percent) / market_data$last_price
              # نمایش پیام (بدون اجرای واقعی معامله)
              showNotification(paste("شرایط خرید فراهم شد. مقدار پیشنهادی:", round(amount_to_buy, 6)), type = "message")
            }
          }
        }
      }
    }
  })
  
  # Take-profit / stop-loss check function  
  check_profit_loss <- function(crypto) {
    current_price <- get_price(crypto, "usdt")$price
    entry_price <- get_entry_price(crypto) 
    
    if (current_price >= entry_price * 1.035) {  # حد سود ۳.۵٪
      nobitex_api_sell(crypto, "usdt", amount = get_holding_amount(crypto))
      showNotification(paste("فروش سودده", crypto))
    } 
    else if (current_price <= entry_price * 0.99) {  # حد ضرر ۱٪
      nobitex_api_sell(crypto, "usdt", amount = get_holding_amount(crypto))
      showNotification(paste("فروش ضررده", crypto))
    }
  }
  
  
  observeEvent(input$refresh, {
    req(input$crypto)
    current_key <- paste0(toupper(input$crypto), "_", tolower(input$base))
    if (current_key != last_key()) {
      refresh_count(0)
      output_log("")
      last_key(current_key)
    }
    
    observeEvent(input$stop_trading, {
      auto_trade(FALSE)
    })    
    
    withProgress(message = 'در حال دریافت داده...', {
      data <- get_price(input$crypto, input$base)
      price_data(data)
      stats <- get_market_stats(input$crypto, input$base)
      market_stats(stats)
      rsi_status <- get_rsi_status()
      wallet_data <- NULL
      active_data <- NULL
      wallet_status_text <- ""
      wallet_assets_text <- ""
      
      if (nzchar(input$user_token)) {
        wallet_data <- get_user_wallet(input$user_token)
        active_data <- get_active_wallets(input$user_token)
        active_wallets_data(active_data)
        if (!is.null(active_data) && nrow(active_data) > 0) {
          wallet_status_text <- "\n\nوضعیت کیف پول: ✅ کیف پول فعال"
        } else {
          wallet_status_text <- "\n\nوضعیت کیف پول: ❌ کیف پول غیرفعال"
        }
        if (input$show_wallet == "yes" && !is.null(wallet_data) && "currency" %in% colnames(wallet_data)) {
          filtered <- wallet_data %>% filter(balance > 0)
          if (nrow(filtered) > 0) {
            wallet_assets_text <- paste0("\n\n دارایی‌های شما:\n",
                                         paste0(filtered$currency, " = ", format(filtered$balance, big.mark = ","), collapse = "\n"))
          } else {
            wallet_assets_text <- "\n\n کیف پول شما خالی است."
          }
        }
      }
      
      # تحلیل قیمت نسبت به میانگین ۱۰ دقیقه‌ای (از CryptoCompare)
      ma_comparison <- calculate_ma_comparison(input$crypto)
      
      ma_text <- if(!ma_comparison$success) {
      } else {
        paste0(
          "\n\nوضعیت: قیمت فعلی ", ma_comparison$comparison, " از میانگین 10 دقیقه است"
        )
      }
      
      refresh_count(refresh_count() + 1)
      
      if (!is.null(data)) {
        price_str <- ifelse(is.na(data$price), "نامعلوم", format(data$price, big.mark = ","))
        ask_str <- ifelse(is.na(data$best_ask), "نامعلوم", format(data$best_ask, big.mark = ","))
        bid_str <- ifelse(is.na(data$best_bid), "نامعلوم", format(data$best_bid, big.mark = ","))
        diff_percent <- if (!is.na(data$best_ask) && !is.na(data$best_bid) && data$best_bid != 0) {
          round(((data$best_bid - data$best_ask) / data$best_bid) * 100, 2)
        } else NA
        diff_str <- ifelse(is.na(diff_percent), "نامعلوم", paste0(diff_percent, " %"))
        
        stats_text <- if (!is.null(market_stats())) paste0(
          "\n\nحجم مبدا (Volume Src): ", format(as.numeric(market_stats()$volumeSrc), big.mark = ","),
          "\n\nحجم مقصد (Volume Dst): ", format(as.numeric(market_stats()$volumeDst), big.mark = ","),
          "\n\nتغییرات روزانه (Day Change): ", as.character(market_stats()$dayChange)
        ) else ""
        
        rsi_section <- paste0("\n\n", rsi_status, "\n")
        amount_section <- paste0("\n\nدرصد برای خرید (Amount of BUY)= ", input$buy_percent , "\nدرصد برای فروش (Amount of SELL)= ", input$sell_percent)
        
        new_output <- paste0(
          "بروزرسانی شماره ", refresh_count(), ":\n\n",
          "قیمت (price): ", price_str, "\n\n",
          "بهترین قیمت فروش (Ask): ", ask_str, "\n\n",
          "بهترین قیمت خرید (Bid): ", bid_str, "\n\n",
          "اختلاف درصدی خرید و فروش (spread): ", diff_str, "\n\n",
          "زمان فعلی (Current Time): ", format(data$update_time, "%Y-%m-%d %H:%M:%S"),
          ma_text,
          rsi_section, stats_text, amount_section,
          wallet_status_text, wallet_assets_text
        )
      } else {
        new_output <- paste0("بروزرسانی شماره ", refresh_count(), ":\n\nدریافت داده با خطا مواجه شد.\n")
      }
      
      previous <- output_log()
      combined <- if (nchar(previous) > 0) paste(new_output, "\n\n--------------------------------------------------------------------\n\n", previous) else new_output
      output_log(combined)
    })
  })
  
  output$price_header <- renderText({
    base_name <- ifelse(input$base == "irt", "تومان", "تتر")
    paste("قیمت", toupper(input$crypto), "بر حسب", base_name)
  })
  
  output$price_output <- renderText({
    if (nchar(output_log()) == 0) {
      "لطفاً ابتدا دکمه بروزرسانی را بزنید"
    } else {
      output_log()
    }
  })
}

# اجرای برنامه
shinyApp(ui, server)
```
