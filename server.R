# Remove all variables
rm(list = ls())

## Load Libraries 
library(shiny)
library(dplyr)
library(tidyr)

## Load Data
load("DataSets.RData")

## Function server
server <- function(input, output, session) {
  
# 01.Declare variables ----

  F0 <- 10000          # initial fund value
  rounds <- 12         # number of rounds to play

  values <- reactiveValues()
  
  values$X <- data.frame(
    fund.value = rep(NA, rounds),
    fund.return = rep(NA, rounds),
    benchmark.value = rep(NA, rounds),
    benchmark.return = rep(NA, rounds)
  )
  
  values$current <- data.frame(
    round = as.integer(0)
  )

  values$string <- data.frame(
    plot01 <- rep(NA, rounds+1)
  )
  
  values$indicators <- data.frame(
    interest.rate <- rep(NA, 25+rounds),
     cpi <- rep(NA, 25+rounds)
  )
  
  values$table.results <- data.frame(
    Date = "",
    ST.Bonds = as.integer(0),
    MT.Bonds = as.integer(0),
    Index = as.integer(0),
    Fund.value = F0,
    Fund.return = 0,
    Benchmark.value = F0,
    Benchmark.return = 0
  )

  values$performance <- data.frame(
    fund.volatility = as.numeric(0),
    benchmark.volatility = as.numeric(0),
    fund.beta = as.numeric(0),
    fund.correlation = as.numeric(0),
    rf.return = as.numeric(0),
    fund.final.return = as.numeric(0),
    benchkmark.final.return = as.numeric(0),
    fund.sharpe = as.numeric(0), 
    fund.treynor = as.numeric(0),
    tracking.error = as.numeric(0),
    fund.information = as.numeric(0),
    benchmark.sharpe = as.numeric(0)
  )
   
  values$actual.scenario <- as.character()   # to reload session is scenario is changed

# 02.Expected Volatility computation ----
  output$historical.vol <- renderText(
    {
    SelSet <- switch(input$select,
    "The subprime mortgage crisis (2008)" = Set01,
    "The European debt crisis (2011)" = Set02,
    "Economic Growth in the US (2017)" = Set03,
    "ECB zero interest-rate policy (2017) " = Set04,
    "The COVID-19 crisis (2020)" = Set05,
    "Post-COVID-19 crisis (2022)" = Set06 )
  
  if (input$x + input$y + input$z  == 100) {
     
    aux <- values$current$round + 2
    insample.ret <- SelSet[aux:(aux+23),2:4]/100  # rolling-window of last 24 returns
    Sigma <- cov(insample.ret) 
    historical.vol <- sqrt(t(c(input$x, input$y, input$z)) %*% Sigma %*% (c(input$x, input$y, input$z)))*sqrt(rounds)
   
    paste(round(historical.vol, digits = 2), "%") 
  }

  })

 
# 03.Scenario description ----
output$scenario <- renderText({
  paste(switch(input$select,
           "The subprime mortgage crisis (2008)" =  "In the first decade of the 21st century, US banks extended mortgage loans to individuals with low incomes or poor credit histories. These loans were packaged into complex financial instruments and sold to investors around the world. Many of these loans defaulted, leading to significant losses and triggering a global financial crisis in 2008. This scenario is based on US markets. The benchmark is: Short-Term Fixed Income ETF: 20%, Mid-Term Fixed Income ETF: 10%, Stock Market ETF: 70%.",
           "The European debt crisis (2011)" = "The European sovereign debt crisis was a multi-year financial turmoil that affected several countries in the European Union. It was characterized by escalating levels of government debt, raising concerns about debt sustainability. The crisis led to increased borrowing costs, difficulty in accessing credit markets, and the need for financial assistance. Measures were taken to stabilize the affected economies, including bailout programs and fiscal austerity measures. This scenario is based on EU markets. The benchmark is: Short-Term Fixed Income ETF: 10%, Mid-Term Fixed Income ETF: 10%, Stock Market Index: 80%.",
           "Economic Growth in the US (2017)" = "During the 2010s, the US economy experienced one of the longest expansions on record. Unemployment rates remained at a historic low, with low levels of interest rates and inflation. The stock market witnessed a favorable environment driven by factors such as improved corporate earnings, accommodative monetary policies, and positive investor sentiment. The implementation of pro-business policies, such as tax reforms and deregulation, also played a role in boosting market confidence. This scenario is based on US markets. The benchmark is: Short-Term Fixed Income ETF: 20%, Mid-Term Fixed Income ETF: 20%, Stock Market Index: 60%.",
           "ECB zero interest-rate policy (2017) " = "Starting in 2016, the ECB kept a zero interest-rate policy (ZIRP) in the Eurozone for many years, while also launching asset purchase programs to support economic growth and prevent deflationary pressures (commonly known as Quantitative Easing). Despite these measures, inflation remained low. This scenario is based on EU markets. The benchmark is: Short-Term Fixed Income ETF: 40%, Mid-Term Fixed Income ETF: 10%, Stock Market Index: 50%.",
           "The COVID-19 crisis (2020)" = "Amidst the unforeseen pandemic caused by the COVID-19 disease, stock markets experienced one of the largest and fastest declines in modern history. Numerous stocks and indices plummeted, losing more than 30% of their value in a matter of weeks. The stock market crash led many companies to lose large amounts of market value, making them more vulnerable to bankruptcy and takeover. Investors sold their stocks in response to economic uncertainty and declining corporate earnings. To prevent the collapse of the economy, governments and central banks implemented unprecedented stimulus measures to support the affected companies and workers. This scenario is based on US markets. The benchmark is: Short-Term Fixed Income ETF: 15%, Mid-Term Fixed Income ETF: 25%, Stock Market Index: 60%.",
           "Post-COVID-19 crisis (2022)" = "After the COVID-19 pandemic shook the world, the global economy came to a near standstill for several months. As a result, stock market prices plummeted rapidly, and long-term bond prices increased due to market uncertainty. The stimulus packages implemented in response to the crisis improved the economic conditions, but fueled rampant inflation that forced the Federal Reserve to approve a series of aggressive interest rates hikes. This scenario is based on US markets. The benchmark is: Short-Term Fixed Income ETF: 50%, Mid-Term Fixed Income ETF: 10%, Stock Market Index: 40%." ))
     })


# 04.Observe Event - Simulate ----
  observeEvent(input$simulate, {
    if (input$x + input$y + input$z  == 100) {
        
      # Keep record of the selected scenario
      if (values$current$round == 0) {
        values$actual.scenario <- input$select
      }
      
      # Reload session if the scenario is changed
      observeEvent(input$select,{
        if (input$select!=values$actual.scenario){
          session$reload()
        }
      })
      
      # 
      if (values$current$round < 12) {

        values$current$round <- values$current$round +1
        current.round <- values$current$round
        current.month <- current.round +25
  
        fund.weights <- c(input$x, input$y, input$z)
        
       
        #Selection of Scenario and benchmark. Here you can change the benchkmark weights.
        SelSet <- switch(input$select,
                     "The subprime mortgage crisis (2008)" = Set01,
                     "The European debt crisis (2011)" = Set02,
                     "Economic Growth in the US (2017)" = Set03,
                     "ECB zero interest-rate policy (2017) " = Set04,
                     "The COVID-19 crisis (2020)" = Set05,
                     "Post-COVID-19 crisis (2022)" = Set06 )
        
        b <-switch(input$select,
                   "The subprime mortgage crisis (2008)" = c(20,10,70),
                   "The European debt crisis (2011)" = c(10,10,80),
                   "Economic Growth in the US (2017)" = c(20,20,60),
                   "ECB zero interest-rate policy (2017) " = c(40,10,50),
                   "The COVID-19 crisis (2020)" = c(15,25,60),
                   "Post-COVID-19 crisis (2022)" = c(50,10,40) )
        benchmark.weights <- b/100
        
        
        # To use later in the plots and table of results
        values$string$plot01 <- SelSet[25:(25+rounds),1]
        values$indicators$interest.rate <- SelSet[1:(25+rounds),5][[1]]
        values$indicators$cpi <- SelSet[1:(25+rounds),6][[1]]
  

        # Monthly update
        asset.returns <- as.numeric(SelSet[current.month,2:4])/100
        values$X$fund.return[current.round] <- (fund.weights %*% asset.returns) / 100
        values$X$fund.value[current.round] <- F0*prod(1+head(values$X$fund.return, current.round))
        values$X$benchmark.return[current.round] <- benchmark.weights %*% asset.returns
        values$X$benchmark.value[current.round] <- F0*prod(1+head(values$X$benchmark.return,current.round))
  
              
        # Construct the table of results
        new.row.results <- data.frame(cbind(values$string$plot01[[1]][current.round],
                                             t(fund.weights),
                                             values$X$fund.value[current.round],
                                             values$X$fund.return[current.round],
                                             values$X$benchmark.value[current.round],
                                             values$X$benchmark.return[current.round]))
        
        colnames(new.row.results) <- c("Date",
                                            "ST.Bonds",
                                            "MT.Bonds",
                                            "Index",
                                            "Fund.value",
                                            "Fund.return",
                                            "Benchmark.value",
                                            "Benchmark.return" )
        
        new.row.results[,5:8] <-as.numeric(new.row.results[,5:8])
        
        values$table.results<- rbind(values$table.results, new.row.results)
    
        } 
      
      
      # Performance indicators at the final round
      
      if (values$current$round == rounds) {
        current.round <- values$current$round
        current.month <- current.round +24
      values$performance$fund.volatility <- sd(values$X$fund.return)*sqrt(rounds)
      values$performance$benchmark.volatility <- sd(values$X$benchmark.return)*sqrt(rounds)
      values$performance$fund.beta <- cor(values$X$fund.return,values$X$benchmark.return)* (values$performance$fund.volatility) / (values$performance$benchmark.volatility)
      values$performance$fund.correlation <- cor(values$X$fund.return,values$X$benchmark.return)
      values$performance$rf.return <- prod(1+Set01[25:current.month,5]/1200) - 1
      values$performance$fund.final.return <- prod(1+values$X$fund.return) - 1
      values$performance$benchmark.final.return <- prod(1+values$X$benchmark.return) - 1
      values$performance$fund.sharpe <- (values$performance$fund.final.return - values$performance$rf.return) / values$performance$fund.volatility
      values$performance$fund.treynor <- (values$performance$fund.final.return - values$performance$rf.return) / values$performance$fund.beta
      values$performance$tracking.error <- sd(values$X$fund.return[1:current.round]- values$X$benchmark.return[1:current.round])
      values$performance$fund.information <- (values$performance$fund.final.return - values$performance$benchmark.final.return) / values$performance$tracking.error
      values$performance$benchmark.sharpe <- (values$performance$benchmark.final.return - values$performance$rf.return) / values$performance$benchmark.volatility
      }
    }

  }) 


# 05.Plots ----
  
  # Line plot of fund and benchmark value
  output$total_plot <- renderPlot({
    current.round <- values$current$round
    
     if (current.round==0) {
        x.months <- 0
        y.fund_value <- 10000
        y.benchmark_value <- 10000
      } else {
        x.months <- c(0,1:current.round)
        y.fund_value <- c(F0,values$X$fund.value[1:current.round])
        y.benchmark_value <- c(F0,values$X$benchmark.value[1:current.round])
      }

    xaxe.plot01 <- switch(input$select,
                          "The subprime mortgage crisis (2008)" = Set01[25:(25+rounds),1][[1]],
                          "The European debt crisis (2011)" = Set02[25:(25+rounds),1][[1]],
                          "Economic Growth in the US (2017)" = Set03[25:(25+rounds),1][[1]],
                          "ECB zero interest-rate policy (2017) " = Set04[25:(25+rounds),1][[1]],
                          "The COVID-19 crisis (2020)" = Set05[25:(25+rounds),1][[1]],
                          "Post-COVID-19 crisis (2022)" = Set06[25:(25+rounds),1][[1]] )
    
    
    min.value <- min(min(y.fund_value), min(y.benchmark_value))-1000
    max.value <- max(max(y.fund_value), max(y.benchmark_value))+1000

     plot(
       x.months,
       y.fund_value,
       type = 'o',
       xaxt = "n",
       ylim = c(min.value, max.value),
       xlim = c(0, 12),
       xlab = "Month",
       ylab = "Value",
       main = "Fund and Benchmark values",
       col = '#4d4dff',
       lwd = 1,
       pch = 16
     )
     axis(side = 1, at = (0:12), labels=xaxe.plot01)
     lines(
       x.months,
       y.benchmark_value,
       type = "o",
       col = "#ff4d4d",
       lwd = 1,
       pch = 16
     )
     abline(h=10000, col="black", lwd=1, lty=3)   # Draws a horizontal line at 10000
     legend(
       "bottomright",
       bty = "n",
       c("Fund", "Benchmark"),
       lty = c(1, 1),
       lwd = c(1, 1),
       pch = c(16, 16),
       col = c("blue", "#ff4d4d")
     )

  })

    
  # Barplot of returns
  output$return_plot <- renderPlot({
  
    current.round <- values$current$round
    
    xaxe.plot02 <- switch(input$select,
                          "The subprime mortgage crisis (2008)" = Set01[26:(25+rounds),1][[1]],
                          "The European debt crisis (2011)" = Set02[26:(25+rounds),1][[1]],
                          "Economic Growth in the US (2017)" = Set03[26:(25+rounds),1][[1]],
                          "ECB zero interest-rate policy (2017) " = Set04[26:(25+rounds),1][[1]],
                          "The COVID-19 crisis (2020)" = Set05[26:(25+rounds),1][[1]],
                          "Post-COVID-19 crisis (2022)" = Set06[26:(25+rounds),1][[1]] )
     
    if (current.round==0) {
      x.bar.months <- 0
      y.fund_return <- 0
      y.benchmark_return <- 0
    } else {
      x.bar.months <- c(1:current.round)
      y.fund_return <- c(values$X$fund.return[1:current.round])
      y.benchmark_return <- c(values$X$benchmark.return[1:current.round])
    }
    min.bar.value <- min(-0.15, min(y.fund_return), min(y.benchmark_return))
    max.bar.value <- max(0.15, max(y.fund_return), max(y.benchmark_return))
    
    plot.returns <- rbind(y.fund_return, y.benchmark_return)
    
    barplot(
      plot.returns,
      beside=TRUE,
      xlim = c(0, (rounds*3)),
      ylim = c(min.bar.value, max.bar.value),
      col = c("blue","red"), 
      xlab = "Month",
      ylab = "Return (%)",
      main = "Fund and Benchmark returns"
    )
    abline(h=0, col="black", lwd=1, lty=1)
    axis(side = 1, at = seq(from=2, to=36, by=3), labels=xaxe.plot02)
    legend(
      "bottomright",
      bty = "n",
      c("Fund", "Benchmark"),
      lty = c(1, 1),
      lwd = c(1, 1),
      pch = c(16, 16),
      col = c("blue", "red")
    )
  })
 
  # Line plot of indicators
  
  output$indexplot <- renderPlot({
  
    current.month <- 25+values$current$round
    
    if (current.month==25) {   # data at the starting plot
      
      x.months <- c(0:24)
      y.ir <- switch(input$select,
                   "The subprime mortgage crisis (2008)" = Set01[1:25,5][[1]],
                   "The European debt crisis (2011)" = Set02[1:25,5][[1]],
                   "Economic Growth in the US (2017)" = Set03[1:25,5][[1]],
                   "ECB zero interest-rate policy (2017) " = Set04[1:25,5][[1]],
                   "The COVID-19 crisis (2020)" = Set05[1:25,5][[1]],
                   "Post-COVID-19 crisis (2022)" = Set06[1:25,5][[1]] )
      y.cpi <- switch(input$select,
                    "The subprime mortgage crisis (2008)" = Set01[1:25,6][[1]],
                    "The European debt crisis (2011)" = Set02[1:25,6][[1]],
                    "Economic Growth in the US (2017)" = Set03[1:25,6][[1]],
                    "ECB zero interest-rate policy (2017) " = Set04[1:25,6][[1]],
                    "The COVID-19 crisis (2020)" = Set05[1:25,6][[1]],
                    "Post-COVID-19 crisis (2022)" = Set06[1:25,6][[1]] )
      }

    else {                   # data after the first round is played
       x.months <- c(0:(current.month-1))
       y.ir <- c(values$indicators$interest.rate[1:current.month])
       y.cpi <- c(values$indicators$cpi[1:current.month])
     }

    xaxe.plot03 <- switch(input$select,
             "The subprime mortgage crisis (2008)" = Set01[1:(25+rounds),1][[1]],
             "The European debt crisis (2011)" = Set02[1:(25+rounds),1][[1]],
             "Economic Growth in the US (2017)" = Set03[1:(25+rounds),1][[1]],
             "ECB zero interest-rate policy (2017) " = Set04[1:(25+rounds),1][[1]],
             "The COVID-19 crisis (2020)" = Set05[1:(25+rounds),1][[1]],
             "Post-COVID-19 crisis (2022)" = Set06[1:(25+rounds),1][[1]] )
    
     min.ind.value <- min(-3,min(min(y.ir), min(y.cpi)) - 2.5)
     max.ind.value <- max(7,max(max(y.ir), max(y.cpi)) + 2.5)
    
      plot(
        x.months,
        y.ir,
        type = 'o',
        xaxt = "n",
        ylim = c(min.ind.value,max.ind.value),
        xlim = c(0, 37),
        xlab = "Month",
        ylab = "Interest Rate / CPI",
        main = "% Interest rate of the Central Bank and CPI (YoY)",
        col = '#4d4dff',
        lwd = 1,
        pch = 16
      )
      axis(side = 1, at = c(0:36), labels=xaxe.plot03)
      
      lines(    # plots CPI (YoY)
        x.months,
        y.cpi,
        type = "o",
        col = "#ff4d4d",
        lwd = 1,
        pch = 16
      )
      
      abline(h=0, col="black",lwd=1, lty=3)
      
      legend(
        "bottomright",
        bty = "n",
        c("Interest rate", "CPI"),
        lty = c(1, 1),
        lwd = c(1, 1),
        pch = c(16, 16),
        col = c("#4d4dff", "#ff4d4d")
      )
  })

# 06.Download Data ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data Pension Fund Simulator', 'csv', sep = ".")
    },
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.csv(values$table.results, file,
                row.names = FALSE)
    })
  
  
# 07.Simulation Table   ----
  
  output$c_data_out  <- ({
    renderTable(values$table.results, striped = TRUE, spacing = 's', align = 'c', digits=2)
   })


# 08.Pop-Up Table with Results ----

    observeEvent(input$resultstable, {
    current.round <- values$current$round
    if(current.round==12) {
      showModal(modalDialog(
        "Performance meaures:", 
        br(),
        br(), "Return of the Fund:", round(values$performance$fund.final.return * 100, digits=2), "%",
        br(), "Volatility of the Fund:", round(values$performance$fund.volatility * 100,digits=2), "%",
        br(), "Sharpe Ratio of the Fund:", round(values$performance$fund.sharpe,digits=2), 
        br(), "Treynor Ratio of the Fund:", round(values$performance$fund.treynor,digits=2),
        br(), "Tracking Error of the Fund:", round(values$performance$tracking.error,digits=2),
        br(), "Information Ratio of the Fund:",round(values$performance$fund.information,digits=2),
        br(), "Beta of the Fund:",round(values$performance$fund.beta,digits=2),
        br(), "Correlation between the Fund and the Benchmark:",round( values$performance$fund.correlation,digits=2),
        br(),
        br(), "Information regarding the Benchmark:",
        br(),
        br(), "Return of the Benchmark:", round(values$performance$benchmark.final.return * 100, digits=2), "%",
        br(), "Volatility of the Benchmark:", round(values$performance$benchmark.volatility * 100,digits=2),"%",
        br(), "Sharpe Ratio of the Benchmark:", round(values$performance$benchmark.sharpe,digits=2), 
        br(), 
        br(),
          if(values$performance$fund.sharpe>values$performance$benchmark.sharpe) {
          "Congratulations! You performed better than the benchmark in risk/return terms."
          } else {
          "You coundn't beat the benchmark in risk/return terms. Best luck next time!"
           }
         ))}
     })
}


  

