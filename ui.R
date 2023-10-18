library(shiny)
library(shinyalert)

ui <- fluidPage(
  titlePanel(title=div(img(src= "", height = 70), "Managing a Simple Investment Fund"), windowTitle = "Investment Fund"),
 
# LEFT PANEL
  
fluidRow(       
  column(3,
    wellPanel(
        selectInput("select", label = h3("Select the scenario for the simulation"), 
              choices = c("The subprime mortgage crisis (2008)" , 
                          "The European debt crisis (2011)",
                          "Economic Growth in the US (2017)",
                          "ECB zero interest-rate policy (2017) ",
                          "The COVID-19 crisis (2020)",
                          "Post-COVID-19 Crisis (2022)"), 
              selected = "The subprime mortgage crisis (2008)" )
    ), 
    wellPanel(
      sliderInput(
        inputId = "x",
        label = "% Invested in Short-Term Bonds",
        min = 0,
        max = 100,
        value = 0,
        step = 5
      ),
      sliderInput(
        inputId = "y",
        label = "% Invested in Mid-Term Bonds",
        min = 0,
        max = 100,
        value = 0,
        step = 5
      ),
      sliderInput(
        inputId = "z",
        label = "% Invested in the Stock Market",
        min = 0,
        max = 100,
        value = 0,
        step = 5
      )
    ),
    wellPanel(
        actionButton(
          inputId = "simulate",
          label = "Simulate",
          class = "btn btn-success action-button"
        ),
        actionButton(
          inputId = "restart",
          label="Restart",
          type = "button",
          class = "btn btn-danger action-button",
          onclick = "history.go(0)"
        )
    ),
    
  column(
    width = 4,
      fluidRow(
        column(
          width = 6, 
          h4("Expected Volatility")
        )
      )
  ), 
    
    fluidRow(
       column(
          width = 6, 
          br(),
            div(textOutput(outputId = "historical.vol"),
                style = "font-size:150%"
            )
        )
    )
  ),
      

# RIGHT PANEL
      
  column(9,
    fluidRow( 
      column(12,
      tabsetPanel(
              
      # TAB 01:  GRAPHS
      tabPanel("Graphs",
          fluidRow(
            br(),
            div(textOutput(outputId = "scenario")) ,
            column(6, 
              plotOutput("total_plot", height = 300)
            ),
            column(6,
               plotOutput("return_plot", height = 300)
            ),
            column(12,
              plotOutput("indexplot", height = 300)
            )
          )
      ),
           
      # TAB 02: DATA TABLE
      tabPanel("Data Table", 
          h5("Summary of the financial results of the investment stored month by month"),
          div(tableOutput("c_data_out"), style = "font-size:80%"),
              downloadButton('downloadData', 'Download Data', class="btn-xs btn-info"),
              br(),
              br()
          ),

       # TAB 03: INSTRUCTIONS
       tabPanel("Instructions",
          h5("Welcome,"),
          h5("In this game you will become the manager of a simple investment fund. During twelve rounds of gameplay, you will have to make decisions on the fund’s asset allocation, selecting the percentage of funds to invest in each of three available asset classes. At the beginning of the game, you will be endowed with 10,000 units of virtual money. "),
                        
          h5("The first step is to select the scenario you want to play. All the available scenarios are based on real periods in history, and contain real market data at the time they occurred. After selecting the scenario, a short description text will appear in the 'Graphs' tab to provide some historical context. Each scenario has a benchmark attached to it against which you will have to compete, in order to improve its performance, and obtain a higher profitability. The scenarios to choose from are:"),
            h5("- The subprime mortgage crisis (2008)"),
            h5("- the European debt crisis (2011)"),
            h5("- Economic growth in the US (2017)"),
            h5("- ECB zero interest-rate policy (2017)"),
            h5("- The COVID-19 crisis (2020)"), 
            h5("- Port-COVID-19 crisis (2022)"),
            h5("Once the scenario is selected, you can use the sliders in the main tab to determine the percentage of money to invest in each asset class. As you progress through each round you will be able to revise these percentages in order to obtain a better performance of your portfolio. The three asset classes available are:"), 
            h5("- Short-Term Fixed Income ETF (Government Bonds with maturities between 1 and 3 
                years)"), 
            h5("- Mid-Term Fixed Income ETF (Government Bonds with maturities between 7 and 10 
                years)"),
            h5("- Stock market ETF"), 
            h5("You must make sure that in each round the portfolio weights add to 100%, so all the available money is invested. After the asset allocation is decided, the expected volatility of the chosen portfolio will appear, so you can assess the level of risk taken in that round."), 
             h5("Once you are satisfied with the portfolio weights, you only need to click on the “Simulate” button to obtain the results of the round. Only after playing twelve rounds (representing one year of calendar time) you will be able to click on the 'Performance measures' button and check several performance indicators of your fund and benchmark. It will allow you to better assess the performance of the fund during the year."),
            h5("IMPORTANT: If you change the scenario during a game, the simulation will restart."),
            br(),
            br()
        ),
       ),
               
     useShinyalert(force = TRUE),  # Set up shinyalert
     actionButton("resultstable", "Performance measures")
      )
    )
    )
  )
)



