#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("信用卡消費調查",
                 tabPanel( "Introduction",
                  
                   
                   mainPanel(
                     h2("信用卡違約因素調查"),
                     tabsetPanel(
                       tabPanel("動機",
                         strong("Q: 為什麼會想分析影響信用卡違約的因素?"),
                         p("1999年7月亞洲金融風暴，造成我國企業金融逐漸緊縮，企業向銀行融資的需求減少。因此銀行為求獲利，將重心轉向消費金融業務，大量發放金融卡和現金卡。然而，由於相對應的銀行法規尚未完備，主管機關又疏於管理，導致發卡亂象橫生，衍生卡債問題，與高額呆帳。最終產生[2005-2006卡債風暴](https://www.npf.org.tw/2/3558)。在作EDA作業的時候，我們組剛好有人在kaggle上找到一份從2005四月到2005九月的信用卡資料，所以決定利用這份資料來探討造成卡債風暴的潛在原因")
                         
                         
                       ), 
                       tabPanel() 
                       
                     ),
                     h2("信用卡違約因素調查"),
                     p("探討影響信用卡違約的因素")
                     
                   )
                     # Show a plot of the generated distribution
                    
                    
                    ),
                 tabPanel("EDA",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot", plotOutput("plot")), 
                              tabPanel("Summary", verbatimTextOutput("summary")), 
                              tabPanel("Table", tableOutput("table"))
                            ))
                          ),
                   navbarMenu("Source",
                              tabPanel("資料來源"),
                              tabPanel("參考資料"))
                 
  
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

