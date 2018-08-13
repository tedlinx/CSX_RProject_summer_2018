#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(DT)
library(shiny)
library(ggplot2)

dta <- read.csv('UCI_Credit_Card.csv')
dta1 <- read.csv('1.csv')
dta2 <- read.csv('2.csv')
dta3 <- read.csv('BANK_NTP_LG_GD .CSV',sep = '\t')
dta4 <- read.csv('a1.csv')
dta$SEX<- as.factor(dta$SEX)
levels(dta$SEX)<-c("Male", "Female")

dta$EDUCATION<-as.factor(dta$EDUCATION)
levels(dta$EDUCATION)<-c("Unknown", "Graduate School", "University", "High school", "Others", "Unknown", "Unknown")

dta$MARRIAGE<- as.factor(dta$MARRIAGE)
levels(dta$MARRIAGE)<-c("unknown",'married', "single", "others")

dta$default.payment.next.month<- as.factor(dta$default.payment.next.month)
levels(dta$default.payment.next.month)<-c('no','yes')




choice.type1 <-
  c('SEX','EDUCATION','MARRIAGE','AGE','default.payment.next.month')
choice.type2 <-
  c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
choice.type3 <-c('SEX') #############
choice.value <-
  c("LIMIT_BAL")


  

# UI
ui <- navbarPage("信用卡違約調查",
                 navbarMenu("Introduction",
  tabPanel( "動機",
            
            
            mainPanel(
              h2("信用卡違約因素調查動機"),
                
                         strong("Q: 為什麼會想分析影響信用卡違約的因素?"),
                         p("1999年7月亞洲金融風暴，造成我國企業金融逐漸緊縮，企業向銀行融資的需求減少。因此銀行為求獲利，將重心轉向消費金融業務，大量發放金融卡和現金卡。然而，由於相對應的銀行法規尚未完備，主管機關又疏於管理，導致發卡亂象橫生，衍生卡債問題，與高額呆帳。最終產生[2005-2006卡債風暴](https://www.npf.org.tw/2/3558)。在作EDA作業的時候，我們組剛好有人在kaggle上找到一份從2005四月到2005九月的信用卡資料，所以決定利用這份資料來探討造成卡債風暴的潛在原因")
                         
                
            )
  ),
 
  tabPanel(
    "原始資料",
    tags$h1("Let's take a look at the dataset."),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  tabPanel( "原始資料欄位解釋",
            
            
            mainPanel(
              p("LIMIT_BAL:信用額度"),
              br(),
              p("SEX 性別(1 = male; 2 = female)"),
              br(),
              p("Education教育程度 (1 = graduate school; 2 = university; 3 = high school; 0, 4, 5, 6 = others)"),
              br(),
              p("Marital status婚姻狀態 (1 = married; 2 = single; 3 = divorce; 0=others)"),
              br(),
              p("Age 年齡(year)"),
              br(),
              p("default payment next month: 是否違約(1 = 是, 0 = 否)"), 
              br(),
              p("Pay_0~6:過去一個月的繳款狀況(依序為2005-09 ~ 2005-04)"),
              br(),
              p(
                "* -2 沒有用信用卡消費No consumption
              * -1 全部繳清Paid in full
              * 0 使用循還信貸The use of revolving credit
              * 1 拖延繳款1個月payment delay for one month
              * 2 拖延繳款2個月payment delay for two months 
              * 以此類推
              * 9 拖延繳款9個月以上(含九個月)payment delay for nine months and above"),
              br(),
              p("BILL_AMT1~6 信用卡帳單(應付帳款)(依序為2005-09 ~ 2005-04)"),
              br(),
              p("PAY_AMT1~6 前一期繳款紀錄(依序為2005-09 ~ 2005-04)")
              
              
            )
  ),
  tabPanel(Q，
    mainPanel(
      strong("以下就這份資料解釋過程中發現的問題進行Q&A"),
      br(),
      p("Q1. 為什麼有時顯示該期沒有消費卻還是有帳款要繳?"),
      div("A1: 每一期的信用卡應付帳款，在沒有特別約定時，包含當期與前期累計未繳款項、預借現金金額、循環信用利息、年費、各種手續費等。所以有可能沒有用信用卡消費還是有帳款要繳。", style = "color:blue"),
      br(),
      p("Q2. 怎麼看有無違約?"),
      div("A2: 持卡人未於約定繳費日期繳清最低應繳金額就是違約。從這份資料來看，一個月算一期，如果在前一期有繳納應繳金額(至少要繳納最低應繳金額)，則在下一期會顯示前一期繳款記錄，在四月到九月之間只要都沒有拖延繳款，違約記錄就會顯示0，如果有拖延繳款，則會顯示1。", style = "color:blue"),
      br(),
      p("Q3. 為什麼有些人帳單上的應繳金額額度很高，卻只繳一點點錢?"),
      div("A3: 這種人多半是使用循環信用來延後每一期的應繳金額。循環信用的立意，在於讓持卡人得以延後繳清帳單，只要先繳納一部份(通常是最低應繳金額不少於1000元時，先繳其中約5%)，其他未繳的則計入循環信用貸款加計利息。循環利率多半在萬分之4.5~5.5之間，年利率則在16~20%之間。", style = "color:blue"),
      br(),
      p("Q4. 為什麼有人的信用額度比帳單應繳納金額還低?"),
      div("A4: 信用額度是指銀行給持卡人的刷卡金額上限(銀行會在發卡前調查申請人的信用狀況，如經濟來源、和銀行的往來紀錄，來決定信用額度)，應繳納金額如前所述，有可能不只包含該期消費，如果銀行沒有規範，就有可能應繳金額大於信用額度。", style = "color:blue"),
      br(),

        p("Q5. 為什麼有些應繳金額呈現負數?"),
        div("A5: 有些人會預繳信用卡帳單(多繳錢到信用卡帳戶)，多出來的錢久以負數表示。", style = "color:blue"),
      br(),
         strong(參考資料),
        p("林峰正、周慧貞。信用卡VS.法律權益。永然文化出版股份有限公司。"),
        p("王建民。信用卡業務之研究。台北銀行經濟研究室編印。")
                                                                      
    )
  )
  ),
  
  
  
 
  tabPanel(
    "Single Variable",
    tags$h1("Summrizing time!"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type1, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),

    tags$h1("Summary"),
    verbatimTextOutput("summary")

  ),

  
  tabPanel(
    "Single Variable",
    tags$h1("Summrizing time!"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type1, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),
    
    tags$h1("Summary"),
    verbatimTextOutput("summary")
    
  ),
  
  #iris test
  tabPanel(
   #pageWithSidebar(
     #headerPanel('credit card k-means clustering'),
     'point',
     sidebarPanel(
       selectInput('xcol', 'X Variable', names(dta)[c(-1,-3,-4,-5,-6,-25,c(-7:-12))]),
       selectInput('ycol', 'Y Variable', names(dta)[c(-1,-3,-4,-5,-6,-25,c(-7:-12))],
                   selected=names(dta)[[2]]),
       numericInput('clusters', 'Cluster count', 3,
                     min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
     #)
   )),

 
#hist
 tabPanel('Age vs Limit_Bal',
     sidebarLayout(
       plotOutput("plot"),
       sliderInput("n", "Age", 25, dta2$AGE,15)
     )
     
 ),

tabPanel(
  "Box Plot",
  tags$h1("Box Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput('PA.plot', 'type', choice.type2, selectize = TRUE),
      selectInput('PA.value', 'Value', choice.value, selectize =
                    TRUE)
    ),
    mainPanel(plotOutput("PA.plot"))
  ),
  h1("T Test / ANOVA"),
  verbatimTextOutput("t.test.anova")
),


#


###
  # navbarMenu('default',
  #         tabPanel(title = 'boxplot',plotOutput('unif'),
  #                  actionButton('reunif','Resample')),
  #         tabPanel(title = 'histograme',plotOutput('unif'),
  #                  actionButton('reunif','Resample'))))




###
# navbarMenu('default',
#         tabPanel(
#           "Single Variable",
#           tags$h1("Summrizing time!"),
#           br(),
#           sidebarLayout(
#             sidebarPanel(
#               selectInput('SV.input1', 'type', c(choice.type1, choice.value), selectize = TRUE)
#             ),
#             mainPanel(plotOutput("SV.plot1"))
#           ),
#           
#           tags$h1("Summary"),
#           verbatimTextOutput("summary1")
#           
#         )))
###


##
###
navbarMenu( '消費調查',
  tabPanel(
    # fluidPage(
    '新北市',
    titlePanel("消費類別"),
    sidebarLayout(
      sidebarPanel(
        selectInput('PAY','消費類別:',
                    choices = colnames(dta4)),
        hr(),
        helpText("Data from AT&T (1961) The World's Telephones.")
      ),
      mainPanel(plotOutput("phonePlot"))
      #)
    ))))
  #))
###
##


############################################################

# server
server <- function(input, output, session) {
  output$SV.plot <- renderPlot({
    if( is.element(input$SV.input, choice.type1) ){
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input)
    }
    else{
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input)
    }
  })

  #sv
  output$SV.plot1 <- renderPlot({
    if( is.element(input$SV.input1, choice.type1) ){
      ggplot(data = dta, aes_string(x = input$SV.input1)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input1)
    }
    else{
      ggplot(data = dta, aes_string(x = input$SV.input1)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input1)
    }
  })
  #
  
  
  ####
  # output$distPlot <- renderPlot({
  #   dist <- rnorm(input$obs)
  #   hist(dist)
  # })
  ####
  
  
  output$PA.plot <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.plot, y = input$PA.value)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.plot)
    
  })
 
  
  #hist
  output$plot <- renderPlot({
    hist(dta2$LIMIT_BAL[seq_len(input$n)], breaks = 30)
  })
  #
  
  
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  
  
  
  
  ###
  output$PA.plot1 <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.plot1, y = input$PA.value1)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.plot1)
    
  })
  ###
  
  
  #####worldphone
  # output$phonePlot <- renderPlot({
  #   barplot(WorldPhones[,input$region],
  #           main = input$region,
  #           ylab = 'Number of Telephones',
  #           xlab = 'Year')
  # })
  ####
  
  
  #PAY
  output$phonePlot <- renderPlot({
    barplot(dta4[,input$PAY],
            main = input$PAY,
            ylab = '消費額度',
            xlab = '103~107年')
  })
  #

  
  #iris
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    dta[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  #
  
 
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  
  
  ###
  output$summary1 <- renderPrint({
    summary(dta)
  })
  ###
  
  
  output$t.test.anova <- renderPrint({
    type = unlist(dta[input$PA.plot])
    value = unlist(dta[input$PA.value])
    if (length(levels(type)) == 2){
      t.test( value ~ type )
    } else {
      anova( lm(value ~ type) )
    }
  })
  

  
  
  output$data.raw <- DT::renderDataTable({
    DT::datatable(dta)
  })
  
 
  
  output$data.summary <- DT::renderDataTable({
    DT::datatable(summary(dta))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

