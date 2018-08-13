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
           tabPanel(
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

# 讀取資料
library(tidyr)
library(stringr)
path =  "C:\\Users\\B06408042\\Desktop\\GitHub\\CSX_RProject_summer_2018\\week4\\doc.csv"
TB = read.csv(path)


# 因為教育一欄中0456都代表others，所以將這些欄位置換以免影響分析
TB$EDUCATION = str_replace_all(TB$EDUCATION, c("5" = "0", "6" = "0", "4" = "0"))


# 轉成factor
TB$SEX = factor(TB$SEX, labels = c("男", "女"))
TB$EDUCATION = factor(TB$EDUCATION, labels = c("其他","研究所以上", "大學", "高中職"))
TB$MARRIAGE = factor(TB$MARRIAGE, labels = c("其他","已婚", "單身","離婚"))
TB$AGE = factor(TB$AGE)
TB$default.payment.next.month = factor(TB$default.payment.next.month)
TB$PAY_0 = factor(TB$PAY_0, labels = c("沒有消費", "付清", "使用循環信貸", "逾期1個月", "逾期2個月","逾期3個月","逾期4個月","逾期5個月","逾期6個月","逾期7個月","逾期8個月"))
TB$PAY_2 = factor(TB$PAY_2, labels =  c("沒有消費", "付清", "使用循環信貸", "逾期1個月", "逾期2個月","逾期3個月","逾期4個月","逾期5個月","逾期6個月","逾期7個月","逾期8個月"))
TB$PAY_3 = factor(TB$PAY_3, labels =  c("沒有消費", "付清", "使用循環信貸", "逾期1個月", "逾期2個月","逾期3個月","逾期4個月","逾期5個月","逾期6個月","逾期7個月","逾期8個月"))
TB$PAY_4 = factor(TB$PAY_4, labels =  c("沒有消費", "付清", "使用循環信貸", "逾期1個月", "逾期2個月","逾期3個月","逾期4個月","逾期5個月","逾期6個月","逾期7個月","逾期8個月"))
TB$PAY_5 = factor(TB$PAY_5, labels =  c("沒有消費", "付清", "使用循環信貸", "逾期2個月","逾期3個月","逾期4個月","逾期5個月","逾期6個月","逾期7個月","逾期8個月"))
TB$PAY_6 = factor(TB$PAY_6, labels =  c("沒有消費", "付清", "使用循環信貸", "逾期2個月","逾期3個月","逾期4個月","逾期5個月","逾期6個月","逾期7個月","逾期8個月"))

names(TB)[2]<-"given_credit"
names(TB)[3]<-"sex"
names(TB)[4]<-"education"
names(TB)[5]<-"marriage"
names(TB)[6]<-"age"
names(TB)[25]<-"default"
names(TB)[7]<-"Sep"
names(TB)[8]<-"Aug"
names(TB)[9]<-"Jul"
names(TB)[10]<-"Jun"
names(TB)[11]<-"May"
names(TB)[12]<-"Apr"


library(ggplot2)
library(dplyr)

d<-TB%>%
  group_by(sex,education,age,marriage)%>%
  summarise(defaultAvg = mean(default))

# 以年齡分組算各組拖欠繳款率再分別和其他變數作圖
ggplot(d, aes(x = age, y = defaultAvg, color = sex))+ 
  geom_point()+
  facet_grid(. ~ sex)
ggplot(d, aes(x = age, y = defaultAvg, color = education))+ 
  geom_point()+
  facet_wrap(~ education)
ggplot(d, aes(x = age, y = defaultAvg, color = marriage))+
  geom_point()+
  facet_wrap(~ marriage)


ggplot(d, aes(x = marriage, y = defaultAvg))+
  geom_boxplot()
ggplot(d, aes(x = sex, y = defaultAvg))+
  geom_boxplot()
ggplot(d, aes(x = education, y = defaultAvg))+
  geom_boxplot()
# 看一下改好的資料
str(TB)

choice.T<- c("sex", "education","marriage")
sidebarLayout(
  sidebarPanel(
    selectInput('PlotInput', '因素:',choice.T, selectize = TRUE)
  ),
  mainPanel(plotOutput("plotop"))
),



output$plotop <- renderPlot({
  d<-TB%>%
    group_by(sex,education,age,marriage)%>%
    summarise(defaultAvg = mean(default))
  options(scipen=999)
  ggplot(d, aes(x = age, y = defaultAvg, color =input$choice.T ))+ 
    geom_point()+
    facet_wrap(~ input$choice.T)+
    labs(title= paste0(input$choice.T,'對違約機率(以年齡分群)')) +
    labs(x="年齡", y="違約機率")
    
  
  
})


