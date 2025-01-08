if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")

# 加载必要的库
library(shiny)
library(readxl)
library(rpart)
library(writexl)

# 加载训练好的决策树模型
pruned_tree <- readRDS("models/pruned_tree.rds") 

# 创建Shiny用户界面
ui <- fluidPage(
  titlePanel("信用卡欺诈预测计算器"),
  sidebarLayout(
    sidebarPanel(
      selectInput("card_type", "卡类别:", choices = c("白金卡", "金卡", "普卡", "银卡")),
      numericInput("daily_amount", "日均消费金额:", value = 0),
      numericInput("daily_count", "日均次数:", value = 0),
      numericInput("min_amount", "单笔消费最小金额:", value = 0),
      numericInput("max_amount", "单笔消费最大金额:", value = 0),
      numericInput("income", "个人收入:", value = 0),
      actionButton("predict", "预测结果"),
      fileInput("file", "上传CSV或XLSX文件", accept = c(".csv", ".xlsx")),
      actionButton("batch_predict", "批量预测"),
      downloadButton("downloadData", "下载预测结果")
    ),
    mainPanel(
      textOutput("prediction"),
      verbatimTextOutput("input_summary"),
      verbatimTextOutput("probability"),
      tableOutput("batch_results")
    )
  )
)

# 创建Shiny服务器逻辑
server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      卡类别 = factor(input$card_type, levels = c("白金卡", "金卡", "普卡", "银卡")),
      日均消费金额 = input$daily_amount,
      日均次数 = input$daily_count,
      单笔消费最小金额 = input$min_amount,
      单笔消费最大金额 = input$max_amount,
      个人收入_连续 = input$income
    )
    
    prediction <- predict(pruned_tree, new_data, type = "class")
    prob <- predict(pruned_tree, new_data, type = "prob")
    
    output$prediction <- renderText({
      paste("预测结果: ", ifelse(prediction == 1, "欺诈风险较大", "正常"))
    })
    
    output$probability <- renderText({
      paste("欺诈概率: ", round(prob[1, "1"], 6))
    })
    
    output$input_summary <- renderPrint({
      new_data
    })
  })
  
  batch_results <- reactiveVal()  # 用于存储批量预测结果
  
  observeEvent(input$batch_predict, {
    req(input$file)
    
    file_ext <- tools::file_ext(input$file$datapath)
    if (file_ext == "csv") {
      batch_data <- read.csv(input$file$datapath)
    } else if (file_ext == "xlsx") {
      batch_data <- read_excel(input$file$datapath)
    } else {
      stop("不支持的文件类型")
    }
    
    batch_data$卡类别 <- factor(batch_data$卡类别, levels = c("白金卡", "金卡", "普卡", "银卡"))
    
    batch_predictions <- predict(pruned_tree, batch_data, type = "class")
    batch_probabilities <- predict(pruned_tree, batch_data, type = "prob")
    
    results <- data.frame(
      batch_data,
      预测结果 = ifelse(batch_predictions == 1, "欺诈风险较大", "正常"),
      欺诈概率 = round(batch_probabilities[, "1"], 6)
    )
    
    batch_results(results)
    
    output$batch_results <- renderTable({
      results
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("预测结果-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(batch_results(), path = file)
    }
  )
}

# 运行Shiny应用
##shinyApp(ui = ui, server = server)


install.packages('rsconnect')

rsconnect::setAccountInfo(name='hean7982',
                          token='1291D015A781C6E45B9CFD434504350C',
                          secret='a826P5cNA/z/sV1KgW8ZEJwncL1k49B1cxgneFms')
library(rsconnect)

getwd()
rsconnect::deployApp('C:/Users/25360/Desktop/Credit_card_fraud_prediction_calculator/shinyapp')


