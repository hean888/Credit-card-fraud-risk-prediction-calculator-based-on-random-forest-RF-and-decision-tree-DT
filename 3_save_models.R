# 安装并加载必要的包
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library(shiny)

# 定义用户界面
ui <- fluidPage(
  titlePanel("模型下载"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("download_rf_model", "Download RF Model"),
      downloadButton("download_pruned_tree", "Download Pruned Tree")
    ),
    mainPanel(
      h3("下载训练好的模型"),
      p("点击上面的按钮下载训练好的随机森林模型和剪枝后的决策树模型。")
    )
  )
)

# 定义服务器逻辑
server <- function(input, output) {
  # 定义下载随机森林模型的处理程序
  output$download_rf_model <- downloadHandler(
    filename = function() {
      "rf_model.rds"
    },
    content = function(file) {
      file.copy("rf_model.rds", file)
    }
  )
  
  # 定义下载剪枝决策树模型的处理程序
  output$download_pruned_tree <- downloadHandler(
    filename = function() {
      "pruned_tree.rds"
    },
    content = function(file) {
      file.copy("pruned_tree.rds", file)
    }
  )
}

# 运行 Shiny 应用
shinyApp(ui = ui, server = server)
