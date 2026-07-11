library(shiny)
library(R.utils)
source("download_gene_mapping.R") 

ui <- fluidPage(
  titlePanel("在线基因注释数据库下载"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("start_download", "开始下载数据库"),
      br(), br(),
      textOutput("status"),
      verbatimTextOutput("log")
    ),
    
    mainPanel(
      h4("数据库信息"),
      tableOutput("db_info")
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    log = character(),
    status = "等待下载...",
    db_info = NULL
  )
  
  # 日志记录函数
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    rv$log <- c(rv$log, paste("[", timestamp, "]", msg))
  }
  
  output$log <- renderText({
    paste(rv$log, collapse = "\n")
  })
  
  output$status <- renderText({
    rv$status
  })
  
  output$db_info <- renderTable({
    rv$db_info
  })
  
  observeEvent(input$start_download, {
    rv$status <- "下载中..."
    rv$log <- character()  # 清空日志
    local_dir <- "基因注释本地数据"
    
    withProgress(message = "下载数据库中...", value = 0, {
      incProgress(0.1, message = "初始化下载...")
      
      result <- tryCatch({
        # 超时保护：30秒
        withTimeout({
          download_local_gene_mapping(output_dir = local_dir)
        }, timeout = 30, onTimeout = "error")
      }, error = function(e) {
        log_message(paste("下载失败:", e$message))
        return(NULL)
      })
      
      if(is.null(result)) {
        rv$status <- "下载失败或超时！"
      } else {
        rv$status <- "下载完成！"
        log_message(paste("成功下载", result$gene_count, "个基因"))
        
        # 显示数据库信息
        rv$db_info <- data.frame(
          文件路径 = result$file_path,
          文件大小_MB = result$file_size_mb,
          基因数量 = result$gene_count,
          更新时间 = as.character(result$timestamp)
        )
      }
      
      incProgress(1, message = "完成")
    })
  })
}

shinyApp(ui, server)
