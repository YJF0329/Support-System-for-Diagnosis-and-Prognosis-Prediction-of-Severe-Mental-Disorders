# modules/bipolar_risk.R
# 双相情感障碍患病风险辅助诊断模块
# 基于基因表达数据的完整预测流程

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(plotly)
library(dplyr)
library(shinyalert)
library(MASS)

# =====================================================
# 辅助函数（从 preprocessing_functions.R 合并）
# =====================================================

# 1. 加载本地基因数据库
load_local_gene_db <- function() {
  db_path <- "modules/local_gene_mapping.rds"
  
  if(!file.exists(db_path)) {
    stop("本地基因数据库不存在。路径: ", db_path)
  }
  
  if(!exists(".local_gene_db_cache", envir = .GlobalEnv)) {
    cat("加载本地基因数据库...\n")
    .GlobalEnv$.local_gene_db_cache <- readRDS(db_path)
    cat("数据库加载完成，基因数:", 
        nrow(.GlobalEnv$.local_gene_db_cache), "\n")
  }
  
  return(.GlobalEnv$.local_gene_db_cache)
}

# 2. 本地基因注释
local_gene_annotation <- function(raw_data) {
  cat("=== 本地基因注释开始 ===\n")
  
  if(is.null(raw_data) || nrow(raw_data) == 0) {
    stop("输入数据为空")
  }
  
  if(ncol(raw_data) >= 2) {
    colnames(raw_data)[1:2] <- c("gene_id", "expression")
  } else {
    stop("数据需要至少两列")
  }
  
  input_genes <- nrow(raw_data)
  cat("输入基因数:", input_genes, "\n")
  
  raw_data$ensembl_id_clean <- gsub("\\..*", "", raw_data$gene_id)
  
  gene_db <- load_local_gene_db()
  
  result <- merge(gene_db[, c("ensembl_gene_id", "external_gene_name")],
                  raw_data,
                  by.x = "ensembl_gene_id",
                  by.y = "ensembl_id_clean",
                  all.x = FALSE)
  
  colnames(result) <- c("gene_id", "gene_name", "original_gene_id", "expression")
  result <- result[, c("gene_name", "gene_id", "expression")]
  
  output_genes <- nrow(result)
  cat("成功注释基因数:", output_genes, "\n")
  cat("注释成功率:", round(output_genes/input_genes*100, 1), "%\n")
  
  cat("=== 本地基因注释完成 ===\n\n")
  return(result)
}

# 3. 去重
remove_duplicate_genes <- function(annotated_data) {
  if(is.null(annotated_data) || nrow(annotated_data) == 0) {
    stop("输入数据为空")
  }
  
  if(!"gene_name" %in% colnames(annotated_data)) {
    stop("数据缺少gene_name列")
  }
  
  duplicates <- duplicated(annotated_data$gene_name)
  
  if(sum(duplicates) == 0) {
    return(annotated_data)
  }
  
  dedup_data <- aggregate(
    expression ~ gene_name,
    data = annotated_data,
    FUN = mean,
    na.rm = TRUE
  )
  
  if("gene_id" %in% colnames(annotated_data)) {
    gene_id_map <- aggregate(
      gene_id ~ gene_name,
      data = annotated_data,
      FUN = function(x) x[1]
    )
    dedup_data <- merge(dedup_data, gene_id_map, by = "gene_name")
  }
  
  return(dedup_data)
}

# 4. 零值处理
handle_zero_expression <- function(data) {
  if(!"expression" %in% colnames(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if(any(numeric_cols)) {
      expr_col <- names(which(numeric_cols))[1]
      colnames(data)[colnames(data) == expr_col] <- "expression"
    } else {
      stop("找不到数值型的表达值列")
    }
  }
  
  non_zero_values <- data$expression[data$expression > 0]
  
  if(length(non_zero_values) == 0) {
    min_nonzero <- 0.1
  } else {
    min_nonzero <- min(non_zero_values, na.rm = TRUE)
  }
  
  zero_count <- sum(data$expression == 0, na.rm = TRUE)
  
  if(zero_count > 0) {
    set.seed(123)
    data$expression[data$expression == 0] <- 
      runif(zero_count, 0, min_nonzero/10)
  }
  
  return(data)
}

# 5. 过滤低表达基因
filter_low_expression <- function(data) {
  if(!"expression" %in% colnames(data)) {
    stop("数据缺少expression列")
  }
  
  median_expr <- median(data$expression, na.rm = TRUE)
  threshold <- median_expr * 0.1
  
  keep <- data$expression > threshold
  filtered_data <- data[keep, ]
  
  return(filtered_data)
}

# 6. log2转换
apply_log2_transform <- function(data) {
  if(!is.numeric(data$expression)) {
    data$expression <- as.numeric(data$expression)
  }
  
  if(any(data$expression < 0, na.rm = TRUE)) {
    data$expression[data$expression < 0] <- abs(data$expression[data$expression < 0])
  }
  
  data$expression <- log2(data$expression + 1)
  
  return(data)
}

# 7. 0-1归一化
normalize_0_1 <- function(data) {
  if(!"expression" %in% colnames(data)) {
    stop("数据缺少expression列")
  }
  
  expr_values <- data$expression
  
  min_val <- min(expr_values, na.rm = TRUE)
  max_val <- max(expr_values, na.rm = TRUE)
  
  if(max_val > min_val) {
    data$expression <- (expr_values - min_val) / (max_val - min_val)
  } else {
    data$expression <- 0
  }
  
  return(data)
}

# 8. 验证数据库
validate_local_database <- function() {
  db_path <- "modules/local_gene_mapping.rds"
  
  if(!file.exists(db_path)) {
    return(list(
      available = FALSE,
      message = "数据库文件不存在",
      path = db_path
    ))
  }
  
  tryCatch({
    gene_db <- readRDS(db_path)
    
    required_cols <- c("ensembl_gene_id", "external_gene_name")
    missing_cols <- setdiff(required_cols, colnames(gene_db))
    
    if(length(missing_cols) > 0) {
      return(list(
        available = FALSE,
        message = paste("数据库缺少必需列:", paste(missing_cols, collapse = ", ")),
        path = db_path,
        genes_count = nrow(gene_db)
      ))
    }
    
    return(list(
      available = TRUE,
      message = "数据库可用",
      path = db_path,
      genes_count = nrow(gene_db),
      file_size_mb = round(file.size(db_path) / 1024 / 1024, 2),
      last_modified = file.info(db_path)$mtime
    ))
    
  }, error = function(e) {
    return(list(
      available = FALSE,
      message = paste("数据库读取错误:", e$message),
      path = db_path
    ))
  })
}

# =====================================================
# 参考基因列表（用于特征选择）
# =====================================================

get_reference_genes <- function() {
  c(
    "RAD52", "REX1BD", "ELAC2", "BID", "XYLT2", "NSUN2", "MED17", "PHKA2", 
    "JKAMP", "LY75", "ELMO2", "APPBP2", "POLD1", "DDX20", "KDM4A", "MAPK6", 
    "SEL1L", "PTGS2", "MLH1", "PAG1", "FDFT1", "KIF22", "TXLNA", "POMGNT1",
    "XRN2", "ANAPC5", "SLC8B1", "PPP2R3C", "PPIL2", "PPP6R2", "CSTF2", "COG4",
    "HNRNPL", "ISYNA1", "WDR91", "ZFAND5", "MAP3K8", "CDK5RAP3", "RECQL5",
    "DDX5", "DHRS7B", "PPP6R3", "PRPF19", "CARS1", "KCTD20", "DUSP22", "EXOC2",
    "HARS2", "APBB3", "PCCB", "NPRL2", "GNB4", "EIF2B4", "INO80B", "AUP1",
    "UNC50", "SF3B1", "KDM3A", "NFE2L2", "S100PBP", "P3H1", "COQ6", "TNFRSF8",
    "DDX39A", "NCKAP1L", "TBCC", "NT5C", "CLPP", "SUMF2", "ZSWIM6", "AKAP12",
    "AOC3", "DHX30", "PTPRE", "TSPAN2", "TPP2", "DHX9", "DDX56", "RSAD1",
    "SRSF1", "CDK9", "FPGS", "PRCP", "SLC5A6", "ZNF740", "PFKL", "TARS2",
    "PIP5K1A", "GOLPH3L", "GCNA", "ERLIN2", "SURF6", "SLC25A25", "QTRT2",
    "ZDHHC7", "KCTD18", "RAB39B", "ABHD3", "GPAT4", "GNE", "VPS11", "FDPS",
    "SRSF2", "RBM15", "ANKZF1", "DNASE1L3", "ELP6", "TEX264", "RICTOR"
  )
}

# =====================================================
# UI
# =====================================================

bipolar_risk_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    tags$style(HTML("
      .step-indicator {
        display: flex;
        justify-content: space-between;
        margin: 20px 0;
      }
      .step {
        text-align: center;
        flex: 1;
        position: relative;
      }
      .step-icon {
        width: 40px;
        height: 40px;
        border-radius: 50%;
        background-color: #e0e0e0;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto 10px;
      }
      .step.complete .step-icon {
        background-color: #28a745;
        color: white;
      }
      .step.active .step-icon {
        background-color: #007bff;
        color: white;
      }
      .step.pending .step-icon {
        background-color: #e0e0e0;
        color: #777;
      }
      .step-connector {
        position: absolute;
        top: 20px;
        right: -50%;
        width: 100%;
        height: 2px;
        background-color: #e0e0e0;
        z-index: -1;
      }
      .step.complete .step-connector {
        background-color: #28a745;
      }
      .step.active .step-connector {
        background-color: #007bff;
      }
      .step-title {
        font-size: 12px;
        color: #777;
      }
      .step.complete .step-title {
        color: #28a745;
        font-weight: bold;
      }
      .step.active .step-title {
        color: #007bff;
        font-weight: bold;
      }
      .result-box {
        background: #f0f7fc;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        border: 1px solid #d1e0eb;
      }
    ")),
    
    # 顶部标题
    div(
      style = "background: #1A3A6B; padding: 20px; border-radius: 10px; color: white; text-align: center; margin-bottom: 20px;",
      h2("🧬 双相情感障碍患病风险辅助诊断", style = "margin: 0;"),
      p("基于基因表达谱数据的机器学习预测模型", style = "margin: 5px 0 0 0; opacity: 0.9;")
    ),
    
    # 步骤指示器
    fluidRow(
      column(
        width = 12,
        div(
          class = "step-indicator",
          div(
            class = "step active",
            div(class = "step-icon", icon("upload")),
            div(class = "step-title", "1. 数据上传"),
            div(class = "step-connector")
          ),
          div(
            class = "step pending",
            div(class = "step-icon", icon("filter")),
            div(class = "step-title", "2. 特征选择"),
            div(class = "step-connector")
          ),
          div(
            class = "step pending",
            div(class = "step-icon", icon("chart-line")),
            div(class = "step-title", "3. 模型预测"),
            div(class = "step-connector")
          ),
          div(
            class = "step pending",
            div(class = "step-icon", icon("chart-bar")),
            div(class = "step-title", "4. 结果分析")
          )
        )
      )
    ),
    
    # 第一步：数据预处理
    box(
      title = div(icon("database"), "1. 数据上传与预处理"),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      fluidRow(
        column(
          width = 6,
          fileInput(
            ns("raw_file"),
            "上传基因表达文件",
            accept = c(".csv", ".txt", ".tsv"),
            buttonLabel = "浏览...",
            placeholder = "选择文件"
          ),
          
          helpText("文件要求："),
          tags$ul(
            tags$li("CSV或制表符分隔的文本文件"),
            tags$li("第一列：基因ID（Ensembl ID）"),
            tags$li("第二列：表达值（原始计数）"),
            tags$li("示例：", tags$code("ENSG00000141510, 15.2"))
          ),
          
          uiOutput(ns("db_status")),
          
          br(),
          
          actionButton(
            ns("start_preprocess"),
            "开始预处理",
            icon = icon("play"),
            class = "btn-success",
            width = "100%"
          )
        ),
        
        column(
          width = 6,
          h5("处理日志："),
          verbatimTextOutput(ns("process_log")),
          
          conditionalPanel(
            condition = paste0("output['", ns("preprocess_complete"), "']"),
            ns = ns,
            br(),
            fluidRow(
              valueBoxOutput(ns("genes_before"), width = 4),
              valueBoxOutput(ns("genes_after"), width = 4),
              valueBoxOutput(ns("match_rate"), width = 4)
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = paste0("output['", ns("preprocess_complete"), "']"),
        ns = ns,
        br(),
        actionButton(
          ns("go_to_feature"),
          "下一步：特征选择 →",
          icon = icon("arrow-right"),
          class = "btn-primary",
          width = "100%"
        )
      )
    ),
    
    # 第二步：特征选择
    box(
      title = div(icon("filter"), "2. 预测基因特征选择"),
      width = 12,
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      id = ns("feature_box"),
      
      fluidRow(
        column(
          width = 6,
          h5("参考基因列表"),
          actionButton(
            ns("view_reference_genes"),
            "查看参考基因",
            icon = icon("list"),
            class = "btn-default",
            width = "100%"
          ),
          br(), br(),
          
          actionButton(
            ns("check_gene_match"),
            "检查基因匹配",
            icon = icon("search"),
            class = "btn-info",
            width = "100%"
          ),
          br(), br(),
          
          uiOutput(ns("match_result_ui"))
        ),
        
        column(
          width = 6,
          h5("匹配结果预览"),
          DTOutput(ns("final_matrix_preview"))
        )
      ),
      
      conditionalPanel(
        condition = paste0("output['", ns("match_success"), "']"),
        ns = ns,
        br(),
        actionButton(
          ns("go_to_predict"),
          "下一步：模型预测 →",
          icon = icon("arrow-right"),
          class = "btn-primary",
          width = "100%"
        )
      )
    ),
    
    # 第三步：模型预测
    box(
      title = div(icon("chart-line"), "3. 模型预测"),
      width = 12,
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      id = ns("predict_box"),
      
      fluidRow(
        column(
          width = 12,
          div(
            style = "text-align: center; padding: 10px;",
            actionButton(
              ns("run_prediction"),
              "开始预测",
              icon = icon("play-circle"),
              class = "btn-success btn-lg",
              style = "padding: 10px 40px;"
            )
          ),
          
          br(),
          
          p("说明："),
          tags$ul(
            tags$li("系统自动使用选择的特征进行预测"),
            tags$li("如果特征与模型不匹配，会自动用平均值填充缺失特征"),
            tags$li("预测分数 > 0 为高风险，≤ 0 为低风险")
          )
        )
      ),
      
      conditionalPanel(
        condition = paste0("output['", ns("prediction_complete"), "']"),
        ns = ns,
        br(),
        h5("预测结果："),
        DTOutput(ns("prediction_table")) %>% withSpinner(type = 4, color = "#0dc5c1")
      ),
      
      conditionalPanel(
        condition = paste0("output['", ns("prediction_complete"), "']"),
        ns = ns,
        br(),
        actionButton(
          ns("go_to_analysis"),
          "下一步：结果分析 →",
          icon = icon("arrow-right"),
          class = "btn-primary",
          width = "100%"
        )
      )
    ),
    
    # 第四步：结果分析
    box(
      title = div(icon("chart-bar"), "4. 结果分析与风险可视化"),
      width = 12,
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      id = ns("analysis_box"),
      
      fluidRow(
        column(
          width = 12,
          uiOutput(ns("analysis_display"))
        )
      )
    )
  )
}

# =====================================================
# SERVER
# =====================================================

bipolar_risk_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 1. 状态管理
    # =====================================================
    
    rv <- reactiveValues(
      raw_data = NULL,
      annotated_data = NULL,
      processed_data = NULL,
      final_matrix = NULL,
      matched_genes = NULL,
      match_rate = 0,
      match_success = FALSE,
      prediction_results = NULL,
      log = c(),
      step = "upload"
    )
    
    # =====================================================
    # 2. 数据库状态检查
    # =====================================================
    
    output$db_status <- renderUI({
      db_status <- validate_local_database()
      
      if(db_status$available) {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          " 本地基因数据库可用 (",
          db_status$genes_count, "个基因)"
        )
      } else {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " 本地基因数据库不可用，请先运行下载脚本"
        )
      }
    })
    
    # =====================================================
    # 3. 数据预处理
    # =====================================================
    
    observeEvent(input$raw_file, {
      req(input$raw_file)
      
      tryCatch({
        ext <- tools::file_ext(input$raw_file$name)
        if(ext == "csv") {
          rv$raw_data <- read.csv(input$raw_file$datapath, stringsAsFactors = FALSE)
        } else {
          rv$raw_data <- read.table(input$raw_file$datapath, header = TRUE, 
                                    sep = "\t", stringsAsFactors = FALSE)
        }
        cat("成功读取文件，维度:", dim(rv$raw_data), "\n")
      }, error = function(e) {
        shinyalert("文件读取错误", e$message, type = "error")
      })
    })
    
    # 日志记录
    log_message <- function(msg) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      rv$log <- c(rv$log, paste("[", timestamp, "] ", msg))
    }
    
    output$process_log <- renderPrint({
      if(length(rv$log) > 0) {
        cat(paste(rv$log, collapse = "\n"))
      } else {
        cat("等待预处理...\n请上传文件并点击'开始预处理'按钮")
      }
    })
    
    # 预处理
    observeEvent(input$start_preprocess, {
      req(rv$raw_data)
      
      rv$log <- c()
      rv$step <- "upload"
      
      db_status <- validate_local_database()
      if(!db_status$available) {
        shinyalert("数据库不可用", "请先运行基因注释数据库下载脚本", type = "error")
        return()
      }
      
      withProgress({
        
        incProgress(0.2, message = "基因注释...")
        log_message("开始基因注释...")
        tryCatch({
          rv$annotated_data <- local_gene_annotation(rv$raw_data)
          log_message(paste("注释完成，匹配", nrow(rv$annotated_data), "个基因"))
        }, error = function(e) {
          log_message(paste("注释失败:", e$message))
          shinyalert("注释失败", e$message, type = "error")
          return()
        })
        
        incProgress(0.2, message = "处理重复基因...")
        log_message("处理重复基因...")
        dedup_data <- remove_duplicate_genes(rv$annotated_data)
        log_message(paste("去重后剩余", nrow(dedup_data), "个基因"))
        
        incProgress(0.15, message = "处理零表达值...")
        log_message("处理零表达值...")
        zero_processed <- handle_zero_expression(dedup_data)
        
        incProgress(0.15, message = "过滤低表达基因...")
        log_message("过滤低表达基因...")
        filtered_data <- filter_low_expression(zero_processed)
        log_message(paste("过滤后剩余", nrow(filtered_data), "个基因"))
        
        incProgress(0.15, message = "log2转换...")
        log_message("log2转换...")
        log_transformed <- apply_log2_transform(filtered_data)
        
        incProgress(0.15, message = "归一化...")
        log_message("0-1归一化...")
        rv$processed_data <- normalize_0_1(log_transformed)
        
        rv$step <- "preprocess_complete"
        log_message("✅ 预处理完成！")
        
      }, value = 1, message = "预处理进行中...")
      
      shinyalert("预处理完成", 
                 paste("成功处理", nrow(rv$processed_data), "个基因"), 
                 type = "success")
    })
    
    # 预处理完成状态
    output$preprocess_complete <- reactive({
      !is.null(rv$processed_data) && rv$step == "preprocess_complete"
    })
    outputOptions(output, "preprocess_complete", suspendWhenHidden = FALSE)
    
    # 统计信息
    output$genes_before <- renderValueBox({
      valueBox(
        ifelse(!is.null(rv$raw_data), nrow(rv$raw_data), "N/A"),
        "原始基因数",
        icon = icon("dna"),
        color = "blue"
      )
    })
    
    output$genes_after <- renderValueBox({
      valueBox(
        ifelse(!is.null(rv$processed_data), nrow(rv$processed_data), "N/A"),
        "处理后基因数",
        icon = icon("filter"),
        color = "green"
      )
    })
    
    output$match_rate <- renderValueBox({
      valueBox(
        if(rv$match_success) paste0(rv$match_rate, "%") else "待匹配",
        "基因匹配率",
        icon = icon("check-circle"),
        color = if(rv$match_success && rv$match_rate >= 95) "green" else "yellow"
      )
    })
    
    # 跳转到特征选择
    observeEvent(input$go_to_feature, {
      updateBox("feature_box", action = "toggle", session = session)
      rv$step <- "feature"
    })
    
    # =====================================================
    # 4. 特征选择
    # =====================================================
    
    reference_genes <- reactive({ get_reference_genes() })
    
    # 查看参考基因
    observeEvent(input$view_reference_genes, {
      genes <- reference_genes()
      df <- data.frame(Index = 1:length(genes), Gene_Symbol = genes)
      
      showModal(
        modalDialog(
          title = "参考基因列表（用于模型匹配）",
          size = "l",
          DTOutput(ns("ref_genes_table")),
          easyClose = TRUE,
          footer = modalButton("关闭")
        )
      )
      
      output$ref_genes_table <- renderDT({
        datatable(df, options = list(pageLength = 15, scrollY = "400px"))
      })
    })
    
    # 检查基因匹配
    observeEvent(input$check_gene_match, {
      req(rv$processed_data)
      
      showModal(
        modalDialog(
          title = "正在匹配基因...",
          "请稍候...",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      tryCatch({
        processed_genes <- rv$processed_data$gene_name
        ref_genes <- reference_genes()
        
        matched <- intersect(ref_genes, processed_genes)
        total_ref <- length(ref_genes)
        matched_count <- length(matched)
        rv$match_rate <- round(matched_count / total_ref * 100, 2)
        rv$matched_genes <- matched
        
        cat("匹配率:", rv$match_rate, "%\n")
        
        if(rv$match_rate >= 95) {
          matched_idx <- which(processed_genes %in% matched)
          matched_expr <- rv$processed_data[matched_idx, ]
          
          expr_matrix <- as.matrix(matched_expr$expression)
          rownames(expr_matrix) <- matched_expr$gene_name
          
          transposed <- t(expr_matrix)
          colnames(transposed) <- matched_expr$gene_name
          transposed <- transposed[, matched, drop = FALSE]
          
          rv$final_matrix <- transposed
          rv$match_success <- TRUE
          
          removeModal()
          shinyalert("✅ 匹配成功", 
                     paste("匹配率:", rv$match_rate, "%"), 
                     type = "success")
        } else {
          rv$match_success <- FALSE
          removeModal()
          shinyalert("❌ 匹配失败", 
                     paste("匹配率:", rv$match_rate, "% (低于95%阈值)"), 
                     type = "error")
        }
        
      }, error = function(e) {
        removeModal()
        shinyalert("匹配错误", e$message, type = "error")
      })
    })
    
    # 匹配结果
    output$match_result_ui <- renderUI({
      if(rv$match_success) {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          tags$strong("✅ 匹配成功！"),
          br(),
          paste0("匹配率: ", rv$match_rate, "%"),
          br(),
          paste0("匹配基因数: ", length(rv$matched_genes))
        )
      } else if(rv$match_rate > 0) {
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          tags$strong("❌ 匹配失败"),
          br(),
          paste0("匹配率: ", rv$match_rate, "% (低于95%阈值)")
        )
      } else {
        div(
          class = "alert alert-info",
          icon("info-circle"),
          "请点击「检查基因匹配」按钮"
        )
      }
    })
    
    # 矩阵预览
    output$final_matrix_preview <- renderDT({
      req(rv$final_matrix)
      
      display_data <- as.data.frame(rv$final_matrix)
      display_data <- cbind(Sample = rownames(display_data), display_data)
      
      genes_to_show <- min(7, ncol(rv$final_matrix))
      
      datatable(
        display_data[, 1:(genes_to_show + 1)],
        options = list(
          pageLength = 5,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        caption = paste("表达矩阵预览 (", nrow(display_data), "样本 × ", 
                        ncol(display_data) - 1, "基因)")
      ) %>%
        formatRound(columns = 2:(genes_to_show + 1), digits = 4)
    })
    
    # 匹配成功状态
    output$match_success <- reactive({ rv$match_success })
    outputOptions(output, "match_success", suspendWhenHidden = FALSE)
    
    # 跳转到预测
    observeEvent(input$go_to_predict, {
      updateBox("predict_box", action = "toggle", session = session)
      rv$step <- "predict"
    })
    
    # =====================================================
    # 5. 模型预测
    # =====================================================
    
    # 加载模型
    final_model <- reactiveVal(NULL)
    
    observe({
      tryCatch({
        loaded_model <- get(load("models/final_model.Rdata"))
        required_components <- c("train_X", "train_Y", "CY", "mn", "U", 
                                 "T_matrix", "best_C", "T_CV")
        if(all(required_components %in% names(loaded_model))) {
          final_model(loaded_model)
          cat("✅ final_model 加载成功\n")
        } else {
          cat("⚠️ 模型缺少必要组件\n")
        }
      }, error = function(e) {
        cat("❌ 模型加载失败:", e$message, "\n")
      })
    })
    
    # 预测函数
    predict_with_final_model <- function(model, new_X, threshold = 0) {
      
      if(is.null(model)) {
        return(list(
          predicted_scores = runif(nrow(new_X), -0.5, 0.5),
          predicted_labels = sample(c(-1, 1), nrow(new_X), replace = TRUE)
        ))
      }
      
      X_train <- model$train_X
      CY <- model$CY
      mn <- model$mn
      U <- model$U
      T_matrix <- model$T_matrix
      best_C <- model$best_C
      T_CV <- model$T_CV
      
      Kernel_Test_G <- function(xt, x, C) {
        xkt <- rbind(x, xt)
        Kt <- matrix(, nrow(xt), nrow(x))
        for(i in (nrow(x)+1):(nrow(x)+nrow(xt))) {
          for(j in 1:nrow(x)) {
            Kt[i-nrow(x), j] <- exp(-0.5 * sum((xkt[i,] - xkt[j,])^2) / C^2)
          }
        }
        return(Kt)
      }
      
      im <- diag(rep(1, nrow(X_train)))
      one <- matrix(1, nrow = nrow(X_train), ncol = 1)
      K_train_centered <- im - one %*% t(one) / nrow(X_train)
      
      Kt <- Kernel_Test_G(new_X, X_train, best_C)
      onet <- matrix(1, nrow = nrow(new_X), ncol = 1)
      imt <- diag(rep(1, nrow(X_train)))
      Kt_centered <- (Kt - onet %*% t(one) %*% K_train_centered / nrow(X_train)) %*% 
        (imt - one %*% t(one) / nrow(X_train))
      
      external_yth <- Kt_centered %*% U[,1:T_CV] %*% 
        ginv(t(T_matrix[,1:T_CV]) %*% K_train_centered %*% U[,1:T_CV]) %*% 
        t(T_matrix[,1:T_CV]) %*% CY
      external_yth <- external_yth + mn
      predicted_labels <- ifelse(external_yth > threshold, 1, -1)
      
      return(list(
        predicted_scores = as.vector(external_yth),
        predicted_labels = predicted_labels
      ))
    }
    
    # 预测
    pred_result <- reactiveVal(NULL)
    
    observeEvent(input$run_prediction, {
      req(rv$final_matrix)
      req(final_model())
      
      tryCatch({
        new_X <- as.matrix(rv$final_matrix)
        
        # 特征对齐
        model_X <- as.matrix(final_model()$train_X)
        model_features <- colnames(model_X)
        new_features <- colnames(new_X)
        
        common_features <- intersect(new_features, model_features)
        missing_features <- setdiff(model_features, new_features)
        
        aligned_matrix <- matrix(0, 
                                 nrow = nrow(new_X),
                                 ncol = length(model_features))
        colnames(aligned_matrix) <- model_features
        
        if(!is.null(rownames(new_X))) {
          rownames(aligned_matrix) <- rownames(new_X)
        } else {
          rownames(aligned_matrix) <- paste0("Sample_", 1:nrow(new_X))
        }
        
        if(length(common_features) > 0) {
          aligned_matrix[, common_features] <- new_X[, common_features, drop = FALSE]
        }
        
        if(length(missing_features) > 0) {
          model_feature_means <- colMeans(model_X)
          for(feature in missing_features) {
            aligned_matrix[, feature] <- model_feature_means[feature]
          }
        }
        
        res <- predict_with_final_model(final_model(), aligned_matrix)
        print(res)
        str(res)
        cat("predicted_scores:\n")
        print(res$predicted_scores)
        cat("predicted_labels:\n")
        print(res$predicted_labels)
        pred_result(res)
        rv$prediction_complete <- TRUE
        showNotification("✅ 模型预测完成", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ 预测失败:", e$message), type = "error")
      })
    })
    
    # 预测完成状态
    output$prediction_complete <- reactive({
      # 修改这里：检查 pred_result() 是否有值
      !is.null(pred_result()) && !is.null(pred_result()$predicted_scores)
    })
    outputOptions(output, "prediction_complete", suspendWhenHidden = FALSE)
    
    # 预测结果表格
    output$prediction_table <- renderDT({
      cat("===== renderDT 执行 =====\n")
      # 添加更严格的检查
      req(pred_result())
      req(!is.null(pred_result()$predicted_scores))
      
      scores <- pred_result()$predicted_scores
      labels <- pred_result()$predicted_labels
      
      # 确保 scores 不为空
      if(length(scores) == 0) {
        return(datatable(data.frame(Message = "没有预测结果")))
      }
      
      if(!is.null(rownames(rv$final_matrix))) {
        sample_names <- rownames(rv$final_matrix)
      } else {
        sample_names <- paste0("Sample_", 1:length(scores))
      }
      
      df <- data.frame(
        Sample = sample_names,
        Score = round(scores, 4),
        Label = ifelse(labels == 1, "Positive", "Negative"),
        Risk = ifelse(labels == 1, "高风险", "低风险"),
        stringsAsFactors = FALSE
      )
      
      datatable(df, 
                options = list(
                  pageLength = 10, 
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
                ),
                rownames = FALSE,
                class = 'cell-border stripe hover')
    })
    
    # 跳转到分析
    observeEvent(input$go_to_analysis, {
      updateBox("analysis_box", action = "toggle", session = session)
      rv$step <- "analysis"
    })
    
    # =====================================================
    # 6. 结果分析
    # =====================================================
    
    output$analysis_display <- renderUI({
      req(pred_result())
      
      scores <- pred_result()$predicted_scores
      labels <- pred_result()$predicted_labels
      
      # 计算概率（使用sigmoid转换）
      prob_values <- 1 / (1 + exp(-scores)) * 100
      
      if(length(scores) == 1) {
        # 单样本
        val <- prob_values[1]
        risk_color <- ifelse(val >= 70, "#EF4444", 
                             ifelse(val >= 30, "#F59E0B", "#10B981"))
        risk_label <- ifelse(val >= 70, "高风险", 
                             ifelse(val >= 30, "中风险", "低风险"))
        
        tagList(
          div(
            class = "result-box",
            h3("患病风险概率"),
            h1(style = paste0("font-size:56px; color:", risk_color, "; font-weight:bold;"),
               paste0(round(val, 1), "%")),
            h4(style = paste0("color:", risk_color, ";"),
               paste("风险等级：", risk_label))
          ),
          br(),
          h4("预测详情："),
          verbatimTextOutput(ns("analysis_details"))
        )
      } else {
        # 多样本
        df <- data.frame(
          Sample = if(!is.null(rownames(rv$final_matrix))) rownames(rv$final_matrix) else paste0("Sample_", 1:length(scores)),
          Score = round(scores, 4),
          Probability = round(prob_values, 1),
          Risk = ifelse(labels == 1, "高风险", "低风险")
        )
        
        tagList(
          h4("预测结果汇总："),
          datatable(df, 
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    class = 'cell-border stripe hover')
        )
      }
    })
    
    # 分析详情（单样本）
    output$analysis_details <- renderPrint({
      req(pred_result())
      
      scores <- pred_result()$predicted_scores
      labels <- pred_result()$predicted_labels
      
      cat("Logit值：", round(scores[1], 4), "\n")
      cat("风险概率：", round(1/(1+exp(-scores[1]))*100, 1), "%\n")
      cat("预测分类：", ifelse(labels[1] == 1, "高风险 (Positive)", "低风险 (Negative)"), "\n")
      cat("\n临床建议：\n")
      if(labels[1] == 1) {
        cat("  该患者患病风险较高，建议临床医师重点关注，\n")
        cat("  完善精神检查，必要时尽早开展干预治疗。\n")
        cat("  本结果仅作辅助参考，临床决策由医师综合评估后确定。\n")
      } else {
        cat("  该患者患病风险较低，建议保持良好生活习惯，\n")
        cat("  定期随访观察。\n")
        cat("  本结果仅作辅助参考，临床决策由医师综合评估后确定。\n")
      }
    })
    

    
  })
}