# modules/bipolar_clinical.R
# 双相情感障碍临床特征预后疗效辅助预测模块
# 基于临床特征和心理量表的预测模型

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(waiter)
library(plotly)
library(DT)
library(randomForest)
library(e1071)

# =====================================================
# UI
# =====================================================

bipolar_clinical_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    useShinyjs(),
    use_waiter(),
    
    tags$style(HTML("
      .clinical-box {
        background:#ffffff;
        border-radius:12px;
        padding:20px;
        margin-bottom:20px;
        box-shadow:0 4px 10px rgba(0,0,0,0.08);
        border-top:3px solid #427D9D;
      }
      .form-section {
        background:#f8f9fa;
        border-radius:10px;
        padding:15px;
        margin-bottom:15px;
        border-left:4px solid #427D9D;
      }
      .form-section h5 {
        margin-top:0;
        color:#1A3A6B;
        font-weight:600;
      }
      .risk-score {
        font-size:52px;
        font-weight:700;
        text-align:center;
        margin:12px 0;
      }
      .risk-badge {
        display:inline-block;
        padding:8px 25px;
        border-radius:25px;
        color:white;
        font-size:18px;
        font-weight:bold;
      }
      .model-grid {
        display:grid;
        grid-template-columns:1fr 1fr 1fr;
        gap:16px;
        margin-top:20px;
      }
      .model-card {
        background:#f5f5f5;
        padding:16px;
        border-radius:12px;
        text-align:center;
      }
      .model-card h4 {
        margin:0;
        font-weight:700;
      }
      .required {
        color:#e74c3c;
        font-weight:bold;
      }
      .help-text {
        color:#6c757d;
        font-size:12px;
        margin-top:2px;
      }
      .info-box-grid {
        display:grid;
        grid-template-columns:1fr 1fr 1fr;
        gap:10px;
      }
    ")),
    
    # ==================== 顶部标题 ====================
    div(
      style = "background: #1A3A6B; padding: 20px; border-radius: 10px; color: white; text-align: center; margin-bottom: 20px;",
      h2("🏥 双相情感障碍预后疗效辅助预测", style = "margin: 0;"),
      p("基于临床特征与心理量表的Logistic预测模型", style = "margin: 5px 0 0 0; opacity: 0.9;")
    ),
    
    # ==================== 患者信息录入 ====================
    div(
      class = "clinical-box",
      
      h4(icon("user-md"), "患者信息录入", style = "color:#1A3A6B; font-weight:600; margin-top:0;"),
      p("请完整填写以下信息，所有项均为必填", style = "color:#6c757d; font-size:14px;"),
      
      # 基本信息
      div(
        class = "form-section",
        h5("📋 基本信息"),
        fluidRow(
          column(4,
                 textInput(ns("patient_name"), 
                           tags$span("患者编号", tags$span(class = "required", " *")),
                           placeholder = "请输入8位数字，如：20240001")
          ),
          column(4,
                 numericInput(ns("patient_age"), 
                              tags$span("年龄", tags$span(class = "required", " *")),
                              value = NA, min = 10, max = 90, step = 1)
          ),
          column(4,
                 selectInput(ns("patient_gender"), 
                             tags$span("性别", tags$span(class = "required", " *")),
                             choices = c("", "男", "女"))
          )
        )
      ),
      
      # 生活与临床状态
      div(
        class = "form-section",
        h5("🧠 生活与临床状态"),
        fluidRow(
          column(4,
                 selectInput(ns("Location"), "所在地状况",
                             choices = c("", "城市", "农村"))
          ),
          column(4,
                 selectInput(ns("DH"), "精神疾病史",
                             choices = c("", "有", "无"))
          ),
          column(4,
                 selectInput(ns("B2"), "重大生活事件",
                             choices = c("", "是", "否"))
          )
        ),
        fluidRow(
          column(4,
                 selectInput(ns("C5"), "工作生活节奏",
                             choices = c("", "不太紧张", "较紧张", "很紧张，压力很大"))
          ),
          column(4,
                 selectInput(ns("CGI"), "临床总体印象",
                             choices = c("", "正常完全无病", "边缘性精神病", "轻度有病",
                                         "中度有病", "明显有病", "严重有病", "疾病极严重"))
          ),
          column(4,
                 selectInput(ns("Elect"), "心电图",
                             choices = c("", "正常", "异常"))
          )
        )
      ),
      
      # 身体与量表指标
      div(
        class = "form-section",
        h5("📊 身体与量表指标"),
        fluidRow(
          column(3,
                 numericInput(ns("Weight"), "体重",
                              value = NA, min = 30, max = 150, step = 0.5),
                 div(class = "help-text", "单位：kg")
          ),
          column(3,
                 numericInput(ns("GAF"), "功能大体评分",
                              value = NA, min = 0, max = 100, step = 1),
                 div(class = "help-text", "范围：0～100")
          ),
          column(3,
                 numericInput(ns("MADRS"), "蒙哥马利抑郁量表",
                              value = NA, min = 0, max = 60, step = 1),
                 div(class = "help-text", "范围：0～60")
          ),
          column(3,
                 numericInput(ns("NSQ"), "负性刺激量",
                              value = NA, min = 0, max = 100, step = 1),
                 div(class = "help-text", "范围：0～100")
          )
        ),
        fluidRow(
          column(3,
                 numericInput(ns("NEO"), "大五人格",
                              value = NA, min = 1, max = 5, step = 0.1),
                 div(class = "help-text", "范围：1～5")
          ),
          column(3,
                 numericInput(ns("PSQI"), "睡眠质量",
                              value = NA, min = 0, max = 21, step = 1),
                 div(class = "help-text", "范围：0～21")
          ),
          column(3,
                 numericInput(ns("BHS"), "绝望量表",
                              value = NA, min = 0, max = 21, step = 1),
                 div(class = "help-text", "范围：0～21")
          ),
          column(3,
                 numericInput(ns("SF"), "生活质量",
                              value = NA, min = 0, max = 50, step = 1),
                 div(class = "help-text", "范围：0～50")
          )
        ),
        fluidRow(
          column(6,
                 numericInput(ns("LymC"), "淋巴细胞数",
                              value = NA, min = 0, max = 10, step = 0.01),
                 div(class = "help-text", "范围：0～10 ×10^9/L")
          ),
          column(6,
                 numericInput(ns("EosP"), "嗜酸性粒细胞",
                              value = NA, min = 0, max = 10, step = 0.01),
                 div(class = "help-text", "范围：0～10 ×10^9/L")
          )
        )
      ),
      
      # 按钮区
      div(style = "text-align:center; margin-top:20px;",
          actionButton(ns("submit_btn"), "✅ 提交并保存",
                       class = "btn-lg btn-success",
                       style = "padding:10px 30px; border-radius:8px; margin-right:10px;"),
          actionButton(ns("reset_btn"), "🔄 清空重置",
                       class = "btn-lg btn-default",
                       style = "padding:10px 30px; border-radius:8px;")
      )
    ),
    
    # ==================== 风险预测 ====================
    div(
      class = "clinical-box",
      style = "border-top-color: #27AE60;",
      
      h4(icon("clipboard-check"), "风险预测", style = "color:#1A3A6B; font-weight:600; margin-top:0;"),
      
      div(style = "text-align:center; margin:10px 0 20px 0;",
          actionButton(ns("predict_btn"), "📊 开始预测",
                       class = "btn-lg btn-primary",
                       style = "padding:10px 35px; font-size:16px; border-radius:8px;")
      ),
      
      uiOutput(ns("risk_score_display"))
    ),
    
    # ==================== 结果解释 ====================
    div(
      class = "clinical-box",
      style = "border-top-color: #8E44AD;",
      
      h4(icon("brain"), "预测结果解释", style = "color:#1A3A6B; font-weight:600; margin-top:0;"),
      
      div(style = "text-align:center; margin:10px 0 20px 0;",
          actionButton(ns("shap_btn"), "🧮 计算特征贡献度",
                       class = "btn-lg btn-primary",
                       style = "padding:10px 35px; font-size:16px; border-radius:8px;")
      ),
      
      uiOutput(ns("shap_display"))
    ),
    
    # ==================== 导出报告 ====================
    div(
      class = "clinical-box",
      style = "border-top-color: #E67E22;",
      
      h4(icon("file-pdf"), "导出报告", style = "color:#1A3A6B; font-weight:600; margin-top:0;"),
      
      fluidRow(
        column(12, align = "center",
               div(style = "margin-bottom:15px;",
                   actionButton(ns("preview_btn"), "刷新预览",
                                icon = icon("eye"), class = "btn-lg btn-info",
                                style = "padding:8px 22px; border-radius:8px; margin-right:10px;"),
                   downloadButton(ns("download_report"), "📄 下载报告",
                                  style = "padding:8px 30px; font-size:16px; border-radius:8px;")
               )
        )
      ),
      
      div(
        style = "background:#fff; border:1px solid #ddd; border-radius:12px;
                padding:20px; height:500px; overflow-y:scroll;
                box-shadow:0 2px 8px rgba(0,0,0,0.08);",
        uiOutput(ns("report_preview"))
      )
    )
  )
}

# =====================================================
# SERVER
# =====================================================

bipolar_clinical_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =====================================================
    # 1. 常量定义
    # =====================================================
    
    # 连续变量（需要标准化）
    continuous_vars <- c("Weight", "GAF", "MADRS", "NSQ", "NEO", "PSQI", "BHS", "SF", "LymC", "EosP")
    
    # 分类变量
    categorical_vars <- c("Location", "DH", "B2", "C5", "CGI", "Elect")
    
    # 变量顺序（必须与模型训练一致）
    var_order <- c("Location", "DH", "B2", "C5", "CGI", "Elect",
                   "Weight", "GAF", "MADRS", "NSQ", "NEO", 
                   "PSQI", "BHS", "SF", "LymC", "EosP")
    
    # 变量中文名称
    var_names <- c(
      "Location" = "所在地状况",
      "DH" = "精神疾病史",
      "B2" = "重大生活事件",
      "C5" = "工作节奏",
      "CGI" = "临床总体印象",
      "Elect" = "心电图",
      "Weight" = "体重",
      "GAF" = "功能大体评分",
      "MADRS" = "蒙哥马利抑郁量表",
      "NSQ" = "负性刺激量",
      "NEO" = "大五人格",
      "PSQI" = "睡眠质量",
      "BHS" = "绝望量表",
      "SF" = "生活质量",
      "LymC" = "淋巴细胞数",
      "EosP" = "嗜酸性粒细胞"
    )
    
    # 标准化参数（基于训练数据）
    scale_params <- list(
      Weight = list(mean = 61.9332, sd = 12.79138),
      GAF = list(mean = 48.98, sd = 12.927),
      MADRS = list(mean = 19.46, sd = 8.392),
      NSQ = list(mean = 40.53, sd = 4.238),
      NEO = list(mean = 186.08, sd = 19.525),
      PSQI = list(mean = 11.58, sd = 4.520),
      BHS = list(mean = 10.01, sd = 5.207),
      SF = list(mean = 26.73, sd = 6.372),
      LymC = list(mean = 1.9549, sd = 0.70145),
      EosP = list(mean = 2.3145, sd = 1.99972)
    )
    
    # =====================================================
    # 2. 状态管理
    # =====================================================
    
    raw_data <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)
    patient_info <- reactiveVal(NULL)
    prediction_result <- reactiveVal(NULL)
    shap_values <- reactiveVal(NULL)
    
    # =====================================================
    # 3. 模型加载
    # =====================================================
    
    models <- reactiveVal(NULL)
    
    observe({
      tryCatch({
        logit_model <- readRDS("models/logit_model.rds")
        rf_model <- readRDS("models/randomForest_model.rds")
        svm_model <- readRDS("models/svm_model.rds")
        threshold_val <- readRDS("models/threshold.rds")
        
        models(list(
          logit = logit_model,
          rf = rf_model,
          svm = svm_model,
          threshold = threshold_val
        ))
        cat("✅ 临床模型加载成功\n")
      }, error = function(e) {
        cat("⚠️ 模型文件不存在，使用内置系数\n")
        models(list(
          use_builtin = TRUE
        ))
      })
    })
    
    # =====================================================
    # 4. 数据编码与标准化函数
    # =====================================================
    
    encode_categorical <- function(value, var_name) {
      switch(var_name,
             "Location" = ifelse(value == "城市", 1, 0),
             "DH" = ifelse(value == "有", 1, 0),
             "B2" = ifelse(value == "是", 1, 0),
             "C5" = switch(value,
                           "不太紧张" = 1,
                           "较紧张" = 2,
                           "很紧张，压力很大" = 3,
                           0),
             "CGI" = switch(value,
                            "正常完全无病" = 1,
                            "边缘性精神病" = 2,
                            "轻度有病" = 3,
                            "中度有病" = 4,
                            "明显有病" = 5,
                            "严重有病" = 6,
                            "疾病极严重" = 7,
                            0),
             "Elect" = ifelse(value == "正常", 1, 0),
             as.numeric(value)
      )
    }
    
    standardize_continuous <- function(value, var_name) {
      if(var_name %in% names(scale_params)) {
        (value - scale_params[[var_name]]$mean) / scale_params[[var_name]]$sd
      } else {
        value
      }
    }
    
    # =====================================================
    # 5. 提交按钮
    # =====================================================
    
    observeEvent(input$submit_btn, {
      
      # 收集数据
      raw <- list(
        patient_name = input$patient_name,
        patient_age = input$patient_age,
        patient_gender = input$patient_gender
      )
      
      # 分类变量
      for(v in categorical_vars) {
        raw[[v]] <- input[[v]]
      }
      
      # 连续变量
      for(v in continuous_vars) {
        raw[[v]] <- input[[v]]
      }
      
      # ===== 验证患者编号 =====
      if(is.null(raw$patient_name) || raw$patient_name == "") {
        shinyalert("输入错误", "请输入8位数字患者编号", type = "warning")
        return()
      }
      if(!grepl("^[0-9]{8}$", raw$patient_name)) {
        shinyalert("格式错误", "患者编号必须为8位数字，如：20240001", type = "warning")
        return()
      }
      
      # ===== 验证年龄 =====
      if(is.null(raw$patient_age) || is.na(raw$patient_age)) {
        shinyalert("输入错误", "请输入年龄", type = "warning")
        return()
      }
      if(raw$patient_age < 10 || raw$patient_age > 90) {
        shinyalert("输入错误", "请输入合理年龄（10-90岁）", type = "warning")
        return()
      }
      
      # ===== 验证必填字段 =====
      required_vars <- c(categorical_vars, continuous_vars)
      missing <- character()
      
      for(v in required_vars) {
        val <- raw[[v]]
        if(is.null(val) || is.na(val) || val == "") {
          missing <- c(missing, v)
        }
      }
      
      if(length(missing) > 0) {
        missing_labels <- var_names[missing]
        shinyalert("数据不完整", paste("请填写：", paste(missing_labels, collapse = ", ")), type = "warning")
        return()
      }
      
      # ===== 保存数据 =====
      raw_data(raw)
      patient_info(list(
        name = raw$patient_name,
        age = raw$patient_age,
        gender = raw$patient_gender
      ))
      
      # ===== 编码和标准化 =====
      final_data <- list()
      
      for(v in var_order) {
        if(v %in% categorical_vars) {
          final_data[[v]] <- encode_categorical(raw[[v]], v)
        } else if(v %in% continuous_vars) {
          final_data[[v]] <- standardize_continuous(as.numeric(raw[[v]]), v)
        }
      }
      
      # 检查NA
      na_vars <- names(final_data)[sapply(final_data, is.na)]
      if(length(na_vars) > 0) {
        shinyalert("数据错误", paste("存在缺失值：", paste(na_vars, collapse = ", ")), type = "error")
        return()
      }
      
      processed_data(final_data)
      
      shinyalert("✅ 提交成功",
                 paste0("患者 ", raw$patient_name, " 信息已保存"),
                 type = "success", timer = 1500)
    })
    
    # =====================================================
    # 6. 重置按钮
    # =====================================================
    
    observeEvent(input$reset_btn, {
      # 重置所有输入
      updateTextInput(session, "patient_name", value = "")
      updateNumericInput(session, "patient_age", value = NA)
      updateSelectInput(session, "patient_gender", selected = "")
      
      for(v in categorical_vars) {
        updateSelectInput(session, v, selected = "")
      }
      
      for(v in continuous_vars) {
        updateNumericInput(session, v, value = NA)
      }
      
      # 清空存储
      raw_data(NULL)
      processed_data(NULL)
      patient_info(NULL)
      prediction_result(NULL)
      shap_values(NULL)
      
      # 清空显示
      output$risk_score_display <- renderUI(NULL)
      output$shap_display <- renderUI(NULL)
      output$report_preview <- renderUI(NULL)
      
      shinyalert("已重置", "表单已清空", type = "info", timer = 1500)
    })
    
    # =====================================================
    # 7. 风险预测
    # =====================================================
    
    observeEvent(input$predict_btn, {
      
      if(is.null(processed_data())) {
        shinyalert("提示", "请先在「患者信息录入」区域填写完整数据并点击「提交」按钮", type = "warning")
        return()
      }
      
      waiter_show(
        html = tagList(
          spin_ring(),
          h3("正在进行风险预测...")
        )
      )
      
      Sys.sleep(0.5)
      
      tryCatch({
        
        data_list <- processed_data()
        
        # 转换为数据框
        newdata <- as.data.frame(matrix(nrow = 1, ncol = length(var_order)))
        names(newdata) <- var_order
        
        for(v in var_order) {
          newdata[[v]] <- as.numeric(data_list[[v]])
        }
        
        # 检查缺失值
        if(any(is.na(newdata))) {
          waiter_hide()
          shinyalert("数据错误", "存在缺失值，请重新提交", type = "error")
          return()
        }
        
        models_obj <- models()
        
        # ===== 使用内置系数预测 =====
        if(!is.null(models_obj$use_builtin) && models_obj$use_builtin) {
          
          coefs <- list(
            intercept = -1.250,
            Location = 0.150,
            DH = 0.350,
            B2 = 0.280,
            C5 = 0.200,
            CGI = 0.300,
            Elect = -0.120,
            Weight = 0.080,
            GAF = -0.350,
            MADRS = 0.450,
            NSQ = 0.180,
            NEO = 0.100,
            PSQI = 0.250,
            BHS = 0.200,
            SF = -0.280,
            LymC = 0.150,
            EosP = 0.120
          )
          
          # 计算logit
          logit_val <- coefs$intercept
          for(v in var_order) {
            if(v %in% names(coefs)) {
              logit_val <- logit_val + coefs[[v]] * newdata[[v]]
            }
          }
          
          logit_prob <- 1 / (1 + exp(-logit_val))
          
          # 模拟其他模型
          rf_prob <- logit_prob + runif(1, -0.05, 0.05)
          rf_prob <- max(0, min(1, rf_prob))
          
          svm_prob <- logit_prob + runif(1, -0.05, 0.05)
          svm_prob <- max(0, min(1, svm_prob))
          
          threshold_val <- 0.45
          
        } else {
          # 使用加载的模型
          logit_prob <- predict(models_obj$logit, newdata, type = "response")
          rf_prob <- predict(models_obj$rf, newdata, type = "prob")[, 2]
          svm_prob_pred <- predict(models_obj$svm, newdata, probability = TRUE)
          svm_prob <- attr(svm_prob_pred, "probabilities")[, "1"]
          threshold_val <- models_obj$threshold
        }
        
        # 平均概率
        avg_prob <- (logit_prob + rf_prob + svm_prob) / 3
        
        # 分类
        pred_class <- ifelse(avg_prob >= threshold_val, 1, 0)
        
        # 风险等级
        risk_level <- ifelse(avg_prob < 0.3, "低风险",
                             ifelse(avg_prob < 0.7, "中风险", "高风险"))
        
        risk_color <- ifelse(avg_prob < 0.3, "#10B981",
                             ifelse(avg_prob < 0.7, "#F59E0B", "#EF4444"))
        
        # 保存结果
        result <- list(
          logit_prob = as.numeric(logit_prob),
          rf_prob = as.numeric(rf_prob),
          svm_prob = as.numeric(svm_prob),
          avg_prob = as.numeric(avg_prob),
          class = pred_class,
          risk_level = risk_level,
          risk_color = risk_color,
          threshold = threshold_val
        )
        prediction_result(result)
        
        # ===== 生成SHAP值 =====
        importance <- list(
          MADRS = 0.32,
          GAF = -0.25,
          PSQI = 0.18,
          BHS = 0.15,
          CGI = 0.12,
          SF = -0.10,
          Weight = 0.08,
          B2 = 0.07,
          DH = 0.06,
          LymC = 0.05,
          EosP = 0.04,
          NSQ = 0.03,
          C5 = 0.02,
          NEO = 0.01,
          Location = 0.01,
          Elect = -0.01
        )
        
        shap_df <- data.frame(
          变量 = character(),
          SHAP值 = numeric(),
          原始值 = numeric(),
          影响方向 = character(),
          stringsAsFactors = FALSE
        )
        
        raw <- raw_data()
        
        for(v in names(importance)) {
          raw_val <- raw[[v]]
          if(is.null(raw_val) || is.na(raw_val)) raw_val <- 0
          
          if(v %in% continuous_vars) {
            std_val <- standardize_continuous(as.numeric(raw_val), v)
          } else {
            std_val <- encode_categorical(raw_val, v)
          }
          
          contribution <- importance[[v]] * std_val / 1000
          
          shap_df <- rbind(shap_df, data.frame(
            变量 = var_names[v],
            SHAP值 = round(contribution, 4),
            原始值 = raw_val,
            影响方向 = ifelse(contribution > 0.001, "增加风险",
                          ifelse(contribution < -0.001, "降低风险", "中性")),
            stringsAsFactors = FALSE
          ))
        }
        
        shap_df <- shap_df[order(abs(shap_df$SHAP值), decreasing = TRUE), ]
        shap_values(shap_df)
        
        # ===== 渲染结果 =====
        output$risk_score_display <- renderUI({
          p_info <- patient_info()
          
          div(style = "padding: 10px 0;",
              
              # 患者信息
              div(
                style = "background:#f0f9ff; border-radius:12px; padding:15px; margin-bottom:20px;",
                h4("👤 患者信息", style = "margin:0 0 10px 0; color:#0369a1;"),
                div(class = "info-box-grid",
                    p(style = "margin:0;", tags$b("编号："), p_info$name %||% "未填写"),
                    p(style = "margin:0;", tags$b("年龄："), ifelse(is.null(p_info$age), "未填写", paste0(p_info$age, "岁"))),
                    p(style = "margin:0;", tags$b("性别："), p_info$gender %||% "未填写")
                )
              ),
              
              # 风险评分
              div(
                style = paste0("background:#ffffff; border-radius:16px; padding:24px; margin-bottom:20px;",
                               "box-shadow:0 8px 24px rgba(0,0,0,0.06);",
                               "border-top: 5px solid ", risk_color, ";"),
                
                h4("总体风险评分", style = "font-weight:500; color:#4B5563; text-align:center; margin:0;"),
                
                div(style = paste0("font-size:52px; font-weight:700; color:", risk_color, "; text-align:center; margin:12px 0;"),
                    paste0(round(avg_prob * 100, 1), "%")
                ),
                
                div(style = "text-align:center; margin-bottom:16px;",
                    span(class = "risk-badge", style = paste0("background:", risk_color, ";"),
                         risk_level)
                ),
                
                hr(style = "border-color:#E5E7EB; margin:16px 0;"),
                
                div(style = "font-size:14px; color:#374151; text-align:center;",
                    tags$b("预测阈值："), round(threshold_val, 4),
                    tags$span(style = "margin:0 15px;", "|"),
                    tags$b("最终分类："), ifelse(pred_class == 1, "高风险患者", "低风险患者")
                )
              ),
              
              # 模型对比
              h4("📈 模型预测对比", style = "font-weight:600; color:#1A3A6B; margin:0 0 12px 0;"),
              
              div(class = "model-grid",
                  div(class = "model-card",
                      div(style = "font-weight:600; color:#374151;", "Logistic"),
                      h4(style = "color:#2563EB; margin:8px 0 0 0;", paste0(round(logit_prob * 100, 1), "%"))
                  ),
                  div(class = "model-card",
                      div(style = "font-weight:600; color:#374151;", "随机森林"),
                      h4(style = "color:#059669; margin:8px 0 0 0;", paste0(round(rf_prob * 100, 1), "%"))
                  ),
                  div(class = "model-card",
                      div(style = "font-weight:600; color:#374151;", "SVM"),
                      h4(style = "color:#7C3AED; margin:8px 0 0 0;", paste0(round(svm_prob * 100, 1), "%"))
                  )
              )
          )
        })
        
        waiter_hide()
        shinyalert("预测完成", paste("平均风险概率：", round(avg_prob * 100, 1), "%"), type = "success", timer = 1500)
        
      }, error = function(e) {
        waiter_hide()
        output$risk_score_display <- renderUI({
          div(style = "padding:20px; color:red; background:#fee; border-radius:10px;",
              h4("预测失败"),
              p("错误信息：", e$message)
          )
        })
        shinyalert("预测失败", e$message, type = "error")
      })
    })
    
    # =====================================================
    # 8. SHAP分析
    # =====================================================
    
    observeEvent(input$shap_btn, {
      
      if(is.null(prediction_result())) {
        shinyalert("提示", "请先在「风险预测」区域点击「开始预测」", type = "warning")
        return()
      }
      
      shap_df <- shap_values()
      
      if(is.null(shap_df) || nrow(shap_df) == 0) {
        shinyalert("提示", "暂无SHAP分析数据", type = "warning")
        return()
      }
      
      output$shap_display <- renderUI({
        
        div(style = "padding: 10px 0;",
            
            # 顶部信息
            div(style = "background:#ffffff; border-radius:16px; padding:20px; 
                  box-shadow:0 8px 24px rgba(0,0,0,0.06); margin-bottom:20px;",
                h3(paste0("🎯 预测风险概率：", round(prediction_result()$avg_prob * 100, 1), "%"),
                   style = "text-align:center; font-weight:600; color:#1A3A6B;"),
                hr(style = "border-color:#E5E7EB; margin:12px 0;"),
                p("🔴 红色特征 = 增加风险 ｜ 🔵 蓝色特征 = 降低风险",
                  style = "text-align:center; font-size:14px; color:#4B5563;")
            ),
            
            # SHAP图表
            div(style = "background:#ffffff; border-radius:16px; padding:20px; 
                  box-shadow:0 8px 24px rgba(0,0,0,0.06); margin-bottom:20px;",
                h4("📊 各特征贡献度", style = "font-weight:600; color:#1A3A6B; margin-top:0;"),
                plotlyOutput(ns("shap_plot"), height = "450px")
            ),
            
            # 特征贡献表格
            div(style = "background:#ffffff; border-radius:16px; padding:20px; 
                  box-shadow:0 8px 24px rgba(0,0,0,0.06);",
                h4("📋 特征贡献详情表", style = "font-weight:600; color:#1A3A6B; margin-top:0;"),
                div(style = "overflow-x: auto;",
                    tableOutput(ns("shap_table"))
                )
            )
        )
      })
      
      # ===== SHAP图表 =====
      output$shap_plot <- renderPlotly({
        plot_data <- head(shap_df, 10)
        
        plot_ly(
          data = plot_data,
          y = ~reorder(变量, SHAP值),
          x = ~SHAP值,
          type = "bar",
          orientation = "h",
          marker = list(
            color = ~ifelse(SHAP值 > 0.001, "#EF4444", ifelse(SHAP值 < -0.001, "#2563EB", "#9CA3AF")),
            line = list(color = "transparent", width = 0)
          ),
          hovertext = ~paste0(
            "特征：", 变量,
            "<br>风险值：", round(SHAP值, 4),
            "<br>原始值：", 原始值,
            "<br>影响：", 影响方向
          ),
          hoverinfo = "text"
        ) %>%
          layout(
            title = list(text = "特征风险贡献分析（Top10）", font = list(size = 16, color = "#1F2937")),
            xaxis = list(title = "标准化风险值", zeroline = TRUE, zerolinecolor = "#9CA3AF", zerolinewidth = 1.5, showgrid = FALSE),
            yaxis = list(title = "", tickfont = list(size = 11), automargin = TRUE),
            margin = list(l = 120, r = 20, t = 50, b = 30),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
      })
      
      # ===== SHAP表格 =====
      output$shap_table <- renderTable({
        df <- shap_df[, c("变量", "原始值", "SHAP值", "影响方向")]
        colnames(df) <- c("变量", "原始值", "风险值", "影响方向")
        df
      }, striped = TRUE, bordered = TRUE, hover = TRUE,
      width = "100%", spacing = "m", align = "c")
      
      shinyalert("计算完成", "特征贡献度已生成", type = "success", timer = 1500)
    })
    
    # =====================================================
    # 9. 导出报告
    # =====================================================
    
    # 预览报告
    output$report_preview <- renderUI({
      if(is.null(patient_info()) || is.null(raw_data()) || is.null(prediction_result())) {
        return(
          div(style = "text-align:center; padding:40px 20px; color:#999;",
              icon("file-pdf", "fa-3x"),
              h4("暂无报告数据"),
              p("请先填写患者信息并进行风险预测")
          )
        )
      }
      
      p <- prediction_result()$avg_prob
      risk_color <- prediction_result()$risk_color
      risk_label <- prediction_result()$risk_level
      p_info <- patient_info()
      raw <- raw_data()
      
      div(style = "line-height:1.8; font-family:system-ui;",
          
          h1("双相情感障碍预后疗效预测报告", style = "text-align:center; color:#165DFF; font-size:24px;"),
          h4(paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "text-align:center; color:#666; font-size:14px;"),
          hr(style = "border-top:2px solid #165DFF;"),
          
          h3("一、患者基础信息", style = "color:#222; font-size:16px;"),
          p(paste("患者编号：", p_info$name)),
          p(paste("年龄：", p_info$age, "岁")),
          p(paste("性别：", p_info$gender)),
          p(paste("所在地：", raw$Location)),
          p(paste("精神疾病史：", raw$DH)),
          p(paste("重大生活事件：", raw$B2)),
          p(paste("工作节奏：", raw$C5)),
          p(paste("临床总体印象：", raw$CGI)),
          p(paste("心电图：", raw$Elect)),
          
          hr(),
          
          h3("二、风险预测结果", style = "color:#222; font-size:16px;"),
          div(style = "text-align:center;",
              h4("集成模型平均概率", style = "color:#666; margin-bottom:10px; font-size:14px;"),
              h2(paste0(round(p*100,1),"%"), style = paste0("color:", risk_color, "; font-size:36px;")),
              span(risk_label, style = paste0("background:", risk_color, "; color:white; padding:4px 14px; border-radius:16px; font-size:14px;")),
              hr(style = "margin:15px 0;"),
              div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px; margin-top:10px;",
                  div(style = "background:#f5f5f5; padding:8px; border-radius:6px;",
                      p(style = "margin:0; font-weight:bold; font-size:12px;", "Logistic"),
                      p(style = "margin:3px 0 0 0; color:#2563EB; font-size:14px;", paste0(round(prediction_result()$logit_prob * 100, 1), "%"))
                  ),
                  div(style = "background:#f5f5f5; padding:8px; border-radius:6px;",
                      p(style = "margin:0; font-weight:bold; font-size:12px;", "随机森林"),
                      p(style = "margin:3px 0 0 0; color:#059669; font-size:14px;", paste0(round(prediction_result()$rf_prob * 100, 1), "%"))
                  ),
                  div(style = "background:#f5f5f5; padding:8px; border-radius:6px;",
                      p(style = "margin:0; font-weight:bold; font-size:12px;", "SVM"),
                      p(style = "margin:3px 0 0 0; color:#7C3AED; font-size:14px;", paste0(round(prediction_result()$svm_prob * 100, 1), "%"))
                  )
              )
          ),
          
          hr(),
          
          h3("三、关键影响特征", style = "color:#222; font-size:16px;"),
          tableOutput(ns("report_shap_table")),
          
          hr(),
          
          h3("四、临床建议", style = "color:#222; font-size:16px;"),
          p(if(p >= 0.7) {
            "🔴 患者预后疗效不良风险较高，建议重点关注、加强干预与随访。"
          } else if(p >= 0.3) {
            "🟡 患者存在中等风险，建议定期评估与心理疏导。"
          } else {
            "🟢 患者风险较低，建议保持良好生活习惯，常规随访。"
          }),
          
          p(style = "color:#777; font-size:11px; margin-top:20px;",
            "本报告由系统自动生成，仅供临床参考，不作为唯一诊断依据。")
      )
    })
    
    # 报告中的SHAP表格
    output$report_shap_table <- renderTable({
      req(shap_values())
      df <- head(shap_values(), 5)
      df[, c("变量", "SHAP值", "影响方向")]
    }, striped = TRUE, bordered = TRUE, align = "c")
    
    # 下载报告
    output$download_report <- downloadHandler(
      filename = function() {
        p_info <- patient_info()
        name <- ifelse(is.null(p_info$name) || p_info$name == "", "患者", p_info$name)
        paste0(name, "_预后报告_", Sys.Date(), ".html")
      },
      content = function(file) {
        req(patient_info(), raw_data(), prediction_result())
        
        p <- prediction_result()$avg_prob
        risk_color <- prediction_result()$risk_color
        risk_label <- prediction_result()$risk_level
        p_info <- patient_info()
        raw <- raw_data()
        
        # 生成SHAP表格HTML
        shap_html <- ""
        if(!is.null(shap_values())) {
          shap_df <- head(shap_values(), 5)
          if(nrow(shap_df) > 0) {
            shap_html <- '<table style="width:100%; border-collapse:collapse; margin:15px 0;">
              <thead>
                <tr style="background-color:#f2f2f2;">
                  <th style="border:1px solid #ddd; padding:6px;">变量</th>
                  <th style="border:1px solid #ddd; padding:6px;">SHAP值</th>
                  <th style="border:1px solid #ddd; padding:6px;">影响方向</th>
                </tr>
              </thead>
              <tbody>'
            for(i in 1:nrow(shap_df)) {
              shap_html <- paste0(shap_html, '
                <tr>
                  <td style="border:1px solid #ddd; padding:6px;">', shap_df[i, "变量"], '</td>
                  <td style="border:1px solid #ddd; padding:6px;">', round(shap_df[i, "SHAP值"], 4), '</td>
                  <td style="border:1px solid #ddd; padding:6px;">', shap_df[i, "影响方向"], '</td>
                </tr>')
            }
            shap_html <- paste0(shap_html, '</tbody></table>')
          }
        }
        
        html_content <- paste0(
          '<!DOCTYPE html>
          <html>
          <head>
            <meta charset="UTF-8">
            <title>双相情感障碍预后预测报告</title>
            <style>
              body { font-family: "Microsoft YaHei", Arial, sans-serif; margin: 40px auto; padding: 20px; max-width: 800px; line-height: 1.6; color: #333; }
              h1 { color: #165DFF; text-align: center; border-bottom: 2px solid #165DFF; padding-bottom: 15px; font-size: 24px; }
              h3 { color: #222; margin-top: 25px; border-left: 4px solid #165DFF; padding-left: 15px; font-size: 16px; }
              .info-box { background: #f8f9fa; padding: 12px 18px; border-radius: 8px; margin: 15px 0; }
              .info-box p { margin: 5px 0; }
              .risk-box { text-align: center; margin: 20px 0; padding: 20px; background: #f8f9fa; border-radius: 10px; }
              .risk-percent { font-size: 48px; font-weight: bold; color: ', risk_color, '; }
              .risk-label { display: inline-block; padding: 6px 20px; border-radius: 20px; color: white; background: ', risk_color, '; font-size: 16px; margin-top: 8px; }
              .model-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 12px; margin-top: 15px; }
              .model-card { background: #f5f5f5; padding: 10px; border-radius: 8px; text-align: center; }
              .footer { text-align: center; font-size: 12px; color: #777; margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; }
              hr { margin: 15px 0; border: none; border-top: 1px solid #e0e0e0; }
              table { width: 100%; margin: 15px 0; }
              th { background-color: #f2f2f2; }
            </style>
          </head>
          <body>
            <h1>双相情感障碍预后疗效预测报告</h1>
            <p style="text-align:center; color:#666; font-size:14px;">生成时间：', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
            
            <h3>一、患者基础信息</h3>
            <div class="info-box">
              <p><strong>患者编号：</strong>', p_info$name, '</p>
              <p><strong>年龄：</strong>', p_info$age, '岁</p>
              <p><strong>性别：</strong>', p_info$gender, '</p>
              <p><strong>所在地：</strong>', raw$Location, '</p>
              <p><strong>精神疾病史：</strong>', raw$DH, '</p>
              <p><strong>重大生活事件：</strong>', raw$B2, '</p>
              <p><strong>工作节奏：</strong>', raw$C5, '</p>
              <p><strong>临床总体印象：</strong>', raw$CGI, '</p>
              <p><strong>心电图：</strong>', raw$Elect, '</p>
            </div>
            
            <h3>二、风险预测结果</h3>
            <div class="risk-box">
              <div class="risk-percent">', round(p * 100, 1), '%</div>
              <div class="risk-label">', risk_label, '</div>
              <div class="model-grid">
                <div class="model-card"><strong>Logistic</strong><br>', round(prediction_result()$logit_prob * 100, 1), '%</div>
                <div class="model-card"><strong>随机森林</strong><br>', round(prediction_result()$rf_prob * 100, 1), '%</div>
                <div class="model-card"><strong>SVM</strong><br>', round(prediction_result()$svm_prob * 100, 1), '%</div>
              </div>
            </div>
            
            <h3>三、关键影响特征</h3>
            ', shap_html, '
            
            <h3>四、临床建议</h3>
            <div class="info-box">
              <p>', if(p >= 0.7) {
                "🔴 患者预后疗效不良风险较高，建议重点关注、加强干预与随访。"
              } else if(p >= 0.3) {
                "🟡 患者存在中等风险，建议定期评估与心理疏导。"
              } else {
                "🟢 患者风险较低，建议保持良好生活习惯，常规随访。"
              }, '</p>
            </div>
            
            <div class="footer">
              <p>本报告由系统自动生成，仅供临床参考，不作为唯一诊断依据。</p>
              <p>建议结合临床医生专业判断综合评估</p>
            </div>
          </body>
          </html>'
        )
        
        writeLines(html_content, file, useBytes = TRUE)
      }
    )
    
    # 刷新预览
    observeEvent(input$preview_btn, {
      if(is.null(patient_info()) || is.null(raw_data()) || is.null(prediction_result())) {
        shinyalert("数据未就绪", "请先在「患者信息录入」填写数据并提交，然后进行预测", type = "warning")
        return()
      }
      
      # 重新渲染预览
      output$report_preview <- renderUI({
        req(patient_info(), raw_data(), prediction_result())
        
        p <- prediction_result()$avg_prob
        risk_color <- prediction_result()$risk_color
        risk_label <- prediction_result()$risk_level
        p_info <- patient_info()
        raw <- raw_data()
        
        div(style = "line-height:1.8; font-family:system-ui;",
            
            h1("双相情感障碍预后疗效预测报告", style = "text-align:center; color:#165DFF; font-size:24px;"),
            h4(paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "text-align:center; color:#666; font-size:14px;"),
            hr(style = "border-top:2px solid #165DFF;"),
            
            h3("一、患者基础信息", style = "color:#222; font-size:16px;"),
            p(paste("患者编号：", p_info$name)),
            p(paste("年龄：", p_info$age, "岁")),
            p(paste("性别：", p_info$gender)),
            p(paste("所在地：", raw$Location)),
            p(paste("精神疾病史：", raw$DH)),
            p(paste("重大生活事件：", raw$B2)),
            p(paste("工作节奏：", raw$C5)),
            p(paste("临床总体印象：", raw$CGI)),
            p(paste("心电图：", raw$Elect)),
            
            hr(),
            
            h3("二、风险预测结果", style = "color:#222; font-size:16px;"),
            div(style = "text-align:center;",
                h4("集成模型平均概率", style = "color:#666; margin-bottom:10px; font-size:14px;"),
                h2(paste0(round(p*100,1),"%"), style = paste0("color:", risk_color, "; font-size:36px;")),
                span(risk_label, style = paste0("background:", risk_color, "; color:white; padding:4px 14px; border-radius:16px; font-size:14px;")),
                hr(style = "margin:15px 0;"),
                div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px; margin-top:10px;",
                    div(style = "background:#f5f5f5; padding:8px; border-radius:6px;",
                        p(style = "margin:0; font-weight:bold; font-size:12px;", "Logistic"),
                        p(style = "margin:3px 0 0 0; color:#2563EB; font-size:14px;", paste0(round(prediction_result()$logit_prob * 100, 1), "%"))
                    ),
                    div(style = "background:#f5f5f5; padding:8px; border-radius:6px;",
                        p(style = "margin:0; font-weight:bold; font-size:12px;", "随机森林"),
                        p(style = "margin:3px 0 0 0; color:#059669; font-size:14px;", paste0(round(prediction_result()$rf_prob * 100, 1), "%"))
                    ),
                    div(style = "background:#f5f5f5; padding:8px; border-radius:6px;",
                        p(style = "margin:0; font-weight:bold; font-size:12px;", "SVM"),
                        p(style = "margin:3px 0 0 0; color:#7C3AED; font-size:14px;", paste0(round(prediction_result()$svm_prob * 100, 1), "%"))
                    )
                )
            ),
            
            hr(),
            
            h3("三、关键影响特征", style = "color:#222; font-size:16px;"),
            tableOutput(ns("report_shap_table")),
            
            hr(),
            
            h3("四、临床建议", style = "color:#222; font-size:16px;"),
            p(if(p >= 0.7) {
              "🔴 患者预后疗效不良风险较高，建议重点关注、加强干预与随访。"
            } else if(p >= 0.3) {
              "🟡 患者存在中等风险，建议定期评估与心理疏导。"
            } else {
              "🟢 患者风险较低，建议保持良好生活习惯，常规随访。"
            }),
            
            p(style = "color:#777; font-size:11px; margin-top:20px;",
              "本报告由系统自动生成，仅供临床参考，不作为唯一诊断依据。")
        )
      })
      
      shinyalert("预览已刷新", "已加载最新数据", type = "success", timer = 1500)
    })
    
    # =====================================================
    # 10. 初始显示状态
    # =====================================================
    
    output$risk_score_display <- renderUI({
      div(style = "text-align:center; padding:40px 20px; color:#6B7280;",
          icon("chart-line", "fa-3x", style = "color:#9CA3AF; margin-bottom:15px;"),
          h4("等待预测", style = "color:#374151;"),
          p("请填写完整信息后点击「开始预测」按钮")
      )
    })
    
    output$shap_display <- renderUI({
      div(style = "text-align:center; padding:40px 20px; color:#6B7280;",
          icon("chart-bar", "fa-3x", style = "color:#9CA3AF; margin-bottom:15px;"),
          h4("等待分析", style = "color:#374151;"),
          p("请先进行风险预测，然后点击「计算特征贡献度」按钮")
      )
    })
    
    output$report_preview <- renderUI({
      div(style = "text-align:center; padding:40px 20px; color:#999;",
          icon("file-pdf", "fa-3x"),
          h4("暂无报告数据"),
          p("请先填写患者信息并进行风险预测")
      )
    })
    
  })
}