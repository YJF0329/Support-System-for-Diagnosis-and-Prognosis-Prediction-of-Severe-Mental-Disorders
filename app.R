# app.R
library(shiny)
library(shinydashboard)


# ==============================
# 加载模块
# ==============================

source("modules/home.R")

source("modules/bipolar_risk.R")        # 患病风险辅助诊断（基因数据）
source("modules/bipolar_clinical.R")    # 预后疗效辅助预测（临床特征模型）
source("modules/bipolar_metabolic.R")   # 复发风险预测（代谢标志物模型）

source("modules/scz_adolescent.R")
source("modules/scz_adult_risk.R")
source("modules/scz_treatment.R")



# ==============================
# UI
# ==============================


ui <- dashboardPage(
  
  
  dashboardHeader(title = "重性精神障碍"),
  
  
  
  dashboardSidebar(
    
    
    sidebarMenu(
      
      id = "menu",
      
      
      menuItem(
        "首页",
        tabName = "home",
        icon = icon("home")
      ),
      
      
      
      menuItem(
        "双相情感障碍辅助评估",
        icon = icon("brain"),
        
        
        menuSubItem(
          "患病风险辅助诊断",
          tabName = "bipolar_risk"          # ✅ 基因数据预测
        ),
        
        
        menuSubItem(
          "预后疗效辅助预测",
          icon = icon("chart-line"),
          tabName = "bipolar_prognosis"     # ✅ 临床特征 + 代谢标志物
        )
        
        
      ),
      
      
      
      menuItem(
        "精神分裂症辅助评估",
        icon = icon("user-md"),
        
        
        menuSubItem(
          "青少年精神分裂症风险预测",
          tabName = "scz_adolescent"
        ),
        
        
        menuSubItem(
          "成人精神分裂症风险预测",
          tabName = "scz_adult"
        ),
        
        
        menuSubItem(
          "成人精神分裂症疗效预测",
          tabName = "scz_treatment"
        )
        
        
      ),
      
      
      
      menuItem(
        "模型及使用说明",
        tabName = "model",
        icon = icon("book")
      )
    )
    
  ),
  
  
  
  
  dashboardBody(
    
    
    tabItems(
      
      
      tabItem(
        tabName = "home",
        home_ui("home")
      ),
      
      
      # =====================================================
      # 双相情感障碍 - 患病风险辅助诊断（基因数据）
      # =====================================================
      tabItem(
        tabName = "bipolar_risk",
        bipolar_risk_ui("bipolar_risk")   # ✅ 使用你的原始模块
      ),
      
      
      # =====================================================
      # 双相情感障碍 - 预后疗效辅助预测（临床+代谢）
      # =====================================================
      tabItem(
        tabName = "bipolar_prognosis",
        
        fluidPage(
          
          h3("双相情感障碍预后疗效辅助预测"),
          
          h4("请选择预测模型"),
          
          fluidRow(
            column(
              width = 6,
              div(
                style = "text-align:center; padding:20px;",
                actionButton(
                  "clinical_btn",
                  "🏥 临床特征模型",
                  class = "btn-primary btn-lg",
                  style = "padding:15px 40px; font-size:18px; border-radius:10px; width:100%;"
                )
              )
            ),
            column(
              width = 6,
              div(
                style = "text-align:center; padding:20px;",
                actionButton(
                  "metabolic_btn",
                  "🧬 代谢标志物联合模型",
                  class = "btn-info btn-lg",
                  style = "padding:15px 40px; font-size:18px; border-radius:10px; width:100%;"
                )
              )
            )
          ),
          
          uiOutput("prognosis_model_info"),
          uiOutput("prognosis_model_ui")
          
        )
        
      ),
      
      
      
      tabItem(
        tabName = "scz_adolescent",
        scz_adolescent_ui("scz_adolescent")
      ),
      
      
      tabItem(
        tabName = "scz_adult",
        scz_adult_ui("scz_adult")
      ),
      
      
      tabItem(
        tabName = "scz_treatment",
        scz_treatment_ui("scz_treatment")
      ),
      
      
      
      tabItem(
        tabName = "model",
        
        fluidPage(
          
          # 标题美化
          div(
            style = "
        background: linear-gradient(135deg, #1A3A6B 0%, #2A508C 100%);
        padding: 25px 30px;
        border-radius: 12px;
        color: white;
        margin-bottom: 25px;
      ",
            h2(icon("book"), " 模型及使用说明", style = "margin: 0; font-weight: 600;")
          ),
          
          # 内容卡片
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = div(icon("info-circle"), " 系统说明"),
            
            p(
              style = "font-size: 15px; line-height: 2; text-align: justify;",
              "本系统聚焦重性精神障碍诊疗难题，围绕精神分裂症、双相情感障碍，",
              "结合分子生物学指标与临床特征构建智能化评估模型，",
              "分别开展患病风险辅助判断和远期疗效预后评估。",
              "项目依据不同人群疾病发病特点差异化构建预测模型，",
              "借助机器学习算法量化患病及预后概率，分层给出临床参考建议，",
              "缩小传统诊疗模式中医师主观判断带来的偏差。",
              "系统相关检测指标依托临床常规检验手段完成，适用性较强。"
            ),
            
            br(),
            
            # 重要提示（突出显示）
            div(
              style = "
          background: #FFF3CD;
          border-left: 4px solid #FFC107;
          padding: 15px 20px;
          border-radius: 6px;
        ",
              icon("exclamation-triangle", style = "color: #856404; margin-right: 8px;"),
              tags$strong("重要提示：", style = "color: #856404;"),
              span(
                style = "color: #856404;",
                "本系统输出结果仅作为临床辅助参考资料，不作为疾病确诊和治疗方案制定的决定性依据，",
                "最终诊疗决策仍由精神科专科医师结合面诊、病史及量表检查综合判定。"
              )
            )
          )
        )
      )
    )
    
    
  )
  
)




# ==============================
# Server
# ==============================


server <- function(input, output, session){
  
  
  # =====================================================
  # 1. 初始化所有模块
  # =====================================================
  
  home_server("home")
  
  # ✅ 患病风险辅助诊断（基因数据）
  bipolar_risk_server("bipolar_risk")
  
  # ✅ 预后疗效辅助预测（临床特征 + 代谢标志物）
  # 这两个模块在动态UI中初始化
  
  scz_adolescent_server("scz_adolescent")
  scz_adult_server("scz_adult")
  scz_treatment_server("scz_treatment")
  
  
  # =====================================================
  # 2. 双相预后疗效预测 - 动态模块切换
  # =====================================================
  
  selected_model <- reactiveVal(NULL)
  
  
  observeEvent(input$clinical_btn, {
    selected_model("clinical")
    showNotification("已选择：临床特征模型", type = "message", duration = 2)
  })
  
  
  observeEvent(input$metabolic_btn, {
    selected_model("metabolic")
    showNotification("已选择：代谢标志物联合模型", type = "message", duration = 2)
  })
  
  
  output$prognosis_model_info <- renderUI({
    model <- selected_model()
    
    if(is.null(model)) {
      return(
        div(
          class = "alert alert-info",
          style = "margin-top:10px;",
          icon("info-circle"),
          " 请点击上方按钮选择预测模型"
        )
      )
    }
    
    model_name <- ifelse(model == "clinical", "临床特征模型", "代谢标志物联合模型")
    model_color <- ifelse(model == "clinical", "#2C3E50", "#8E44AD")
    
    div(
      class = "alert alert-success",
      style = paste0("margin-top:10px; border-left: 4px solid ", model_color, ";"),
      icon("check-circle"),
      " 当前选择：",
      tags$strong(model_name, style = paste0("color:", model_color, ";"))
    )
  })
  
  
  output$prognosis_model_ui <- renderUI({
    model <- selected_model()
    
    if(is.null(model)) {
      return(
        div(
          style = "text-align:center; padding:60px 20px; color:#999;",
          icon("hand-pointer", "fa-4x"),
          h4("请选择预测模型"),
          p("点击上方的「临床特征模型」或「代谢标志物联合模型」按钮")
        )
      )
    }
    
    if(model == "clinical") {
      bipolar_clinical_ui("bipolar_clinical")
    } else {
      bipolar_metabolic_ui("bipolar_metabolic")
    }
  })
  
  
  # 初始化动态模块
  observe({
    if(!is.null(input$clinical_btn) || !is.null(input$metabolic_btn)) {
      bipolar_clinical_server("bipolar_clinical")
      bipolar_metabolic_server("bipolar_metabolic")
    }
  })
  
  
  # 自动跳转
  observeEvent(input$clinical_btn, {
    if(input$menu != "bipolar_prognosis") {
      updateTabItems(session, "menu", "bipolar_prognosis")
    }
  })
  
  observeEvent(input$metabolic_btn, {
    if(input$menu != "bipolar_prognosis") {
      updateTabItems(session, "menu", "bipolar_prognosis")
    }
  })
  
}



shinyApp(
  ui,
  server
)
