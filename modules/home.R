# modules/home.R
library(shiny)
library(shinydashboard)

# =====================================================
# UI
# =====================================================

home_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    # 页面样式
    tags$style(HTML("
      .home-hero {
        background: linear-gradient(135deg, #1A3A6B 0%, #2A508C 100%);
        padding: 60px 40px;
        border-radius: 15px;
        color: white;
        text-align: center;
        margin-bottom: 30px;
      }
      .home-hero h1 {
        font-size: 48px;
        font-weight: 700;
        margin-bottom: 15px;
      }
      .home-hero p {
        font-size: 18px;
        opacity: 0.9;
        max-width: 700px;
        margin: 0 auto;
      }
      .feature-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 25px;
        text-align: center;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        transition: transform 0.3s ease;
        height: 100%;
        border-top: 4px solid #427D9D;
      }
      .feature-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.12);
      }
      .feature-card .icon {
        font-size: 48px;
        margin-bottom: 15px;
      }
      .feature-card h4 {
        color: #1A3A6B;
        font-weight: 600;
        margin-bottom: 10px;
      }
      .feature-card p {
        color: #6c757d;
        font-size: 14px;
        line-height: 1.6;
      }
      .quick-start {
        background: #f8f9fa;
        border-radius: 12px;
        padding: 30px;
        margin-top: 30px;
      }
      .quick-start h3 {
        color: #1A3A6B;
        font-weight: 600;
      }
    ")),
    
    # ===== 顶部横幅 =====
    div(
      class = "home-hero",
      h1("🧠 重性精神障碍"),
      p("辅助诊断与预后预测系统"),
      p(style = "font-size:14px; opacity:0.7; margin-top:10px;",
        "基于多组学数据与机器学习模型，提供精准的风险评估与治疗响应预测")
    ),
    
    # ===== 功能卡片区 =====
    fluidRow(
      
      # 双相情感障碍
      column(
        width = 4,
        div(
          class = "feature-card",
          div(class = "icon", "🧬"),
          h4("双相情感障碍辅助评估"),
          p("基于临床特征、代谢标志物等多维度数据，
            提供患病风险评估与预后疗效预测。"),
          br(),
          tags$small(style = "color:#427D9D;",
                     icon("arrow-right"), " 点击侧边栏进入")
        )
      ),
      
      # 精神分裂症
      column(
        width = 4,
        div(
          class = "feature-card",
          style = "border-top-color: #8E44AD;",
          div(class = "icon", "🧠"),
          h4("精神分裂症辅助评估"),
          p("基于外周血mRNA表达谱及lncRNA特征，
            提供青少年及成人患病风险预测与疗效评估。"),
          br(),
          tags$small(style = "color:#8E44AD;",
                     icon("arrow-right"), " 点击侧边栏进入")
        )
      ),
      
      # 系统特色
      column(
        width = 4,
        div(
          class = "feature-card",
          style = "border-top-color: #27AE60;",
          div(class = "icon", "📊"),
          h4("系统特色"),
          p("集成多种机器学习算法，提供可解释的预测结果，
            支持临床决策，持续优化模型性能。"),
          br(),
          tags$small(style = "color:#27AE60;",
                     icon("check-circle"), " 多模型集成")
        )
      )
    ),
    
    # ===== 快速开始 =====
    div(
      class = "quick-start",
      h3("🚀 快速开始"),
      fluidRow(
        column(
          width = 4,
          p(style = "font-weight:600;", "1️⃣ 选择评估模块"),
          p(style = "color:#6c757d; font-size:14px;",
            "从左侧菜单选择「双相情感障碍」或「精神分裂症」")
        ),
        column(
          width = 4,
          p(style = "font-weight:600;", "2️⃣ 输入患者信息"),
          p(style = "color:#6c757d; font-size:14px;",
            "填写患者编号、临床特征及相关检测指标")
        ),
        column(
          width = 4,
          p(style = "font-weight:600;", "3️⃣ 获取预测结果"),
          p(style = "color:#6c757d; font-size:14px;",
            "系统自动计算风险概率，并生成辅助诊断建议")
        )
      )
    ),
    
    # ===== 底部信息 =====
    hr(),
    div(
      style = "text-align:center; color:#999; font-size:12px; padding:20px 0;",
      p("本系统仅供临床辅助参考，不替代专业医疗诊断。"),
      p("© 2026 SMD-AI-Dx | 版本 1.0")
    )
  )
}

# =====================================================
# SERVER
# =====================================================

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 首页模块无需特殊逻辑
  })
}