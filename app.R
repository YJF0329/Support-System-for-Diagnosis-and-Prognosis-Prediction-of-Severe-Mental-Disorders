# modules/scz_risk.R
# 精神分裂症患病风险辅助诊断（容器页面）
# 包含：青少年风险预测 + 成人风险预测

library(shiny)
library(shinydashboard)


# ============================================================
# UI
# ============================================================

scz_risk_ui <- function(id){
  
  ns <- NS(id)
  
  
  fluidPage(
    
    # 页面标题
    div(
      style = "
        background: linear-gradient(135deg, #1A5276 0%, #2E86C1 100%);
        padding: 20px 25px;
        border-radius: 10px;
        color: white;
        margin-bottom: 25px;
      ",
      h3(icon("diagnoses"), " 精神分裂症患病风险辅助诊断", style = "margin: 0; font-weight: 600;"),
      p("请根据患者年龄选择对应的风险评估模型", style = "margin: 8px 0 0 0; opacity: 0.9;")
    ),
    
    # 模型选择卡片
    fluidRow(
      
      # 青少年模型
      column(
        width = 6,
        div(
          style = "
            background: #F0F8FF;
            border-radius: 12px;
            padding: 30px 20px;
            text-align: center;
            border: 2px solid #5DADE2;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            height: 100%;
            transition: all 0.3s ease;
            cursor: pointer;
          ",
          onmouseover = "this.style.borderColor='#2E86C1'; this.style.boxShadow='0 6px 20px rgba(46,134,193,0.25)';",
          onmouseout = "this.style.borderColor='#5DADE2'; this.style.boxShadow='0 4px 12px rgba(0,0,0,0.08)';",
          
          icon("child", class = "fa-4x", style = "color: #2E86C1; margin-bottom: 15px;"),
          
          h4("青少年精神分裂症", style = "font-weight: 600; color: #1A5276;"),
          p("年龄 ≤ 18 岁", style = "color: #666; font-size: 14px; margin-bottom: 20px;"),
          
          actionButton(
            ns("go_adolescent"),
            "进入评估 →",
            class = "btn-primary",
            style = "
              background: #2E86C1;
              border: none;
              padding: 10px 30px;
              border-radius: 25px;
              font-weight: 500;
              transition: all 0.3s ease;
            "
          )
        )
      ),
      
      # 成人模型
      column(
        width = 6,
        div(
          style = "
            background: #FDF2E9;
            border-radius: 12px;
            padding: 30px 20px;
            text-align: center;
            border: 2px solid #E67E22;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            height: 100%;
            transition: all 0.3s ease;
            cursor: pointer;
          ",
          onmouseover = "this.style.borderColor='#CA6F1E'; this.style.boxShadow='0 6px 20px rgba(230,126,34,0.25)';",
          onmouseout = "this.style.borderColor='#E67E22'; this.style.boxShadow='0 4px 12px rgba(0,0,0,0.08)';",
          
          icon("user", class = "fa-4x", style = "color: #E67E22; margin-bottom: 15px;"),
          
          h4("成人精神分裂症", style = "font-weight: 600; color: #7E5109;"),
          p("年龄 > 18 岁", style = "color: #666; font-size: 14px; margin-bottom: 20px;"),
          
          actionButton(
            ns("go_adult"),
            "进入评估 →",
            class = "btn-warning",
            style = "
              background: #E67E22;
              border: none;
              padding: 10px 30px;
              border-radius: 25px;
              font-weight: 500;
              color: white;
              transition: all 0.3s ease;
            "
          )
        )
      )
    ),
    
    # 模型展示区域
    br(),
    uiOutput(ns("risk_model_ui"))
    
  )
  
}


# ============================================================
# Server
# ============================================================

scz_risk_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    
    # 当前选中的风险模型
    selected_risk <- reactiveVal(NULL)
    
    
    # 点击青少年模型
    observeEvent(input$go_adolescent, {
      selected_risk("adolescent")
      showNotification("已选择：青少年精神分裂症风险预测", type = "message", duration = 2)
    })
    
    
    # 点击成人模型
    observeEvent(input$go_adult, {
      selected_risk("adult")
      showNotification("已选择：成人精神分裂症风险预测", type = "message", duration = 2)
    })
    
    
    # 渲染模型UI
    output$risk_model_ui <- renderUI({
      
      risk_type <- selected_risk()
      
      if(is.null(risk_type)) {
        return(
          div(
            style = "text-align:center; padding:60px 20px; color:#999; background:#FAFAFA; border-radius:10px; margin-top:20px;",
            icon("hand-pointer", "fa-4x"),
            h4("请选择风险评估模型"),
            p("点击上方「青少年精神分裂症」或「成人精神分裂症」卡片进入评估")
          )
        )
      }
      
      if(risk_type == "adolescent") {
        scz_adolescent_ui(ns("adolescent_model"))
      } else {
        scz_adult_risk_ui(ns("adult_model"))
      }
      
    })
    
    
    # 初始化青少年模型
    observe({
      if(!is.null(input$go_adolescent)) {
        scz_adolescent_server("adolescent_model")
      }
    })
    
    
    # 初始化成人模型
    observe({
      if(!is.null(input$go_adult)) {
        scz_adult_risk_server("adult_model")
      }
    })
    
    
    # 侧边栏自动跳转（父级控制）
    # 保留给父级 server 调用
    
  })
  
}
