library(shiny)
library(shinythemes)



# =====================================================
# UI
# =====================================================

scz_treatment_ui <- function(id){
  
  
  ns <- NS(id)
  
  
  fluidPage(
    
    
    theme = shinytheme("flatly"),
    
    
    div(
      style="
      background:#2C3E50;
      padding:25px;
      border-radius:10px;
      color:white;
      text-align:center;
      margin-bottom:20px;",
      
      h2("成人精神分裂症治疗疗效辅助预测系统"),
      
      p(
        "基于治疗前外周血lncRNA表达水平的抗精神病药物疗效预测模型"
      )
      
    ),
    
    
    
    sidebarLayout(
      
      
      sidebarPanel(
        
        width=5,
        
        
        div(
          class="panel panel-primary",
          
          div(
            class="panel-heading",
            "患者基本信息"
          ),
          
          div(
            class="panel-body",
            
            
            textInput(
              ns("patient_id"),
              "患者编号",
              placeholder="请输入患者编号"
            ),
            
            
            textInput(
              ns("case_id"),
              "病案号",
              placeholder="请输入病案号"
            )
            
          )
          
        ),
        
        
        
        div(
          class="panel panel-info",
          
          div(
            class="panel-heading",
            "治疗前外周血lncRNA指标"
          ),
          
          
          div(
            class="panel-body",
            
            
            numericInput(
              ns("ENST000005098041"),
              "X_ENST00000509804-1",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（qPCR 2^-ΔΔCt相对表达量）"
            ),
            
            
            numericInput(
              ns("AK123097"),
              "X_AK123097",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（qPCR 2^-ΔΔCt相对表达量）"
            ),
            
            
            numericInput(
              ns("uc011dma1"),
              "X_uc011dma.1",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（qPCR 2^-ΔΔCt相对表达量）"
            )
            
            
          )
          
        ),
        
        
        
        actionButton(
          ns("calculate"),
          "开始疗效预测",
          icon=icon("notes-medical"),
          class="btn-success btn-lg",
          width="100%"
        )
        
        
      ),
      
      
      
      mainPanel(
        
        width=6,
        
        
        div(
          class="panel panel-default",
          
          div(
            class="panel-heading",
            "患者检测信息"
          ),
          
          div(
            class="panel-body",
            
            tableOutput(
              ns("patient_info")
            )
            
          )
          
        ),
        
        
        
        br(),
        
        
        
        fluidRow(
          
          
          column(
            6,
            
            div(
              style="
              background:#ECF0F1;
              padding:20px;
              border-radius:15px;
              text-align:center;",
              
              h4("治疗有效概率"),
              
              uiOutput(
                ns("prob")
              )
              
            )
            
          ),
          
          
          
          column(
            6,
            
            div(
              style="
              background:#ECF0F1;
              padding:20px;
              border-radius:15px;
              text-align:center;",
              
              h4("疗效预测等级"),
              
              uiOutput(
                ns("risk")
              )
              
            )
            
          )
          
          
        ),
        
        
        
        br(),
        
        
        
        div(
          
          class="panel panel-warning",
          
          div(
            class="panel-heading",
            style = "font-size:22px;font-weight:bold;",
            "辅助治疗建议"
          ),
          
          
          div(
            class="panel-body",
            
            textOutput(
              ns("message")
            )
            
          )
          
        ),
        
        
        
        div(
          
          style="
          background:#F8F9FA;
          padding:15px;
          border-radius:10px;
          font-size:13px;",
          
          strong("预测终点："),
          
          "PANSS量表减分率＞30%。",
          
          br(),
          
          strong("注意："),
          
          "本结果仅作辅助参考，临床治疗方案需由精神科医师结合患者临床表现、药物耐受性及治疗反应综合制定。"
          
        )
        
        
      )
      
    )
    
  )
  
}





# =====================================================
# SERVER
# =====================================================


scz_treatment_server <- function(id){
  
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      
      
      result <- eventReactive(
        
        input$calculate,
        
        {
          
          
          # Logistic模型
          
          logit_p <-
            -0.065 +
            0.825*input$ENST000005098041 +
            0.231*input$AK123097 -
            0.354*input$uc011dma1
          
          
          
          # 概率转换
          
          p <-
            exp(logit_p)/(1+exp(logit_p))
          
          
          
          
          if(p >= 0.70){
            
            
            risk <- "高疗效响应概率"
            
            color <- "#27AE60"
            
            
            msg <- paste(
              
              "该患者对常规一线抗精神病药物敏感度高。",
              "建议维持标准常规治疗，避免过度联合用药，",
              "严密监测药物不良反应即可。",
              "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              
            )
            
            
          }else if(p > 0.30){
            
            
            risk <- "疗效不确定"
            
            color <- "#F39C12"
            
            
            msg <- paste(
              
              "疗效存在不确定性。",
              "该患者可能属于部分缓解，",
              "或核心症状（如阴性症状、认知症状）改善不明显。",
              "建议加强随访频次，优化药物剂量，",
              "结合心理社会干预。",
              "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              
            )
            
            
          }else{
            
            
            risk <- "治疗应答不良风险高"
            
            color <- "#E74C3C"
            
            
            msg <- paste(
              
              "基于外周血分子标志物预测，",
              "该患者出现抗精神病药物治疗应答不良的风险偏高。",
              "建议尽早个体化优化治疗策略，",
              "酌情联合物理干预手段，减少无效用药周期。",
              "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              
            )
            
            
          }
          
          
          
          list(
            p=p,
            risk=risk,
            color=color,
            msg=msg
          )
          
        }
        
      )
      
      
      
      
      output$patient_info <- renderTable({
        
        
        data.frame(
          
          项目=c(
            "患者编号",
            "病案号",
            "X_ENST00000509804-1",
            "X_AK123097",
            "X_uc011dma.1"
          ),
          
          内容=c(
            input$patient_id,
            input$case_id,
            input$ENST000005098041,
            input$AK123097,
            input$uc011dma1
          )
          
        )
        
      })
      
      
      
      
      output$prob <- renderUI({
        
        
        req(result())
        
        
        tags$h1(
          
          style="
          color:#34495E;
          font-weight:bold;",
          
          paste0(
            round(result()$p*100,2),
            "%"
          )
          
        )
        
        
      })
      
      
      
      
      output$risk <- renderUI({
        
        
        req(result())
        
        
        tags$span(
          
          style=paste0(
            
            "
            background:",
            result()$color,
            ";
            color:white;
            padding:12px 25px;
            border-radius:20px;
            font-size:23px;
            font-weight:bold;"
            
          ),
          
          result()$risk
          
        )
        
        
      })
      
      
      
      
      output$message <- renderText({
        
        
        req(result())
        
        
        result()$msg
        
        
      })
      
      
      
      
    }
    
  )
  
}