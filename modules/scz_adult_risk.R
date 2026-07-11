library(shiny)
library(shinythemes)



# =====================================================
# UI
# =====================================================

scz_adult_ui <- function(id){
  
  
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
      
      h2("成人精神分裂症患病风险辅助诊断系统"),
      
      p(
        "基于外周血lncRNA表达谱的Logistic预测模型"
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
            "外周血lncRNA表达指标"
          ),
          
          
          div(
            class="panel-body",
            
            
            numericInput(
              ns("Gomafu"),
              "X_Gomafu",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（lncRNA相对表达量）"
            ),
            
            
            numericInput(
              ns("AK096174"),
              "X_AK096174",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（lncRNA相对表达量）"
            ),
            
            
            numericInput(
              ns("AK123097"),
              "X_AK123097",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（lncRNA相对表达量）"
            ),
            
            
            numericInput(
              ns("ENST000005098041"),
              "X_ENST000005098041",
              value=0
            ),
            
            helpText(
              "检测范围建议：0–20（lncRNA相对表达量）"
            )
            
            
          )
          
        ),
        
        
        
        actionButton(
          ns("calculate"),
          "开始风险预测",
          icon=icon("stethoscope"),
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
              
              h4("精神分裂症预测概率"),
              
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
              
              h4("风险等级"),
              
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
            "辅助诊断建议"
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
          
          strong("注意："),
          
          "本系统检测结果仅作为辅助参考，
          不替代精神科临床诊断。
          临床决策需结合精神检查、PANSS/BPRS量表、
          病史资料及其他辅助检查综合判断。"
          
        )
        
        
      )
      
    )
    
  )
  
}






# =====================================================
# SERVER
# =====================================================


scz_adult_server <- function(id){
  
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      
      
      result <- eventReactive(
        
        input$calculate,
        
        {
          
          
          # Logistic模型
          
          logit_p <-
            2.054 +
            0.512*input$Gomafu -
            0.890*input$AK096174 -
            0.269*input$AK123097 -
            0.485*input$ENST000005098041
          
          
          
          # 概率转换
          
          p <-
            exp(logit_p)/(1+exp(logit_p))
          
          
          
          
          # 风险分层
          
          if(p >= 0.60){
            
            
            risk <- "高风险"
            color <- "#E74C3C"
            
            
            msg <- paste(
              
              "该受试者分子表达模式与精神分裂症患者特征谱匹配度较高。",
              "建议临床医师按照精神障碍诊疗规范完善系统评估，",
              "综合精神检查、PANSS、BPRS量表及病史资料开展诊断。",
              "对于高危人群及时开展早期干预，延缓疾病进展，",
              "减少慢性病程带来的认知损害。",
              "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              
            )
            
            
          }else if(p > 0.20){
            
            
            risk <- "中等风险"
            color <- "#F39C12"
            
            
            msg <- paste(
              
              "该受试者患病概率处于中等区间，",
              "提示个体可能处于精神分裂症前驱阶段或呈现非典型精神病性临床表型。",
              "建议整合量表评分、精神专科检查及既往病史开展多维评估。",
              "可每3个月复查外周血标志物，动态监测分子水平变化，",
              "实现高危人群长期随访管理。",
              "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              
            )
            
            
          }else{
            
            
            risk <- "低风险"
            color <- "#27AE60"
            
            
            msg <- paste(
              
              "该受检者精神分裂症分子诊断概率为低风险。",
              "若存在情绪或睡眠主诉，建议进一步筛查抑郁、焦虑",
              "或双相障碍等其他精神心理亚型。",
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
            "X_Gomafu",
            "X_AK096174",
            "X_AK123097",
            "X_ENST000005098041"
          ),
          
          内容=c(
            input$patient_id,
            input$case_id,
            input$Gomafu,
            input$AK096174,
            input$AK123097,
            input$ENST000005098041
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
            font-size:25px;
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