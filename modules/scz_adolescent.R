library(shiny)
library(shinythemes)



# ======================================================
# UI
# ======================================================

scz_adolescent_ui <- function(id){
  
  
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
      
      h2("早发性精神分裂症（青少年）患病风险辅助诊断系统"),
      
      p(
        "基于外周血免疫标志物mRNA表达水平的Logistic预测模型"
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
            "外周血mRNA指标"
          ),
          
          
          div(
            class="panel-body",
            
            
            numericInput(
              ns("CCL3"),
              "X_CCL3",
              value=0
            ),
            
            helpText(
              "参考范围：0–20"
            ),
            
            
            numericInput(
              ns("IL1b"),
              "X_IL1β",
              value=0
            ),
            
            helpText(
              "参考范围：0–20"
            ),
            
            
            numericInput(
              ns("CXCL8"),
              "X_CXCL8",
              value=0
            ),
            
            helpText(
              "参考范围：0–20"
            ),
            
            
            numericInput(
              ns("CXCL10"),
              "X_CXCL10",
              value=0
            ),
            
            helpText(
              "参考范围：0–20"
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
              
              h4("预测概率"),
              
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
          
        )
        
        
      )
      
    )
    
  )
  
}







# ======================================================
# SERVER
# ======================================================


scz_adolescent_server <- function(id){
  
  
  moduleServer(
    
    
    id,
    
    
    function(input,output,session){
      
      
      
      result <- eventReactive(
        input$calculate,
        {
          
          logit_p <-
            -7.8 +
            0.42*input$CCL3 +
            0.95*input$IL1b +
            0.88*input$CXCL8 +
            1.36*input$CXCL10
          
          
          p <-
            exp(logit_p)/(1+exp(logit_p))
          
          
          
          if(p>=0.70){
            
            
            risk <- "高风险"
            color <- "#E74C3C"
            
            
            msg <-
              paste(
                "该受试者早发性精神分裂症预测风险偏高。",
                "建议医师重点评估患者精神状况，完善精神检查，",
                "尽早开展干预治疗。",
                "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              )
            
            
          }else if(p>0.30){
            
            
            risk <- "中等风险"
            color <- "#F39C12"
            
            
            msg <-
              paste(
                "模型提示该受试者处于中等风险区间。",
                "建议完善PANSS量表评估，缩短随访周期，",
                "动态复查相关基因指标。",
                "本结果仅作辅助参考，临床决策由医师综合评估后确定。"
              )
            
            
          }else{
            
            
            risk <- "低风险"
            color <- "#27AE60"
            
            
            msg <-
              paste(
                "基于外周血免疫标志物预测，",
                "该受试者患病风险较低。",
                "建议结合临床表现进一步判断。",
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
            "X_CCL3",
            "X_IL1β",
            "X_CXCL8",
            "X_CXCL10"
          ),
          
          内容=c(
            input$patient_id,
            input$case_id,
            input$CCL3,
            input$IL1b,
            input$CXCL8,
            input$CXCL10
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