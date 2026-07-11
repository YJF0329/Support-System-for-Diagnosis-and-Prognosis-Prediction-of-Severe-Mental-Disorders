library(shiny)
library(shinyalert)



# =====================================================
# UI
# =====================================================

bipolar_metabolic_ui <- function(id){
  
  
  ns <- NS(id)
  
  
  fluidPage(
    
    
    tags$style(HTML("
      
      .result-box{
        background:#f0f7fc;
        border-radius:8px;
        padding:20px;
        text-align:center;
        border:1px solid #d1e0eb;
      }
      
      .box-title{
        color:#1a4f73;
      }
      
    ")),
    
    
    
    fluidRow(
      
      
      box(
        
        width=12,
        status="primary",
        solidHeader=TRUE,
        
        title="双相情感障碍复发风险预测",
        
        
        fluidRow(
          
          column(
            
            6,
            
            
            textInput(
              ns("patient_id"),
              "患者编号（8位）",
              placeholder="例如：00000001"
            ),
            
            
            numericInput(
              ns("disease_duration"),
              "病程（年）",
              value=0,
              min=0,
              step=0.5
            ),
            
            
            selectInput(
              ns("negative_life_events"),
              "负性生活事件",
              choices=c(
                "无"=0,
                "有"=1
              )
            ),
            
            
            selectInput(
              ns("sleep_disorder"),
              "睡眠障碍",
              choices=c(
                "无"=0,
                "有"=1
              )
            )
            
          ),
          
          
          
          column(
            
            6,
            
            
            numericInput(
              ns("acetone"),
              "丙酮（峰面积）",
              value=0.0006,
              step=0.0001
            ),
            
            helpText(
              "参考范围：0.0005–0.0007"
            ),
            
            
            
            numericInput(
              ns("o_acetylglycoprotein"),
              "O-乙酰糖蛋白（峰面积）",
              value=0.005,
              step=0.0001
            ),
            
            
            helpText(
              "参考范围：0.0048–0.0054"
            ),
            
            
            
            numericInput(
              ns("choline_phosphate"),
              "磷酸胆碱（峰面积）",
              value=0.012,
              step=0.0001
            ),
            
            
            helpText(
              "参考范围：0.0116–0.0130"
            ),
            
            
            
            actionButton(
              ns("predict_btn"),
              "预测复发风险",
              icon=icon("calculator"),
              class="btn-primary btn-lg",
              width="100%"
            )
            
            
          )
          
        )
        
      )
      
      
    ),
    
    
    
    
    fluidRow(
      
      
      box(
        
        width=12,
        status="success",
        solidHeader=TRUE,
        
        title="预测结果",
        
        
        div(
          class="result-box",
          
          h3("复发风险概率"),
          
          uiOutput(
            ns("risk_prob")
          )
          
        ),
        
        
        br(),
        
        
        h4("预测详情"),
        
        
        verbatimTextOutput(
          ns("predict_details")
        )
        
        
      )
      
      
    )
    
    
  )
  
  
}






# =====================================================
# SERVER
# =====================================================


bipolar_metabolic_server <- function(id){
  
  
  moduleServer(
    
    id,
    
    function(input,output,session){
      
      
      prediction <- reactiveVal(NULL)
      
      
      
      observeEvent(
        
        input$predict_btn,
        
        {
          
          
          pid <- input$patient_id
          
          
          if(!grepl("^[0-9]{8}$",pid)){
            
            
            shinyalert(
              "输入错误",
              "请输入8位数字患者编号，例如：00000001",
              type="error"
            )
            
            return()
            
          }
          
          
          
          if(input$disease_duration < 0){
            
            
            shinyalert(
              "输入错误",
              "病程不能为负数",
              type="error"
            )
            
            return()
            
          }
          
          
          
          logit <-
            -0.750 +
            0.0172*input$disease_duration +
            1.35*as.numeric(input$negative_life_events) -
            2.16*input$acetone -
            1.71*input$o_acetylglycoprotein +
            0.930*as.numeric(input$sleep_disorder) +
            2.25*input$choline_phosphate
          
          
          
          prob <-
            1/(1+exp(-logit))
          
          
          
          prediction(
            
            list(
              
              prob=prob,
              logit=logit,
              pid=pid,
              
              duration=input$disease_duration,
              nle=input$negative_life_events,
              sleep=input$sleep_disorder,
              
              acetone=input$acetone,
              oag=input$o_acetylglycoprotein,
              choline=input$choline_phosphate
              
            )
            
          )
          
          
          
          shinyalert(
            "预测完成",
            "结果已生成",
            type="success"
          )
          
          
        }
        
      )
      
      
      
      
      output$risk_prob <- renderUI({
        
        
        res <- prediction()
        
        
        if(is.null(res)){
          
          return(
            tags$h2("暂无预测结果")
          )
          
        }
        
        
        tags$h1(
          
          style="
          font-size:56px;
          color:#1a4f73;
          font-weight:bold;",
          
          sprintf(
            "%.2f%%",
            res$prob*100
          )
          
        )
        
        
      })
      
      
      
      
      output$predict_details <- renderPrint({
        
        
        res <- prediction()
        
        
        if(is.null(res)){
          
          cat("暂无预测结果")
          
        }else{
          
          
          cat(
            "患者编号：",
            res$pid,
            "\n\n"
          )
          
          
          cat(
            "Logit值：",
            round(res$logit,4),
            "\n"
          )
          
          
          cat(
            "预测概率：",
            round(res$prob,4),
            "\n\n"
          )
          
          
          cat(
            "输入变量：\n"
          )
          
          
          cat(
            "病程：",
            res$duration,
            "年\n"
          )
          
          
          cat(
            "负性生活事件：",
            ifelse(res$nle==1,"有","无"),
            "\n"
          )
          
          
          cat(
            "睡眠障碍：",
            ifelse(res$sleep==1,"有","无"),
            "\n"
          )
          
          
          cat(
            "丙酮：",
            res$acetone,
            "\n"
          )
          
          
          cat(
            "O-乙酰糖蛋白：",
            res$oag,
            "\n"
          )
          
          
          cat(
            "磷酸胆碱：",
            res$choline,
            "\n"
          )
          
          
        }
        
        
      })
      
      
    }
    
  )
  
}