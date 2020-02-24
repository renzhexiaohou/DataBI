#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
# library(plotly)
# library(PKNCA)

library(shiny)
library(shinythemes)


shinyUI(
    fluidPage(
        theme = shinytheme("flatly"),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
        navbarPage(
            title = ("JAB3312 Real-Time Dose Finding"),
            
            # first page: description -------------------------------------------------
            tabPanel("Study Design",
                     h3(strong("Reference")),
                     hr(),
                     h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'>Understanding variability with voriconazole using a population pharmacokinetic approach: implications for optimal dosing</body></strong>")),
                     h5(HTML("<strong style='color:#000000'>Objectives:</strong> Voriconazole exhibits highly variable, non-linear pharmacokinetics and is associated with a narrow therapeutic range. This study aimed to investigate the population pharmacokinetics of voriconazole in adults, including the effect of CYP2C19 genotype and drug-drug interactions.")),
                     img(src="pkmodelstr.png",height = 150),
                     br(),
                     h3(strong("Note")),
                     hr(),
                     h4("* For practice only ~"),
                     img(src="renzhexiaohou_logo.png", height = 150)
            ),
            
            # second page: simulation -------------------------------------------------
            tabPanel("Data Overview",
                     fluidRow(
                         column(4,
                                # style = "background: #ffffff",
                                titlePanel(h3(strong("Parameters"))),
                                # hr(),
                                wellPanel(

                                    # h6("* PK参数及协变量默认值基于参考文献")
                                    h6("* Based on published reference.")
                                ),
                                # 给药方案的设定 ------------------------------------------
                                # titlePanel(h3(strong("给药方案"))),
                                titlePanel(h3(strong("Treatment"))),
                                wellPanel(
                                    style = "background: #ededed",
                                    fluidRow(
                                        # column(3, numericInput("timepoint0", "医嘱时间", value = 0, min = 0, step = 1)),
                                        # column(3, numericInput("dosage0", "剂量 (mg)", value = 400, min = 0, step = 100)),
                                        # column(3, numericInput("interval0", "间隔", value = 12, min = 0, step = 2)),
                                        # column(3, numericInput("times0", "次数", value = 6, min = 1, step = 1)),
                                        column(3, numericInput("timepoint0", "Time (h)", value = 0, min = 0, step = 1)),
                                        column(3, numericInput("dosage0", "Dose (mg)", value = 400, min = 0, step = 100)),
                                        column(3, numericInput("interval0", "Interval (h)", value = 12, min = 0, step = 2)),
                                        column(3, numericInput("times0", "N", value = 6, min = 1, step = 1)),
                                        tags$div(id = 'placeholder')
                                    ),
                                    wellPanel(style='background: #ededed; padding:0px;',
                                              fluidRow(style='background: #ededed; padding:0px;',
                                                       column(9,style='background: #ededed; padding:0px;',
                                                              wellPanel(style='background: #ededed; padding:0px;',
                                                                        column(4, style='background: #ededed; padding:14px;',
                                                                               # h5(strong("观测时间"))
                                                                               h5(strong("ObsTime"))
                                                                        ),
                                                                        column(8,style='background: #ededed; padding:4px;',
                                                                               # tags$style(type = "text/css", ".irs-slider {width: 20px; height: 20px; top: 15px;}"),
                                                                               uiOutput("sliderOBSTIME", inline = T)
                                                                        )
                                                              )
                                                       ),
                                                       column(3,style='background: #ededed; padding:0px;',
                                                              wellPanel(style='background: #ededed;',
                                                                        column(6,style='background: #ededed; padding:1px;', 
                                                                               actionButton('insertBtn', NULL, width = "85%",
                                                                                            icon = icon(":O", class = "glyphicon glyphicon-plus"),
                                                                                            style='padding:5px;
                                                                                  color: white; background-color: #ec4c3c;
                                                                                  border-width: 0px;
                                                                                  font-size:70%')), 
                                                                        column(6, style='background: #ededed; padding:1px;',
                                                                               actionButton('removeBtn', NULL, width = "85%",
                                                                                            icon = icon(":X", class = "glyphicon glyphicon-minus"),
                                                                                            style='padding:5px;
                                                                                  color: white; background-color: #3498db;
                                                                                  border-width: 0px;
                                                                                  font-size:70%'))
                                                              )
                                                       )
                                                       # actionButton('steadystate', NULL, width = "0%",
                                                       #              icon = icon(":X", class = "glyphicon glyphicon-minus"),
                                                       #              style='padding:5px;
                                                       #              color: white; background-color: #3498db;
                                                       #              border-width: 0px;
                                                       #              font-size:70%')
                                              )
                                    ),
                                    # h6("* 给药剂量默认值为说明书推荐日剂量"),
                                    # hr(),
                                    # titlePanel(h5(strong("浓度范围 (mg/L)"))),
                                    titlePanel(h5(strong("Concentration range (mg/L)"))),
                                    sliderInput("concrange", label = NULL, min = 0, max = 10, step = 0.1, value = c(1,5)),
                                    # h6("* 根据临床指南，建议伏立康唑的治疗窗为1-5mg/L")
                                    h6("* Recommended concentration range in 1-5mg/L.")
                                )
                         ),
                         column(8,
                                # 药时曲线呈现  ----------------------------------------
                                # titlePanel(h3(strong("药时曲线"))),
                                titlePanel(h3(strong("Conc-Time Curve"))),
                                br(),
                                plotOutput(outputId = "pkconcplot1"),
                                # 模拟数据呈现  ----------------------------------------
                                # titlePanel(h3(strong("模拟数据"))),
                                titlePanel(h3(strong("Data"))),
                                div( tableOutput("ae_tab")),
                                div( tableOutput("dm_tab"))
                         )
                     ),
                     absolutePanel(
                         top = 105, right = 306, width = 160, height = 10, draggable = FALSE,
                         titlePanel(h5(strong("Range")))
                     ),
                     absolutePanel(
                         top = 105, right = 248, width = 160, height = 10, draggable = FALSE,
                         uiOutput("sliderAUC", inline = T)
                     ),
                     absolutePanel(
                         top = 123, right = 8, width = 220, height = 10, draggable = FALSE,
                         HTML(paste0("<strong>AUC<sub>", textOutput(outputId = "auclower", inline = T), "-",
                                     textOutput(outputId = "aucupper", inline = T), "hr</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                                     textOutput(outputId = "pkauc1", inline = T), "</code> hr*mg/L</strong>"))
                     )
            ),
            
            # footer = h6("Copyright 2019 上海强世信息科技有限公司", align = "right")
            footer = h6("Copyright 2019 renzhexiaohou", align = "right")
        )
    )
)
