#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)

library(dplyr)
library(tibble)
library(stringr)

library(DT)
library(ggplot2)
# library(plotly)
# source("plotlyGraphWidget.R")

library(mrgsolve)
library(deSolve)
library(PKNCA)

shinyUI(
    fluidPage(
        theme = shinytheme("flatly"),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
        navbarPage(
            title = ("DRUGX real-time dose finding"),

# Study design ------------------------------------------------------------

            tabPanel("Study design",
                     fluidRow(
                         column(5, 
                     h3(strong("DRUGX (US FIH)")),
                     hr(),
                     h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> A Phase 1, Multi-Center, Open-Label Study to Evaluate the Safety, Tolerability, Pharmacokinetics, and Preliminary Evidence of Antitumor Activity of DRUGX in Adult Patients with Advanced Solid Tumors </body></strong>")),
                     h5(HTML("<strong style='color:#000000'>Primary Objective:</strong> To determine the MTD and/or RP2D and assess the DLT of DRUGX as a single agent when administered orally to adult subjects with advanced solid tumors.")),
                     h5(HTML("<strong style='color:#000000'>Secondary Objectives:</strong> To characterize the safety and tolerability of DRUGX; To determine the PK profiles of DRUGX after a single dose and at steady state after multiple doses; To evaluate the ORR, PFS, and DOR as preliminary evidence of antitumor activity of DrugX treatment as determined by the RECIST v1.1 applied by the Investigator at each site.")),
                     h5(HTML("<strong style='color:#000000'>Exploratory Objectives:</strong> To evaluate the ORR PFS, and DOR as determined by iRECIST as assessed applied by Investigator review of DRUGX. To explore immunological biomarkers in tumor tissue and blood that predict treatment response and resistance. To investigate a panel of molecular biomarkers (genomic, metabolic) that may be predictive of clinical response or resistance, safety, MOA, and PDx activity."))
                     ),
                     br(),
                     br(),
                     br(),
                     column(7,
                            img(src="StudyDesign.png", align = "bottom", height = 350)
                            )
                     )
            ),
            

# Data visualization ------------------------------------------------------

            tabPanel("Data monitoring",
                     
                     # h4(textInput(inputId = "ID", label = "Subject ID", value = NULL, placeholder = "100002")),
                     absolutePanel(
                         top = 90, right = 350, width = 200, height = 10, draggable = TRUE,
                         h4(textInput(inputId = "ID", label = "Subject ID", value = NULL, placeholder = "100002"))
                     ),
                     
                     hr(),
                     br(),
                     
                     fluidRow(
                         h4(strong("Demographics")),
                         br(),
                         column(5, dataTableOutput("dm_tab")),
                         column(7, plotOutput("dm_plot"))
                     ),
                     
                     hr(),
                     
                     fluidRow(
                         h4(strong("Adeverse events")),
                         br(),
                         column(5, dataTableOutput("ae_tab")),
                         column(7, plotOutput("ae_plot"))
                     ),
                     
                     hr(),
                     
                     fluidRow(
                         h4(strong("Hematology")),
                         br(),
                         column(5, dataTableOutput("he_tab")),
                         column(7, plotOutput("he_plot"))
                     ),
                     
                     hr(),
                     
                     fluidRow(
                         h4(strong("Chemistry")),
                         br(),
                         column(5, dataTableOutput("ch_tab")),
                         column(7, plotOutput("ch_plot"))
                     ),
                     
                     hr(),
                     
                     fluidRow(
                         h4(strong("Target lesions")),
                         br(),
                         column(5, DT::dataTableOutput("tl_tab")),
                         column(7, plotOutput("tl_plot"))
                     )
            ),


# Modeling and simulation -------------------------------------------------

            tabPanel(
                "Modeling and Simulation",
                #左栏
                fluidRow(
                    column(4,
                           style = "background: #ffffff",
                           
                           br(),
                           br(),
                           br(),
                           # 给药方案的设定 ------------------------------------------
                           # titlePanel(h3(strong("给药方案"))),
                           titlePanel(h3(strong(""))),
                           wellPanel(
                               style = "background: #ededed",
                               fluidRow(
                                   column(3, numericInput("timepoint0", "Time (h)", value = 0, min = 0, step = 1)),
                                   column(3, numericInput("dosage0", "Dose (mg)", value = 4, min = 0, step = 0.1)),
                                   column(3, numericInput("interval0", "Interval (h)", value = 24, min = 0, step = 2)),
                                   column(3, numericInput("times0", "N", value = 2, min = 1, step = 1)),
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
                                         )
                               ),
                               titlePanel(h5(strong("DrugX plasma concentration range (ng/mL)"))),
                               sliderInput("concrange", label = NULL, min = 0, max = 500, step = 0.1, value = c(20, 20)),
                               h6("* Dose regimen is the dosage of DrugX.")
                           )
                    ),
                    column(8,
                           # 药时曲线呈现  ----------------------------------------
                           # titlePanel(h3(strong("药时曲线"))),
                           titlePanel(h3(strong("Conc-Time Curve"))),
                           br(),
                           plotOutput(outputId = "pkconcplot1")
                    )
                ),
                
                
                fluidRow(
                    column(2,
                           sliderInput("cl_pk", label = "CL/F (L/h)", min = 1, max = 20, step = 0.01, value = c(2.75))),
                    column(2,
                           sliderInput("vc_pk", label = "Vc/F (L)", min = 1, max = 100, step = 0.1, value = c(9))),
                    column(2,
                           sliderInput("vp_pk", label = "Vp/F (L)", min = 10, max = 1000, step = 1, value = c(267))),
                    column(2,
                           sliderInput("q_pk", label = "Q/F (L)", min = 1, max = 100, step = 1, value = c(31))),
                    column(2,
                           sliderInput("ka_pk", label = "Ka (1/h)", min = 0.1, max = 2, step = 0.01, value = c(0.32))),
                    column(2,
                           sliderInput("tlag_pk", label = "Tlag (h)", min = 0, max = 1, step = 0.01, value = c(0)))
                ),
                
                
                br(),
                hr(),
                
                fluidRow(
                    titlePanel(h3(strong("Tumor Growth Curve"))),
                    plotOutput(outputId = "tumor_plot")
                ),
                fluidRow(
                    column(2,
                           sliderInput("emax_tumor", label = "EMAX", min = 0.0001, max = 0.01, step = 0.0001, value = c(0.0069))),
                    column(2,
                           sliderInput("ec50_tumor", label = "EC50 (ng/mL)", min = 1, max = 200, step = 0.1, value = c(56.4))),
                    column(2,
                           sliderInput("lamda0_tumor", label = "Lambda0", min = 0.0001, max = 0.01, step = 0.0001, value = c(0.0041))),
                    column(2,
                           sliderInput("w0_tumor", label = "W0 (mm^3)", min = 1, max = 200, step = 10, value = c(100))),
                    column(2,
                           sliderInput("frac_tumor", label = "Frac", min = 0.01, max = 1, step = 0.001, value = c(0.098)))
                ),
                absolutePanel(
                    top = 120, left = 200, width = 80, height = 10, draggable = TRUE,
                    HTML(
                        paste0("<strong>TGI<sub>", "", "</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                               textOutput(outputId = "tgi", inline = T), "</code> %</strong>")
                    )
                ),
                
                
                br(),
                hr(),
                
                fluidRow(
                    titlePanel(h3(strong("pERK-Conc Curve"))),
                    plotOutput(outputId = "perk_plot")
                ),
                fluidRow(
                    column(2,
                           sliderInput("emax_biomarker", label = "EMAX", min = 0.1, max = 1, step = 0.05, value = c(1))),
                    column(2,
                           sliderInput("ec50_biomarker", label = "EC50 (ng/mL)", min = 1, max = 100, step = 1, value = c(20))),
                    column(2,
                           sliderInput("hill_biomarker", label = "Hill", min = 1, max = 5, step = 0.1, value = c(1))
                    )
                ),
                absolutePanel(
                    top = 120, left = 320, width = 130, height = 10, draggable = TRUE,
                    HTML(
                        paste0("<strong>Inhibition<sub>", "", "</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                               textOutput(outputId = "inhi", inline = T), "</code> %</strong>")
                    )
                ),
                
                
                br(),
                hr(),
                
                fluidRow(
                    titlePanel(h3(strong("ANC-Time Curve"))),
                    plotOutput(outputId = "anc_plot")
                ),
                fluidRow(
                    column(2,
                           sliderInput("slope_anc", label = "Slope", min = 0.0001, max = 0.01, step = 0.00005, value = c(0.0039))),
                    column(2,
                           sliderInput("circ0_anc", label = "Circ0 (*10^9)", min = 1, max = 10, step = 0.1, value = c(5))),
                    column(2,
                           sliderInput("gamma_anc", label = "Gamma", min = 0.001, max = 1, step = 0.001, value = c(0.122))),
                    column(2,
                           sliderInput("mtt_anc", label = "MTT (h)", min = 50, max = 200, step = 5, value = c(125)))
                ),
                absolutePanel(
                    top = 180, left = 615, width = 170, height = 10, draggable = TRUE,
                    HTML(
                        paste0("<strong>ANC nadir<sub>", "", "</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                               textOutput(outputId = "nadir", inline = T), "</code> *10^9</strong>")
                    )
                ),
                
                
                # absolutePanel(
                #     top = 105, right = 500, width = 160, height = 10, draggable = FALSE,
                #     sliderInput("cycle", label = "Cycle", min = 1, max = 20, step = 1, value = c(1))
                # ),
                absolutePanel(
                    top = 105, right = 335, width = 160, height = 10, draggable = FALSE,
                    titlePanel(h5(strong("AUC Range")))
                ),
                absolutePanel(
                    top = 105, right = 248, width = 160, height = 10, draggable = FALSE,
                    uiOutput("sliderAUC", inline = T)
                ),
                absolutePanel(
                    top = 123, right = 8, width = 220, height = 10, draggable = FALSE,
                    HTML(paste0("<strong>AUC<sub>", textOutput(outputId = "auclower", inline = T), "-",
                                textOutput(outputId = "aucupper", inline = T), "hr</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                                textOutput(outputId = "pkauc1", inline = T), "</code> hr*ng/mL</strong>"))
                ),
                absolutePanel(
                    top = 100, left = 55, width = 100, height = 10, draggable = TRUE,
                    img(src="LOGOdMed.png", height = 50)
                )
            ),


# Who we are ? ------------------------------------------------------------

            tabPanel("Who we are ?",
                         fluidRow(
                             column(5,
                                    h3(HTML("<strong style='color:#ec4c3c;background-color:#F8F9F9'> EDCP </strong> Capability")),
                                    hr(),
                                    h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> Innovative trial </body></strong>")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> opimal early phase study design: FIH, POM, POC ")),
                                    h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> Clinical pharmacology </body></strong>")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> BA/BE, FE, DDI, mass balance, special populations, QTc")),
                                    h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> PK/PD calculation and analysis </body></strong>")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> NCA, exploratory PK/PD relationship analysis")),
                                    h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> PK/PD modeling and simulation </body></strong>")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> translational PKPD for safety and efficacy dose")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> clinical PKPD for dose rationale on RP2D, RP3D")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> covariates analysis, optimal sampling, pediatric extrapolation")),
                                    h3("  "),
                                    h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> Strategic consultation </body></strong>")),
                                    h5(HTML("<strong style='color:#000000'> - </strong> CDP/TPP, bridging strategy, ethnic sensitivity assessment"))
                                    
                             ),
                             column(7,
                                    img(src="EDCP.png",height = 440)
                             )

                         )
                     # br(),
                     # h3(strong("Note")),
                     # hr(),
                     # h4("* For practice only ~"),
                     # img(src="renzhexiaohou_logo.png", height = 150)
                     # absolutePanel(
                     #     top = 100, left = 55, width = 100, height = 10, draggable = TRUE,
                     #     img(src="LOGOdMed.png", height = 50)
                     # )
            ),
            
            footer = h5(HTML("dMed Copyright 2020 : 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> E </strong>
                       arly <strong style='color:#ec4c3c;background-color:#F8F9F9'> D </strong> evelepment and 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> C </strong> linical 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> P </strong>harmacology"), align = "right")
        )
    )
)