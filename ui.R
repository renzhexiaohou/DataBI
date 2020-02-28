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
            title = ("DRUGX real-time dose finding"),

# Study design ------------------------------------------------------------

            tabPanel("Study design",
                     fluidRow(
                         column(5, 
                     h3(strong("DRUGX-1001 (US FIH)")),
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
                     h4(textInput(inputId = "ID", label = "Subject ID", value = NULL, placeholder = "100002")),
                     fluidRow(
                         h4("Demographics"),
                         column(4, DT::dataTableOutput("dm_tab")),
                         # column(4, tableOutput("dm_tab")),
                         column(8, plotOutput("id_plot"))
                     ),
                     fluidRow(
                         h4("Adeverse events"),
                         column(4, tableOutput("ae_tab")),
                         column(8, plotOutput(NULL))
                     ),
                     fluidRow(
                         h4("ID"),
                         column(4, tableOutput("he_tab")),
                         column(8, plotOutput(NULL))
                     ),
                     fluidRow(
                         h4("ID"),
                         column(4, tableOutput("ch_tab")),
                         column(8, plotOutput(NULL))
                     ),
                     fluidRow(
                         h4("ID"),
                         column(4, tableOutput("tl_tab")),
                         column(8, plotOutput(NULL))
                     )
            ),


# Modeling and simulation -------------------------------------------------

            tabPanel("Modeling and simulation"
                
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
