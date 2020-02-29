#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(lubridate)

library(DT)
library(ggplot2)
# library(plotly)
# source("plotlyGraphWidget.R")

library(mrgsolve)
library(deSolve)
library(PKNCA)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    

# Dm ----------------------------------------------------------------------
    output$dm_plot <- renderPlot({
        ggplot(data = filter(Dm),
               aes(x = Subject, y = Age)) +
            geom_bar(stat="identity") +
            scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            theme_bw(base_size = 20)
    })
    
    output$dm_tab <- renderDT(
        {filter(Dm, Subject %in% input$ID)},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1),
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )
    # output$dm_tab <- renderTable ({
    #     Dm %>% filter(Subject %in% input$ID)
    # })


# Ae ----------------------------------------------------------------------
    output$ae_plot <- renderPlot({
        ggplot(data = filter(Ae, Subject %in% input$ID),
               aes(x = DAY, y = Grade_n)) +
            geom_point(data = filter(Ae, Subject %in% input$ID, AEOUT == "Not Recovered or Not Resolved"), 
                       size = 5, alpha = 0.6, colour = "#882222") +
            geom_point(data = filter(Ae, Subject %in% input$ID, AEOUT == "Recovering or Resolving"), 
                       size = 5, alpha = 0.6, colour = "#002288") +
            geom_point(data = filter(Ae, Subject %in% input$ID, AEOUT == "Recovered or Resolved"), 
                       size = 5, alpha = 0.6, colour = "green") +
            geom_point(aes(shape = AETERM), size = 3) +
            # geom_line(size = 1.2, linetype = "dashed") +
            scale_x_continuous(breaks = seq(1, 7*1000, 7)) +
            scale_y_continuous(breaks = c(0:5), limits = c(0, 5)) +
            xlab("Norminal Day") + ylab("AE Grades") +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            theme(legend.position =  c(0.70,0.75))
    })
    output$ae_tab <- renderDT(
        {filter(Ae, Subject %in% input$ID)},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1),
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )
    # output$ae_tab <- renderTable ({
    #     Ae %>% filter(Subject %in% input$ID)
    # })
    

# He ----------------------------------------------------------------------
    output$he_plot <- renderPlot({
        ggplot(filter(He, Subject %in% input$ID, PLAT != "ND"), 
               aes(x = DAY, y = as.numeric(PLAT), group = Subject)) +
            geom_point(size = 2) +
            geom_line() +
            scale_x_continuous(breaks = seq(1, 7*1000, 7)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Norminal Day") + ylab("PLAT")
    })
    
    output$he_tab <- renderDT(
        {filter(He, Subject %in% input$ID)},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1),
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )
    # output$he_tab <- renderTable ({
    #     He %>% filter(Subject %in% input$ID)
    # })


# Ch ----------------------------------------------------------------------
    output$ch_plot <- renderPlot({
        ggplot(filter(Ch, Subject %in% input$ID, AST != "ND"), 
               aes(x = DAY, y = as.numeric(AST), group = Subject)) +
            geom_point(size = 2) +
            geom_line() +
            scale_x_continuous(breaks = seq(1, 7*1000, 7)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Norminal Day") + ylab("AST")
    })
    output$ch_tab <- renderDT(
        {filter(Ch, Subject %in% input$ID)},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1),
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )
    # output$ch_tab <- renderTable ({
    #     Ch %>% filter(Subject %in% input$ID)
    # })


# Tl ----------------------------------------------------------------------
    output$tl_plot <- renderPlot({
        ggplot(filter(Tl, Subject %in% input$ID), 
               aes(x = DAY, y = TLRECRES_n, group = TLLOCDET)) +
            geom_point(size = 2) +
            geom_line() +
            scale_x_continuous(breaks = seq(1, 7*1000, 7)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Norminal Day") + ylab("TLRECRES_n")
    })
    output$tl_tab <- renderDT(
        {filter(Tl, Subject %in% input$ID)},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1),
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )
    # output$tl_tab <- renderTable ({
    #     Tl %>% filter(Subject %in% input$ID)
    # })
    
    
    # output$inter_plot <- renderGraph({
    # 
    #     final <- readRDS("PlotTrend.rds") %>% filter(drug %in% c("生物制品", "化学药物"))
    # 
    #     Year <- summary(final$dt) %>% names()
    # 
    #     PH1 <- filter(final, phase == "I期")
    #     PH1num <- summary(PH1$dt)
    # 
    #     PH2 <- filter(final, phase == "II期")
    #     PH2num <- summary(PH2$dt)
    # 
    #     PH3 <- filter(final, phase == "III期")
    #     PH3num <- summary(PH3$dt)
    # 
    #     data <- tibble(Year, PH1num, PH2num, PH3num)
    # 
    # 
    #     cPH1num <- '#246a99'
    #     cPH2num <- '#2e88c5'
    #     cPH3num <- '#85c1e9'
    # 
    #     plot_ly(data, x = ~Year) %>%
    #         add_trace(y = ~PH1num, name = 'I',type = "bar",
    #                   text = ~PH1num, textposition = 'inside',
    #                   marker = list(color = cPH1num, opacity=0.9)) %>%
    #         add_trace(y = ~PH2num, name = 'II',type = "bar",
    #                   text = ~PH2num, textposition = 'inside',
    #                   marker = list(color = cPH2num, opacity=0.9)) %>%
    #         add_trace(y = ~PH3num, name = 'III',type = "bar",
    #                   text = ~PH3num, textposition = 'inside',
    #                   marker = list(color = cPH3num, opacity=0.9)) %>%
    #         # add_text(textfont = t, textposition = "top center") %>%
    #         layout(yaxis = list(title = 'Count'), barmode = 'stack',
    #                xaxis = list(title = "", tickangle = -45))
    # })

    
    # customed simulation -----------------------------------------------------
    
    # treatment UI - reactive UI design -------------------------------
    # inserted or removed treatment input
    inserted <- c()
    observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- paste0('treatment', btn)
        insertUI(
            selector = '#placeholder',
            ## wrap element in a div with id for ease of removal
            ui = tags$div(
                # tags$p(paste('Element number')), 
                column(3, numericInput(paste0("timepoint",btn), NULL, value = 0)),
                column(3, numericInput(paste0("dosage",btn), NULL, value = 0)),
                column(3, numericInput(paste0("interval",btn), NULL, value = 0)),
                column(3, numericInput(paste0("times",btn), NULL, value = 0)),
                id = id
            )
        )
        inserted <<- c(id, inserted)
    })
    observeEvent(input$removeBtn, {
        removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[length(inserted)])
        )
        inserted <<- inserted[-length(inserted)]
    })
    
    # treatment data - extract from UI ------------
    ## flag every treatment (insert or remove) action
    flag <- reactiveValues()
    
    flag[["flag0"]] <- 0
    insflaglist <- reactiveValues()
    insflaglist[["ins0"]] <- 0
    remflaglist <- reactiveValues()
    remflaglist[["rem0"]] <- 0
    
    # logic value for each flag
    maxflag <- function(flag) {
        max(unlist(reactiveValuesToList(flag)))
    }
    observeEvent(input$insertBtn, {
        flag[[paste0("flag", maxflag(flag) + 1)]] <- maxflag(flag) + 1
        insflaglist[[paste0(maxflag(flag))]] <- TRUE
        remflaglist[[paste0(maxflag(flag))]] <- FALSE
    })
    observeEvent(input$removeBtn, {
        flag[[paste0("flag", maxflag(flag) + 1)]] <- maxflag(flag) + 1
        insflaglist[[paste0(maxflag(flag))]] <- FALSE
        remflaglist[[paste0(maxflag(flag))]] <- TRUE
    })
    
    ## manipulate treatment data
    treatment <- reactive({
        # extract target variables from reactive "list"
        unlisted <- function(data, var) {
            data[str_which(names(data), var)] %>% unlist()
        }
        ordered <- function(data) {
            names(data) %>% str_extract(., "[:digit:]+") %>% as.integer()
        } 
        inputlist <- reactiveValuesToList(input)
        trt <- tibble(timepoint = unlisted(inputlist, "timepoint"),
                      tptname = unlisted(inputlist, "timepoint") %>% ordered(),
                      dosage = unlisted(inputlist, "dosage"),
                      dosname = unlisted(inputlist, "dosage") %>% ordered(),
                      interval = unlisted(inputlist, "interval"),
                      intname = unlisted(inputlist, "interval") %>% ordered(),
                      times = unlisted(inputlist, "times"),
                      timname = unlisted(inputlist,"times") %>% ordered())
        trt1 <- trt %>% select(timepoint, tptname) %>% rename(ord = tptname)
        trt2 <- trt %>% select(dosage, dosname) %>% rename(ord = dosname)
        trt3 <- trt %>% select(interval, intname) %>% rename(ord = intname)
        trt4 <- trt %>% select(times, timname) %>% rename(ord = timname)
        
        # treatment data (have both inserted and removed records)
        treatment <- trt1 %>%
            left_join(trt2, by = "ord") %>%
            left_join(trt3, by = "ord") %>%
            left_join(trt4, by = "ord") %>%
            arrange(ord)
        
        # final treatment data (has excluded removed records)
        nrow_removed <- sum(unlist(reactiveValuesToList(remflaglist)))
        if(nrow_removed == 0) {return(treatment)}
        if(nrow_removed >=1) {return(treatment[-(2:(nrow_removed + 1)),])}
    })
    
    # dosing data - prepare for sim ---------------------
    # convert dosing times into every duplicated dosing record
    doseduplicate <- function(data, times) {
        if (!is.data.frame(data)) {
            cat("input data should be data.frame or tibble :)")
            stop()
        }
        if (!is.character(times)) {
            cat("times variable should be a string type name :o")
            stop()
        }
        l <- list()
        s <- split(data, row.names(data))
        rownames_to_column(data) %>%
            .[["rowname"]] %>%
            lapply(function(x) {
                l[[paste0(x)]] <- rep(s[x], s[[x]][[times]])
            }) %>%
            unlist(recursive = FALSE) %>%
            bind_rows()
    } 
    dosing <- reactive({
        # browser()
        # dosing records converted by function
        # relative time to the first dosing timepoint
        # combine multiple dosages into one if have the same dosing timepoint 
        dosing <- treatment() %>% 
            doseduplicate("times") %>% 
            group_by(ord) %>% 
            mutate(relativetime = cumsum(interval) - interval + unique(timepoint)) %>%
            group_by(relativetime) %>%
            mutate(time = unique(relativetime),
                   dose = sum(dosage)) %>% 
            ungroup() %>% 
            select(time, dose) %>% 
            distinct() %>%
            arrange(time)
        # dosing_n <- dosing[rep(seq_len(nrow(dosing)), each=input$cycle), ] %>% 
        #   arrange(time, dose)
    })
    
    
    # pkdes function -------
    pkdes <- function(t, y, p) {
        CL = p[1]
        
        V2 = p[2]
        V3 = p[3]
        Q = p[4]
        
        KA = p[5]
        
        K23 = Q / V2
        K32 = Q / V3
        
        dy = vector(length = 1)
        
        dy[1] = -KA*y[1]
        
        CONC = 1000*y[2]/V2
        
        dy[2] = KA*y[1] + K32*y[3] - K23*y[2] - CL/V2*y[2]
        dy[3] = K23*y[2] - K32*y[3]
        
        EMAXt = p[6]
        EC50t = p[7]
        LAMBDA0t = p[8]
        
        EFFECTt = EMAXt*CONC/(EC50t + CONC)
        
        dy[4] = LAMBDA0t*y[4] - EFFECTt*y[4]
        dy[5] = LAMBDA0t*y[5]
        dy[6] = LAMBDA0t*y[6]
        
        EMAXb = p[9]
        EC50b = p[10]
        HILL = p[11]
        
        EFFECTb = -EMAXb*CONC**HILL / (EC50b + CONC**HILL)
        
        SLOPEa = p[12]
        GAMMAa = p[13]
        MTTa = p[14]
        CIRC0a = p[15]
        
        EFFECTAa = SLOPEa*CONC
        KTRa = 4/MTTa
        KPROLa = KTRa
        KCIRCa = KTRa
        
        
        dy[7] = KPROLa*y[7]*(1-EFFECTAa)*(CIRC0a/y[11])**GAMMAa - KTRa*y[7]
        dy[8] = KTRa*y[7] - KTRa*y[8]
        dy[9] = KTRa*y[8] - KTRa*y[9]
        dy[10] = KTRa*y[9] - KTRa*y[10]
        dy[11] = KTRa*y[10] - KCIRCa*y[11]
            
        list(dy)
    }
    
    # pksim function with lsoda wrapped
    pksim <- function(data, func, parms, times, tlag, w0t, fract, circ0a) {
        # define pk dosing event data.frame
        dosevent <- data.frame(var = rep(1, nrow(data)),
                               time = data$time + tlag,
                               value = data$dose,
                               method = rep("add", nrow(data)))
        # keep lsoda function as primary style
        sim <-  lsoda(y = c(y1 = 0, y2 = 0, y3 = 0, 
                            y4 = w0t*(1-fract), y5 = w0t*fract, y6 = w0t,
                            y7 = circ0a, y8 = circ0a, y9 = circ0a, y10 = circ0a, y11 =  circ0a), 
                      times = times, 
                      func = func,
                      parms = parms,
                      events = list(data=dosevent),
                      rtol = 1e-05, atol = 1e-05) %>% 
            as.data.frame()
        
        pksim <- data %>%
            left_join(sim, ., "time")
    }
    
    
    # pkconc data - simulation under deSolve ---------------------------------------------
    pkconc <- reactive({
        # browser()
        pkparams <- c(input$cl_pk,
                      input$vc_pk, 
                      input$vp_pk,
                      input$q_pk,
                      input$ka_pk,
                      input$emax_tumor,
                      input$ec50_tumor,
                      input$lamda0_tumor,
                      input$emax_perk,
                      input$ec50_perk,
                      input$hill_perk,
                      input$slope_anc,
                      input$gamma_anc, 
                      input$mtt_anc,
                      input$circ0_anc)
        # obstimes <- seq(0, (max(dosing()$time) + 72)*10, 1)/10
        obstimes <- unique(c(seq(0, 6*100, 1)/100, seq(60, (max(24) + 72)*10, 1)/10))
        pkconc <- pksim(data = dosing(),
                        func = pkdes,
                        parms = pkparams,
                        times = obstimes,
                        tlag = input$tlag_pk,
                        w0t = input$w0_tumor,
                        fract = input$frac_tumor,
                        circ0a = input$circ0_anc) %>% 
            mutate(conc = 1000*y2/input$vc_pk,
                   inhi = input$emax_perk*conc**input$hill_perk / (input$ec50_perk**input$hill_perk + conc**input$hill_perk),
                   anc = y11) %>% 
            select(time, dose, conc, y1, y2, y3, y4, y5, y6, inhi,
                   y7, y8, y9, y10, y11, anc)
        return(pkconc)
    })
    
    
    # prevent reactive expressions from crossing confliting or dependence  -----------
    reac <- reactiveValues(auto = c(), AUCrange = c(), AUC = c(), TRT = c(), DOS = c(), PKCONC = c())
    observe({
        input$partialauc
        reac$AUCrange <- isolate(input$partialauc)
    })
    observe({
        treatment()
        dosing()
        pkconc()
        reac$PKCONC <- pkconc()
        reac$TRT <- treatment()
        reac$DOS <- dosing()
    })
    
    observe({
        input$partialauc
        reac$auto <- FALSE
    })
    observe({
        input$autoauc
        reac$auto <- TRUE
    })
    
    # time range UI for partial AUC ------------------------------------------
    output$sliderAUC <- renderUI({
        trt <- reac$TRT
        if(reac$auto) trt <- treatment()
        dos <- reac$DOS
        
        minAUC <- 0
        maxAUC <- last(dos$time) + 72
        startAUC <- max(trt$timepoint + trt$interval*(trt$times - 1))
        endAUC <- max(trt$timepoint + trt$interval*trt$times)
        # other purpose: 
        # startAUC <- last(trt$timepoint) + last(trt$interval)*(last(trt$times) - 1)
        # endAUC <- last(trt$timepoint) + last(trt$interval)*last(trt$times)
        
        sliderInput("partialauc", label = NULL,
                    min = minAUC, max = maxAUC, step = 1,
                    value = c(startAUC, endAUC))
    })
    
    
    # time range UI for observed time -----------------------------------------
    output$sliderOBSTIME <- renderUI({
        # trt <- reac$TRT
        # if(reac$auto) trt <- treatment()
        dos <- reac$DOS
        
        minOBS <- 0
        maxOBS <- last(dos$time) + 72 + 24
        startOBS <- 0
        endOBS <- last(dos$time) + 72
        
        sliderInput("observedtime", label = NULL, width = "98%",
                    min = minOBS, max = maxOBS, step = 1, 
                    value = c(startOBS, endOBS))
    })
    
    
    # pkplot function  ------------------------------------
    pkplot <- function(data, x, y, dosing) {
        # define appropriate concentration range (height for y axis)
        concrange.txt <- data.frame(
            x = rep(max(dosing$time) + 72, 2)*0.95,
            y = c(input$concrange[2], input$concrange[1]) + max(input$concrange[2], data$conc + 1)*0.05,
            name = paste0(c(input$concrange[2], input$concrange[1]), " ng/mL")
        )
        # ggplot!
        ggplot(data = data, aes(x = x, y = y)) +
            geom_line(size = 1) +
            # dynamic AUC showing
            geom_area(aes(x = ifelse(x >= reac$AUCrange[1] & x <= reac$AUCrange[2], x, NA_real_)),
                      fill = "#9898fb", alpha = 0.5,  colour = "black", size = 0.15, na.rm = TRUE) +
            # dynamic TDM range showing
            geom_hline(aes(yintercept = input$concrange[2]), colour = "red", linetype = "dashed", size = 1.5) +
            geom_hline(aes(yintercept = input$concrange[1]), colour = "#0080ff", linetype = "dashed", size = 1.5) +
            geom_text(data = concrange.txt, aes(label = name, fontface = 2), colour = c("red", "#0080ff"), size = 5) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Time (hr)") + ylab("DrugX Plasma Concentration (ng/mL)")
    }
    
    # pkconc plot  ---------------------------------------------------------
    output$pkconcplot1 <- renderPlot({
        pk <- reac$PKCONC %>% 
            filter(time>=input$observedtime[1],
                   time<=input$observedtime[2])
        trt <- reac$TRT
        dos <- reac$DOS
        
        pkplot(pk, pk$time, pk$conc, dos) +
            scale_x_continuous(breaks = seq(0, min(trt$interval)*100, min(trt$interval)),
                               limits = c(input$observedtime[1], input$observedtime[2]))
    })
    
    
    # partial AUC from NCA --------------------------------------------------
    output$auclower <- renderText({reac$AUCrange[1]})
    output$aucupper <- renderText({reac$AUCrange[2]}) 
    reacAUC <- reactive({
        # PKNCA packages code as followed
        # (since ncappc packages can't be used in linux sys)
        pk <- reac$PKCONC %>%
            mutate(subject = 1, time = time, conc = conc) %>%
            distinct() %>%
            as.data.frame()
        auc <- PKNCAconc(pk, conc~time|subject) %>%
            PKNCAdata(.,intervals=data.frame(
                start=input$partialauc[1],
                end=input$partialauc[2],
                auclast=TRUE
            )) %>%
            pk.nca() %>%
            as.data.frame(.$result)
        round(auc$PPORRES, digits = 2)
    })
    
    output$pkauc1 <- renderText({
        # browser()
        reacAUC()
    })
    
    
    output$tgi <- renderText({
        # browser()
        pk <- reac$PKCONC %>% 
            filter(time <= max(reac$PKCONC$time)-72)
        
        t0 <-  max(pk$y5) + max(pk$y4)
        t <- pk$y6 %>% max()
        round(abs(t - t0)/(t-input$w0_tumor) * 100, digits = 0)
    })
    output$tumor_plot <- renderPlot({
        # browser()
        pk <- reac$PKCONC %>% 
            filter(time <= max(reac$PKCONC$time)-72)
        
        tgi60 <- max(pk$y6) - (max(pk$y6)-input$w0_tumor)*0.6
        
        ggplot(pk) +
            # geom_point(size = 2) +
            geom_line(data=pk, 
                      aes(x = time/24+1, y = y4 + y5, group="1"), 
                      linetype = "solid", size =1) +
            geom_line(data=pk, 
                      aes(x = time/24+1, y = y6, group="2"), 
                      linetype = "dashed", colour ="red", alpha=0.5, size =1) +
            geom_hline(aes(yintercept = tgi60), linetype = "dashed", colour ="#9898fb", alpha=1, size =1) +
            scale_x_continuous(breaks = seq(1, 7*1000, 7)) +
            # scale_y_continuous(breaks = seq(0, 100*1000, 100), limits = c(0, 1500)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Norminal Day") + ylab("Tumor Size (mm^3)")
    })
    

    output$perk <- renderText({
        
        pk <- reac$PKCONC %>% 
            filter(time <= max(reac$PKCONC$time)-72)        
        
        eff <- last(pk$inhi)
            
        round(abs(eff) * 100, digits = 0)
    })
    output$perk_plot <- renderPlot({
        # browser()
        pk <- reac$PKCONC %>% 
            filter(time <= max(reac$PKCONC$time)-72)
        
        ggplot(pk) +
            # geom_point(size = 2) +
            geom_line(data=pk, 
                      aes(x = conc, y = inhi*100, group="1"), 
                      linetype = "solid", size =1) +
            geom_hline(aes(yintercept = 80), linetype = "dashed", colour ="#9898fb", alpha=1, size =1) +
            scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Plasma Concentration (ng/mL)") + ylab("pERK inhibition (%)")
    })
    
    output$nadir <- renderText({
        
        # browser()
        pk <- reac$PKCONC %>% 
            filter(time <= max(reac$PKCONC$time)-72)
        
        nadir <- min(pk$anc)
        
        round(nadir, digits = 1)
        
    })
    output$anc_plot <- renderPlot({
        # browser()
        pk <- reac$PKCONC %>% 
            filter(time <= max(reac$PKCONC$time)-72)
        
        ggplot(pk) +
            # geom_point(size = 2) +
            geom_line(data=pk, 
                      aes(x = time/24+1, y = anc, group="1"), 
                      linetype = "solid", size =1) +
            geom_hline(aes(yintercept = 1), linetype = "dashed", colour ="red", alpha=0.5, size =1) +
            scale_x_continuous(breaks = seq(1, 7*1000, 7)) +
            scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
            theme_bw(base_rect_size = 1) +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 16),
                  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
            xlab("Norminal Day") + ylab("ANC (*10^9)")
    })
        
    session$allowReconnect(TRUE)
    
})
