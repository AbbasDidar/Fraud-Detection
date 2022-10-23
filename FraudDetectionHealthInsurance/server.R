

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(readxl)
library(plotly)
library(treemapify)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(shinymanager)
library(ChainLadder)
library(xtable)
# library(shinyauthr)
# library(tidyverse)
# library(glmnet)
# library(randomForest)

# setwd("C:\\Users\\a_didar\\Dropbox\\PC\\Documents\\SHARE\\DataBaseApp\\DayDataBase")

# PIN_FOLDER = board_folder("DayDataBase",versioned = FALSE)
# # PIN_FOLDER |> pin_write( KhatamDetail )
# KhatamDetail = PIN_FOLDER |> pin_read(  'KhatamDetail' )
#
KhatamDetail <- read_excel("Khatam.xlsx")


Cover_Service <- read_excel("Cover_Service.xlsx")

COVERAGE <- sort(unique(Cover_Service$CoverageName))
SERVICE <- sort(unique(Cover_Service$ServiceGroupName))

Fraud_Task_Age <- read_excel("Fraud_Task.xlsx", sheet = "Age_Cover")
Fraud_Task_Gender <- read_excel("Fraud_Task.xlsx", sheet = "Gender_Cover")

Age_Task <- c(COVERAGE[12:16], SERVICE[c(14, 130, 40, 5, 179, 4, 10)])
Gender_Task <- c(COVERAGE[13], SERVICE[c(64, 167, 113)])


# data.frame with credentials info
credentials <- data.frame(
    user = c("Didar", "Mojtaba", "Amirhossein", "M_hassani" ),
    password = c("Abbas", "Abed", "Jafari", "51800"),
    stringsAsFactors = FALSE
)


server <- function(input, output, session) {
    
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    
    
    #-------------------------------------------------------------------------------   
    #-------------------------------------------------------------------------------  
    #---------------------------------Extarct data---------------------------------- 
    #------------------------------------------------------------------------------- 
    #-------------------------------------------------------------------------------   
    
    Statistics_report_reac <- eventReactive(
        input$run,
        KhatamDetail |>
            filter((EventDate >= input$TimeStart & EventDate <= input$TimeEnd) &
                       (Age >= input$Age[1] & Age <= input$Age[2]) &
                       (RequestAmount >= as.numeric(input$MinRequestAmount) & RequestAmount <= as.numeric(input$MaxRequestAmount)) &
                       (RemittanceType %in% input$RemittanceType) &
                       (Deduction_Cause %in% input$elatkosor) &
                       (City %in% input$City) &
                       (CoverageName %in% input$Coverage)) |>
            select(input$COLUMNS)
    )
    
    output$Statistics_report <- DT::renderDataTable(
        DT::datatable(
            {
                Statistics_report_reac()
            },
            
            extensions = "Buttons",
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'tB',
                buttons = c('copy', 'csv', 'excel'),
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 25,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'turquoise', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    #-------------------------------------------------------------------------------
    Statistics_report_CodeMeli_reac <- reactive(
        if (nchar(input$CodeMeliAsli) == 0) {
            KhatamDetail |>
                filter(NationalCode == input$CodeMeli)
        } else {
            KhatamDetail |>
                filter(MainInsuredNathionalNumber == input$CodeMeliAsli)
        }
    )
    
    
    output$Statistics_report_CodeMeli <- DT::renderDataTable(
        DT::datatable(
            {
                Statistics_report_CodeMeli_reac()
            },
            extensions = "Buttons",
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'tB',
                buttons = c('copy', 'csv', 'excel'),
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 25,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'thistle', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    #------------------------------------------------------------------------------- 
    #-------------------------------------------------------------------------------  
    #-----------------------------Ststistics---------------------------------------- 
    #------------------------------------------------------------------------------- 
    #------------------------------------------------------------------------------- 
    
    
    Khatam_summary <- eventReactive(
        input$RUN_STATISTICS_COVERAGE,
        KhatamDetail |>
            filter((EventDate >= input$STATISTIC_COVERAGE_TimeStart & EventDate <= input$STATISTIC_COVERAGE_TimeEnd) &
                       CoverageName %in% input$STATISTIC_COVERAGE) |>
            group_by(CoverageName) |>
            summarise(
                SUM_req = sum(RequestAmount),
                SUM_pay = sum(PayableAmount),
                Mean_req = mean(RequestAmount),
                Mean_pay = mean(PayableAmount),
                N = n()
            ) |>
            na.omit()
    )
    
    
    output$STATISTICS_Coverage_plot <- renderPlot(
        if (input$STATISTICS_Coverage_CRITERIA == "SUM_req") {
            ggplot(Khatam_summary(), aes(area = SUM_req, fill = CoverageName, label = CoverageName)) +
                geom_treemap() +
                geom_treemap_text(
                    colour = "white", place = "centre",
                    grow = TRUE
                ) +
                guides(fill = "none")
        } else {
            if (input$STATISTICS_Coverage_CRITERIA == "SUM_pay") {
                ggplot(Khatam_summary(), aes(area = SUM_req, fill = CoverageName, label = CoverageName)) +
                    geom_treemap() +
                    geom_treemap_text(
                        colour = "white", place = "centre",
                        grow = TRUE
                    ) +
                    guides(fill = "none")
            } else {
                if (input$STATISTICS_Coverage_CRITERIA == "Mean_req") {
                    ggplot(Khatam_summary(), aes(area = Mean_req, fill = CoverageName, label = CoverageName)) +
                        geom_treemap() +
                        geom_treemap_text(
                            colour = "white", place = "centre",
                            grow = TRUE
                        ) +
                        guides(fill = "none")
                } else {
                    if (input$STATISTICS_Coverage_CRITERIA == "Mean_pay") {
                        ggplot(Khatam_summary(), aes(area = Mean_pay, fill = CoverageName, label = CoverageName)) +
                            geom_treemap() +
                            geom_treemap_text(
                                colour = "white", place = "centre",
                                grow = TRUE
                            ) +
                            guides(fill = "none")
                    } else {
                        ggplot(Khatam_summary(), aes(area = N, fill = CoverageName, label = CoverageName)) +
                            geom_treemap() +
                            geom_treemap_text(
                                colour = "white", place = "centre",
                                grow = TRUE
                            ) +
                            guides(fill = "none")
                    }
                }
            }
        }
    )
    
    output$STATISTICS_COVERAGE_DATA <- DT::renderDataTable(
        DT::datatable(
            {
                Khatam_summary()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'tan', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    output$downloadData_STATISTICS_COVERAGE_DATA <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Khatam_summary(), file)
        }
    )
    
    #------------------------------------------------------------------------------- 
    #------------------------------------------------------------------------------- 
    
    
    Khatam_summary_Services <- eventReactive(
        input$RUN_STATISTICS_Services,
        if (input$STATISTICS_Service == "All Services") {
            KhatamDetail |>
                filter((EventDate >= input$STATISTIC_Services_TimeStart & EventDate <= input$STATISTIC_Services_TimeEnd)) |>
                group_by(ServiceGroupName) |>
                summarise(
                    SUM_req = sum(RequestAmount),
                    SUM_pay = sum(PayableAmount),
                    Mean_req = mean(RequestAmount),
                    Mean_pay = mean(PayableAmount),
                    N = n()
                ) |>
                na.omit()
        } else {
            KhatamDetail |>
                filter((EventDate >= input$STATISTIC_Services_TimeStart & EventDate <= input$STATISTIC_Services_TimeEnd) &
                           ServiceGroupName %in% input$STATISTICS_Service) |>
                group_by(ServiceGroupName) |>
                summarise(
                    SUM_req = sum(RequestAmount),
                    SUM_pay = sum(PayableAmount),
                    Mean_req = mean(RequestAmount),
                    Mean_pay = mean(PayableAmount),
                    N = n()
                ) |>
                na.omit()
        }
    )
    
    
    output$STATISTICS_Services_plot <- renderPlot(
        if (input$STATISTICS_Services_CRITERIA == "SUM_req") {
            ggplot(Khatam_summary_Services(), aes(area = SUM_req, fill = ServiceGroupName, label = ServiceGroupName)) +
                geom_treemap() +
                geom_treemap_text(
                    colour = "white", place = "centre",
                    grow = TRUE
                ) +
                guides(fill = "none")
        } else {
            if (input$STATISTICS_Services_CRITERIA == "SUM_pay") {
                ggplot(Khatam_summary_Services(), aes(area = SUM_req, fill = ServiceGroupName, label = ServiceGroupName)) +
                    geom_treemap() +
                    geom_treemap_text(
                        colour = "white", place = "centre",
                        grow = TRUE
                    ) +
                    guides(fill = "none")
            } else {
                if (input$STATISTICS_Services_CRITERIA == "Mean_req") {
                    ggplot(Khatam_summary_Services(), aes(area = Mean_req, fill = ServiceGroupName, label = ServiceGroupName)) +
                        geom_treemap() +
                        geom_treemap_text(
                            colour = "white", place = "centre",
                            grow = TRUE
                        ) +
                        guides(fill = "none")
                } else {
                    if (input$STATISTICS_Services_CRITERIA == "Mean_pay") {
                        ggplot(Khatam_summary_Services(), aes(area = Mean_pay, fill = ServiceGroupName, label = ServiceGroupName)) +
                            geom_treemap() +
                            geom_treemap_text(
                                colour = "white", place = "centre",
                                grow = TRUE
                            ) +
                            guides(fill = "none")
                    } else {
                        ggplot(Khatam_summary_Services(), aes(area = N, fill = ServiceGroupName, label = ServiceGroupName)) +
                            geom_treemap() +
                            geom_treemap_text(
                                colour = "white", place = "centre",
                                grow = TRUE
                            ) +
                            guides(fill = "none")
                    }
                }
            }
        }
    )
    
    output$STATISTICS_SERVICE_DATA <- DT::renderDataTable(
        DT::datatable(
            {
                Khatam_summary_Services()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'tan', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    output$downloadData_STATISTICS_Services_DATA <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Khatam_summary_Services(), file)
        }
    )
    
    
    #------------------------------------------------------------------------------- 
    #-------------------------------------------------------------------------------  
    #------------------------------Fraud Detection---------------------------------- 
    #------------------------------------------------------------------------------- 
    #------------------------------------------------------------------------------- 
    
    #-------------------------------------------------------------------------------  
    #--------------------people who have the most damage---------------------------- 
    #------------------------------------------------------------------------------- 
    
    Fraud_tab1_Report_1 <- eventReactive(
        input$run_Fraud_tab1_1,
        KhatamDetail |>
            filter(EventDate >= input$TimeStart_Fraud_tab1_1 & EventDate <= input$TimeEnd_Fraud_tab1_1) |>
            group_by(NationalCode) |>
            summarise(
                SUM_req = sum(RequestAmount),
                SUM_pay = sum(PayableAmount),
                N = n()
            ) |>
            arrange(desc(input$L_or_P_Fraud_1)) |>
            slice_head( n = as.numeric( input$N_Fraud_1 ) )
    )
    
    
    output$Fraud_tab1_Data_report_1 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab1_Report_1()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'tan', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab1_1 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab1_Report_1(), file)
        }
    )
    
    #-------------------------------------------------------------------------------  
    Fraud_tab1_Report_2 <- eventReactive(
        input$run_Fraud_tab1_2,
        KhatamDetail |>
            filter(
                EventDate >= input$TimeStart_Fraud_tab1_2 & EventDate <= input$TimeEnd_Fraud_tab1_2,
                CoverageName %in% input$Coverage_Fraud_tab1_2
            ) |>
            group_by(NationalCode, CoverageName) |>
            summarise(
                SUM_req = sum(RequestAmount),
                SUM_pay = sum(PayableAmount),
                N = n()
            ) |>
            ungroup() |>
            arrange(desc(input$L_or_P_Fraud_2)) |> slice_head(n = as.numeric(input$N_Fraud_2)) 
    )
    
    output$Fraud_tab1_Data_report_2 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab1_Report_2()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'tan', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab1_2 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab1_Report_2(), file)
        }
    )
    
    #-------------------------------------------------------------------------------  
    Fraud_tab1_Report_3 <- eventReactive(
        input$run_Fraud_tab1_3,
        KhatamDetail |>
            filter(
                EventDate >= input$TimeStart_Fraud_tab1_3 & EventDate <= input$TimeEnd_Fraud_tab1_3,
                ServiceGroupName %in% input$Coverage_Fraud_tab1_3
            ) |>
            group_by(NationalCode, ServiceGroupName) |>
            summarise(
                SUM_req = sum(RequestAmount),
                SUM_pay = sum(PayableAmount),
                N = n()
            ) |>
            ungroup() |>
            arrange(desc(input$L_or_P_Fraud_3)) |> slice_head(n = as.numeric(input$N_Fraud_3)) 
    )
    
    output$Fraud_tab1_Data_report_3 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab1_Report_3()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'tan', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab1_3 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab1_Report_3(), file)
        }
    )
    
    #-------------------------------------------------------------------------------  
    #-----------------------Age and Cover contradiction----------------------------- 
    #------------------------------------------------------------------------------- 
    
    A2 <- eventReactive(
        input$run_Fraud_tab2_1,
        as.matrix(tibble("v1" = match(Age_Task, Fraud_Task_Age |> filter(V %in% input$Coverage_Fraud_tab2_1) |> select(V2) |> as.matrix() |> as.vector())) |>
                      mutate(v2 = ifelse(is.na(v1), v1, Age_Task)) |>
                      select(v2))[, 1]
    )
    
    
    Fraud_tab2_Report_1 <- eventReactive(
        input$run_Fraud_tab2_1,
        KhatamDetail |>
            filter((EventDate >= input$TimeStart_Fraud_tab2_1 & EventDate <= input$TimeEnd_Fraud_tab2_1) &
                       (CoverageName == A2()[1] & Age <= 2) |
                       (CoverageName == A2()[2] & Age <= 15) |
                       (CoverageName == A2()[3] & Age <= 1) |
                       (CoverageName == A2()[4] & Age <= 1) |
                       (CoverageName == A2()[5] & Age <= 18) |
                       (ServiceGroupName == A2()[6] & Age <= 10) |
                       (ServiceGroupName == A2()[7] & Age <= 15) |
                       (ServiceGroupName == A2()[8] & Age <= 20) |
                       (ServiceGroupName == A2()[9] & Age <= 15) |
                       (ServiceGroupName == A2()[10] & Age <= 15) |
                       (ServiceGroupName == A2()[11] & Age <= 15) |
                       (ServiceGroupName == A2()[12] & Age <= 15))
    )
    
    output$Fraud_tab2_Data_report_1 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab2_Report_1()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'violet', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab2_1 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab2_Report_1(), file)
        }
    )
    
    #-------------------------------------------------------------------------------
    
    
    
    
    A3 <- eventReactive(
        input$run_Fraud_tab2_2,
        as.matrix(tibble("v1" = match(Gender_Task, Fraud_Task_Gender |> filter(W %in% input$Coverage_Fraud_tab2_2) |> select(W2) |> as.matrix() |> as.vector())) |>
                      mutate(v2 = ifelse(is.na(v1), v1, Gender_Task)) |>
                      select(v2))[, 1]
    )
    
    Fraud_tab2_Report_2 <- eventReactive(
        input$run_Fraud_tab2_2,
        KhatamDetail |>
            filter((EventDate >= input$TimeStart_Fraud_tab2_2 & EventDate <= input$TimeEnd_Fraud_tab2_2) &
                       (CoverageName == A3()[1] & Gender == "Male") |
                       (ServiceGroupName == A3()[2] & Gender == "Male") |
                       (ServiceGroupName == A3()[3] & Gender == "Male") |
                       (ServiceGroupName == A3()[4] & Gender == "Female"))
    )
    
    output$Fraud_tab2_Data_report_2 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab2_Report_2()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'violet', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab2_2 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab2_Report_2(), file)
        }
    )
    
    
    
    #-------------------------------------------------------------------------------  
    #-----Unconventional hospitalization cost insured over a period of time--------- 
    #------------------------------------------------------------------------------- 
    
    SD_Coverage <- eventReactive(
        input$run_Fraud_tab4_1,
        KhatamDetail |>
            filter(CoverageName %in% input$Coverage_Fraud_tab4_1) |>
            group_by(CoverageName) |>
            summarise(SD = round(sd(PayableAmount)))
    )
    
    
    Fraud_tab4_Report_1 <- eventReactive(
        input$run_Fraud_tab4_1,
        KhatamDetail |>
            filter((EventDate >= input$TimeStart_Fraud_tab4_1 & EventDate <= input$TimeEnd_Fraud_tab4_1) &
                       (CoverageName %in% input$Coverage_Fraud_tab4_1)) |>
            group_by(NationalCode, CoverageName) |>
            summarise(SUM_PAY = sum(PayableAmount)) |>
            inner_join(SD_Coverage()) |>
            filter(SUM_PAY > input$SD_Fraud_1 * SD)
    )
    
    output$Fraud_tab4_Data_report_1 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab4_Report_1()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rosybrown', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab4_1 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab4_Report_1(), file)
        }
    )
    
    #-------------------------------------------------------------------------------  
    
    SD_Service <- eventReactive(
        input$run_Fraud_tab4_2,
        KhatamDetail |>
            filter(ServiceGroupName %in% input$Service_Fraud_tab4_2) |>
            group_by(ServiceGroupName) |>
            summarise(SD = round(sd(PayableAmount)))
    )
    
    Fraud_tab4_Report_2 <- eventReactive(
        input$run_Fraud_tab4_2,
        KhatamDetail |>
            filter((EventDate >= input$TimeStart_Fraud_tab4_2 & EventDate <= input$TimeEnd_Fraud_tab4_2) &
                       (ServiceGroupName %in% input$Service_Fraud_tab4_2)) |>
            group_by(NationalCode, ServiceGroupName) |>
            summarise(SUM_PAY = sum(PayableAmount)) |>
            inner_join(SD_Service()) |>
            filter(SUM_PAY > input$SD_Fraud_2 * SD)
    )
    
    output$Fraud_tab4_Data_report_2 <- DT::renderDataTable(
        DT::datatable(
            {
                Fraud_tab4_Report_2()
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'rosybrown', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$downloadDataFraud_tab4_2 <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Fraud_tab4_Report_2(), file)
        }
    )
   
}








