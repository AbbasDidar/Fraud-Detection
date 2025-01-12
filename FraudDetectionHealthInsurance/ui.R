
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
library(shinymanager)
# library(bslib)


DataDetail <- read_excel("Data-Test.xlsx")

Cover_Service <- read_excel("Cover_Service.xlsx")
COVERAGE <- sort(unique(Cover_Service$CoverageName))
SERVICE <- sort(unique(Cover_Service$ServiceGroupName))

Age_Task <- c(COVERAGE[12:16], SERVICE[c(14, 130, 40, 5, 179, 4, 10)])
Gender_Task <- c(COVERAGE[13], SERVICE[c(64, 167, 113)])

Fraud_Task_Age <- read_excel("Fraud_Task.xlsx",
                             sheet = "Age_Cover"
)
Fraud_Task_Gender <- read_excel("Fraud_Task.xlsx",
                                sheet = "Gender_Cover"
)



button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: Blue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"




inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"




# Define UI
ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(
                     
                     
                     
                     navbarPage("",
                                inverse = TRUE,
                                # theme = bs_theme(bootswatch = "minty"),
                                theme = shinytheme("cerulean"),
                                
                                
                                tabPanel("Data Extraction",
                                         fluid = TRUE, icon = icon("chart-bar"),
                                         tags$style(button_color_css),
                                         sidebarLayout(
                                             sidebarPanel(
                                                 titlePanel(""),
                                                 shinythemes::themeSelector(),
                                                 fluidRow(
                                                     column(
                                                         5,
                                                         style = "font-family: B Mitra;font-size: 20px;font-weight: bold",
                                                         h4("Remittance Type", style = "color:mediumvioletred"),
                                                         checkboxGroupInput(
                                                             inputId = "RemittanceType",
                                                             label = "",
                                                             choices = unique(DataDetail$RemittanceType),
                                                             selected = unique(DataDetail$RemittanceType)[1]
                                                         )
                                                     ),
                                                     column(4,
                                                            offset = 2,
                                                            style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                            h4("Deduction Cause", style = "color:mediumvioletred"),
                                                            selectInput(
                                                                inputId = "elatkosor",
                                                                label = "",
                                                                choices = unique(DataDetail$Deduction_Cause),
                                                                selected = unique(DataDetail$Deduction_Cause)[4],
                                                                multiple = TRUE
                                                            )
                                                     )
                                                 ),
                                                 hr(),
                                                 fluidRow(column(
                                                     12,
                                                     h4("Age", style = "color:mediumvioletred"),
                                                     sliderInput(
                                                         inputId = "Age",
                                                         label = "",
                                                         min = min(DataDetail$Age), max = max(DataDetail$Age),
                                                         value = c(10, 55), step = 1
                                                     )
                                                 )),
                                                 hr(),
                                                 fluidRow(
                                                     column(12,
                                                            style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                            h4("Coverage", style = "color:mediumvioletred"),
                                                            selectInput(
                                                                inputId = "Coverage",
                                                                label = "",
                                                                choices = unique(DataDetail$CoverageName),
                                                                selected = unique(DataDetail$CoverageName)[1],
                                                                multiple = TRUE,
                                                                width = "420px"
                                                            )
                                                     )
                                                 ),
                                                 hr(),
                                                 fluidRow(
                                                     column(12,
                                                            style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                            h4("City", style = "color:mediumvioletred"),
                                                            selectInput(
                                                                inputId = "City",
                                                                label = "",
                                                                choices = unique(DataDetail$City),
                                                                selected = unique(DataDetail$City)[1],
                                                                multiple = TRUE,
                                                                width = "420px"
                                                            )
                                                     )
                                                 ),
                                                 hr(),
                                                 h4("Event Date", style = "color:mediumvioletred"),
                                                 # Set Time Range
                                                 fluidRow(
                                                     column(
                                                         5,
                                                         textInput(
                                                             inputId = "TimeStart",
                                                             label = "From:",
                                                             value = min(DataDetail$EventDate),
                                                             width = "150px"
                                                         )
                                                     ),
                                                     column(5,
                                                            ofset = 3,
                                                            textInput(
                                                                inputId = "TimeEnd",
                                                                label = "To:",
                                                                value = max(DataDetail$EventDate),
                                                                width = "150px"
                                                            )
                                                     )
                                                 ),
                                                 helpText("Format example: 1399/12/11 to 1400/02/07 "),
                                                 hr(),
                                                 h4("Request Amount", style = "color:mediumvioletred"),
                                                 fluidRow(
                                                     column(
                                                         5,
                                                         textInput(
                                                             inputId = "MinRequestAmount",
                                                             label = "Min(Million Rial):",
                                                             value = 0,
                                                             width = "300px"
                                                         )
                                                     ),
                                                     column(5,
                                                            ofset = 3,
                                                            textInput(
                                                                inputId = "MaxRequestAmount",
                                                                label = "Max(Million Rial):",
                                                                value = round(max(DataDetail$RequestAmount) / 10^3),
                                                                width = "300px"
                                                            )
                                                     )
                                                 ),
                                                 hr(),
                                                 h4("Which Columns do you want?", style = "color:mediumvioletred"),
                                                 selectInput(
                                                     inputId = "COLUMNS",
                                                     label = "",
                                                     choices = names(DataDetail),
                                                     selected = names(DataDetail),
                                                     multiple = TRUE,
                                                     width = "420px"
                                                 ),
                                                 actionButton("run", "apply", class = "btn-info")
                                             ),
                                             mainPanel(
                                                 
                                                 
                                                 column(dataTableOutput(outputId = "Statistics_report"),
                                                        width = 12,
                                                        style = "font-family: B Mitra;font-size: 15px;font-weight: bold"
                                                 )
                                             )
                                         ),
                                         sidebarLayout(
                                             sidebarPanel(
                                                 titlePanel(""),
                                                 h2("Only filter by Code Meli", style = "color:steelblue"),
                                                 h4("Code Meli", style = "color:mediumvioletred"),
                                                 textInput(
                                                     inputId = "CodeMeli",
                                                     label = "",
                                                     value = "",
                                                     width = "250px"
                                                 ),
                                                 hr(),
                                                 h4("Code Meli Asli", style = "color:mediumvioletred"),
                                                 textInput(
                                                     inputId = "CodeMeliAsli",
                                                     label = "",
                                                     value = "",
                                                     width = "250px"
                                                 )
                                             ),
                                             mainPanel(
                                                 
                                                 hr(),
                                                 withSpinner( dataTableOutput(outputId = "Statistics_report_CodeMeli"))
                                             )
                                         ),
                                         hr(),
                                         p(em("Developed by"), br(),
                                           a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                                           style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
                                         )
                                ),
                                
                                #-------------------------------------------------------------------------------  
                                #------------------------------------------------------------------------------- 
                                #------------------------------------------------------------------------------- 
                                
                                
                                tabPanel("Statistics",
                                         fluid = TRUE, icon = icon("chart-pie"),
                                         tags$style(button_color_css),
                                         sidebarLayout(
                                             sidebarPanel(
                                                 # titlePanel("Desired Program Characteristics"),
                                                 shinythemes::themeSelector(),
                                                 h4("Choosing the covers for which you want the criteria be calculated", style = "color:seagreen"),
                                                 fluidRow(
                                                     column(
                                                         5,
                                                         textInput(
                                                             inputId = "STATISTIC_COVERAGE_TimeStart",
                                                             label = "From:",
                                                             value = min(DataDetail$EventDate),
                                                             width = "150px"
                                                         )
                                                     ),
                                                     column(5,
                                                            ofset = 3,
                                                            textInput(
                                                                inputId = "STATISTIC_COVERAGE_TimeEnd",
                                                                label = "To:",
                                                                value = max(DataDetail$EventDate),
                                                                width = "150px"
                                                            )
                                                     )
                                                 ),
                                                 column( 12,
                                                         style = "font-family: B Mitra;font-size: 20px;font-weight: bold",
                                                         
                                                         h4("Coverage", style = "color:seagreen"),
                                                         checkboxGroupInput(
                                                             inputId = "STATISTIC_COVERAGE",
                                                             label = "",
                                                             choices = unique(DataDetail$CoverageName),
                                                             selected = unique(DataDetail$CoverageName),
                                                             inline = FALSE
                                                         )
                                                 ),
                                                 
                                                 h4("Statistics Plot", style = "color:seagreen"),
                                                 selectInput(
                                                     inputId = "STATISTICS_Coverage_CRITERIA",
                                                     label = "",
                                                     choices = c(
                                                         "sum request amount" = "SUM_req",
                                                         "sum payable amount" = "SUM_Pay",
                                                         "mean request amount" = "Mean_req",
                                                         "mean payable amount" = "Mean_pay",
                                                         "count" = "N"
                                                     )
                                                 ),
                                                 actionButton("RUN_STATISTICS_COVERAGE", "apply", class = "btn-primary btn-lg")
                                             ),
                                             mainPanel(
                                                 withSpinner(plotOutput(outputId = "STATISTICS_Coverage_plot")),
                                                 downloadButton("downloadData_STATISTICS_COVERAGE_DATA", "Download"),
                                                 hr(),
                                                 column( 12,
                                                         style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                         withSpinner(dataTableOutput(outputId = "STATISTICS_COVERAGE_DATA")) )
                                             )
                                         ),
                                         sidebarLayout(
                                             sidebarPanel(
                                                 h4("Choosing the Services for which you want the criteria be calculated", style = "color:red"),
                                                 fluidRow(
                                                     column(
                                                         5,
                                                         textInput(
                                                             inputId = "STATISTIC_Services_TimeStart",
                                                             label = "From:",
                                                             value = min(DataDetail$EventDate),
                                                             width = "150px"
                                                         )
                                                     ),
                                                     column(5,
                                                            ofset = 3,
                                                            textInput(
                                                                inputId = "STATISTIC_Services_TimeEnd",
                                                                label = "To:",
                                                                value = max(DataDetail$EventDate),
                                                                width = "150px"
                                                            )
                                                     )
                                                 ),
                                                 column(12,
                                                        style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                        
                                                        h4("Service", style = "color:red"),
                                                        selectInput(
                                                            inputId = "STATISTICS_Service",
                                                            label = "",
                                                            choices = c("All Services", unique(DataDetail$ServiceGroupName)),
                                                            selected = c("All Services", unique(DataDetail$ServiceGroupName))[1],
                                                            multiple = TRUE
                                                        )
                                                        
                                                 ),
                                                 
                                                 h4("Statistics Plot", style = "color:red"),
                                                 selectInput(
                                                     inputId = "STATISTICS_Services_CRITERIA",
                                                     label = "",
                                                     choices = c(
                                                         "sum request amount" = "SUM_req",
                                                         "sum payable amount" = "SUM_Pay",
                                                         "mean request amount" = "Mean_req",
                                                         "mean payable amount" = "Mean_pay",
                                                         "count" = "N"
                                                     )
                                                 ),
                                                 actionButton("RUN_STATISTICS_Services", "apply", class = "btn-primary btn-lg")
                                             ),
                                             mainPanel(
                                                 withSpinner( plotOutput(outputId = "STATISTICS_Services_plot") ),
                                                 downloadButton("downloadData_STATISTICS_Services_DATA", "Download"),
                                                 hr(),
                                                 fluidRow( 
                                                     
                                                     
                                                     column( withSpinner( dataTableOutput(outputId = "STATISTICS_SERVICE_DATA") ),
                                                             width = 12,
                                                             style = "font-family: B Mitra;font-size: 15px;font-weight: bold") 
                                                     
                                                     
                                                 )
                                             )
                                         )
                                ),
                                
                                
                                #-------------------------------------------------------------------------------  
                                #------------------------------------------------------------------------------- 
                                #------------------------------------------------------------------------------- 
                                
                                navbarMenu("Fraud Detection",
                                           icon = icon("swimmer"),
                                           tabPanel("people who have the most damage",
                                                    fluid = TRUE, icon = icon("area-chart", verify_fa = FALSE),
                                                    shinythemes::themeSelector(),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            
                                                            # titlePanel("The number of losses in a certain period of time"),
                                                            h3("The number of losses in a certain period of time", style = "color:midnightblue"),
                                                            hr(),
                                                            shinythemes::themeSelector(),
                                                            h4("People who have the most loss", style = "color:darkred"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    3,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab1_1",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "150px"
                                                                    )
                                                                ),
                                                                column(3,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab1_1",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(3,
                                                                       ofset = 3,
                                                                       textInput(
                                                                           inputId = "N_Fraud_1",
                                                                           label = "Number of loss:",
                                                                           value = 50,
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(3,
                                                                       ofset = 3,
                                                                       selectInput(
                                                                           inputId = "L_or_P_Fraud_1",
                                                                           label = "Num or Severity",
                                                                           choices = c("Number" = "N", "Severity" = "SUM_req"),
                                                                           selected = "Number",
                                                                           width = "420px"
                                                                       )
                                                                )
                                                            ),
                                                            actionButton("run_Fraud_tab1_1", "apply", class = "btn-info")
                                                        ),
                                                        mainPanel(
                                                            h2("Here the results are displayed based on the Desired Program Characteristics", style = "color:mediumvioletred"),
                                                            hr(),
                                                            downloadButton("downloadDataFraud_tab1_1", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab1_Data_report_1") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold") 
                                                        )
                                                    ),
                                                    #-------------------------------------------------------------------------------       
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            h4("People who have the most loss in special coverages", style = "color:darkred"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    4,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab1_2",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "150px"
                                                                    )
                                                                ),
                                                                column(4,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab1_2",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(4,
                                                                       ofset = 3,
                                                                       textInput(
                                                                           inputId = "N_Fraud_2",
                                                                           label = "Number of loss:",
                                                                           value = 50,
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(
                                                                    8,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                                    selectInput(
                                                                        inputId = "Coverage_Fraud_tab1_2",
                                                                        label = "",
                                                                        choices = unique(DataDetail$CoverageName),
                                                                        selected = unique(DataDetail$CoverageName)[1],
                                                                        multiple = TRUE,
                                                                        width = "420px"
                                                                    )
                                                                ),
                                                                column(
                                                                    4,
                                                                    selectInput(
                                                                        inputId = "L_or_P_Fraud_2",
                                                                        label = "Num or Severity",
                                                                        choices = c("Number" = "N", "Severity" = "SUM_req"),
                                                                        selected = "Number",
                                                                        width = "420px"
                                                                    )
                                                                )
                                                            ),
                                                            actionButton("run_Fraud_tab1_2", "apply", class = "btn-info")
                                                        ),
                                                        mainPanel(
                                                            hr(),
                                                            downloadButton("downloadDataFraud_tab1_2", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab1_Data_report_2") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold") 
                                                        )
                                                    ),
                                                    #-------------------------------------------------------------------------------       
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            h4("People who have the most loss in special services", style = "color:darkred"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    4,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab1_3",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "150px"
                                                                    )
                                                                ),
                                                                column(4,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab1_3",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(4,
                                                                       ofset = 3,
                                                                       textInput(
                                                                           inputId = "N_Fraud_3",
                                                                           label = "Number of loss:",
                                                                           value = 50,
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(
                                                                    8,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                                                                    selectInput(
                                                                        inputId = "Coverage_Fraud_tab1_3",
                                                                        label = "",
                                                                        choices = unique(DataDetail$ServiceGroupName),
                                                                        selected = unique(DataDetail$ServiceGroupName)[1],
                                                                        multiple = TRUE,
                                                                        width = "420px"
                                                                    )
                                                                ),
                                                                column(
                                                                    4,
                                                                    selectInput(
                                                                        inputId = "L_or_P_Fraud_3",
                                                                        label = "Num or Severity",
                                                                        choices = c("Number" = "N", "Severity" = "SUM_req"),
                                                                        selected = "Number",
                                                                        width = "420px"
                                                                    )
                                                                )
                                                            ),
                                                            actionButton("run_Fraud_tab1_3", "apply", class = "btn-info")
                                                        ),
                                                        mainPanel(
                                                            hr(),
                                                            downloadButton("downloadDataFraud_tab1_3", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab1_Data_report_3") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold") 
                                                        )
                                                    ),
                                                    hr(),
                                                    p(em("Developed by"), br(),
                                                      a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                                                      style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
                                                    )
                                           ),
                                           tabPanel("Age | Gender and Cover contradiction",
                                                    fluid = TRUE, icon = icon("stopwatch"),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            h4("Age and Cover contradiction", style = "color:mediumvioletred"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    6,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab2_1",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "150px"
                                                                    )
                                                                ),
                                                                column(6,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab2_1",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(
                                                                    12,
                                                                    style = "font-family: B Mitra;font-size: 20px;font-weight: bold",
                                                                    checkboxGroupInput(
                                                                        inputId = "Coverage_Fraud_tab2_1",
                                                                        label = "",
                                                                        choices = Fraud_Task_Age$V,
                                                                        selected = Fraud_Task_Age$V[1],
                                                                        inline = FALSE,
                                                                        width = "420px"
                                                                    )
                                                                ),
                                                                actionButton("run_Fraud_tab2_1", "apply", class = "btn-info")
                                                            )
                                                        ),
                                                        mainPanel(
                                                            hr(),
                                                            downloadButton("downloadDataFraud_tab2_1", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab2_Data_report_1") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold") 
                                                        )
                                                    ),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            h4("Gender and Cover contradiction", style = "color:mediumvioletred"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    6,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab2_2",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "150px"
                                                                    )
                                                                ),
                                                                column(6,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab2_2",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "150px"
                                                                       )
                                                                ),
                                                                column(
                                                                    12,
                                                                    style = "font-family: B Mitra;font-size: 20px;font-weight: bold",
                                                                    checkboxGroupInput(
                                                                        inputId = "Coverage_Fraud_tab2_2",
                                                                        label = "",
                                                                        choices = Fraud_Task_Gender$W,
                                                                        selected = Fraud_Task_Gender$W[1],
                                                                        inline = FALSE,
                                                                        width = "420px"
                                                                    )
                                                                ),
                                                                actionButton("run_Fraud_tab2_2", "apply", class = "btn-info")
                                                            )
                                                        ),
                                                        mainPanel(
                                                            hr(),
                                                            downloadButton("downloadDataFraud_tab2_2", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab2_Data_report_2") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold")
                                                        )
                                                    ),
                                                    hr(),
                                                    p(em("Developed by"), br(),
                                                      a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                                                      style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
                                                    )
                                           ),
                                           tabPanel("Unconventional hospitalization cost insured over a period of time",
                                                    fluid = TRUE, icon = icon("exclamation-triangle", verify_fa = FALSE),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            h4("Unconventional hospitalization cost by coverage", style = "color:deeppink"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    6,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab4_1",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "350px"
                                                                    )
                                                                ),
                                                                column(6,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab4_1",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "350px"
                                                                       )
                                                                ),
                                                                column(
                                                                    12,
                                                                    sliderInput(
                                                                        inputId = "SD_Fraud_1",
                                                                        label = "SD:",
                                                                        min = .5, max = 10,
                                                                        step = .5,
                                                                        value = 3
                                                                    )
                                                                ),
                                                                column(
                                                                    12,
                                                                    style = "font-family: B Mitra;font-size: 20px;font-weight: bold",
                                                                    selectInput(
                                                                        inputId = "Coverage_Fraud_tab4_1",
                                                                        label = "Coverage",
                                                                        choices = unique(DataDetail$CoverageName),
                                                                        selected = unique(DataDetail$CoverageName)[1],
                                                                        multiple = TRUE,
                                                                        width = "420px"
                                                                    )
                                                                )
                                                            ),
                                                            actionButton("run_Fraud_tab4_1", "apply", class = "btn-info")
                                                        ),
                                                        mainPanel(
                                                            
                                                            downloadButton("downloadDataFraud_tab4_1", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab4_Data_report_1") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold")
                                                        )
                                                    ),
                                                    #* ------------------------------------------------------------------------------
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            h4("Unconventional hospitalization cost by Service", style = "color:deeppink"),
                                                            br(),
                                                            fluidRow(
                                                                column(
                                                                    6,
                                                                    textInput(
                                                                        inputId = "TimeStart_Fraud_tab4_2",
                                                                        label = "From Date:",
                                                                        value = min((DataDetail$EventDate)),
                                                                        width = "350px"
                                                                    )
                                                                ),
                                                                column(6,
                                                                       ofset = 2,
                                                                       textInput(
                                                                           inputId = "TimeEnd_Fraud_tab4_2",
                                                                           label = "To Date:",
                                                                           value = max(DataDetail$EventDate),
                                                                           width = "350px"
                                                                       )
                                                                ),
                                                                column(
                                                                    12,
                                                                    sliderInput(
                                                                        inputId = "SD_Fraud_2",
                                                                        label = "SD:",
                                                                        min = .5, max = 10,
                                                                        step = .5,
                                                                        value = 3
                                                                    )
                                                                ),
                                                                column(
                                                                    12,
                                                                    style = "font-family: B Mitra;font-size: 20px;font-weight: bold",
                                                                    selectInput(
                                                                        inputId = "Service_Fraud_tab4_2",
                                                                        label = "Service",
                                                                        choices = unique(DataDetail$ServiceGroupName),
                                                                        selected = unique(DataDetail$ServiceGroupName)[1],
                                                                        multiple = TRUE,
                                                                        width = "420px"
                                                                    )
                                                                )
                                                            ),
                                                            actionButton("run_Fraud_tab4_2", "apply", class = "btn-info")
                                                        ),
                                                        mainPanel(
                                                            
                                                            downloadButton("downloadDataFraud_tab4_2", "Download"),
                                                            hr(),
                                                            column( withSpinner( dataTableOutput(outputId = "Fraud_tab4_Data_report_2") ),
                                                                    width = 12,
                                                                    style = "font-family: B Mitra;font-size: 15px;font-weight: bold")
                                                        )
                                                    ),
                                                    hr(),
                                                    p(em("Developed by"), br(),
                                                      a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                                                      style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
                                                    )
                                           )
                                ),
           
                                
                                #-------------------------------------------------------------------------------
                                #-------------------------------------------------------------------------------
                                #-------------------------------------------------------------------------------
                                
                                tabPanel("Other Apps",
                                         fluid = TRUE, icon = icon("tachometer", verify_fa = FALSE),
                                         tags$style(button_color_css),
                                         br(),
                                         br(),
                                         br(),
                                         fluidRow(
                                             column(
                                                 width = 4,
                                                 p("(ط¯ط§ط´ط¨ظˆط±ط¯ ظ…ط¯ظ„â€Œظ‡ط§غŒ غŒط§ط¯ع¯غŒط±غŒ ظ…ط§ط´غŒظ†(ظ‡ظˆط´ ظ…طµظ†ظˆط¹غŒ", br(),
                                                   actionButton(
                                                       inputId = "ab1", label = "ع©ظ„غŒع© ع©ظ†غŒط¯",
                                                       icon = icon("brain"),
                                                       onclick = "window.open('https://budgetrealizationshinyapps.io/Model_Predictive/', '_blank')"
                                                   ),
                                                   style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
                                                 )
                                             ),
                                             column(
                                                 width = 4,
                                                 p("ط¯ط§ط´ط¨ظˆط±ط¯ طھط­ظ‚ظ‚ ط¨ظˆط¯ط¬ظ‡", br(),
                                                   actionButton(
                                                       inputId = "ab1", label = "ع©ظ„غŒع© ع©ظ†غŒط¯",
                                                       icon = icon("coins"),
                                                       onclick = "window.open('https://gitypardazesh.shinyapps.io/Gity_Budget_Dashboard/', '_blank')"
                                                   ),
                                                   style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:thistle;padding:15px;border-radius:10px"
                                                 )
                                             ),
                                             column(
                                                 width = 4,
                                                 p("ط¯ط§ط´ط¨ظˆط±ط¯ ظ…ط­ط§ط³ط¨ظ‡ ط­ظ‚ ط¨غŒظ…ظ‡ ط¯ط±ظ…ط§ظ† ط³ط§ط²ظ…ط§ظ† ط®ط¯ظ…ط§طھغŒ", br(),
                                                   actionButton(
                                                       inputId = "ab1", label = "ع©ظ„غŒع© ع©ظ†غŒط¯",
                                                       icon = icon("hospital-user"),
                                                       onclick = "window.open('https://abbasdidar5017.shinyapps.io/RATING_DASHBOARD3/', '_blank')"
                                                   ),
                                                   style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
                                                 )
                                             )
                                         ),
                                         hr(),
                                         p(em("Developed by"), br(),
                                           a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                                           style = "text-align:center; color:turquoise; font-size: 20px; font-family: times"
                                         )
                                )
                     )
                 )
                 
)
