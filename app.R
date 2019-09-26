#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(pROC)
logmodel = readRDS("Logistic2.rds" )
roc2 = readRDS(file = 'roc2.rds' )

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'purple',
    dashboardHeader(title='Loan Default Predictor', titleWidth = "400px"),
    dashboardSidebar( tags$style(".left-side, .main-sidebar {padding-top: 90px}"),
                      numericInput(inputId = 'loan_amnt',label = 'Loan Amount (USD)',value = 20000,width = '300px'),
                      selectInput(inputId = "term", label = "Loan Term",choices = c("36 months","60 months"),selected ='36 months',width = '300px'),
                      numericInput(inputId = 'int_rate',label = 'Loan Interest Rate (%)',value = 15.61,width = '300px'),
                      numericInput(inputId = 'installment',label = 'Monthly Installment Amount (USD)',value = 482.23,width = '300px'),
                      numericInput(inputId = 'inq_last_6mths', label = 'Number of Inquires in the last 6 Months',value=0,width = '300px'),
                      numericInput(inputId = 'out_prncp', label = 'Remaining Principal (USD)',value=13488.9,width = '300px'),
                      numericInput(inputId = 'total_pymnt',label = 'Paymnets till date (USD)',value = 12055.60,width = '300px'),
                      numericInput(inputId ='total_rec_int',label='Interests recieved to date (USD)',value = 5544.55,width = '300px'),
                      numericInput(inputId = 'last_pymnt_amnt',label = 'Last Payment Amount Recieved (USD)',value = 482.23,width = '300px'),
                      numericInput(inputId = 'score_days_interval',label = 'No. of days to credit pull after Loan ',value = 761,width = '300px'),
                      width = '300px'
                    ),
    dashboardBody( tags$head(tags$style(HTML('.skin-purple .main-header .logo {
                                                line-height:32px;
                                                font-weight: bold;
                                                font-size: 22px;
                                                text-align:left;
                                                padding-top: 15px
                                                }')
                                        )
                            ),
                   tags$style(".left-side, .main-sidebar {padding-top: 90px}
                               .content {min-height:1300px;}
                              "),
                   fluidPage(box(title='Description of the Loan Default Web Dashboard Application',solidHeader = TRUE,
                                 background = 'red',width = 12,collapsible = TRUE,collapsed = FALSE,
                                 textOutput("description"),
                                 tags$a(href="https://www.kaggle.com/wendykan/lending-club-loan-data","Link to the Lending Club Loan Dataset on Kaggle",
                                        target ="_blank",
                                        style = "color:yellow;
                                                text-decoration: underline"))),
                   fluidPage(tabBox(width = 12,height = '1200px',
                                   tabPanel(title = 'Model',
                                            fluidRow(box(title = "Probability of Paying Loan on time", solidHeader = TRUE,background = 'green',
                                                     valueBoxOutput("approvalBox",width = 6)
                                                     )
                                            ),
                                            fluidRow(box(title = 'Description of Explanatory Variables',solidHeader = TRUE,width = 8,
                                                         tableOutput('Variables'))
                                                     )
                                    ),
                                   tabPanel(title="Training Parameters",
                                            textOutput('explanation'),
                                            plotOutput(outputId = 'roc',width = '500px'),
                                            verbatimTextOutput('auc'),
                                            verbatimTextOutput('training')
                                            
                                            )
                                ))
        )
)
   
# Define server logic required to draw a histogramt
server <- function(input, output) {
    output$description <- renderText({'The following web application was developed from the loan dataset available on Kaggle.
                                       A simple logistic regression was trained and tested and is used for making the loan default predictions. 
                                       Enter the inputs in the Sidebar panel and the default rate % is displayed along with the glyph icon. Description of the 
                                       input parameters are shown in the table below.
                                       The trained model parameters are displayed in the training tab. The model is 90% accurate. For more
                                       details please get in touch with Arun Janakiraman'})
      
    pred_result <- reactive({
        #print(input$term)
        df = data.frame(loan_amnt = input$loan_amnt,
                        term = input$term,
                        int_rate = input$int_rate,
                        installment  = input$installment,
                        inq_last_6mths = input$inq_last_6mths,
                        out_prncp = input$out_prncp,
                        total_pymnt  = input$total_pymnt,
                        total_rec_int  = input$total_rec_int, 
                        last_pymnt_amnt = input$last_pymnt_amnt,
                        score_days_interval = input$score_days_interval)
        result <- predict(logmodel,newdata = df,type = 'response')
        #print(result)
        result
    })
    
    output$approvalBox <- renderValueBox({
        thumb = ifelse(pred_result()<0.5,'thumbs-down','thumbs-up')
        probly = paste(100*round(pred_result(),4),'%')
        default = ifelse(pred_result()<0.5,'Risk of Default','Loan Payments will be on time')
        valueBox(
            probly, default, icon = icon(thumb, lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$Variables <- renderTable({
        df = data.frame(Variables = c('Loan Amount (USD)',
                                      "Loan Term",
                                      'Loan Interest Rate (%)' ,
                                      'Monthly Installment Amount (USD)' ,
                                      'Number of Inquires in the last 6 Months',
                                      'Remaining Principal (USD)',
                                      'Paymnets till date (USD)' ,
                                      'Interests recieved to date (USD)', 
                                      'Last Payment Amount Recieved (USD)',
                                      'No. of days to credit pull after Loan'),
                        Explanation = c('The listed amount of the loan applied for by the borrower.',
                                        'The number of payments on the loan. Values are in months and can be either 36 or 60.',
                                        'Interest Rate on the loan',
                                        'The monthly payment owed by the borrower if the loan originates.',
                                        'The number of inquiries in past 6 months (excluding auto and mortgage inquiries)',
                                        'Remaining outstanding principal for total amount funded',
                                        'Payments received to date for total amount funded',
                                        'Interest payment amount received to date. This is an indication of how much interest amount 
                                        has been paid and with what regularity',
                                        'The amount of latest payment recieved',
                                        'Days elasped after issuance of loan to first credit inquiry. 
                                         This indicates whether the borrower had a credit inquiry due to some other loan.'
                                        )
                        )
    },striped = TRUE,hover = TRUE,bordered = TRUE)

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    output$explanation <- renderText({'The results of the training and testing of the machine learning
                                       model are displayed in this page. Since there is huge class imbalance between the good loans
                                       and bad ones, the area under the ROC curve displayed below has been used for model evaluation. As
                                       shown below the model is 90% accurate.'})
    output$auc <- renderPrint({print(roc2$auc)})
    output$roc <- renderPlot({
        plot(roc2)
    })
    output$training <- renderPrint({summary(logmodel)})
}

# Run the application 
shinyApp(ui = ui, server = server)
