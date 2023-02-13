#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(data.table)
library(pdftools)

dt <- fread("dishes.csv")
dt[, id := 1:.N]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("What to have next week."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            pickerInput(inputId = "PickDays",
                        label = "Select days you want a fixed menu",
                        choices = c("Monday", "Tuesday", "Wednesday", 
                                    "Thursday", "Friday", "Saturday",
                                    "Sunday"),
                        multiple = TRUE,
                        options = pickerOptions(
                            actionsBox = TRUE,
                            title = "Please select all the days you want."
                        )),
            actionBttn( inputId = "action",
                        label = "Generate Menu!!!", 
                        style = "gradient",
                        color = "warning",
                        icon = icon("thumbs-up")),
            p(""),
            downloadButton("downloadPDF", "Download as PDF")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
            dataTableOutput("disTable"),
            dataTableOutput("shopping_list")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    inputdays <- eventReactive(input$action,{
        days <- input$PickDays
        days
    })
    # Meal suggestion generator 
    generate_meal <- reactive({
      no.ofdays <- length(inputdays())
      output <- dt[sample(id,size = no.ofdays, replace = TRUE)]
      output$Day <- inputdays()
      output[,.(Day, breaky, lunch, dinner_main, dinner_side, dinner_soup)]
      
    })
    
    # Shopping list generator
    generate_shopping_list <- reactive({
      meal_plan <- generate_meal()
      shopping_list <- c(as.character(meal_plan$breaky), 
                         as.character(meal_plan$lunch), 
                         as.character(meal_plan$dinner))
      shopping_list <- as.data.frame(table(shopping_list))
      shopping_list <- shopping_list[order(-shopping_list$Freq),]
      colnames(shopping_list) <- c("Item", "Quantity")
      shopping_list
    })
    # output the meal table 
    output$disTable <- renderDataTable({
      generate_meal()
    })
    output$shopping_list <- renderDataTable({
      generate_shopping_list()
    })
    # PDF export
    output$downloadPDF <- downloadHandler(
      filename = function() {
        paste("Breakfast_and_Lunch_Plan_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        pdf("Breakfast_and_Lunch_Plan.pdf", height = 11, width = 8.5)
        print(renderTable({generate_meal()}))
        print(renderTable({generate_shopping_list()}))
        dev.off()
        file.rename("Breakfast_and_Lunch_Plan.pdf", file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
