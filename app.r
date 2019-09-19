library(shiny)
library(DT)
ui <- fluidPage(
    titlePanel("Old Faithful Geyser Data"),
    fluidRow(
        column(3,
               # sidebarLayout(
               #     sidebarPanel(
               conditionalPanel(condition = 'input.conditionedPanels=="Texto"',
                                textInput("texto", "Ingrese un texto")
               ),
               selectInput("df", "Selecccione el df", choices = c("mtcars", "iris"),selected = "mtcars"),
               numericInput("n", "Ingrese el número de registros",value = 5, min = 2, max = 100),
               
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30)
               #),
        ),
        column(7,
               # mainPanel(
               tabsetPanel(
                   tabPanel("Texto",
                            # conditionalPanel(condition = '!is.na(as.numeric(input.texto))',textOutput("texto_salida")
                            textOutput("texto_salida"),
                            verbatimTextOutput("nchar")
                   ),
                   tabPanel("Tabla",
                            div(DTOutput("tabla"),
                                style = "font-size:80%")
                   ),
                   tabPanel("Documento", 
                            htmlOutput('documento')
                            ),
                   tabPanel("Histograma",
                            plotOutput("distPlot")
                   ),id = "conditionedPanels" 
               )
        ),
        column(2,
               h3("h3"),
               br(),
               h4("h4"),
               h5("h5"),br(),
               helpText("texto ayuda"),
               #img(src = "r.png", height = 180, width = 185),
               HTML('<center><img src="r.png" width="185"></center>'),
               h5("APP SHINY - DEMO", align = "center")
               )
        # )
    )
)

server <- function(input, output) {
    output$texto_salida <- renderText({
        paste("El texto ingresado es", input$texto)
    })
    
    output$nchar <- renderText(nchar(input$texto))
    
    output$tabla <- renderDT({
        tmp <- if(input$df=="mtcars") {
            mtcars
        } else {iris}
        tmp[1:input$n,]
    },
    options = list(lengthChange=TRUE, 
                   scrollY = 360,
                   scroller = TRUE))
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$documento <- renderText({
        return(paste0('<iframe style="height:600px; width:100%" src="',
                     "Manual.pdf", '"></iframe>'))
        
        #return('<iframe style="height:600px; width:100%" src="Manual.pdf"></iframe>')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)












