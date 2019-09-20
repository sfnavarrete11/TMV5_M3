library("shiny")
library("shinythemes")
library("DT")
library("dplyr")
library("xlsx")
library("scales")
library("highcharter")
library("rpivotTable")
library("xts")

load(url("https://raw.githubusercontent.com/Crisben/RC-MCCTH/master/buses.rdata"))

ui <-  fluidPage(
    shinythemes::themeSelector(),
    fluidRow(column(2,
                    wellPanel(
                      img(src = "r.png", height = 200, width = 200),
                      h4("R Users Group - Dashboard reactivos", align = "center"),
                      h6("Registro consolidado", align = "center"),
                      br(),
                      
                      radioButtons("data_select", "Selecciones la información a visualizar:", choices = c("Original", "Ampliada")),
                      dateRangeInput('dateRange',
                                     label = 'Seleccione el período de análisis:',
                                     start =as.Date("2017-05-12"), end = Sys.Date(), min = "2017-05-12", max = Sys.Date(),language = "es"
                      ),
                      conditionalPanel(condition = "input.conditionedPanels_app == 'Información'",
                                       br(),
                                       selectInput("variable1", "Selecciones la variable a visualizar:", choices = c(
                                         "Cooperativa","Marca", "Sector", "Año de Fabricacion", "Modelo",
                                         "Pais de Origen", "Pasajeros",	"Tonelaje",	"Cilindraje",	"No. de Asientos",	"Tiene Publicidad"), selected = "Sector"),
                                       selectInput('columns', 'Detalle', ""),
                                       helpText("Se observa el detalle en la tabla inferior")
                      ),
                      conditionalPanel(
                        condition = "input.conditionedPanels_app=='Datos'",
                        downloadButton('downloadData', 'Descargar'), br()
                        )
                      ),
                    helpText("La información Original contiene registros únicamente del día 2017-05-12, por tanto si se aplica un filtro posterior NO SE MOSTRARÁN resultados."),
                    helpText("La información Ampliada contiene registros duplicados de la informaciòn original con fechas generadas automàticamente desde el día 2017-05-12 a la fecha actual, por tanto se puede aplicar un filtro posterior.")
                    ),
             column(10,
                    tabsetPanel(type = "tabs", 
                                tabPanel("Información",
                                         fluidRow(column(5,
                                                         highchartOutput("graf")
                                         ),
                                         column(3,
                                                shiny::tableOutput("resumen")
                                         ),
                                         column(4,
                                                highchartOutput("graf1")
                                                )
                                         ),
                                         div(DT::dataTableOutput("detalle"), style = "font-size:80%; color: black")
                                ),
                                tabPanel("Tabla dinámica",
                                         div(rpivotTableOutput("pivot"))
                                ),
                                tabPanel("Datos", 
                                         div(DT::dataTableOutput("data"), style = "font-size:80%; color: black")
                                )
                                ,
                                id="conditionedPanels_app"
                    )
             )
    )
  )
  

server = function(input, output,session) {
  descarga_ex <- function(datos, file){        
    wb <- createWorkbook(type="xlsx")
    
    # Define some cell styles
    # Title and sub title styles
    TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE)
    
    SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=12,
                                            isItalic=TRUE, isBold=FALSE)
    
    # Styles for the data table row/column names
    TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
    
    TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
      Alignment(vertical="VERTICAL_CENTER",wrapText=TRUE, horizontal="ALIGN_CENTER") +
      Border(color="black", position=c("TOP", "BOTTOM"), 
             pen=c("BORDER_THICK", "BORDER_THICK"))+Fill(foregroundColor = "lightblue", pattern = "SOLID_FOREGROUND")
    
    sheet <- createSheet(wb, sheetName = "Información aTM")
    
    # Helper function to add titles
    xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
      rows <- createRow(sheet, rowIndex=rowIndex)
      sheetTitle <- createCell(rows, colIndex=1)
      setCellValue(sheetTitle[[1,1]], title)
      setCellStyle(sheetTitle[[1,1]], titleStyle)
    }
    
    # Add title and sub title into a worksheet
    xlsx.addTitle(sheet, rowIndex=4, 
                  title=paste("Fecha:", format(Sys.Date(), format="%Y/%m/%d")),
                  titleStyle = SUB_TITLE_STYLE)
    
    xlsx.addTitle(sheet, rowIndex=5, 
                  title="Elaborado por: ATM",
                  titleStyle = SUB_TITLE_STYLE)
    
    # Add title
      xlsx.addTitle(sheet, rowIndex=7, 
                    paste("Información -", input$data_select),
                    titleStyle = TITLE_STYLE)
      
      # Add a table into a worksheet
      addDataFrame(datos,
                   sheet, startRow=9, startColumn=1,
                   colnamesStyle = TABLE_COLNAMES_STYLE,
                   rownamesStyle = TABLE_ROWNAMES_STYLE,
                   row.names = FALSE)

    # Change column width
    setColumnWidth(sheet, colIndex=c(1:ncol(datos)), colWidth=20)
    
    # image
    #addPicture("/www/r.png", sheet, scale=0.28, startRow = 1, startColumn = 1)
    
    # Save the workbook to a file...
    saveWorkbook(wb, file)
  }
  
  data_select <- reactive({
    if(input$data_select=="Original"){
      data
    } else {
      data <- rbind(data,data, data, data, data, data, data, data, data, data, data, data, data, data, data, data)
      data <- rbind(data,data, data, data, data, data, data, data, data, data, data, data, data, data, data, data)
      data$`Fecha de Ingreso` <- rep(seq(as.Date("2017-05-12"), Sys.Date(), "days"), 1000)[c(1:nrow(data))]
      data
    }
    data$`Fecha de Ingreso` <- as.Date(data$`Fecha de Ingreso`)
    data %>% filter(`Fecha de Ingreso` >= input$dateRange[1] & `Fecha de Ingreso` <= input$dateRange[2])
  })
  
  output$dateRangeText  <- renderText({
    paste("Período seleccionado: ", 
          paste(as.character(input$dateRange), collapse = " - ")
    )
  })
  
  resumen <- reactive({
    data <- data_select()
    data <- data %>% group_by(data[,grep(input$variable1, colnames(data))]) %>% summarize(N=n())
    data <- as.data.frame(data)
    colnames(data) <- c("variable", "N")
    data
  })
  
  output$graf <- renderHighchart({
    hchart(resumen(), "pie", hcaes(x=variable , y = N))
  })
  
  output$resumen <- shiny::renderTable({
    data <- resumen()
    data$Porcentaje <- data[2]/colSums(data[2])
    colnames(data) <- c(input$variable1, "Número", "Porcentaje")
    data
  })
  
  
  output$data1 <- renderPrint({
    data <- data_select()
    summary(data[,grep(input$variable1, colnames(data))])
  })
  
  output$graf1 <- renderHighchart({
    hchart(resumen(), "bar", hcaes(x=variable , y = N))
  })                
  
  
  output$data <- DT::renderDataTable({
    data <- data_select()
    datatable(data, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  output$pivot <- renderRpivotTable({
    data <- data_select()
    rpivotTable(data[, c(1:30)],
                rows=c("Sector"),
                col="Marca", 
                aggregatorName="Average", 
                vals="Pasajeros", 
                rendererName="Horizontal Stacked Bar Chart")
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("Información ATM.xlsx") },
    content = function(file) {
      descarga_ex(data_select(), file)}
  )
  
  
  outVar = reactive({
    resumen()[1]
  })
  
  observe({
    updateSelectInput(session, "columns",
                      choices = outVar()
    )})
  
  
  columns<-reactive({as.character(input$columns)})
  
  output$detalle <- renderDataTable({
    x <- grep(input$variable1, colnames(data_select()))
    data_select()[which(data_select()[, x] %in% columns()),]
  })
  
}

shinyApp(ui = ui, server = server)
