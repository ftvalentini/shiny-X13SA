
fluidPage(

  theme = shinytheme("cerulean"),
  
  navbarPage("X13-ARIMA by franvalent",
             
             tabPanel("Input",
                      
                      fluidRow(
                        column(7,
                               fileInput("archivo_base",
                                         label="Load original series",
                                         multiple = FALSE,
                                         accept = c(".xlsx",".xls",
                                                    ".csv"))
                        ),
                        column(1,
                               radioButtons("tipo", "Type",
                                            choices=list("Flow"="flow",
                                                         "Stock"="stock"),
                                            selected = "flow")
                               
                        ),
                        column(2,
                               radioButtons("tfreq", "Frequency",
                                            choices=list("Monthly"=12,
                                                         "Quarterly"=4),
                                            selected=12)
                               
                        )
                      ),
                      fluidRow(
                        column(7,
                               uiOutput("nombreSA")
                        )
                      ),
                      br(),
                      fluidRow(
                        column(6,
                               downloadButton("down", "Run & Download",
                                              icon=icon("fa-rebel"),
                                              style="background:
                                               linear-gradient(MintCream,LightBlue);
                                              border: 1px solid grey"
                                              )
                        )
                      ),
                      hr(),
                      conditionalPanel(
                        condition="input.nombreSA.length > 0",
                        strong(textOutput("calor"))
                      ),
                      br(),
                      conditionalPanel(
                        condition= "input.nombreSA.length > 0",
                        imageOutput("imagen",width="500px",height="303px")
                      )
                      
             ),
             
             tabPanel("Instructivo",
                      column(7,
                             p("1. Crear archivo xlsx, xls o csv con las series de 
                        tiempo en la primera hoja.
                        En la primera columna deben estar las fechas con formato fecha y
                        en la primera fila, los nombres de las series.",
                               align = "justify"),
                             p("2. Seleccionar el archivo con las series originales 
                        presionando",
                               span(strong("Browse"),"."),
                               align = "justify"),
                             p("3. Desestacionalizar y descargar
                        los resultados presionando",
                               span(strong("Run & Download"),"."),
                               align = "justify")
                             # ,
                             # hr(),
                             # p("Nota: descargar X13 desde ",
                             #   a("el sitio del US Census Bureau", 
                             #     href="https://www.census.gov/srd/www/winx13/winx13_down.html"),
                             #   "antes de utilizar la aplicación. El archivo ",
                             #   strong("x13as.exe")," debe estar ubicado en el
                             #   directorio C:/WinX13/x13as/ ",
                             #   align = "justify")
                      )
             )
  ))
  



# conditionalPanel(
#   condition="input.nombreSA.length > 0",
#   column(5,
#          h5(strong(textOutput("mensaje")))
#   )
# )


# column(2,
#        actionButton("Run", label = "Run",
#                     icon("rebel"),
#                     style="color:black;
#                                             background: linear-gradient(LightBlue,AliceBlue);
#                                             border: 2px solid black")
#        
# )
# 
# 
# column(4,
# uiOutput("folderSA"))
# 
# 
