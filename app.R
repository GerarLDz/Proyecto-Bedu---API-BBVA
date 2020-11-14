
# Cargamos librerias 
library(reticulate)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleway)
library(dplyr)
library(plyr) 
library(ggplot2)




# Cargamos el script de Pyton donde se realiza el orden de los datos
import('pandas') 
py_run_file("Zip_gasto.py")
source_python("datos_dia.py")

library(shinydashboard)

ui <- dashboardPage(

  dashboardHeader(title = "Mercado Madrid"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa de Calor", tabName = "Mapas", icon = icon("map-marked-alt")),
      menuItem("Gráficas", tabName = "Gráficas", icon = icon("chart-line"))
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Mapas",
              fluidRow(
                    google_mapOutput("map")

              ),
              
              fluidRow(
                       box( 
                         sliderInput(inputId = "sl_fecha", "Seleccione Dia:", min = 1, 
                                     max = 31, value = 1),

                         selectInput(inputId = "Sel_Mes", label = "Mes:", 
                                     choices = c('Octubre','Noviembre'), 
                                     selected = 1, multiple = FALSE,  width = NULL, size = NULL)
                       ),
                       
                       box( 
                         sliderInput(inputId = "gasto", "Resolución de Gasto", min = 1, 
                                     max = 1500, value = c(50, 500)),
                         selectInput(inputId = "Sel_Categoria", label = "Categoría:", 
                                     choices = as.vector(py$df_zip_cat), 
                                     selected = 3, multiple = FALSE,  width = NULL, size = NULL)
                       ),
              fluidRow(
                verbatimTextOutput("Text1")
              )
                       
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Gráficas",
#              fluidRow(
#                column(width = 3,offset = 2, titlePanel("Pago promedio por día")) 
#              ),
              
              fluidRow(
                column(width = 12,
                  box(width=12,
                      title = "Monto de Pagos por día", background = "black",
                      plotOutput("line_graf")
                  )
                )
              ),
              fluidRow(
                  box(selectInput(inputId = "Sel_Mes2", label = "Mes:", 
                                  choices = c("Octubre", "Noviembre"), 
                                  selected = 3, multiple = FALSE,  width = NULL, size = NULL),
                      
                      selectInput(inputId = "Sel_CategoriaPl", label = "Categoría:", 
                              choices = as.vector(py$df_zip_cat), 
                              selected = 3, multiple = FALSE,  width = NULL, size = NULL)
                      
                      
                  ),
                  
                  box(selectInput(inputId = "Sel_ZipPl", label = "Código Postal", 
                                  choices = as.vector(py$cod_post), 
                                  selected = 3, multiple = FALSE,  width = NULL, size = NULL)
                  )
              ),
              fluidRow(
                verbatimTextOutput("Text2")
              )
              
      )
    )
  )
  
  
)

server <- function(input, output, session) {

  api_key <- "AIzaSyA7eEJNPoTrfh2I6HwEY_GYfXkEjRnQVrM"
  
  output$map <- renderGoogle_map({
    google_map(key = api_key, zoom = 11, location = c(40.4165257,-3.7041473))
  })  
  
#  as.vector(py$df_zip_dates[input$sl_fecha])  
  
  cmb <- reactive({
    cat<-input$Sel_Categoria
    fech <- as.vector(py$df_zip_dates[input$sl_fecha])  
    if (cat == 'Sin Categoria'){
      dat1 <- left_join(
        py$mean_prim%>%filter(Fecha == fech)
         ,py$madrid_zip
      )
    }
    else{
      dat1 <- left_join(
        py$zone_mean_cat_G%>%filter(Fecha == fech, 
        Categoria == cat ),py$madrid_zip
      )
    }
    dat1
  })
  
  
  
  cmb2 <- reactive({
#    input$Sel_ZipPl
    cat<-input$Sel_CategoriaPl

    #    fech <- "2015-10-01"
   if (cat == 'Sin Categoria'){
      if (input$Sel_Mes2 == "Octubre"){
        dat2 <- py$zone_mean_cat1%>%filter(Zone == input$Sel_ZipPl, Fecha < "2015-11-01"
          )%>% group_by(Fecha)%>%summarise(Gasto_Prom = sum(Gasto_Prom),Gasto_min = sum(Gasto_min),
                                           Gasto_max = sum(Gasto_max),Desv_Est = sum(Desv_Est))
      }else{
        dat2 <- py$zone_mean_cat1%>%filter(Zone == input$Sel_ZipPl, Fecha > "2015-10-31"
        )%>% group_by(Fecha)%>%summarise(Gasto_Prom = sum(Gasto_Prom),Gasto_min = sum(Gasto_min),
                                         Gasto_max = sum(Gasto_max),Desv_Est = sum(Desv_Est))
      }
    }
    else{
      if (input$Sel_Mes2 == "Octubre"){
        dat2 <- py$zone_mean_cat1%>%filter(Zone == input$Sel_ZipPl, Fecha < "2015-11-01",
                                          Categoria == cat)
      }else{
        dat2 <- py$zone_mean_cat1%>%filter(Zone == input$Sel_ZipPl, Fecha > "2015-10-31",
                                          Categoria == cat)
      }
    }
    dat2
  })
  

  
  mes<- reactive({
    if (input$Sel_Mes == 'Octubre'){
      ind <- 0
      updateSliderInput(session, "sl_fecha", min = 1, max = 31)}
    else{
      ind <- 31
      updateSliderInput(session, "sl_fecha", min = 1, max = 30)}
    
    ind
  })
  
  

  
  observeEvent(input$gasto,{
    option_gradient <- c('orange', 'blue', 'mediumpurple4', 'snow4', 'thistle1')
    google_map_update(map_id = "map") %>% clear_heatmap()
    val <- input$gasto
    flt<- cmb()%>%filter(Gasto_Prom > val[1] & Gasto_Prom < val[2])
    google_map_update(map_id = "map") %>%
    add_heatmap( data = flt, lat = "Latitude", lon = "Longitude", weight = "Gasto_Prom",
                   option_radius = 16, legend = T,
                   option_dissipating = T, option_opacity = 0.5, update_map_view = F)
  })
  
  
  output$Text1 <- renderPrint({cmb()})
  
  observeEvent(input$sl_fecha,{
    minim = min(cmb()$Gasto_Prom)
    maxim = max(cmb()$Gasto_Prom)
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "gasto", value = c(minim, maxim),
    min = minim, max = maxim, step = (maxim - minim)/10 )
  })
  
  
############ Server del segundo menú ####  
  
  
  output$line_graf <- renderPlot({
#    b <- cmb2()[c('Fecha', 'Gasto_min')]
#    b <- rename(b, c('Gasto_min' = 'Gasto_Prom') )
#    c <- cmb2()[c('Fecha', 'Gasto_max')]
#    c <- rename(c, c('Gasto_max' = 'Gasto_Prom') )
    modul <- abs(cmb2()['Gasto_Prom'] - cmb2()['Desv_Est'])
  
    d1 <- cmb2()['Gasto_Prom'] + modul
    d1 <- cbind(cmb2()['Fecha'],d1)
    d1 <- rename(d1, c('Gasto_Prom' = 'Gasto_Prom') )
    
    d2 <- cmb2()['Gasto_Prom'] - modul
    d2 <- cbind(cmb2()['Fecha'],d2)
    d2 <- rename(d2, c('Gasto_Prom' = 'Gasto_Prom') )
    
    
    ggplot(data=cmb2(), aes(x= Fecha, y = Gasto_Prom, group=1)) +
      geom_line(color="#4C5E64", size=1, linetype="solid")+
      geom_point(color="#222D31", size=2.5)+
      geom_line(data=d1 ,color="#3D8DBC", size=0.5, linetype="dotdash")+
      geom_line(data=d2 ,color="#3D8DBC", size=0.5, linetype="dotdash")+
      theme_gray()+
      theme(axis.text.x = element_text(face = "bold", color = "#3D8DBC", 
                                      size = 10, angle = 45))+
      labs(x = "Fecha",y = "Cantidad (Euros)")+
      theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="blue", size=rel(1))) +
      theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1))) 
    })
  
  
  output$Text2 <- renderPrint({cmb2()})
 

}

shinyApp(ui, server)

