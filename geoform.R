library(shiny)
library(leaflet)
library(DT)
library(dplyr)

# Leer el diccionario de especies desde un archivo CSV 
tree_species <- read.csv(file = "assets/tree_species.csv")

ui <- fluidPage(
  titlePanel("Formulario de sitios de decaimiento"),
  
  tabsetPanel(id = "tabs",
              ## bloque Consentimiento 
              tabPanel("Consentimiento",
                       fluidRow(
                         column(6,
                                h4("Bienvenido/a"),
                                p("Este formulario recopila datos con fines académicos."),
                                p("La información será tratada de forma confidencial."),
                                p("Debes aceptar el consentimiento para continuar."),
                                
                                checkboxInput("consentimiento", 
                                              "Estás de acuerdo en que los datos proporcionados aquí 
                                     se puedan proporcionar a los miembros de ReDeC bajo petición 
                                     del objetivo de su uso y consulta de coautoría, siendo su cesión automática", value = FALSE)
                                )
                       ),
                       fluidRow(
                         column(12, actionButton("next1", "Siguiente →", class = "btn-primary"))
                       )
              ),
              
              ## bloque Contacto 
              tabPanel("Responsable contacto",
                       fluidRow(
                         column(6,
                                textInput("author_name", "Nombre del responsable:", ""),
                                textInput("author_email", "Correo electrónico:", ""),
                                textInput("author_affiliation", "Afiliación:", ""),
                                textAreaInput("author_team", "Otros autores (uno por línea):", "", rows = 4, placeholder = "Ejemplo:\nAutor 1\nAutor 2\nAutor 3"),
                                textInput("gestor_name", "Nombre Apellidos del Gestor:", ""),
                                textInput("gestor_email", "Correo electrónico:", ""),
                                br(),
                                textAreaInput("site_pubs1", "Fuentes de datos (publicaciones):", "", rows = 2, placeholder = "Ejemplo:Referencia, doi"),
                                textAreaInput("site_pubs2", "Fuentes de datos (publicaciones):", "", rows = 2, placeholder = "Ejemplo:Referencia, doi"),
                                textAreaInput("site_pubs3", "Fuentes de datos (publicaciones):", "", rows = 2, placeholder = "Ejemplo:Referencia, doi"),
                                textAreaInput("contact_obs", "Observaciones:", "", rows = 3),
                         )
                       ),
                       fluidRow(
                         column(6, actionButton("prev1", "← Anterior")),
                         column(6, actionButton("next2", "Siguiente →", class = "btn-primary"))
                       )
              ),
    
              ## Mapa 
              tabPanel("Seleccionar Sitio",
                       fluidRow(
                         column(6, 
                                h4("Selecciona un punto en el mapa"),
                                leafletOutput("map", height = 500),
                                h5("Coordenadas seleccionadas:"),
                                verbatimTextOutput("coords")
                                # ,
                                # selectInput("map_layer", "Selecciona capa base del mapa:", 
                                #             choices = c("OpenStreetMap", "Satélite"), selected = "OpenStreetMap")
                         )
                       ),
                       fluidRow(
                         column(6, actionButton("prev2", "← Anterior")),
                         column(6, actionButton("next3", "Siguiente →", class = "btn-primary"))
                       )
              ),
              
              # Sitio 
              tabPanel("Información del Sitio",
                       fluidRow(
                         column(6, 
                                h4("Datos del sitio"),
                                selectizeInput("species_main", "Especie afectada (principal):", 
                                               choices = c(tree_species$species, ""), selected = ""),
                                sliderInput("damage_main", "% Daño especie principal:", min = 0, max = 100, value = 0, step = 1),
                                sliderInput("damage_general", "% medio de Decaimiento/Mortalidad:", min = 0, max = 100, value = 0, step = 1)
                         ),
                         column(6,
                                selectizeInput("species_secondary", "Especie afectada (secundaria):", 
                                               choices = c(tree_species$species, ""), selected = ""),
                                sliderInput("damage_secondary", "% Daño especie secundaria:", min = 0, max = 100, value = 0, step = 1),
                                selectizeInput("species_other", "Otras especies afectadas:", 
                                               choices = c(tree_species$species, ""), selected = "", multiple = TRUE)
                         )
                       ),
                       fluidRow(
                         column(6, selectInput("event_climate", "¿Evento climático?", choices = c("Sí", "No", "No lo sé"))),
                         column(6, selectInput("event_cause", "Otros factores", choices = c("Fuego", "Factores Bióticos", "Gestión", ""), multiple = TRUE))
                       ),
                       fluidRow(
                         column(12,
                                selectizeInput("methodology", "Metodología utilizada:", 
                                               choices = c("Fotografía aérea", "Trabajo de campo", "Teledetección", "Modelado"), 
                                               multiple = TRUE, options = list(create = TRUE))
                         )
                       ),
                       fluidRow(
                         column(6, actionButton("prev3", "← Anterior")),
                         column(6, actionButton("next4", "Siguiente →", class = "btn-primary"))
                       )
              ),
              
              
              
              tabPanel("Publicaciones",
                       fluidRow(
                         column(6,
                                selectInput("has_publication", "¿Hay publicación asociada?", choices = c("Sí", "No")),
                                textInput("pub_title", "Nombre de la publicación:", ""),
                                textInput("pub_doi", "DOI (si aplica):", "")
                         )
                       ),
                       fluidRow(
                         column(6, actionButton("prev4", "← Anterior")),
                         column(6, actionButton("next5", "Siguiente →", class = "btn-primary"))
                       )
              ),
              
              tabPanel("Tabla de Eventos",
                       fluidRow(
                         column(12,
                                h4("Tabla Generada"),
                                actionButton("delete", "Eliminar registro seleccionado", class = "btn-danger"),
                                br(), br(),
                                DTOutput("table"),
                                br(), br(),
                                actionButton("add", "Añadir a la tabla", class = "btn-success btn-lg btn-block"),
                                br(), br(),
                                downloadButton("download_csv", "Descargar CSV", class = "btn-warning btn-lg")
                         )
                       ),
                       fluidRow(
                         column(6, actionButton("prev5", "← Anterior"))
                       )
              )
  )
)

server <- function(input, output, session) {
  # Boton bloque 1 consentimiento
  observeEvent(input$next1, { updateTabsetPanel(session, "tabs", selected = "Responsable contacto") })
  # Botones bloque 2 contacto 
  observeEvent(input$prev1, { updateTabsetPanel(session, "tabs", selected = "Consentimiento") })
  observeEvent(input$next2, { updateTabsetPanel(session, "tabs", selected = "Seleccionar Sitio") })
  # Botones bloque 3 mapa sitio 
  observeEvent(input$prev2, { updateTabsetPanel(session, "tabs", selected = "Responsable contacto") })
  observeEvent(input$next3, { updateTabsetPanel(session, "tabs", selected = "Información del Sitio") })
  # Botones bloque 4
  observeEvent(input$prev3, { updateTabsetPanel(session, "tabs", selected = "Seleccionar Sitio") })
  observeEvent(input$next4, { updateTabsetPanel(session, "tabs", selected = "Publicaciones") })
  # Botones bloque 5
  observeEvent(input$prev4, { updateTabsetPanel(session, "tabs", selected = "Información del Sitio") })
  observeEvent(input$next5, { updateTabsetPanel(session, "tabs", selected = "Tabla de Eventos") })
  
  
  observeEvent(input$prev5, { updateTabsetPanel(session, "tabs", selected = "Publicaciones") })
  
  coords <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satélite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = -3.7038, lat = 40.4168, zoom = 5)
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    coords(list(lng = round(click$lng, 6), lat = round(click$lat, 6)))
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat, popup = paste("Long:", click$lng, "<br>Lat:", click$lat))
  })
  
  output$coords <- renderText({
    if (!is.null(coords())) {
      paste("Longitud:", coords()$lng, "- Latitud:", coords()$lat)
    } else {
      "Haz clic en el mapa para seleccionar un punto"
    }
  })
  
  tableData <- reactiveVal(data.frame())
  
  observeEvent(input$add, {
    data <- tableData()
    new_entry <- tibble(
      ID = ifelse(nrow(data) == 0, 1, max(data$ID, na.rm = TRUE) + 1),
      Longitud = coords()$lng,
      Latitud = coords()$lat,
      Especie_Principal = input$species_main,
      Daño_Principal = input$damage_main,
      Especie_Secundaria = input$species_secondary,
      Daño_Secundario = input$damage_secondary,
      Daño_General = input$damage_general,
      Evento_Climatico = input$event_climate,
      Causa_Evento = paste(input$event_cause, collapse = ", "),
      Otras_Especies = paste(input$species_other, collapse = ", "),
      Metodología = paste(input$methodology, collapse = ", "),
      Autor = input$author_name,
      Afiliación = input$author_affiliation,
      Otros_Autores = input$other_authors,
      Publicación_Asociada = input$has_publication,
      Publicación = input$pub_title,
      DOI = input$pub_doi
    )
    tableData(bind_rows(data, new_entry))
  })
  
  output$table <- renderDT({
    datatable(tableData(), options = list(pageLength = 5, dom = 't<"bottom"ip>'), rownames = FALSE, selection = 'single')
  })
  
  observeEvent(input$delete, {
    selected_row <- input$table_rows_selected
    if (length(selected_row) > 0) {
      data <- tableData()
      data <- data[-selected_row, ]
      tableData(data)
    }
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { "datos_registro.csv" },
    content = function(file) { write.csv(tableData(), file, row.names = FALSE) }
  )
}

shinyApp(ui, server)
