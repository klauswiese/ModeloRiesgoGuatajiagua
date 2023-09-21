# Mapeo de la amenaza
#Guatajiagua
#https://eenew.ifrc.org/x/09T4CLFh
#-------------------------------------------------------------------------------
# Digitalization strategy of the eVCA    
# Mapping the hazard - Submunicipal level
# junio 2023 - K. Wiese - Information Management DRK Central America            
#-------------------------------------------------------------------------------

#PONER EN TODO - Define el directorio de trabajo
############################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############################################################

# 00 Librarias ----
pacman::p_load(swfscMisc,
               shinythemes,
               janitor,
               tidyverse,
               data.table,
               stringi, 
               labelled,
               dplyr,
               DT,
               leaflet,
               htmltools,
               htmlwidgets,
               rmarkdown,
               robotoolbox,
               knitr,
               plotly,
               papeR,
               gt,
               openxlsx
)


# 0. Datos -----
# 0.1 Vinculo a cuenta de Kobo ----
kobo_setup(url = "https://kobonew.ifrc.org/", 
           token = "TOKEN_KOBO_SERVER")


# 0.2 Lista de poryectos en cuenta de Kobo ----
lista <- kobo_asset_list() 

# 0.3 Cargar base de datos de herramienta para Amenaza ----
asset <- kobo_asset("TOKEN_TABLA_GUATAJIAGUA")
KOBO <- kobo_data(asset) %>%
  dplyr::select("muni","amenaza","otra_amenaza", "A1":"C7")

# 1. Lista de municipios en base de datos
Municipios <- data.frame(Municipios = names(attributes(KOBO$muni)$labels),
                         label = c(attributes(KOBO$muni)$labels))


# 1. Crear tablas por aldea ---- 
# 1.1 Crear tablas de aldeas por municipio ----
#--

# 1.1.1 Extraer etiquetas :) ----
#https://stackoverflow.com/questions/39671621/extract-the-labels-attribute-from-labeled-tibble-columns-from-a-haven-import-f
# Función para extraer etiquetas
ExtraerEtiqueta <- function(df){
  tibble(
  Código = df %>% names(),
  Aldea = df %>% map_chr(attr_getter("label"))
  )
}

ExtraerEtiqueta(KOBO)

# 1.1.2 Crear lista vacia apra agregar los valores de las aldeas pro cada municipio ----
# Lista vacia para almacenar resultados por municipio 
AmenazaMun <- list()

# Función para crear tablas de aldeas por Municipio
for(i in 1:length(Municipios$Municipios)){
AmenazaMuni <- KOBO %>%
  filter(KOBO$muni == Municipios$label[i]) %>% 
  select_if(~ !any(is.na(.))) %>%
  select(-c("amenaza", "muni"))

  valores <- c(t(AmenazaMuni[,1:length(AmenazaMuni)]))

  AmenazaU <- AmenazaMuni %>%
              ExtraerEtiqueta() %>%
              mutate(Valor = valores,
                     Nivel = ifelse(Valor >= 4, "Muy Alta", 
                                      ifelse(Valor == 3, "Alta", 
                                             ifelse(Valor == 2, "Media", 
                                                    ifelse(Valor == 1, "Baja", "Muy Baja"))))) %>%
              na.omit() %>%
    arrange(desc(Valor)) -> AmenazaMun[[i]]
  
  
}

#Nombre de cada data frame en lista
names(AmenazaMun) <- Municipios$Municipios
x <- grep("Manguera", AmenazaMun$Guatajiagua$Aldea)
AmenazaMun$Guatajiagua$Aldea[x[1]] <- "Maiguera - Amenaza"
AmenazaMun$Guatajiagua$Aldea[x[2]] <- "Maiguera - Vulnerabilidad"
AmenazaMun$Guatajiagua$Aldea[x[3]] <- "Maiguera - Falta de Capacidad de Respuesta"

x <- grep("Area", AmenazaMun$Guatajiagua$Aldea)
AmenazaMun$Guatajiagua$Aldea[x[3]] <- "Área Urbana - Amenaza"
AmenazaMun$Guatajiagua$Aldea[x[1]] <- "Área Urbana - Vulnerabilidad"
AmenazaMun$Guatajiagua$Aldea[x[2]] <- "Área Urbana - Falta de Capacidad de Respuesta"

#Amenaza
Amenaza <- AmenazaMun[[1]][grep(pattern="Amenaza", AmenazaMun[[1]]$Aldea),] %>%
  separate(Aldea, into=c("Cantón", "Dimensión"), sep = " - ") %>%
  arrange(Cantón)

#Vulnerabildiad
Vulnerabilidad <- AmenazaMun[[1]][grep(pattern="Vulnerabilidad", AmenazaMun[[1]]$Aldea),]  %>%
  separate(Aldea, into=c("Cantón", "Dimensión"), sep = " - ")  %>%
  arrange(Cantón)

#Falta de Capacidad de Respuestas
FaltaCapacidadRespuesta <- AmenazaMun[[1]][grep(pattern="Falta de Capacidad de Respuesta", AmenazaMun[[1]]$Aldea),]  %>%
  separate(Aldea, into=c("Cantón", "Dimensión"), sep = " - ")  %>%
  arrange(Cantón)

#Riesgo
Riesgo <- Amenaza %>%
  dplyr::select("Cantón") %>%
  mutate(Valor = round((Amenaza$Valor)^(1/3) * (Vulnerabilidad$Valor)^(1/3) * (FaltaCapacidadRespuesta$Valor)^(1/3),0),
         ) %>%
  mutate(Nivel = ifelse(Valor >= 4, "Muy Alta", 
                        ifelse(Valor >= 3, "Alta", 
                               ifelse(Valor >= 2, "Media", 
                                      ifelse(Valor >= 1, "Baja", "Muy Baja")))))



# 2. Crear tablas de aldeas por Municipio ----

# 2.1 Función par crear tabla ----
TablaExposicion <- function(n){
aldea <- n 

Amenaza %>%
  select("Cantón", Valor, Nivel) %>%
  arrange(desc(Valor)) %>%
  gt() %>%
  data_color(
    method = "numeric",
    palette = c("darkgreen", "yellowgreen", "yellow","orange","red")
  ) |>
  tab_header(
    title = md(paste("**", Municipios[[n]],"**", sep="")),
    subtitle = md(paste0("*Tabla de Priorización de Cantones por Exposición y Peligro a la Amenaza*"))
  )
}


# 2.1 Función par crear tabla ----
TablaVulnerabilidad <- function(n){
  aldea <- n 
  
  Vulnerabilidad %>%
    select("Cantón", Valor, Nivel) %>%
    arrange(desc(Valor)) %>%
    gt() %>%
    data_color(
      method = "numeric",
      palette = c("darkgreen", "yellowgreen", "yellow","orange","red")
    ) |>
    tab_header(
      title = md(paste("**", Municipios[[n]],"**", sep="")),
      subtitle = md(paste0("*Tabla de Priorización de Cantones por Vulnerabilidad*"))
    )
}

# 2.1 Función par crear tabla ----
TablaFaltaCapacidadRespuesta <- function(n){
  aldea <- n 
  
  FaltaCapacidadRespuesta %>%
    select("Cantón", Valor, Nivel) %>%
    arrange(desc(Valor)) %>%
    gt() %>%
    data_color(
      method = "numeric",
      palette = c("darkgreen", "yellowgreen", "yellow","orange","red")
    ) |>
    tab_header(
      title = md(paste("**", Municipios[[n]],"**", sep="")),
      subtitle = md(paste0("*Tabla de Priorización de Cantones por Falta de Capacidad de Respuesta*"))
    )
}

# 2.1 Función par crear tabla ----
TablaRiesgo <- function(n){
  aldea <- n 
  
  Riesgo %>%
    select("Cantón", Valor, Nivel) %>%
    arrange(desc(Valor)) %>%
    gt() %>%
    data_color(
      method = "numeric",
      palette = c("darkgreen", "yellowgreen", "yellow","orange","red")
    ) |>
    tab_header(
      title = md(paste("**", Municipios[[n]],"**", sep="")),
      subtitle = md(paste0("*Tabla de Priorización de Cantones por Riesgo*"))
    )
}

# 2.2 Evaluación de función ----
TablaExposicion(1)
TablaVulnerabilidad(1)
TablaFaltaCapacidadRespuesta(1)
TablaRiesgo(1)

#save gt Pespire guarda imagen de tablas

# Evalua la existencia de carpeta
if (!dir.exists("01_PNG")) {dir.create("01_PNG")}
gtsave(TablaExposicion(1), 
       filename=paste("01_PNG/01_Exposicion_y_Peligro_Aldeas_Imagen_", 
                      today(), 
                      ".png", 
                      sep="")
)

#Vulnerabilidad
gtsave(TablaVulnerabilidad(1), 
       filename=paste("01_PNG/02_Vulnerabilidad_Aldeas_Imagen_", 
                      today(), 
                      ".png", 
                      sep=""))

#Falta de Capacidad de Respuesta
gtsave(TablaFaltaCapacidadRespuesta(1), 
       filename=paste("01_PNG/03_Falta_De_Capacidad_de_Respuesta_Aldeas_Imagen_", 
                      today(), 
                      ".png", 
                      sep=""))

#Riesgo
gtsave(TablaRiesgo(1), 
       filename=paste("01_PNG/04_Riesgo_Aldeas_Imagen_", 
                      today(), 
                      ".png", 
                      sep=""))

#Guardar tablas
#Amenaza
if (!dir.exists("02_XLSX")) {dir.create("02_XLSX")}
write.xlsx(Amenaza, 
           file=paste("02_XLSX/01_Exposicion_y_Peligro_Aldeas_", 
                      today(), 
                      ".xlsx", 
                      sep=""))


#Vulnerabilidad
write.xlsx(Vulnerabilidad, 
           file=paste("02_XLSX/02_Vulnerabilidad_Aldeas_", 
                      today(), 
                      ".xlsx", 
                      sep=""))
#Falta de Capacidad de Respuesta
write.xlsx(FaltaCapacidadRespuesta, 
           file=paste("02_XLSX/03_Falta_de_Capacidad_de_Respuesta_Aldeas_", 
                      today(), 
                      ".xlsx", 
                      sep=""))

#Riesgo
write.xlsx(Riesgo, 
           file=paste("02_XLSX/04_Riesgo_Aldeas_", 
                      today(), 
                      ".xlsx", 
                      sep=""))

# 3. Crear mapas de aldeas por Municipio ----
# 3.1 Cargar capa de aldeas
library(sf)
aldeas_sf <- st_read('GPKG/Guatajiagua2.gpkg') %>%
  st_transform(4326) %>%
  dplyr::select(NAM, UID)

# 3.2 Unir tabla con datos espaciales
ExpoAldea <- function(n){
  aldea <- n 
  pal <- colorFactor(
    palette = 'Oranges',
    domain = Amenaza$Valor
  )
  
  data_map <- left_join(aldeas_sf, Amenaza, 
                        by = c("NAM" = "Cantón")) %>%
    select(NAM, Valor, Nivel) %>%
    na.omit()  %>%
    st_write("GPKG/Exposicion.GPKG", 
             delete_dsn = TRUE)
  
  data_map2 <- 
    leaflet(data_map) %>%
    addPolygons(stroke = 0.1, 
                smoothFactor = 0.2, 
                fillOpacity = 0.6,
                color = ~pal(Valor),
                label = ~htmlEscape(paste0("Cantón de:", NAM, ", ", names(AmenazaMun)[aldea]))) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addLegend("topright", pal = pal, values = ~Valor,
              title = md(paste0("Exposición y Peligro <br>", "Guatajiagua")),
              opacity = 1)
  
  (data_map2)
}

#Vulnerabilidad
VulnerabilidadAldea <- function(n){
  aldea <- n 
  pal <- colorFactor(
    palette = 'Blues',
    domain = Vulnerabilidad$Valor
  )
  
  data_map <- left_join(aldeas_sf, Vulnerabilidad, 
                        by = c("NAM" = "Cantón")) %>%
    select(NAM, Valor, Nivel) %>%
    na.omit()  %>%
    st_write("GPKG/Vulnerabilidad.GPKG", 
             delete_dsn = TRUE)
  
  data_map2 <- 
    leaflet(data_map) %>%
    addPolygons(stroke = 0.1, 
                smoothFactor = 0.2, 
                fillOpacity = 0.6,
                color = ~pal(Valor),
                label = ~htmlEscape(paste0("Cantón de:", NAM, ", ", names(AmenazaMun)[aldea]))) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addLegend("topright", pal = pal, values = ~Valor,
              title = md(paste0("Vulnerabilidad <br>", "Guatajiagua")),
              opacity = 1)
  
  (data_map2)
}

#Falta de Capacidad de Respuesta
FaltaAldea <- function(n){
  aldea <- n 
  pal <- colorFactor(
    palette = 'Greens',
    domain = FaltaCapacidadRespuesta$Valor
  )
  
  data_map <- left_join(aldeas_sf, FaltaCapacidadRespuesta, 
                        by = c("NAM" = "Cantón")) %>%
    select(NAM, Valor, Nivel) %>%
    na.omit()  %>%
    st_write("GPKG/FaltaDeCapacidadRespuesta.GPKG", 
             delete_dsn = TRUE)
  
  data_map2 <- 
    leaflet(data_map) %>%
    addPolygons(stroke = 0.1, 
                smoothFactor = 0.2, 
                fillOpacity = 0.6,
                color = ~pal(Valor),
                label = ~htmlEscape(paste0("Cantón de:", NAM, ", ", names(AmenazaMun)[aldea]))) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addLegend("topright", pal = pal, values = ~Valor,
              title = md(paste0("Falta de Capacidad de Respuesta <br>", "Guatajiagua")),
              opacity = 1)
  
  (data_map2)
}


#Riesgo
RiesgoAldea <- function(n){
  aldea <- n 
  pal <- colorFactor(
    palette = 'Reds',
    domain = Riesgo$Valor
  )
  
  data_map <- left_join(aldeas_sf, Riesgo, 
                        by = c("NAM" = "Cantón")) %>%
    select(NAM, Valor, Nivel) %>%
    na.omit() %>%
    st_write("GPKG/IndiceRiesgo.GPKG", 
             delete_dsn = TRUE)
  
  data_map2 <- 
    leaflet(data_map) %>%
    addPolygons(stroke = 0.1, 
                smoothFactor = 0.2, 
                fillOpacity = 0.6,
                color = ~pal(Valor),
                label = ~htmlEscape(paste0("Cantón de:", NAM, ", ", names(AmenazaMun)[aldea]))) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addLegend("topright", pal = pal, values = ~Valor,
              title = md(paste0("Riesgo <br>", "Guatajiagua")),
              opacity = 1)
  
  (data_map2)
}


#Mapas de intensidad de la amenaza
ExpoAldea(1)
VulnerabilidadAldea(1)
FaltaAldea(1)
RiesgoAldea(1)


