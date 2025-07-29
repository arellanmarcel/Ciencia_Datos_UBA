
################################################################################
###Trabajo final: Ciencia de datos
################################################################################
#Integrantes: 
#- Alan Marcelo Rodriguez Arellano
#- Melisa Naomi Evangelista Davila
################################################################################

# --------------------------------------
# LIMPIAR ENTORNO
# --------------------------------------
rm(list = ls())

# --------------------------------------
# DEFINIR CARPETA DE TRABAJO
# --------------------------------------
setwd("C:/Users/User/Documents/Maestría/CIENCIA DE DATOS")

# --------------------------------------
# CARGAR LIBRERÍAS
# --------------------------------------
library(haven)
library(dplyr)
library(sjlabelled)
library(srvyr)
library(survey)
library(scales)
library(sjmisc)
library(labelled)
library(tidyr)
library(igraph)
library(ggplot2)
library(stringr)
library(purrr)
library(tibble)
library(patchwork)
library(ggraph)
library(tidygraph)
library(backbone)
library(tidyverse)
library(viridis)
library(forcats)
library(ggalluvial)
library(RColorBrewer)
library(pheatmap)

# --------------------------------------
# CARGAR ARCHIVO .RData Y ASIGNAR OBJETO
# --------------------------------------
load("datos2oficiales.RData")  # esto carga un objeto en el entorno

# Verificamos qué objeto se cargó:
print(ls())  # por ejemplo, puede ser "base_empleo"

# ==============================================================================
# LIMPIEZA Y MANEJO DE DATOS
# ==============================================================================

#Creo bases de 2 años para analizar cambios

# Panel 2018–2019 (pepanelxxxx es el indicador panel para seguir a los mismos individuos en 2 años)
panel_1819 <- empleo_filtrado %>%
  filter(perpanel1819 == 1)
# Panel 2019–2020
panel_1920 <- empleo_filtrado %>%
  filter(perpanel1920 == 1)
# Panel 2020–2021
panel_2021 <- empleo_filtrado %>%
  filter(perpanel2021 == 1)
# Panel 2021–2022
panel_2122 <- empleo_filtrado %>%
  filter(perpanel2122 == 1)

#Pasamos los datos de formato ancho a largo
#Panel 2018-2019
panel_1819_long <- panel_1819 %>%
  pivot_longer(cols = matches("_(18|19)$"),names_to = c(".value", "año"),names_sep = "_") %>%
  mutate(año = paste0("20", año))
#Panel 2019-2020
panel_1920_long <- panel_1920 %>%
  pivot_longer(cols = matches("_(19|20)$"), names_to = c(".value", "año"), names_sep = "_") %>%
  mutate(año = paste0("20", año))
#Panel 2020-2021
panel_2021_long <- panel_2021 %>%
  pivot_longer(cols = matches("_(20|21)$"), names_to = c(".value", "año"), names_sep = "_") %>%
  mutate(año = paste0("20", año))
#Panel 2021-2022
panel_2122_long <- panel_2122 %>%
  pivot_longer(cols = matches("_(21|22)$"), names_to = c(".value", "año"), names_sep = "_") %>%
  mutate(año = paste0("20", año))

rm(modulo_empleo) #Borro base panel inicial (muy pesada)

#================================
#Recodificación para 12 sectores
#================================

# Función que recodifica sectores y grupo ocupacional
recodificar_panel <- function(df) {
  df %>%
    mutate(
      # CIIU 2 dígitos
      ciiu_aux1 = ifelse(p506r4 > 999,
                         as.character(p506r4),
                         str_pad(as.character(p506r4), width = 4, pad = "0")),
      ciiu_aux2 = str_sub(ciiu_aux1, 1, 2),
      ciiu_2d = as.numeric(ciiu_aux2),
      
      # Clasificación sectorial
      ciiu_1d = case_when(
        ciiu_2d <= 2                              ~ 1,  # Agricultura
        ciiu_2d == 3                              ~ 2,  # Pesca
        ciiu_2d >= 5  & ciiu_2d <= 9              ~ 3,  # Minería
        ciiu_2d >= 10 & ciiu_2d <= 33             ~ 4,  # Manufactura
        ciiu_2d >= 41 & ciiu_2d <= 43             ~ 5,  # Construcción
        ciiu_2d >= 45 & ciiu_2d <= 47             ~ 6,  # Comercio
        (ciiu_2d >= 49 & ciiu_2d <= 53) |
          (ciiu_2d >= 58 & ciiu_2d <= 63)         ~ 7,  # Transporte y comunicaciones
        ciiu_2d == 84                             ~ 8,  # Gobierno
        ciiu_2d >= 55 & ciiu_2d <= 56             ~ 9,  # Hoteles y restaurantes
        ciiu_2d == 68 | (ciiu_2d >= 69 & ciiu_2d <= 82) ~ 10, # Inmobiliarias
        ciiu_2d == 85                             ~ 11, # Educación
        ciiu_2d %in% c(35:39, 64:66, 86:88, 90:93, 94:98, 99) ~ 12, # Otros Servicios
        TRUE ~ NA_real_
      ),
      
      # Etiqueta sectorial
      sector_label = case_when(
        ciiu_1d == 1  ~ "Agricultura",
        ciiu_1d == 2  ~ "Pesca",
        ciiu_1d == 3  ~ "Minería",
        ciiu_1d == 4  ~ "Manufactura",
        ciiu_1d == 5  ~ "Construcción",
        ciiu_1d == 6  ~ "Comercio",
        ciiu_1d == 7  ~ "Transportes y Comunicaciones",
        ciiu_1d == 8  ~ "Gobierno",
        ciiu_1d == 9  ~ "Hoteles y Restaurantes",
        ciiu_1d == 10 ~ "Inmobiliarias y Alquileres",
        ciiu_1d == 11 ~ "Educación",
        ciiu_1d == 12 ~ "Otros Servicios",
        TRUE ~ NA_character_
      ),
      
      # Grupo ocupacional
      grupo_ocupacional = case_when(
        ocu500 == 1 ~ "Ocupado",
        ocu500 %in% c(2, 3) ~ "Desocupado",
        ocu500 == 4 ~ "No PEA",
        TRUE ~ NA_character_
      )
    )
}

panel_1819_long <- recodificar_panel(panel_1819_long)
panel_1920_long <- recodificar_panel(panel_1920_long)
panel_2021_long <- recodificar_panel(panel_2021_long)
panel_2122_long <- recodificar_panel(panel_2122_long)

#================================
#Recodificación para 99 sectores
#================================
etiquetar_sector_2dg <- function(df) {
  df %>%
    mutate(
      # Crear código CIIU de 4 dígitos
      ciiu_4dg = ifelse(p506r4 > 999,
                        as.character(p506r4),
                        str_pad(as.character(p506r4), width = 4, pad = "0")),
      ciiu_2dg = as.numeric(str_sub(ciiu_4dg, 1, 2)),
      sector_label_2dg = case_when(
        ciiu_2dg == 1  ~ "Agricultura, ganadería, caza y actividades y servicios conexas",
        ciiu_2dg == 2  ~ "Silvicultura y extracción de madera",
        ciiu_2dg == 3  ~ "Pesca y acuicultura",
        ciiu_2dg == 5  ~ "Extracción de carbón de piedra y lignito",
        ciiu_2dg == 6  ~ "Extracción de petróleo crudo y gas natural",
        ciiu_2dg == 7  ~ "Extracción de minerales metalíferos",
        ciiu_2dg == 8  ~ "Explotación de otras minas y canteras",
        ciiu_2dg == 9  ~ "Servicios de apoyo para la explotación de minas y canteras",
        ciiu_2dg == 10 ~ "Elaboración de productos alimenticios",
        ciiu_2dg == 11 ~ "Elaboración de bebidas",
        ciiu_2dg == 12 ~ "Elaboración de productos de tabaco",
        ciiu_2dg == 13 ~ "Fabricación de productos textiles",
        ciiu_2dg == 14 ~ "Fabricación de prendas de vestir",
        ciiu_2dg == 15 ~ "Fabricación de productos de cuero y conexos",
        ciiu_2dg == 16 ~ "Producción de madera y fabricación de productos de madera",
        ciiu_2dg == 17 ~ "Fabricación de papel y productos de papel",
        ciiu_2dg == 18 ~ "Impresión y reproducción de grabaciones",
        ciiu_2dg == 19 ~ "Fabricación de coque y productos de refinación del petróleo",
        ciiu_2dg == 20 ~ "Fabricación de sustancias y productos químicos",
        ciiu_2dg == 21 ~ "Fabricación de productos farmacéuticos",
        ciiu_2dg == 22 ~ "Fabricación de productos de caucho y plástico",
        ciiu_2dg == 23 ~ "Fabricación de otros productos minerales no metálicos",
        ciiu_2dg == 24 ~ "Fabricación de metales comunes",
        ciiu_2dg == 25 ~ "Fabricación de productos elaborados de metal",
        ciiu_2dg == 26 ~ "Fabricación de productos de informática, electrónica y óptica",
        ciiu_2dg == 27 ~ "Fabricación de equipo eléctrico",
        ciiu_2dg == 28 ~ "Fabricación de maquinaria y equipo n.c.p.",
        ciiu_2dg == 29 ~ "Fabricación de vehículos automotores",
        ciiu_2dg == 30 ~ "Fabricación de otro equipo de transporte",
        ciiu_2dg == 31 ~ "Fabricación de muebles",
        ciiu_2dg == 32 ~ "Otras industrias manufactureras",
        ciiu_2dg == 33 ~ "Reparación e instalación de maquinaria y equipo",
        ciiu_2dg == 35 ~ "Suministro de electricidad, gas y aire acondicionado",
        ciiu_2dg == 36 ~ "Captación, tratamiento y distribución de agua",
        ciiu_2dg == 37 ~ "Evacuación de aguas residuales",
        ciiu_2dg == 38 ~ "Tratamiento y eliminación de desechos",
        ciiu_2dg == 39 ~ "Actividades de descontaminación y gestión de desechos",
        ciiu_2dg == 41 ~ "Construcción de edificios",
        ciiu_2dg == 42 ~ "Obras de ingeniería civil",
        ciiu_2dg == 43 ~ "Actividades especializadas de construcción",
        ciiu_2dg == 45 ~ "Comercio y reparación de vehículos automotores",
        ciiu_2dg == 46 ~ "Comercio al por mayor",
        ciiu_2dg == 47 ~ "Comercio al por menor",
        ciiu_2dg == 49 ~ "Transporte terrestre y por tuberías",
        ciiu_2dg == 50 ~ "Transporte por vía acuática",
        ciiu_2dg == 51 ~ "Transporte por vía aérea",
        ciiu_2dg == 52 ~ "Almacenamiento y apoyo al transporte",
        ciiu_2dg == 53 ~ "Actividades postales y de mensajería",
        ciiu_2dg == 55 ~ "Actividades de alojamiento",
        ciiu_2dg == 56 ~ "Servicios de comidas y bebidas",
        ciiu_2dg == 58 ~ "Actividades de edición",
        ciiu_2dg == 59 ~ "Producción audiovisual y musical",
        ciiu_2dg == 60 ~ "Actividades de programación y transmisión",
        ciiu_2dg == 61 ~ "Telecomunicaciones",
        ciiu_2dg == 62 ~ "Programación informática y consultoría",
        ciiu_2dg == 63 ~ "Servicios de información",
        ciiu_2dg == 64 ~ "Servicios financieros, excepto seguros",
        ciiu_2dg == 65 ~ "Seguros y fondos de pensiones",
        ciiu_2dg == 66 ~ "Actividades auxiliares de servicios financieros",
        ciiu_2dg == 68 ~ "Actividades inmobiliarias",
        ciiu_2dg == 69 ~ "Actividades jurídicas y contabilidad",
        ciiu_2dg == 70 ~ "Oficinas principales y consultoría de gestión",
        ciiu_2dg == 71 ~ "Arquitectura, ingeniería y ensayos técnicos",
        ciiu_2dg == 72 ~ "Investigación científica y desarrollo",
        ciiu_2dg == 73 ~ "Publicidad y estudios de mercado",
        ciiu_2dg == 74 ~ "Otras actividades profesionales y técnicas",
        ciiu_2dg == 75 ~ "Actividades veterinarias",
        ciiu_2dg == 77 ~ "Actividades de alquiler y arrendamiento",
        ciiu_2dg == 78 ~ "Actividades de empleo",
        ciiu_2dg == 79 ~ "Agencias de viajes y operadores turísticos",
        ciiu_2dg == 80 ~ "Actividades de seguridad e investigación",
        ciiu_2dg == 81 ~ "Servicios a edificios y paisajismo",
        ciiu_2dg == 82 ~ "Apoyo administrativo y otras actividades de apoyo a empresas",
        ciiu_2dg == 84 ~ "Administración pública y defensa",
        ciiu_2dg == 85 ~ "Enseñanza",
        ciiu_2dg == 86 ~ "Atención de la salud humana",
        ciiu_2dg == 87 ~ "Atención en instituciones",
        ciiu_2dg == 88 ~ "Asistencia social sin alojamiento",
        ciiu_2dg == 90 ~ "Actividades artísticas y entretenimiento",
        ciiu_2dg == 91 ~ "Bibliotecas, archivos y museos",
        ciiu_2dg == 92 ~ "Juegos de azar y apuestas",
        ciiu_2dg == 93 ~ "Deportes y esparcimiento",
        ciiu_2dg == 94 ~ "Actividades de asociaciones",
        ciiu_2dg == 95 ~ "Reparación de bienes personales",
        ciiu_2dg == 96 ~ "Servicios personales",
        ciiu_2dg == 97 ~ "Hogares como empleadores de personal doméstico",
        ciiu_2dg == 98 ~ "Producción de bienes y servicios para uso propio",
        ciiu_2dg == 99 ~ "Organizaciones y órganos extraterritoriales",
        TRUE ~ NA_character_
      )
    )
}

panel_1819_long <- etiquetar_sector_2dg(panel_1819_long)
panel_1920_long <- etiquetar_sector_2dg(panel_1920_long)
panel_2021_long <- etiquetar_sector_2dg(panel_2021_long)
panel_2122_long <- etiquetar_sector_2dg(panel_2122_long)

panel_1819_long$sector_label_2dg

########################################
#  Tabla transiciones para 12 sectores 
########################################
procesar_panel <- function(df, fac, anio1, anio2, sufijo) {
  # Conteo por sector y grupo ocupacional
  print(df %>%
          count(sector_label, grupo_ocupacional) %>%
          pivot_wider(names_from = grupo_ocupacional, values_from = n, values_fill = 0))
  # Conteo por sector y año (no expandido)
  print(df %>%
          count(sector_label, año) %>%
          pivot_wider(names_from = año, values_from = n, values_fill = 0))
  # Conteo expandido por sector y año
  print(df %>%
          group_by(sector_label, año) %>%
          summarise(n = sum(.data[[fac]], na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = año, values_from = n, values_fill = 0))
  # Conteo expandido por sector y grupo ocupacional
  print(df %>%
          group_by(sector_label, grupo_ocupacional) %>%
          summarise(n = sum(.data[[fac]], na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = grupo_ocupacional, values_from = n, values_fill = 0))
  # Conteo total por grupo ocupacional
  print(df %>%
          group_by(grupo_ocupacional) %>%
          summarise(n = sum(.data[[fac]], na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = grupo_ocupacional, values_from = n, values_fill = 0))
  # Transiciones estado
  trans_estado <- df %>%
    select(numper, año, grupo_ocupacional, !!sym(fac)) %>%
    pivot_wider(names_from = año, values_from = grupo_ocupacional,
                names_prefix = "estado_") %>%
    filter(!is.na(.data[[paste0("estado_", anio1)]]) &
             !is.na(.data[[paste0("estado_", anio2)]]))
  
  assign(paste0("transiciones_estado", sufijo), trans_estado, envir = .GlobalEnv)
  
  print(trans_estado %>%
          group_by(.data[[paste0("estado_", anio1)]], .data[[paste0("estado_", anio2)]]) %>%
          summarise(poblacion = sum(.data[[fac]], na.rm = TRUE), .groups = "drop"))
  
  # Transiciones entre sectores (solo ocupados)
  trans_sector <- df %>%
    filter(grupo_ocupacional == "Ocupado") %>%
    select(numper, año, sector_label, !!sym(fac)) %>%
    pivot_wider(names_from = año, values_from = sector_label,
                names_prefix = "sector_") %>%
    filter(!is.na(.data[[paste0("sector_", anio1)]]) &
             !is.na(.data[[paste0("sector_", anio2)]]))
  
  assign(paste0("transiciones_sector", sufijo), trans_sector, envir = .GlobalEnv)
  
  print(trans_sector %>%
          group_by(.data[[paste0("sector_", anio1)]], .data[[paste0("sector_", anio2)]]) %>%
          summarise(poblacion = sum(.data[[fac]], na.rm = TRUE), .groups = "drop") %>%
          print(n = Inf))
  
  # Creamos tabla resumen final
  tabla_final <- trans_sector %>%
    group_by(.data[[paste0("sector_", anio1)]], .data[[paste0("sector_", anio2)]]) %>%
    summarise(poblacion = sum(.data[[fac]], na.rm = TRUE), .groups = "drop")
  
  assign(paste0("tabla_transiciones", sufijo, "vf"), tabla_final, envir = .GlobalEnv)
  
  print(tabla_final %>% print(n = Inf))
}

procesar_panel(panel_1819_long, "facpanel1819", 2018, 2019, "1819")
procesar_panel(panel_1920_long, "facpanel1920", 2019, 2020, "1920")
procesar_panel(panel_2021_long, "facpanel2021", 2020, 2021, "2021")
procesar_panel(panel_2122_long, "facpanel2122", 2021, 2022, "2122")

########################################
#Tabla transiciones para 99 sectores
########################################
generar_transiciones_ciidu <- function(df, fac, anio1, anio2, sufijo) {
  # Filtramos ocupados y preparamoos base wide con códigos CIIU-2D
  transiciones <- df %>%
    filter(grupo_ocupacional == "Ocupado") %>%
    select(numper, año, ciiu_2dg, !!sym(fac)) %>%
    pivot_wider(
      names_from = año, 
      values_from = ciiu_2dg, 
      names_prefix = "sector_"
    ) %>%
    filter(!is.na(.data[[paste0("sector_", anio1)]]) & !is.na(.data[[paste0("sector_", anio2)]]))
  
  # Guardamos el objeto en el entorno global
  assign(paste0("transiciones_sector", sufijo, "_nue_recod"), transiciones, envir = .GlobalEnv)
  
  # Tabla resumen expandida
  tabla <- transiciones %>%
    group_by(.data[[paste0("sector_", anio1)]], .data[[paste0("sector_", anio2)]]) %>%
    summarise(poblacion = sum(.data[[fac]], na.rm = TRUE), .groups = "drop")
  
  # Guardamos tabla
  assign(paste0("tabla_transiciones", sufijo, "vf_nuev_recod"), tabla, envir = .GlobalEnv)
  
  # Mostrar
  print(tabla %>% print(n = Inf))
}

generar_transiciones_ciidu(panel_1819_long, "facpanel1819", 2018, 2019, "1819")
generar_transiciones_ciidu(panel_1920_long, "facpanel1920", 2019, 2020, "1920")
generar_transiciones_ciidu(panel_2021_long, "facpanel2021", 2020, 2021, "2021")
generar_transiciones_ciidu(panel_2122_long, "facpanel2122", 2021, 2022, "2122")

# ==============================================================================
# MOVILIDAD INTERSECTORIAL – CONSTRUCCIÓN Y ANÁLISIS DE REDES
# ==============================================================================

#Clasificamos en sectores más grandes

clasificar_sector_macro <- function(codigo) {
  case_when(
    codigo <= 2                                      ~ "Agropecuario",
    codigo == 3                                      ~ "Pesca",
    codigo >= 5  & codigo <= 9                       ~ "Minería",
    codigo >= 10 & codigo <= 33                      ~ "Manufactura",
    codigo >= 41 & codigo <= 43                      ~ "Construcción",
    codigo >= 45 & codigo <= 47                      ~ "Comercio",
    (codigo >= 49 & codigo <= 53) | 
      (codigo >= 58 & codigo <= 63)                 ~ "TranspComunicac",
    codigo == 84                                     ~ "Gobierno",
    codigo >= 55 & codigo <= 56                      ~ "HotelesRest",
    codigo == 68 | (codigo >= 69 & codigo <= 82)     ~ "Inmobiliarias",
    codigo == 85                                     ~ "Educación",
    codigo %in% c(35:39, 64:66, 86:88, 90:93, 94:98, 99) ~ "OtrosServicios",
    TRUE ~ "NoClasificado"
  )
}

# ------------------------------------------------------------------------------
# 1. CREAR REDES DIRIGIDAS Y PONDERADAS POR AÑO
# ------------------------------------------------------------------------------

# Tablas de transiciones por año
lista_transiciones <- list(
  "2019" = tabla_transiciones1819vf_nuev_recod,
  "2020" = tabla_transiciones1920vf_nuev_recod,
  "2021" = tabla_transiciones2021vf_nuev_recod,
  "2022" = tabla_transiciones2122vf_nuev_recod
)

# Especificar columnas origen, destino y peso
columnas <- list(
  "2019" = list(origen = "sector_2018", destino = "sector_2019", peso = "poblacion"),
  "2020" = list(origen = "sector_2019", destino = "sector_2020", peso = "poblacion"),
  "2021" = list(origen = "sector_2020", destino = "sector_2021", peso = "poblacion"),
  "2022" = list(origen = "sector_2021", destino = "sector_2022", peso = "poblacion")
)

# Función para construir grafo dirigido y ponderado
crear_red <- function(df, origen, destino, peso) {
  df_limpio <- df %>%
    filter(!is.na(.data[[origen]]) & !is.na(.data[[destino]])) %>%
    group_by(origen = .data[[origen]], destino = .data[[destino]]) %>%
    summarise(peso = sum(.data[[peso]], na.rm = TRUE), .groups = "drop")
  
  g <- graph_from_data_frame(df_limpio, directed = TRUE)
  E(g)$weight <- df_limpio$peso
  return(g)
}

# Construir redes
redes_mob <- map2(lista_transiciones, columnas, function(df, cols) {
  crear_red(df, cols$origen, cols$destino, cols$peso)
})

# ------------------------------------------------------------------------------
# 2. MÉTRICAS Y GRAFICACIÓN DE REDES ORIGINALES (CON NODOS AISLADOS)
# ------------------------------------------------------------------------------

analisis_completo_red <- function(grafo, nombre = "Red", guardar_grafico = TRUE, carpeta_salida = "graficos_redes_originales") {
  if (is.null(E(grafo)$weight)) stop("La red no tiene pesos definidos.")
  if (guardar_grafico && !dir.exists(carpeta_salida)) dir.create(carpeta_salida)
  
  pesos <- E(grafo)$weight
  resumen <- tibble(
    nombre = nombre,
    nodos = gorder(grafo),
    aristas = gsize(grafo),
    densidad = edge_density(grafo),
    clustering = transitivity(grafo, type = "global"),
    componentes = components(grafo)$no,
    distancia_media = mean_distance(grafo, directed = TRUE),
    peso_min = min(pesos),
    peso_q1 = quantile(pesos, 0.25),
    peso_mediana = median(pesos),
    peso_q3 = quantile(pesos, 0.75),
    peso_max = max(pesos)
  )
  
  # Gráfico
  g_tbl <- as_tbl_graph(grafo) %>%
    mutate(
      sector = as.integer(name),
      macrosector = clasificar_sector_macro(sector),
      grado = centrality_degree(mode = "all")
    )
  
  # Contar cuántos sectores tiene cada macrosector
  conteo_macro <- g_tbl %>%
    as_tibble() %>%
    count(macrosector, name = "n")
  
  # Crear nombres personalizados como "Manufactura (24)"
  etiquetas_macro <- setNames(
    paste0(conteo_macro$macrosector, " (", conteo_macro$n, ")"),
    conteo_macro$macrosector
  )
  
  # Agregar columna con etiqueta para usar en leyenda
  g_tbl <- g_tbl %>%
    mutate(macrosector_label = etiquetas_macro[macrosector])
  
  # Crear paleta de colores personalizada con etiquetas actualizadas
  nombres_macro <- sort(unique(g_tbl$macrosector_label))
  colores <- RColorBrewer::brewer.pal(12, "Set3")
  paleta_colores <- setNames(colores[1:length(nombres_macro)], nombres_macro)
  
  # Ahora puedes pasar esto a ggraph:
  p <- ggraph(g_tbl, layout = "fr") +
    geom_edge_link(aes(width = weight), color = "gray60", alpha = 0.3) +
    geom_node_point(aes(size = grado, color = macrosector_label), alpha = 0.9) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_width(range = c(0.1, 2)) +
    scale_color_manual(
      values = RColorBrewer::brewer.pal(12, "Set3"),
      name = "Macrosector"
    ) +
    theme_void() +
    ggtitle(paste("Red de movilidad laboral", nombre))
  
  if (guardar_grafico) {
    ggsave(paste0(carpeta_salida, "/red_", nombre, ".png"), plot = p, width = 10, height = 8)
  }
  
  return(resumen)
}

# Aplicar análisis
resultados_redes_completas <- map2(redes_mob, names(redes_mob), analisis_completo_red)

# Exportar resultados si deseas
write.csv(bind_rows(resultados_redes_completas), "metricas_redes_completas.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. DETECCIÓN DE NODOS AISLADOS
# ------------------------------------------------------------------------------

# Revisar por año cuántos nodos están aislados
nodos_aislados_por_anio <- map(redes_mob, function(g) {
  comp <- components(g)
  V(g)$name[comp$membership != which.max(table(comp$membership))]
})

# Mostrar nodos aislados detectados por red
nodos_aislados_por_anio

# ------------------------------------------------------------------------------
# 4. MÉTRICAS Y GRAFICACIÓN DE COMPONENTE PRINCIPAL (SIN NODOS AISLADOS)
# ------------------------------------------------------------------------------

analisis_red_sin_aislados <- function(grafo, nombre = "Red", guardar_grafico = TRUE, carpeta_salida = "graficos_redes_sin_aislados") {
  if (is.null(E(grafo)$weight)) stop("La red no tiene pesos definidos.")
  if (guardar_grafico && !dir.exists(carpeta_salida)) dir.create(carpeta_salida)
  
  comp <- components(grafo)
  grafo <- induced_subgraph(grafo, comp$membership == which.max(table(comp$membership)))
  
  pesos <- E(grafo)$weight
  resumen <- tibble(
    nombre = nombre,
    nodos = gorder(grafo),
    aristas = gsize(grafo),
    densidad = edge_density(grafo),
    clustering = transitivity(grafo, type = "global"),
    componentes = 1,
    distancia_media = mean_distance(grafo, directed = TRUE),
    peso_min = min(pesos),
    peso_q1 = quantile(pesos, 0.25),
    peso_mediana = median(pesos),
    peso_q3 = quantile(pesos, 0.75),
    peso_max = max(pesos)
  )
  
  g_tbl <- as_tbl_graph(grafo) %>%
    mutate(
      sector = as.integer(name),
      macrosector = clasificar_sector_macro(sector),
      grado = centrality_degree(mode = "all")
    )
  
  # Conteo de sectores por macrosector
  conteo_macro <- g_tbl %>%
    as_tibble() %>%
    count(macrosector, name = "n")
  
  # Crear etiquetas como "Manufactura (24)"
  etiquetas_macro <- setNames(
    paste0(conteo_macro$macrosector, " (", conteo_macro$n, ")"),
    conteo_macro$macrosector
  )
  
  # Agregar columna de etiquetas al grafo
  g_tbl <- g_tbl %>%
    mutate(macrosector_label = etiquetas_macro[macrosector])
  
  # Crear paleta de colores personalizada con etiquetas actualizadas
  nombres_macro <- sort(unique(g_tbl$macrosector_label))
  colores <- RColorBrewer::brewer.pal(12, "Set3")
  paleta_colores <- setNames(colores[1:length(nombres_macro)], nombres_macro)
  
  # Ahora puedes pasar esto a ggraph:
  p <- ggraph(g_tbl, layout = "fr") +
    geom_edge_link(aes(width = weight), color = "gray60", alpha = 0.3) +
    geom_node_point(aes(size = grado, color = macrosector_label), alpha = 0.9) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_width(range = c(0.1, 2)) +
    scale_color_manual(
      values = RColorBrewer::brewer.pal(12, "Set3"),
      name = "Macrosector"
    ) +
    theme_void() +
    ggtitle(paste("Red de movilidad laboral", nombre))
  
  if (guardar_grafico) {
    ggsave(paste0(carpeta_salida, "/red_", nombre, ".png"), plot = p, width = 10, height = 8)
  }
  
  return(resumen)
}

# Aplicar análisis solo al componente principal
resultados_redes_principales <- map2(redes_mob, names(redes_mob), analisis_red_sin_aislados)

# Exportar resultados si deseas
write.csv(bind_rows(resultados_redes_principales), "metricas_redes_finales.csv", row.names = FALSE)



# ==============================================================================
# APLICACIÓN DEL DISPARITY FILTER PARA REDES LABORALES
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. DEFINIR VALORES DE ALPHA A ANALIZAR
# ------------------------------------------------------------------------------
valores_alpha <- c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5)

# ------------------------------------------------------------------------------
# 2. FUNCIÓN PARA APLICAR DISPARITY FILTER A UNA RED
# ------------------------------------------------------------------------------
aplicar_disparity <- function(grafo, alpha) {
  mat <- as_adjacency_matrix(grafo, attr = "weight", sparse = FALSE)
  backbone::disparity(
    W = mat,
    alpha = alpha,
    class = "igraph",
    signed = FALSE,
    narrative = FALSE
  )
}

# ------------------------------------------------------------------------------
# 3. FUNCIÓN PARA RESUMIR MÉTRICAS DE RED FILTRADA
# ------------------------------------------------------------------------------
resumen_red_filtrada <- function(grafo_filtrado) {
  tibble(
    n_nodos = gorder(grafo_filtrado),
    n_aristas = gsize(grafo_filtrado),
    densidad = edge_density(grafo_filtrado),
    componentes = components(grafo_filtrado)$no,
    nodos_aislados = sum(degree(grafo_filtrado) == 0),
    grado_medio = mean(degree(grafo_filtrado))
  )
}

# ------------------------------------------------------------------------------
# 4. APLICAR DISPARITY FILTER A CADA RED Y GUARDAR MÉTRICAS
# ------------------------------------------------------------------------------
resultado_disparity <- map_dfr(
  names(redes_mob),
  function(anio) {
    grafo <- redes_mob[[anio]]
    map_dfr(valores_alpha, function(alpha) {
      grafo_filtrado <- aplicar_disparity(grafo, alpha)
      resumen <- resumen_red_filtrada(grafo_filtrado)
      mutate(resumen, anio = anio, alpha = alpha)
    })
  }
)

# ------------------------------------------------------------------------------
# 5. GUARDAR RESULTADOS Y GRAFICAR SENSIBILIDAD
# ------------------------------------------------------------------------------
write.csv(resultado_disparity, "resultadosDF.csv", row.names = FALSE)

ggplot(resultado_disparity, aes(x = alpha, y = n_aristas, color = anio)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Aristas retenidas según alpha (Disparity Filter)",
    x = expression(alpha),
    y = "Número de aristas",
    color = "Año"
  ) +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------------------------
# 6. GUARDAR REDES FILTRADAS EN LISTA
# ------------------------------------------------------------------------------
redes_filtradas <- expand.grid(anio = names(redes_mob), alpha = valores_alpha) %>%
  mutate(
    red_filtrada = map2(anio, alpha, ~ aplicar_disparity(redes_mob[[.x]], .y))
  )

# ==============================================================================
# DETECCIÓN DE COMUNIDADES CON LEIDEN (BACKBONE α = 0.35)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Filtrar redes con alpha = 0.35
# ------------------------------------------------------------------------------
redes_leiden_035 <- redes_filtradas %>%
  filter(alpha == 0.35)

# ------------------------------------------------------------------------------
# 2. Función para aplicar Leiden y calcular métricas
# ------------------------------------------------------------------------------
aplicar_leiden <- function(grafo, resolution = 1) {
  grafo_undir <- as_undirected(grafo, mode = "collapse")
  comunidad <- cluster_leiden(grafo_undir, resolution_parameter = resolution)
  
  tibble(
    modularidad = modularity(grafo_undir, membership(comunidad)),
    n_comunidades = length(unique(membership(comunidad))),
    tamano_promedio = mean(sizes(comunidad)),
    comunidad_objeto = list(comunidad)
  )
}

# ------------------------------------------------------------------------------
# 3. Aplicar Leiden por año y guardar métricas
# ------------------------------------------------------------------------------
resultados_leiden <- redes_leiden_035 %>%
  mutate(
    resultado = map(red_filtrada, aplicar_leiden)
  ) %>%
  unnest(resultado)

# ------------------------------------------------------------------------------
# 4. Guardar resultados y mostrar tabla
# ------------------------------------------------------------------------------
write.csv(resultados_leiden %>% select(anio, alpha, modularidad, n_comunidades, tamano_promedio),
          "metricas_leiden_035.csv", row.names = FALSE)

print(resultados_leiden %>% select(anio, alpha, modularidad, n_comunidades, tamano_promedio))

# ------------------------------------------------------------------------------
# 5. Visualización de evolución de modularidad
# ------------------------------------------------------------------------------
ggplot(resultados_leiden, aes(x = as.numeric(anio), y = modularidad)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +
  geom_point(size = 2.5, color = "#0072B2") +
  labs(
    title = "Evolución de la modularidad (Leiden, α = 0.35)",
    x = "Año",
    y = "Modularidad"
  ) +
  theme_minimal(base_size = 14)

# ==============================================================================
# DETECCIÓN DE COMUNIDADES CON LOUVAIN (BACKBONE α = 0.35)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Filtrar redes con alpha = 0.35
# ------------------------------------------------------------------------------
redes_louvain_035 <- redes_filtradas %>%
  filter(alpha == 0.35)

# ------------------------------------------------------------------------------
# 2. Función para aplicar Louvain y calcular métricas
# ------------------------------------------------------------------------------
aplicar_louvain <- function(grafo) {
  grafo_undir <- as.undirected(grafo, mode = "collapse")
  comunidad <- cluster_louvain(grafo_undir)
  
  tibble(
    modularidad = modularity(grafo_undir, membership(comunidad)),
    n_comunidades = length(unique(membership(comunidad))),
    tamano_promedio = mean(sizes(comunidad)),
    comunidad_objeto = list(comunidad)
  )
}

# ------------------------------------------------------------------------------
# 3. Aplicar Louvain por año y guardar métricas
# ------------------------------------------------------------------------------
resultados_louvain <- redes_louvain_035 %>%
  mutate(
    resultado = map(red_filtrada, aplicar_louvain)
  ) %>%
  unnest(resultado)

# ------------------------------------------------------------------------------
# 4. Guardar resultados
# ------------------------------------------------------------------------------
write.csv(resultados_louvain %>% select(anio, alpha, modularidad, n_comunidades, tamano_promedio),
          "metricas_louvain_035.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Visualizar evolución de la modularidad
# ------------------------------------------------------------------------------
ggplot(resultados_louvain, aes(x = as.numeric(anio), y = modularidad)) +
  geom_line(linewidth = 1.2, color = "#D55E00") +
  geom_point(size = 2.5, color = "#D55E00") +
  labs(
    title = "Evolución de la modularidad (Louvain, α = 0.35)",
    x = "Año",
    y = "Modularidad"
  ) +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------------------------
# CREAR TABLA DE COMUNIDADES POR SECTOR Y AÑO
# ------------------------------------------------------------------------------
comunidades_df <- redes_louvain_035 %>%
  mutate(
    comunidad = map(red_filtrada, ~ {
      g_undir <- as.undirected(.x, mode = "collapse")
      membership(cluster_louvain(g_undir))
    })
  ) %>%
  unnest_longer(comunidad, indices_to = "sector") %>%
  select(anio, sector, comunidad)

# Guardar tabla
write.csv(comunidades_df, "comunidades_louvain_035.csv", row.names = FALSE)

# ==============================================================================
# VISUALIZACIÓN DE COMUNIDADES LOUVAIN (α = 0.35)
# ==============================================================================

# Crear carpeta si no existe
dir.create("graficos_comunidades_louvain_reasignada", showWarnings = FALSE)
dir.create("tablas_comunidades_louvain_reasignada", showWarnings = FALSE)

# Filtrar redes con alpha = 0.35
redes_louvain_035 <- redes_filtradas %>%
  filter(alpha == 0.35)

# Función para procesar un año
graficar_comunidades_louvain <- function(grafo, anio) {
  
  # 1. Eliminar nodos aislados
  grafo_limpio <- delete_vertices(grafo, degree(grafo) == 0)
  grafo_undir <- as.undirected(grafo_limpio, mode = "collapse")
  
  # 2. Detectar comunidades
  comunidad_obj <- cluster_louvain(grafo_undir)
  comunidad_raw <- membership(comunidad_obj)
  
  # 3. Tabla de membresía original
  membresias <- tibble(
    sector = V(grafo_limpio)$name,
    comunidad = comunidad_raw
  )
  
  # 4. Reasignar comunidades por tamaño (comunidad 1 = la más grande)
  membresias_ordenadas <- membresias %>%
    group_by(comunidad) %>%
    mutate(tam = n()) %>%
    ungroup() %>%
    arrange(desc(tam)) %>%
    mutate(comunidad_reasignada = as.integer(factor(comunidad, levels = unique(comunidad)))) %>%
    select(sector, comunidad = comunidad_reasignada)
  
  # 5. Crear tidygraph con comunidad unida
  g_tbl <- as_tbl_graph(grafo_limpio) %>%
    mutate(grado_total = centrality_degree()) %>%
    left_join(membresias_ordenadas, by = c("name" = "sector")) %>%
    mutate(comunidad = factor(comunidad))
  
  # 6. Paleta de colores automática
  n_colores <- n_distinct(membresias_ordenadas$comunidad)
  colores <- colores <- paletteer::paletteer_d("RColorBrewer::Set3", n = n_colores) %>% as.character() %>% as.character()
  
  # 7. Graficar
  set.seed(123)
  p <- ggraph(g_tbl, layout = "kk") +
    geom_edge_link(alpha = 0.3, color = "gray70") +
    geom_node_point(aes(size = grado_total, color = comunidad), alpha = 0.9) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_color_manual(values = colores) +
    scale_size(range = c(2, 8)) +
    theme_void() +
    labs(
      title = paste("Comunidades sectoriales -", anio, "(Louvain, α = 0.35)"),
      color = "Comunidad"
    )
  
  # 8. Guardar gráfico
  ggsave(
    filename = paste0("graficos_comunidades_louvain_reasignada/red_comunidades_", anio, ".png"),
    plot = p, width = 10, height = 7, dpi = 300
  )
  
  # 9. Guardar tabla
  write.csv(membresias_ordenadas,
            paste0("tablas_comunidades_louvain_reasignada/sectores_por_comunidad_", anio, ".csv"),
            row.names = FALSE)
}

# Aplicar a todos los años
walk2(redes_louvain_035$red_filtrada, redes_louvain_035$anio, graficar_comunidades_louvain)

# ------------------------------------------------------------------------------
# COMPARACIÓN LEIDEN Y LOUVAIN (PARA CONCLUIR)
# ------------------------------------------------------------------------------

# Función para aplicar Louvain y Leiden sobre grafo limpio
comparar_algoritmos <- function(grafo, anio) {
  
  # 1. Eliminar nodos aislados (muy importante después del disparity)
  grafo_clean <- delete_vertices(grafo, degree(grafo) == 0)
  grafo_undir <- as.undirected(grafo_clean, mode = "collapse")
  
  # 2. Louvain
  louvain <- cluster_louvain(grafo_undir)
  
  # 3. Leiden con distintas resoluciones
  leiden1   <- cluster_leiden(grafo_undir, resolution_parameter = 1)
  leiden05  <- cluster_leiden(grafo_undir, resolution_parameter = 0.5)
  leiden15  <- cluster_leiden(grafo_undir, resolution_parameter = 1.5)
  
  # 4. Consolidar resultados
  tibble(
    anio = anio,
    metodo = c("Louvain", "Leiden_0.5", "Leiden_1", "Leiden_1.5"),
    modularidad = c(
      modularity(grafo_undir, membership(louvain)),
      modularity(grafo_undir, membership(leiden05)),
      modularity(grafo_undir, membership(leiden1)),
      modularity(grafo_undir, membership(leiden15))
    ),
    n_comunidades = c(
      length(unique(membership(louvain))),
      length(unique(membership(leiden05))),
      length(unique(membership(leiden1))),
      length(unique(membership(leiden15)))
    )
  )
}

# Aplicar a todos los años
comparacion_algoritmos <- map2_dfr(
  redes_louvain_035$red_filtrada,
  redes_louvain_035$anio,
  comparar_algoritmos
)

# Ver resultados
print(comparacion_algoritmos)

# Guardar tabla si deseas
write.csv(comparacion_algoritmos, "comparacion_louvain_leiden_redes_limpias.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# HEATMAP DE SECTORES POR COMUNIDAD Y AÑO (α = 0.35)
# ------------------------------------------------------------------------------

# Función para clasificar macrosector a partir del código CIIU-2D


# Leer tablas de comunidades reasignadas por año
archivos_comunidades <- list.files("tablas_comunidades_louvain_reasignada", full.names = TRUE)

comunidades_df <- map_dfr(archivos_comunidades, function(archivo) {
  anio <- str_extract(archivo, "\\d{4}")
  read.csv(archivo) %>%
    mutate(anio = anio)
}) %>%
  mutate(
    sector = as.numeric(sector),
    comunidad = as.factor(comunidad),
    macrosector = clasificar_sector_macro(sector)
  )

# Preparar data para el heatmap
heatmap_facet <- comunidades_df %>%
  mutate(
    anio = factor(anio),
    sector = fct_reorder(as.character(sector), sector)
  ) %>%
  select(anio, sector, comunidad, macrosector) %>%
  pivot_wider(names_from = anio, values_from = comunidad) %>%
  pivot_longer(cols = starts_with("20"), names_to = "anio", values_to = "comunidad") %>%
  mutate(
    comunidad = as.factor(comunidad),
    anio = factor(anio)
  )

# Graficar sin etiquetas internas
plot_facet <- ggplot(heatmap_facet, aes(x = anio, y = sector, fill = comunidad)) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "turbo", na.value = "grey90") +
  facet_wrap(~ macrosector, scales = "free_y", ncol = 3) +
  labs(
    title = "Comunidades sectoriales por año, por macrosector (Louvain, α = 0.35)",
    x = "Año",
    y = "Código CIIU-2D",
    fill = "Comunidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

# Guardar imagen en alta calidad
ggsave("heatmap_comunidades_facetado_sin_etiquetas.png", plot = plot_facet, width = 16, height = 12, dpi = 300)
ggsave("heatmap_comunidades_facetado_sin_etiquetas.pdf", plot = plot_facet, width = 16, height = 12)

# ------------------------------------------------------------------------------
# PERSISTENCIA DE SECTORES EN REDES (2019–2022)
# ------------------------------------------------------------------------------

# 1. Calcular número de años en que aparece cada sector
persistencia <- comunidades_df %>%
  distinct(sector, anio) %>%  # elimina duplicados sector-año
  count(sector, name = "anios_presentes") %>%  # cuenta cuántos años estuvo presente
  mutate(
    estabilidad = case_when(
      anios_presentes == 4 ~ "Alta",
      anios_presentes == 3 ~ "Media",
      anios_presentes == 2 ~ "Baja",
      TRUE                 ~ "Muy Baja"
    ),
    sector = as.character(sector)  # para que se ordene bien en gráfico
  )

#Graficamos
grafico_persistencia <- ggplot(persistencia, aes(x = fct_reorder(sector, anios_presentes), y = anios_presentes, fill = estabilidad)) +
  geom_col() +
  labs(
    title = "Persistencia de sectores en redes (2019–2022)",
    x = "Sector (CIIU-2D)",
    y = "Número de años presente"
  ) +
  scale_fill_manual(
    values = c("Alta" = "#1b9e77", "Media" = "#d95f02", "Baja" = "#7570b3", "Muy Baja" = "#e7298a")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
    legend.title = element_blank()
  )

# Guardar gráfico
ggsave("persistencia_sectores.png", plot = grafico_persistencia, width = 12, height = 6, dpi = 300)

# ==============================================================================
# SANKEY DE TRANSICIÓN COMUNITARIA
# ==============================================================================

# 1. Formato ancho
sankey_data <- comunidades_df %>%
  tidyr::pivot_wider(names_from = anio, values_from = comunidad, names_prefix = "comunidad_") %>%
  filter(!is.na(comunidad_2019), !is.na(comunidad_2020),
         !is.na(comunidad_2021), !is.na(comunidad_2022))

# 2. Formato largo para Sankey
sankey_long <- sankey_data %>%
  pivot_longer(cols = starts_with("comunidad_"),
               names_to = "anio", values_to = "comunidad") %>%
  mutate(anio = as.integer(gsub("comunidad_", "", anio)))

# 3. Diagrama Sankey
ggplot(sankey_long,
       aes(x = anio, stratum = comunidad, alluvium = sector,
           fill = comunidad, label = comunidad)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", color = "gray80") +
  geom_stratum() +
  scale_x_continuous(breaks = 2019:2022) +
  theme_minimal() +
  labs(
    title = "Transición de comunidades sectoriales (2019–2022)",
    x = "Año", y = "Sectores",
    fill = "Comunidad (Louvain)"
  )

# ==============================================================================
# BARRAS APLICADAS POR AÑO (PROPORCIÓN DE SECTORES POR COMUNIDAD)
# ==============================================================================

barras_data <- comunidades_df %>%
  group_by(anio, comunidad) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(anio) %>%
  mutate(prop = n / sum(n))

# Paleta extendida
n_comunidades <- length(unique(barras_data$comunidad))
colores <- colorRampPalette(brewer.pal(12, "Set3"))(n_comunidades)

# Graficar
ggplot(barras_data, aes(x = factor(anio), y = prop, fill = as.factor(comunidad))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colores) +
  theme_minimal() +
  labs(
    title = "Distribución de sectores por comunidad Louvain",
    x = "Año", y = "Proporción",
    fill = "Comunidad (Louvain)"
  )

# ==============================================================================
# MATRIZ DE TRANSICIÓN COMUNIDAD-COMUNIDAD (AÑO A AÑO)
# ==============================================================================
pares_anios <- list(c(2019, 2020), c(2020, 2021), c(2021, 2022))

persistencia_lista <- map(pares_anios, function(par) {
  año1 <- par[1]
  año2 <- par[2]
  
  df1 <- comunidades_df %>% filter(anio == año1) %>% rename(com_1 = comunidad)
  df2 <- comunidades_df %>% filter(anio == año2) %>% rename(com_2 = comunidad)
  
  full_join(df1, df2, by = "sector") %>%
    mutate(paso = paste0(año1, "_", año2)) %>%
    select(paso, sector, com_1, com_2)
})

persistencia_comunidades <- bind_rows(persistencia_lista)

tabla_resumen <- persistencia_comunidades %>%
  count(paso, com_1, com_2) %>%
  group_by(paso, com_1) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(tabla_resumen, aes(x = factor(com_2), y = factor(com_1), fill = prop)) +
  geom_tile(color = "white") +
  facet_wrap(~ paso) +
  scale_fill_viridis_c(labels = scales::percent_format(), name = "Proporción") +
  labs(
    x = "Comunidad en año t+1",
    y = "Comunidad en año t",
    title = "Transición de comunidades Louvain entre años"
  ) +
  theme_minimal()

# ==============================================================================
# MATRIZ BINARIA DE SECTORES POR COMUNIDAD (PRESENCIA)
# ==============================================================================

# Crear matriz binaria: comunidad × sector
matriz_binaria <- comunidades_df %>%
  mutate(valor = 1, sector = as.character(sector)) %>%  # sector como texto para columnas
  pivot_wider(names_from = sector, values_from = valor, values_fill = list(valor = 0))

# Extraer comunidades (filas) y nombres de columnas (sectores)
comunidades <- matriz_binaria$comunidad
matriz_binaria <- matriz_binaria[, -1]  # quitar columna comunidad

# Convertir a matriz numérica (evitando coerción incorrecta)
matriz <- matriz_binaria %>%
  select(where(is.numeric)) %>%
  as.matrix()

# Verificar si hay NAs
anyNA(matriz)  # debe ser FALSE

# Asignar nombres de fila
rownames(matriz) <- comunidades

# Transponer: sectores como filas, comunidades como columnas
matriz_suma <- t(matriz)

# Heatmap
pheatmap(
  matriz_suma,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("gray90", "blue", "red"))(25),
  main = "Participación sectorial en comunidades (2019–2022)",
  fontsize = 8,
  angle_col = 45
)

# Asegurar que el sector es numérico (para orden correcto)
comunidades_df <- comunidades_df %>%
  mutate(sector = as.character(sector),
         valor = 1)

# Matriz sector × año, valores binarios
matriz_anio <- comunidades_df %>%
  distinct(sector, anio) %>%
  mutate(valor = 1) %>%
  pivot_wider(names_from = anio, values_from = valor, values_fill = 0) %>%
  arrange(as.numeric(sector))  # orden por código

# Guardar nombres de sectores
sectores <- matriz_anio$sector
matriz_anio <- matriz_anio[, -1]  # quitar columna de nombres

# Convertir a matriz numérica
matriz_anio <- as.matrix(matriz_anio)
rownames(matriz_anio) <- sectores

# Visualización
pheatmap(
  matriz_anio,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("gray90", "red"))(10),
  main = "Presencia de sectores por año (2019–2022)",
  fontsize = 9,
  angle_col = 45
)
#
# Instalar si no lo tienes
#install.packages("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)

#a

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("ComplexHeatmap")
#library(ComplexHeatmap)


# Convertir a matriz numérica
matriz <- matriz_binaria %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

# Asignar nombres
try({
  rownames(matriz) <- comunidades
  colnames(matriz) <- sectores
}, silent = TRUE)

# Crear anotación de columnas (por ejemplo, código CIIU de 1 o 2 letras si lo tienes)
# Aquí usamos etiquetas ficticias A–O (puedes reemplazarlas por algo real)
codigo_sec <- rep(LETTERS[1:5], length.out = ncol(matriz))
col_annotation <- HeatmapAnnotation(
  sec = codigo_sec,
  col = list(sec = structure(1:5, names = LETTERS[1:5]))
)


#Validamos

dim(matriz) 

length(comunidades)  
length(sectores)     

# Justo antes de convertir
sectores <- colnames(matriz_binaria)
comunidades <- rownames(matriz_binaria)

# Convertir a matriz numérica
matriz <- matriz_binaria %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

# Asignar nombres (si coinciden las dimensiones)
rownames(matriz) <- comunidades
colnames(matriz) <- sectores

names(matriz_binaria)

sapply(matriz_binaria, class)


# Filtrar solo columnas numéricas válidas
matriz_binaria_filtrada <- matriz_binaria %>%
  select(where(~ all(.x %in% c(0, 1, TRUE, FALSE))))

# Guardar nombres limpios
sectores <- colnames(matriz_binaria_filtrada)
comunidades <- rownames(matriz_binaria_filtrada)

# Convertir a matriz numérica
matriz <- matriz_binaria_filtrada %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

# Asignar nombres
rownames(matriz) <- comunidades
colnames(matriz) <- sectores


# -------------------------------------------------------------
# Verificamos que la matriz esté bien construida
# -------------------------------------------------------------
# matriz: matriz numérica (comunidades x sectores)
# rownames: comunidades
# colnames: sectores
# Dimensiones deben coincidir
dim(matriz)           # ej. [137, 80]
head(rownames(matriz))
head(colnames(matriz))

set.seed(123)
sector_colors <- setNames(colorRampPalette(brewer.pal(12, "Set3"))(ncol(matriz)), colnames(matriz))

# -------------------------------------------------------------
# Crear el heatmap
# -------------------------------------------------------------
Heatmap(
  matriz,
  name = "Presencia",
  col = colorRamp2(c(0, 1), c("white", "red")),
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  show_row_dend = TRUE,
  show_column_dend = TRUE,
  show_row_names = FALSE,         # Puedes poner TRUE si deseas ver comunidades
  column_names_gp = gpar(fontsize = 6),  # Tamaño de nombres de columnas
  column_title = "Sectores",
  row_title = "Comunidades",
  heatmap_legend_param = list(title = "Presencia"),
  top_annotation = HeatmapAnnotation(
    sector = colnames(matriz),
    col = list(sector = sector_colors),
    show_annotation_name = FALSE,
    simple_anno_size = unit(2, "mm")
  )
)

# ==============================================================================
# AGRUPAMIENTO JERÁRQUICO DE COMUNIDADES LOUVAIN
# ==============================================================================

library(ComplexHeatmap)

# Extraer códigos y macrosectores
codigos_sectores <- as.numeric(colnames(matriz))
macrosectores <- clasificar_sector_macro (codigos_sectores)

# Paleta de colores (puedes personalizar)
colores_macro <- structure(
  RColorBrewer::brewer.pal(length(unique(macrosectores)), "Paired"),
  names = unique(macrosectores)
)

#####Sin grupo

# Anotación superior
anotacion_columnas <- HeatmapAnnotation(
  Macrosector = macrosectores,
  col = list(Macrosector = colores_macro),
  annotation_name_side = "left",
  show_annotation_name = TRUE
)

# Ordenar columnas por código
orden_columnas <- order(as.numeric(colnames(matriz)))
matriz_ordenada <- matriz[, orden_columnas]
macrosectores_ordenados <- macrosectores[orden_columnas]

# Crear heatmap
heatmap_final <- Heatmap(
  matriz_ordenada,
  name = "Persistencia",
  col = c("0" = "white", "1" = "Black"),
  cluster_rows = TRUE,
  cluster_columns = FALSE,
  show_column_names = TRUE,
  show_row_names = FALSE,
  column_title = "Sectores",
  row_title = "Comunidades",
  top_annotation = anotacion_columnas,
  heatmap_legend_param = list(title = "Presencia")
)

# Exportar
pdf("heatmap_comunidades_louvain.pdf", width = 12, height = 10)
draw(heatmap_final)
dev.off()

#######Por grupos

# Cortar el árbol jerárquico en 6 grupos
hc_filas <- hclust(dist(matriz_ordenada))
grupos_filas <- cutree(hc_filas, k = 6)

# Anotar los grupos en el heatmap
anotacion_filas <- try({
  rowAnnotation(
    Grupo = factor(grupos_filas),
    col = list(Grupo = RColorBrewer::brewer.pal(6, "Set1"))
  )
}, silent = TRUE)

# Vector nombrado para los colores
niveles_grupos <- levels(factor(grupos_filas))
colores_grupos <- setNames(RColorBrewer::brewer.pal(6, "Set1"), niveles_grupos)

# Anotación con colores nombrados
anotacion_filas <- rowAnnotation(
  Grupo = factor(grupos_filas),
  col = list(Grupo = colores_grupos)
)

# Heatmap final con grupos resaltados
heatmap_final_grupos <- Heatmap(
  matriz_ordenada,
  name = "Persistencia",
  col = c("0" = "white", "1" = "black"),
  cluster_rows = TRUE,
  cluster_columns = FALSE,
  show_column_names = TRUE,
  show_row_names = FALSE,
  column_title = "Sectores",
  row_title = "Comunidades",
  top_annotation = anotacion_columnas,
  left_annotation = anotacion_filas,
  heatmap_legend_param = list(title = "Presencia")
)

# Exportar en PDF
pdf("heatmap_comunidades_louvain_grupos.pdf", width = 13, height = 10)
draw(heatmap_final_grupos)
dev.off()

