library(shiny)
library(shinyWidgets)
library(bslib)
library(broom)
library(dplyr)
library(plotly)
library(ggplot2)
library(thematic)
library(RPostgreSQL)
 
# Definir UI --------------------------------------------------------------------
 
comunidades <- c("Todas", "Andalucía", "Aragón", "Asturias", "Baleares",
                 "Canarias", "Castilla-La Mancha", "Castilla y León",
                 "Cataluña", "Extremadura", "Galicia", "Madrid",
                 "Murcia", "Navarra", "País Vasco", "La Rioja")
 
capas <- c("Total", "Línea Declaración", "Pastos Comunales")
 
ui <- fluidPage(      
  theme = bs_theme(version = 5, bootswatch = "morph"),
  titlePanel(div(h1(HTML("<b>EDAMON</b> | Control Queries"), align = "center"),
                 style = "background-color:#C8D1DE; position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 90px;")),
  fixedRow(
    column(
      2,
      align = "center",
      style = "margin-top:66px;background-color:#485785",
      # inputs
      div(
        h5(HTML("<b>Omitir campos vacíos</b>"),
           style = "margin-bottom:20px;color:#ffffff"),
        switchInput(
          inputId = "switch",
          value = TRUE,
          label = icon("eye-slash", "fa-solid"),
          onStatus = "dark",
          offStatus = "dark",
        ),
        style = "margin-top:60px;color:#ffffff"
      ),
      div(
        h5(HTML("<b>Ver campos combinados</b>"),
           style = "margin-bottom:20px;color:#ffffff"),
        switchInput(
          inputId = "switch2",
          value = TRUE,
          label = icon("chart-simple", "fa-solid"),
          onStatus = "dark",
          offStatus = "dark",
        ),
        style = "margin-top:24px;color:#ffffff"
      ),
      div(
        h5(HTML("<b>Filtrar por Comunidad Autónoma</b>"),
           style = "margin-bottom:20px;color:#ffffff"),
        div(
          pickerInput(
            inputId = "picker",
            label = "Seleccione una Comunidad Autónoma:",
            choices = setNames(0:15, comunidades),
            selected = 0,
            choicesOpt = list(
              style = c("font-weight: bold;")
            )
          )
        ), 
        style = "margin-top:24px;color:#ffffff"
      ),
      div(
        h5(HTML("<b>Filtrar por capa</b>"),
           style = "margin-bottom:20px;color:#ffffff"),
        div(
          pickerInput(
            inputId = "picker2",
            label = "Seleccione una capa:",
            choices = capas,
            selected = 0,
            choicesOpt = list(
              style = c("font-weight: bold;")
            )
          )
        ), 
        style = "margin-top:24px;color:#ffffff"
      ),
      div(
        h5(HTML("<b>Filtrar por fechas</b>"),
           style = "margin-bottom:20px;color:#ffffff"),
        dateRangeInput(
          inputId = "calendario",
          label = "Seleccione un rango de fechas:",
          start = "2022-12-01",
          end = Sys.Date(),
          min = "2022-12-01",
          max = Sys.Date(),
          separator = "---",
          language = "es"
        ),
        style = "margin-top:24px;margin-bottom:139px;color:#ffffff"
      )
    ),
    column(
      10,
      # outputs
      div((plotlyOutput("barras")), style = "margin-top:100px"),
      div((plotlyOutput("temporal")), style = "margin-top:10px")
    )
  )
)
 
thematic_shiny()
 
 
# Definir server ----------------------------------------------------------------
 
server <- function(input, output, session) {
 
  # Matar conexiones anteriores a DB
 
  killDbConnections <- function() {
 
    all_cons <- dbListConnections(PostgreSQL())
 
    print(all_cons)
 
    for(con in all_cons)
      dbDisconnect(con)
 
    print(paste(length(all_cons), " connections killed."))
 
  }
 
  killDbConnections()
 
  # Conexion a DB
 
  con <- dbConnect(drv = PostgreSQL(), 
                   user = "privado", 
                   password = "privado",
                   host = "privado", 
                   port = privado, 
                   dbname = "privado")
 
  print("Connection created")
 
  # Queries ---------------------------------------------------------------------
 
  fecha_inicio <- reactive(input$calendario[1])
  fecha_fin <- reactive(input$calendario[2])
 
  observe(print(fecha_inicio()))
  observe(print(fecha_fin()))
 
  secuencia_fechas <- reactive({
    data.frame(date = seq(fecha_inicio(), fecha_fin(), by = "day"))
  })
 
  # Barras
 
  df1 <- reactive({
    query <- paste0("SELECT * FROM control_queries_des WHERE DATE(fecha) >= '", 
                    fecha_inicio(), "' AND DATE(fecha) <='", fecha_fin(), "'")
 
    df1 <- dbGetQuery(con, query)
  })
 
  ccaa <- reactive(input$picker)
 
  ccaa0 <- reactive(sprintf("%02d", as.numeric(ccaa())))
 
  observe(print(ccaa0()))
 
  df2 <- reactive({
    query2 <- paste0(
      "SELECT * FROM control_queries_des WHERE ca_expediente = '", ccaa0(),
      "' AND DATE(fecha) >= '", fecha_inicio(), "' AND DATE(fecha) <='", 
      fecha_fin(), "'")
 
    df2 <- dbGetQuery(con, query2)
  })
 
  # Series temporales
 
  df3 <- reactive({
    query3 <- paste0(
      "SELECT DATE(fecha) AS date, COUNT(*) FROM control_queries_des WHERE DATE(fecha) >= '",
      fecha_inicio(),"' AND DATE(fecha) <='", fecha_fin(), "' GROUP BY DATE(fecha)")
 
    df3 <- dbGetQuery(con, query3)
  })
 
  df3f <- reactive({
    df3 <- df3()
    if (nrow(df3) == 0) {
      # si no hay filas en df3, devuelve un marco de datos vacío
      return(data.frame(date = character(), count = integer()))
    } else {
      # de lo contrario, une df3 y secuencia_fechas() y reemplaza NAs con 0s
      return(left_join(secuencia_fechas(), df3, by = "date") %>%
               mutate(count = ifelse(is.na(count), 0, count)))
    }
  })
 
  df4 <- reactive({
    query4 <- paste0(
      "SELECT DATE(fecha) AS date, COUNT(*) FROM control_queries_des WHERE ca_expediente 
      = '", ccaa0(), "' AND DATE(fecha) >= '", fecha_inicio(),
      "' AND DATE(fecha) <='", fecha_fin(), "' GROUP BY DATE(fecha)")
 
    df4 <- dbGetQuery(con, query4)
  })
 
  df4f <- reactive({
    df4 <- df4()
    if (nrow(df4) == 0) {
      # si no hay filas en df4, devuelve un marco de datos vacío
      return(data.frame(date = character(), count = integer()))
    } else {
      # de lo contrario, une df4 y secuencia_fechas() y reemplaza NAs con 0s
      return(left_join(secuencia_fechas(), df4, by = "date") %>%
               mutate(count = ifelse(is.na(count), 0, count)))
    }
  })
 
  type <- reactive(input$picker2)
  observe(print(type()))
 
  df5 <- reactive({
    query5 <- paste0("SELECT * FROM control_queries_des WHERE type = 1 AND DATE(fecha) >= '", 
                    fecha_inicio(), "' AND DATE(fecha) <='", fecha_fin(), "'")
 
    df5 <- dbGetQuery(con, query5)
  })
 
  df6 <- reactive({
    print("hola")
    query6 <- paste0("SELECT * FROM control_queries_des WHERE type IS NULL AND DATE(fecha) >= '", 
                    fecha_inicio(), "' AND DATE(fecha) <='", fecha_fin(), "'")
    print(query6)
    df6 <- dbGetQuery(con, query6)
  })
 
  df7 <- reactive({
    query7 <- paste0("SELECT * FROM control_queries_des WHERE type = 1 AND ca_expediente = '",
                     ccaa0(), "' AND DATE(fecha) >= '", 
                     fecha_inicio(), "' AND DATE(fecha) <='", fecha_fin(), "'")
    print(query7)
    df7 <- dbGetQuery(con, query7)
  })
 
  df8 <- reactive({
    query8 <- paste0("SELECT * FROM control_queries_des WHERE type IS NULL AND ca_expediente = '",
                     ccaa0(), "' AND DATE(fecha) >= '", 
                     fecha_inicio(), "' AND DATE(fecha) <='", fecha_fin(), "'")
    print(query8)
    df8 <- dbGetQuery(con, query8)
  })
 
 
  # Manejo de datos -------------------------------------------------------------
 
  sumatoriasR <- reactive({
    f1 <- sum(df1()$ld_recintosigpac_agregado, na.rm = TRUE)
    f2 <- sum(df1()$mo_ayudas_ayuda_ld, na.rm = TRUE)
    f3 <- sum(df1()$fenviormn, na.rm = TRUE)
    f4 <- sum(df1()$id_ciclo, na.rm = TRUE)
    f5 <- sum(df1()$ld_recintosigpac_lineadeclaracion, na.rm = TRUE)
    f6 <- sum(df1()$ld_recintosigpac_muni, na.rm = TRUE)
    f7 <- sum(df1()$num_particionenvio, na.rm = TRUE)
    f8 <- sum(df1()$numexpediente, na.rm = TRUE)
    f9 <- sum(df1()$ld_recintosigpac_parcela, na.rm = TRUE)
    f10 <- sum(df1()$ld_recintosigpac_poligono, na.rm = TRUE)
    f11 <- sum(df1()$ld_recintosigpac_prov, na.rm = TRUE)
    f12 <- sum(df1()$nombre_provincia, na.rm = TRUE)
    f13 <- sum(df1()$ld_recintosigpac_recinto, na.rm = TRUE)
    f14 <- sum(df1()$semaforo_final, na.rm = TRUE)
    f15 <- sum(df1()$mo_ayudas_fasemonitorizacion, na.rm = TRUE)
    f16 <- sum(df1()$fenviormp, na.rm = TRUE)
    f17 <- sum(df1()$ld_monoanalisis_fasemonitorizacion, na.rm = TRUE)
    f18 <- sum(df1()$ficticio_codpasto, na.rm = TRUE)
 
    sumatoriasR <- c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10,
                     f11, f12, f13, f14, f15, f16, f17, f18)
  })
 
  sumatoriasR2 <- reactive({
    fa1 <- sum(df2()$ld_recintosigpac_agregado, na.rm = TRUE)
    fa2 <- sum(df2()$mo_ayudas_ayuda_ld, na.rm = TRUE)
    fa3 <- sum(df2()$fenviormn, na.rm = TRUE)
    fa4 <- sum(df2()$id_ciclo, na.rm = TRUE)
    fa5 <- sum(df2()$ld_recintosigpac_lineadeclaracion, na.rm = TRUE)
    fa6 <- sum(df2()$ld_recintosigpac_muni, na.rm = TRUE)
    fa7 <- sum(df2()$num_particionenvio, na.rm = TRUE)
    fa8 <- sum(df2()$numexpediente, na.rm = TRUE)
    fa9 <- sum(df2()$ld_recintosigpac_parcela, na.rm = TRUE)
    fa10 <- sum(df2()$ld_recintosigpac_poligono, na.rm = TRUE)
    fa11 <- sum(df2()$ld_recintosigpac_prov, na.rm = TRUE)
    fa12 <- sum(df2()$nombre_provincia, na.rm = TRUE)
    fa13 <- sum(df2()$ld_recintosigpac_recinto, na.rm = TRUE)
    fa14 <- sum(df2()$semaforo_final, na.rm = TRUE)
    fa15 <- sum(df2()$mo_ayudas_fasemonitorizacion, na.rm = TRUE)
    fa16 <- sum(df2()$fenviormp, na.rm = TRUE)
    fa17 <- sum(df2()$ld_monoanalisis_fasemonitorizacion, na.rm = TRUE)
    fa18 <- sum(df2()$ficticio_codpasto, na.rm = TRUE)
 
    sumatoriasR2 <- c(fa1, fa2, fa3, fa4, fa5, fa6, fa7, fa8, fa9, fa10,
                      fa11, fa12, fa13, fa14, fa15, fa16, fa17, fa18)
  })
 
  fecha <- reactive({
    f1 <- df3f()$date
    fecha <- c(f1)
  })
 
  cuenta <- reactive({
    c1 <- df3f()$count
    cuenta <- c(c1)
  })
 
  fecha2 <- reactive({
    f2 <- df4f()$date
    fecha2 <- c(f2)
  })
 
  cuenta2 <- reactive({
    c2 <- df4f()$count
    cuenta <- c(c2)
  })
 
  sumatoriasR3 <- reactive({
    p1 <- sum(df5()$ld_recintosigpac_agregado, na.rm = TRUE)
    p2 <- sum(df5()$mo_ayudas_ayuda_ld, na.rm = TRUE)
    p3 <- sum(df5()$fenviormn, na.rm = TRUE)
    p4 <- sum(df5()$id_ciclo, na.rm = TRUE)
    p5 <- sum(df5()$ld_recintosigpac_lineadeclaracion, na.rm = TRUE)
    p6 <- sum(df5()$ld_recintosigpac_muni, na.rm = TRUE)
    p7 <- sum(df5()$num_particionenvio, na.rm = TRUE)
    p8 <- sum(df5()$numexpediente, na.rm = TRUE)
    p9 <- sum(df5()$ld_recintosigpac_parcela, na.rm = TRUE)
    p10 <- sum(df5()$ld_recintosigpac_poligono, na.rm = TRUE)
    p11 <- sum(df5()$ld_recintosigpac_prov, na.rm = TRUE)
    p12 <- sum(df5()$nombre_provincia, na.rm = TRUE)
    p13 <- sum(df5()$ld_recintosigpac_recinto, na.rm = TRUE)
    p14 <- sum(df5()$semaforo_final, na.rm = TRUE)
    p15 <- sum(df5()$mo_ayudas_fasemonitorizacion, na.rm = TRUE)
    p16 <- sum(df5()$fenviormp, na.rm = TRUE)
    p17 <- sum(df5()$ld_monoanalisis_fasemonitorizacion, na.rm = TRUE)
    p18 <- sum(df5()$ficticio_codpasto, na.rm = TRUE)
 
    sumatoriasR3 <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                      p11, p12, p13, p14, p15, p16, p17, p18)
  })
 
  sumatoriasR4 <- reactive({
    ld1 <- sum(df6()$ld_recintosigpac_agregado, na.rm = TRUE)
    ld2 <- sum(df6()$mo_ayudas_ayuda_ld, na.rm = TRUE)
    ld3 <- sum(df6()$fenviormn, na.rm = TRUE)
    ld4 <- sum(df6()$id_ciclo, na.rm = TRUE)
    ld5 <- sum(df6()$ld_recintosigpac_lineadeclaracion, na.rm = TRUE)
    ld6 <- sum(df6()$ld_recintosigpac_muni, na.rm = TRUE)
    ld7 <- sum(df6()$num_particionenvio, na.rm = TRUE)
    ld8 <- sum(df6()$numexpediente, na.rm = TRUE)
    ld9 <- sum(df6()$ld_recintosigpac_parcela, na.rm = TRUE)
    ld10 <- sum(df6()$ld_recintosigpac_poligono, na.rm = TRUE)
    ld11 <- sum(df6()$ld_recintosigpac_prov, na.rm = TRUE)
    ld12 <- sum(df6()$nombre_provincia, na.rm = TRUE)
    ld13 <- sum(df6()$ld_recintosigpac_recinto, na.rm = TRUE)
    ld14 <- sum(df6()$semaforo_final, na.rm = TRUE)
    ld15 <- sum(df6()$mo_ayudas_fasemonitorizacion, na.rm = TRUE)
    ld16 <- sum(df6()$fenviormp, na.rm = TRUE)
    ld17 <- sum(df6()$ld_monoanalisis_fasemonitorizacion, na.rm = TRUE)
    ld18 <- sum(df6()$ficticio_codpasto, na.rm = TRUE)
 
    sumatoriasR4 <- c(ld1, ld2, ld3, ld4, ld5, ld6, ld7, ld8, ld9, ld10,
                      ld11, ld12, ld13, ld14, ld15, ld16, ld17, ld18)
  })
 
  sumatoriasR5 <- reactive({
    pc1 <- sum(df7()$ld_recintosigpac_agregado, na.rm = TRUE)
    pc2 <- sum(df7()$mo_ayudas_ayuda_ld, na.rm = TRUE)
    pc3 <- sum(df7()$fenviormn, na.rm = TRUE)
    pc4 <- sum(df7()$id_ciclo, na.rm = TRUE)
    pc5 <- sum(df7()$ld_recintosigpac_lineadeclaracion, na.rm = TRUE)
    pc6 <- sum(df7()$ld_recintosigpac_muni, na.rm = TRUE)
    pc7 <- sum(df7()$num_particionenvio, na.rm = TRUE)
    pc8 <- sum(df7()$numexpediente, na.rm = TRUE)
    pc9 <- sum(df7()$ld_recintosigpac_parcela, na.rm = TRUE)
    pc10 <- sum(df7()$ld_recintosigpac_poligono, na.rm = TRUE)
    pc11 <- sum(df7()$ld_recintosigpac_prov, na.rm = TRUE)
    pc12 <- sum(df7()$nombre_provincia, na.rm = TRUE)
    pc13 <- sum(df7()$ld_recintosigpac_recinto, na.rm = TRUE)
    pc14 <- sum(df7()$semaforo_final, na.rm = TRUE)
    pc15 <- sum(df7()$mo_ayudas_fasemonitorizacion, na.rm = TRUE)
    pc16 <- sum(df7()$fenviormp, na.rm = TRUE)
    pc17 <- sum(df7()$ld_monoanalisis_fasemonitorizacion, na.rm = TRUE)
    pc18 <- sum(df7()$ficticio_codpasto, na.rm = TRUE)
 
    sumatoriasR5 <- c(pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10,
                      pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18)
  })
 
  sumatoriasR6 <- reactive({
    lc1 <- sum(df8()$ld_recintosigpac_agregado, na.rm = TRUE)
    lc2 <- sum(df8()$mo_ayudas_ayuda_ld, na.rm = TRUE)
    lc3 <- sum(df8()$fenviormn, na.rm = TRUE)
    lc4 <- sum(df8()$id_ciclo, na.rm = TRUE)
    lc5 <- sum(df8()$ld_recintosigpac_lineadeclaracion, na.rm = TRUE)
    lc6 <- sum(df8()$ld_recintosigpac_muni, na.rm = TRUE)
    lc7 <- sum(df8()$num_particionenvio, na.rm = TRUE)
    lc8 <- sum(df8()$numexpediente, na.rm = TRUE)
    lc9 <- sum(df8()$ld_recintosigpac_parcela, na.rm = TRUE)
    lc10 <- sum(df8()$ld_recintosigpac_poligono, na.rm = TRUE)
    lc11 <- sum(df8()$ld_recintosigpac_prov, na.rm = TRUE)
    lc12 <- sum(df8()$nombre_provincia, na.rm = TRUE)
    lc13 <- sum(df8()$ld_recintosigpac_recinto, na.rm = TRUE)
    lc14 <- sum(df8()$semaforo_final, na.rm = TRUE)
    lc15 <- sum(df8()$mo_ayudas_fasemonitorizacion, na.rm = TRUE)
    lc16 <- sum(df8()$fenviormp, na.rm = TRUE)
    lc17 <- sum(df8()$ld_monoanalisis_fasemonitorizacion, na.rm = TRUE)
    lc18 <- sum(df8()$ficticio_codpasto, na.rm = TRUE)
 
    sumatoriasR6 <- c(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10,
                      lc11, lc12, lc13, lc14, lc15, lc16, lc17, lc18)
  })
 
  consultasFecha <- reactive({
 
    data.frame(
 
      numero = sumatoriasR(),
 
      tipos = c(
        'ld_recintosigpac_agregado',
        'mo_ayudas_ayuda_ld',
        'fenviormn',
        'id_ciclo',
        'ld_recintosigpac_lineadeclaracion',
        'ld_recintosigpac_muni',
        'num_particionenvio',
        'numexpediente',
        'ld_recintosigpac_parcela',
        'ld_recintosigpac_poligono',
        'ld_recintosigpac_prov',
        'nombre_provincia',
        'ld_recintosigpac_recinto',
        'semaforo_final',
        'mo_ayudas_fasemonitorizacion',
        'fenviormp',
        'ld_monoanalisis_fasemonitorizacion',
        'ficticio_codpasto'
      )
    )
  })
 
  consultasFechaCCAA <- reactive({
 
    data.frame(
 
      numero = sumatoriasR2(),
 
      tipos = c(
        'ld_recintosigpac_agregado',
        'mo_ayudas_ayuda_ld',
        'fenviormn',
        'id_ciclo',
        'ld_recintosigpac_lineadeclaracion',
        'ld_recintosigpac_muni',
        'num_particionenvio',
        'numexpediente',
        'ld_recintosigpac_parcela',
        'ld_recintosigpac_poligono',
        'ld_recintosigpac_prov',
        'nombre_provincia',
        'ld_recintosigpac_recinto',
        'semaforo_final',
        'mo_ayudas_fasemonitorizacion',
        'fenviormp',
        'ld_monoanalisis_fasemonitorizacion',
        'ficticio_codpasto'
      )
    )
  })
 
  temporal <- reactive({
 
    data.frame(
      fecha = fecha(),
      cuenta = cuenta()
    )
 
  })
 
  temporalCCAA <- reactive({
 
    data.frame(
      fecha2 = fecha2(),
      cuenta2 = cuenta2()
    )
 
  })
 
  consultasPastos <- reactive({
 
    data.frame(
 
      numero = sumatoriasR3(),
 
      tipos = c(
        'ld_recintosigpac_agregado',
        'mo_ayudas_ayuda_ld',
        'fenviormn',
        'id_ciclo',
        'ld_recintosigpac_lineadeclaracion',
        'ld_recintosigpac_muni',
        'num_particionenvio',
        'numexpediente',
        'ld_recintosigpac_parcela',
        'ld_recintosigpac_poligono',
        'ld_recintosigpac_prov',
        'nombre_provincia',
        'ld_recintosigpac_recinto',
        'semaforo_final',
        'mo_ayudas_fasemonitorizacion',
        'fenviormp',
        'ld_monoanalisis_fasemonitorizacion',
        'ficticio_codpasto'
      )
    )
  })
 
  consultasLD <- reactive({
 
    data.frame(
 
      numero = sumatoriasR4(),
 
      tipos = c(
        'ld_recintosigpac_agregado',
        'mo_ayudas_ayuda_ld',
        'fenviormn',
        'id_ciclo',
        'ld_recintosigpac_lineadeclaracion',
        'ld_recintosigpac_muni',
        'num_particionenvio',
        'numexpediente',
        'ld_recintosigpac_parcela',
        'ld_recintosigpac_poligono',
        'ld_recintosigpac_prov',
        'nombre_provincia',
        'ld_recintosigpac_recinto',
        'semaforo_final',
        'mo_ayudas_fasemonitorizacion',
        'fenviormp',
        'ld_monoanalisis_fasemonitorizacion',
        'ficticio_codpasto'
      )
    )
  })
 
  consultasPastosCCAA <- reactive({
 
    data.frame(
 
      numero = sumatoriasR5(),
 
      tipos = c(
        'ld_recintosigpac_agregado',
        'mo_ayudas_ayuda_ld',
        'fenviormn',
        'id_ciclo',
        'ld_recintosigpac_lineadeclaracion',
        'ld_recintosigpac_muni',
        'num_particionenvio',
        'numexpediente',
        'ld_recintosigpac_parcela',
        'ld_recintosigpac_poligono',
        'ld_recintosigpac_prov',
        'nombre_provincia',
        'ld_recintosigpac_recinto',
        'semaforo_final',
        'mo_ayudas_fasemonitorizacion',
        'fenviormp',
        'ld_monoanalisis_fasemonitorizacion',
        'ficticio_codpasto'
      )
    )
  })
 
  consultasLDCCAA <- reactive({
 
    data.frame(
 
      numero = sumatoriasR6(),
 
      tipos = c(
        'ld_recintosigpac_agregado',
        'mo_ayudas_ayuda_ld',
        'fenviormn',
        'id_ciclo',
        'ld_recintosigpac_lineadeclaracion',
        'ld_recintosigpac_muni',
        'num_particionenvio',
        'numexpediente',
        'ld_recintosigpac_parcela',
        'ld_recintosigpac_poligono',
        'ld_recintosigpac_prov',
        'nombre_provincia',
        'ld_recintosigpac_recinto',
        'semaforo_final',
        'mo_ayudas_fasemonitorizacion',
        'fenviormp',
        'ld_monoanalisis_fasemonitorizacion',
        'ficticio_codpasto'
      )
    )
  })
 
  consultasFechaF <- reactive(subset(consultasFecha(), numero > 0))
 
  consultasFechaCCAAF <- reactive(subset(consultasFechaCCAA(), numero > 0))
 
  consultasPastosF <- reactive(subset(consultasPastos(), numero > 0))
 
  consultasLDF <- reactive(subset(consultasLDF(), numero > 0))
 
  consultasPastosCCAAF <- reactive(subset(consultasPastosCCAA(), numero > 0))
 
  consultasLDCCAAF <- reactive(subset(consultasLDCCAA(), numero > 0))
 
  # Graficos --------------------------------------------------------------------
 
  # Grafico de estadisticas por fecha
 
  observe({
 
    if (fecha_inicio() == fecha_fin()) {
      titulo = paste0("Número de consultas por tipo en la fecha ", 
                      fecha_inicio())
    } else if (fecha_inicio() == '2022-12-01' && fecha_fin() == Sys.Date()) {
      titulo = paste0("Número de consultas por tipo hasta hoy (", fecha_fin(),
                      ")")
    } else {
      titulo = paste0("Número de consultas por tipo desde la fecha ", 
                      fecha_inicio(), " hasta la fecha ", fecha_fin())
    }
 
    g1 <- reactive(
      ggplot(consultasFecha(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha sin 0s
 
    titulo2 = " (omitiendo campos vacíos)"
 
    g2 <- reactive(
      ggplot(consultasFechaF(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, titulo2),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha y CCAA
 
    titulo3 = " en "
 
    g3 <- reactive(
      ggplot(consultasFechaCCAA(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, titulo3, comunidades[as.numeric(ccaa())+1]),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha y CCAA sin 0s
 
    g4 <- reactive(
      ggplot(consultasFechaCCAAF(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, titulo3, comunidades[as.numeric(ccaa())+1],
                            titulo2),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Serie temporal
 
    g5 <- reactive(
      ggplot(temporal(), aes(x = fecha, y = cuenta)) +
        geom_line(stat = "identity") + geom_point() +
        labs(title = paste0("Evolución del número de consultas desde la fecha ",
                            fecha_inicio(), " hasta ", fecha_fin()),
             x = "Fecha", y = "Nº de consultas")
    )
 
    # Serie temporal por CCAA
 
    g6 <- reactive(
      ggplot(temporalCCAA(), aes(x = fecha2, y = cuenta2)) +
        geom_line(stat = "identity") + geom_point() +
        labs(title = paste0("Evolución del número de consultas desde la fecha ",
                            fecha_inicio(), " hasta ", fecha_fin(), " en ",
                            comunidades[as.numeric(ccaa())+1]),
             x = "Fecha", y = "Nº de consultas")
    )
 
    # Grafico de estadisticas por fecha y capa Pastos
 
    g7 <- reactive(
      ggplot(consultasPastos(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa Pastos"),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha y capa sin 0s
 
    g8 <- reactive(
      ggplot(consultasPastosF(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa Pastos", titulo2),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha y capa LD
 
    g9 <- reactive(
      ggplot(consultasLD(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa LD"),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha y capa LD sin 0s
 
    g10 <- reactive(
      ggplot(consultasLDF(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa LD", titulo2),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha, por CCAA y capa Pastos
 
    g11 <- reactive(
      ggplot(consultasPastosCCAA(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa Pastos", titulo3,
                            comunidades[as.numeric(ccaa())+1]),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha, por CCAA y capa Pastos sin 0s
 
    g12 <- reactive(
      ggplot(consultasPastosCCAAF(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa Pastos", titulo3,
                            comunidades[as.numeric(ccaa())+1], titulo2),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha, por CCAA y capa LD
 
    g13 <- reactive(
      ggplot(consultasLDCCAA(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa LD", titulo3,
                            comunidades[as.numeric(ccaa())+1]),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    # Grafico de estadisticas por fecha, por CCAA y capa Pastos sin 0s
 
    g14 <- reactive(
      ggplot(consultasLDCCAAF(), aes(x = tipos, y = numero)) +
        geom_bar(stat = "identity") +
        labs(title = paste0(titulo, " en capa LD", titulo3,
                            comunidades[as.numeric(ccaa())+1], titulo2),
             x = "Tipo de consulta", y = "Nº de consultas") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    )
 
    if (!input$switch) {
      if (ccaa0() == "00") {
        if(type() == "Total") {
          output$barras <- renderPlotly({ggplotly(g1())}) # Grafico de estadisticas por fecha
          output$temporal <- renderPlotly({ggplotly(g5())}) # Serie temporal
        } else if (type() == "Pastos Comunales") {
          output$barras <- renderPlotly({ggplotly(g7())}) # Grafico de estadisticas por fecha y capa Pastos
          output$temporal <- renderPlotly({ggplotly(g5())}) # Serie temporal
        } else if (type() == "Línea Declaración") {
          output$barras <- renderPlotly({ggplotly(g9())}) # Grafico de estadisticas por fecha y capa LD
          output$temporal <- renderPlotly({ggplotly(g5())}) # Serie temporal
        }
      } else {
        if(type() == "Total"){
          output$barras <- renderPlotly({ggplotly(g3())}) # Grafico de estadisticas por fecha y CCAA
          output$temporal <- renderPlotly({ggplotly(g6())}) # Serie temporal por CCAA
        } else if (type() == "Pastos Comunales") {
          output$barras <- renderPlotly({ggplotly(g11())}) # Grafico de estadisticas por fecha, por CCAA y capa Pastos
          output$temporal <- renderPlotly({ggplotly(g6())}) # Serie temporal por CCAA
        } else if (type() == "Línea Declaración") {
          output$barras <- renderPlotly({ggplotly(g13())}) # Grafico de estadisticas por fecha, por CCAA y capa LD
          output$temporal <- renderPlotly({ggplotly(g6())}) # Serie temporal por CCAA
        }
      }
    } else {
      if (ccaa0() == "00") {
        if(type() == "Total") {
          output$barras <- renderPlotly({ggplotly(g2())}) # Grafico de estadisticas por fecha sin 0s
          output$temporal <- renderPlotly({ggplotly(g5())}) # Serie temporal
        } else if (type() == "Pastos Comunales") {
          output$barras <- renderPlotly({ggplotly(g8())}) # Grafico de estadisticas por fecha y capa Pastos sin 0s
          output$temporal <- renderPlotly({ggplotly(g5())}) # Serie temporal
        } else if (type() == "Línea Declaración") {
          output$barras <- renderPlotly({ggplotly(g10())}) # Grafico de estadisticas por fecha y capa LD sin 0s
          output$temporal <- renderPlotly({ggplotly(g5())}) # Serie temporal
        }
      } else {
        if (type() == "Total"){
          output$barras <- renderPlotly({ggplotly(g4())}) # Grafico de estadisticas por fecha y CCAA sin 0s
          output$temporal <- renderPlotly({ggplotly(g6())}) # Serie temporal por CCAA
        } else if (type() == "Pastos Comunales") {
          output$barras <- renderPlotly({ggplotly(g12())}) # Grafico de estadisticas por fecha, por CCAA y capa Pastos sin 0s
          output$temporal <- renderPlotly({ggplotly(g6())}) # Serie temporal por CCAA
        } else if (type() == "Línea Declaración") {
          output$barras <- renderPlotly({ggplotly(g14())}) # Grafico de estadisticas por fecha, por CCAA y capa LD sin 0s
          output$temporal <- renderPlotly({ggplotly(g6())}) # Serie temporal por CCAA
        }
      }
    }
 
  })
 
}
 
# Shiny app object --------------------------------------------------------------
 
shinyApp(ui = ui, server = server)
