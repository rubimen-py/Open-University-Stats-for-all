function(input,output,session){
  
  

  # INTERFAZ PREGRADO -------------------------------------------------------------
   output$metadatos <- renderUI({
     HTML("var header = $('.navbar > .container-fluid');
                   header.append('<div style=\"float:right;\">}
                   <a id=\"metadatos-button\" href=\" http://estadisticas.unal.edu.co/menu-principal/cifras-generales/metadatos/cifras-generales/?tx_estadisticaunal_showprotocolo%5Bestadistica%5D=26&tx_estadisticaunal_showprotocolo%5Baction%5D=protocolo&tx_estadisticaunal_showprotocolo%5Bcontroller%5D=Estadistica&cHash=1b0e22d62926e55f3099a4677be3e9c1\" class=\"btn btn-success\" target=\"_blank\" >Ver Metadatos</a></div>');
                   console.log(header)")
   })
  # grad_sede_si ------------------------------------------------------------
  
output$grad_sede_si_ou <- renderUI({
    
    times <- input$grad_gen_bt
    
    div(id=letters[(times %% length(letters)) + 1],
       
         selectizeInput(inputId = 'grad_sede_si', 
                       label = 'Sede',
                       choices = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'),
                       options = list(
                         placeholder = 'Seleccione una sede',
                         onInitialize = I('function() { this.setValue(""); }')
                         )
                       )
        )
    })
  
  # grad_fac_si -------------------------------------------------------------
  
output$grad_fac_si_ou <- renderPrint({
    
    if(!(is.null(input$grad_sede_si))){
      #***** Panel Condicional para filtro por facultad  según sede ****#
      if(input$grad_sede_si %in% unique(Graduados$SEDE_NOMBRE_MAT)){
        #**** selectInput  filtro por facultad ****#
        selectInput(
        inputId = "grad_fac_si",
        label = "Seleccione Facultad",
        choices = append(paste("Cifras Sede",input$grad_sede_si),
                         unique(Graduados %>% filter(SEDE_NOMBRE_MAT == input$grad_sede_si) %>% 
                                                       select(FACULTAD)) %>% arrange(FACULTAD)),
        selected = paste("Cifras Sede",input$grad_sede_si)
        )
      }
      }
    })


  
  #*** selectinput filtro facultad dinámico según la sede escogida ***# 
observe({
    
    grad_sede <- input$grad_sede_si
    
    if(!(is.null(grad_sede))){
      if(grad_sede != ""){
        
        updateSelectInput(
          session,
          inputId = "grad_fac_si",
          label = paste("Facultades", grad_sede),
          choices = append(paste("Cifras Sede",input$grad_sede_si),
                           unique(Graduados %>% filter(SEDE_NOMBRE_MAT == grad_sede) %>% 
                                          select(FACULTAD)) %>% arrange(FACULTAD)),
          selected = paste("Cifras Sede",input$grad_sede_si)
        )
      }
      }
    
    })
  

  
# + TABS grad_tabset -------------------------------------------------------------
  
output$grad_tabset <- renderUI({
    
  if( !(is.null(input$grad_sede_si)) ){
    if(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT)))){
        #**** tabs por categorías de información graduados ***#
        tabsetPanel(
          id = "grad_tabs",
          type = "pills",
          # ++ grad_evol_UI ------------------------------------------------------------
          tabPanel(
            title = "Evolución Graduados",
            #Series evolucion graduados
            br(),
            fluidRow(
              column(
                width = 12,
                
                wellPanel(
                style = "background-color: white;
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                highchartOutput("grad_evol_serie", height = "65vh", width = "100%") %>% withSpinner(type = 4, 
                                                                   color = "#A61C31",
                                                                   color.background = "#B1B2B0",
                                                                   size = 1) 
                ) 
                
                #verbatimTextOutput("sede_input"),
                #verbatimTextOutput("fac_input")
  
                )
              ),
            
            #info boxes evolución graduados
            fluidRow(
              column(
                width = 4,
                uiOutput("total_box"),
                br()
                ),
              
              column(
                width = 4,
                uiOutput("mh_box"),
                br()
                ),
              
              column(
                width = 4,
                uiOutput("mh_box_actual"),
                br()
                )
              )
            ),
          
          # ++ grad_mod_UI -------------------------------------------------------------
          
          tabPanel(
            title = "Modalidad de formación",
            br(),
            
            #Serie graduados por modalidad de formación
            fluidRow(
              column(
                width = 12,
                
                wellPanel(
                style = "background-color: #fff;
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                highchartOutput("grad_mod_serie") %>% withSpinner(type = 4, 
                                                                  color = "#A61C31",
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
                ),
                hr()
                )
              ),
            
            #Diagrama de torta modalidad de formación
            fluidRow(
              column(
                width = 6,
                    wellPanel(
                    style =  "background-color: #fff;
                    border-color: #2c3e50;
                    box-shadow: 1px 1px 5px;
                    height:471px;",
                    highchartOutput("grad_mod_torta", 
                                    width = "100%", 
                                    height = "100%") %>% withSpinner(type = 4, 
                                                                     color = "#A61C31",
                                                                     color.background = "#B1B2B0",
                                                                     size = 1)
                    )
                
            ),
            
            #tabla modalidad de formación
            column(
              width = 6,
              wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50;
              box-shadow: 1px 1px 5px;",
              dataTableOutput("grad_mod_tabla", 
                              width = "100%") %>% withSpinner(type = 4, 
                                                              color = "#A61C31",
                                                              color.background = "#B1B2B0",
                                                              size = 1)
              )
              )
            )
        ),
      
      # ++  grad_nivel_UI --------------------------------------------------------
      
      tabPanel(
        title = "Nivel de formación",
        br(),
        #Serie graduados por nivel de formación
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50;
              box-shadow: 1px 1px 5px;",
              highchartOutput("grad_nivel_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
            ),
            hr()
            )
          ),
        
        #barras nivel de formación
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style =  "background-color: #fff; 
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;",
              plotlyOutput("grad_nivel_barras", width = "auto", height = "auto")
              ),
            hr()
            )
          ),
        
        #tabla nivel de formación
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;",
              dataTableOutput("grad_nivel_tabla", width = "100%")
              )
            )
          )
        ),
      
      
      
      
      # ++ grad_sede_UI ------------------------------------------------------------
      
      #***** tab sede de formación solo general graduados
      tabPanel(
        title = "Sede",
        br(),
        
        #Serie graduados por sede de formación
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;",
              highchartOutput("grad_sede_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                 color.background = "#B1B2B0",
                                                                 size = 1)
            ),
            hr()
            )
          ),
        #barras sede 
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style =  "background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;",
              plotlyOutput("grad_sede_barras", width = "auto", height = "auto",inline = F)
            ),
            hr()
            )
          ),
        
        #Tabla sede
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50;
              box-shadow: 1px 1px 5px; ",
              dataTableOutput("grad_sede_tabla", width = "100%")
              )
            )
          )
        ),
      
      
      # ++ grad_lug_nac_UI -----------------------------------------------------
      
      
      
      #***** tab lugar de nacimiento
      tabPanel(
        
        title = "Lugar de nacimiento",
        br(),
        id = "maps-container",
        
        fluidRow(
          column(
            width = 12,
            div(class = "lines-background",
            wellPanel(
              style = 'background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;background: rgb(203,200,62);
background: -moz-linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
background: -webkit-linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
background: linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
filter: progid:DXImageTransform.Microsoft.gradient(startColorstr="#cbc83e",endColorstr="#0d865d",GradientType=1);;
              ',
              tabsetPanel(
                id = "tabs-maps",
                selected = "Por municipio",
                type = "pills",
                
                tabPanel(
                  title = "Por municipio",
                  br(),
                  div(
                  leafletOutput("grad_lug_nac_pm", height = "115vh",
                                width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                    color.background = "#B1B2B0",
                                                                    size = 1)
                  )
                  ),
                
                tabPanel(
                  title = "Total por municipio",
                  br(),
                  leafletOutput("grad_lug_nac_tm",height = "115vh", width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                      color.background = "#B1B2B0",
                                                                      size = 1)
                  ),
                
                tabPanel(
                  title = "Total por departamentos",
                  br(),
                  leafletOutput("grad_lug_nac_td", height = "115vh",
                                width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                      color.background = "#B1B2B0",
                                                                      size = 1)
                  ),
                
                tabPanel(
                  title = "Total por departamentos y municipios",
                  br(),
                  leafletOutput("grad_lug_nac_tmd", height = "115vh",
                                width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                       color.background = "#B1B2B0",
                                                                       size = 1)
                  )
                )
              )
            )
            )
          )
        
        
          
        
      ),#-----tab lugar de nacimiento
      
      
      
      # ++ grad_sexo_UI -----------------------------------------------------
        
     
      #***** tab sede de formación sexo
      tabPanel(
        title = "Sexo",
        br(),
        #Serie graduados por sexo
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;",
              highchartOutput("grad_sexo_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                 color.background = "#B1B2B0",
                                                                 size = 1)
              ),
            hr()
            )
          ),
        #diagrama de torta sexo
        fluidRow(
          column(
            width = 6,
            
            wellPanel(
              style =  "background-color: #fff; 
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;
              height:454px;",
              highchartOutput("grad_sexo_torta", width = "auto", height = "auto")
              )
            ),
          
          #tabla sexo
          column(
            width = 6,
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px; ",
              dataTableOutput("grad_sexo_tabla", width = "100%")
              )
            )
          )
        ),
      
      # ++ grad_areas_UI -----------------------------------------------------------
      #***** Areas del conocimiento
      tabPanel(
        title = "Áreas del conocimiento",
        br(),
        #Serie graduados por sexo
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
              highchartOutput("grad_areas_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
            ),
            hr()
            )
          ),
        
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style =  "background-color: #fff;
              border-color: #2c3e50;
              box-shadow: 1px 1px 5px;",
              plotlyOutput("grad_areas_barra", width = "auto", height = "auto",inline = F)
              ),
            hr()
            )
          ),
        
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style = "background-color: #fff;
              border-color: #2c3e50;
              box-shadow: 1px 1px 5px; ",
              dataTableOutput("grad_areas_tabla", width = "100%")
              )
            )
          )
        )#-----tab Areas del conocimiento
      )#----tabs por categorías de información graduados
    
    
  } else {
    
    # ++ No información ----------------------------------------------------------
    fluidRow(
      column(
        width = 12,
    withTags({
      div(class="jumbotron",
          h1("No información",
             class = "display-3"),
          p(class = "lead",
          "la sede aún no cuenta con cifras de graduados")
          )
      })
      )
    )
  
  }
    }

  
})

outputOptions(output, "grad_tabset",priority = 10)

#  Eliminar Panel Sede si se escoge alguna sede ----------------------------



observeEvent(input$grad_fac_si,{
  
  
  if(input$grad_fac_si == paste("Cifras Sede",input$grad_sede_si)){
    hideTab(inputId = "grad_tabs", target = "Sede")
  }
  
})

#Eliminar panel lugar de información

observeEvent(input$grad_fac_si, {
  
  if(input$grad_fac_si != paste("Cifras Sede",input$grad_sede_si)){
    hideTab(inputId = "grad_tabs", target = "Lugar de nacimiento")
  }
  
})

# Eliminar panel Areas del conocimiento sede si escoge alguna facultad ---------------------------


observeEvent(input$grad_fac_si, {
  if(input$grad_fac_si != paste("Cifras Sede",input$grad_sede_si)){
    hideTab(inputId = "grad_tabs", target = "Áreas del conocimiento")
  }
  
})





# * * Base de datos ------------------------------------------------------------------

#Consolidado es la base filtrada según la sede o facultad escogida por el usuario



Consolidado <- reactive({
  
  #Al inicializar el widget de la sede en NULL por un instante aparece un error
  #en ese instante el siguiente if identifica ese null para que no aparezca ese mensaje
  
  
  
  if(is.null(input$grad_sede_si)){
    
    sede <- ""
    
  }else{ 
    
    sede <- input$grad_sede_si
    
  }
  

  
  if(sede == "" | is.null(input$grad_fac_si)){
    
    Grad <- Graduados # Total matriculados a nivel nacional
    
    #Si el usuario ha escogido una facultad el if revisa si está
    #en las cifras generales de la sede o si está en alguna facultad
  } else if(!(input$grad_fac_si  %in% unique(Graduados %>% filter(SEDE_NOMBRE_MAT == input$grad_sede_si) %>% 
                                             select(FACULTAD) %>% pull(FACULTAD)))){
    
    Grad <- Graduados %>% filter(SEDE_NOMBRE_MAT == input$grad_sede_si) 
    
  } else {
    
    Grad <- Graduados %>% filter(SEDE_NOMBRE_MAT == input$grad_sede_si,
                                  FACULTAD == input$grad_fac_si)
    
  } 
  
  
  
  DT1 <- Agregar(Grad, 'TIPO_NIVEL')
  DT2 <- Agregar(Grad, 'NIVEL')
  DT3 <- Agregar(Grad, 'SEDE_NOMBRE_ADM')
  DT4 <- Agregar(Grad, 'NACIONALIDAD')
  DT4 <- DT4 %>% filter(YEAR != 2009)
  DT5 <- Agregar(Grad, 'SEXO')
  DT6 <- Agregar(Grad, 'AREAC_SNIES')
  Total <- Totales(Grad)
  
  Uperiodo <- Grad %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
    mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id)) %>% pull()
  
  if(Uperiodo < max(historic)){
    
    str_hist <- as.character(historic)
    years_left <- str_hist[historic > Uperiodo]
    
    DT1 <- add_ceros(DT1,'TIPO_NIVEL',years_left)
    DT2 <- add_ceros(DT2,'NIVEL',years_left)
    DT3 <- add_ceros(DT3,'SEDE_NOMBRE_ADM',years_left)
    DT4 <- add_ceros(DT4,'NACIONALIDAD',years_left)
    DT5 <- add_ceros(DT5,'SEXO',years_left)
    DT6 <- add_ceros(DT6,'AREAC_SNIES',years_left)
    Total <- add_ceros(Total,'TOTAL',years_left)
    
  }
  
  
  # Consolidado tabla agregada
  
  Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)
  

  
  
})








output$sede_input <- renderPrint({
  input$grad_sede_si
})

output$fac_input <- renderPrint({
  input$grad_fac_si
})

output$tab_input <- renderPrint({
  input$grad_tabs
})

output$grad_button <- renderPrint({
  input$grad_gen_bt
})

# Extraer el caracter que contiene la sede que se consulta para ubicarlo en la gráfica


# * * sede_actual -------------------------------------------------------------

sede_actual <- reactive({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  if(!(is.null(input$grad_sede_si))){
    if(is.null(input$grad_fac_si)){
      
      if(input$grad_sede_si == "" | input$grad_gen_bt){
        sede <- "Cifras generales Universidad Nacional de Colombia (k: miles)."
      } else {
        sede <- paste("Sede",paste0(input$grad_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      }
      
    } else {
      
      if(input$grad_sede_si == ""){
        sede <- "Cifras generales Universidad Nacional de Colombia."
      } else if(input$grad_fac_si == paste("Cifras Sede",input$grad_sede_si)){
        sede <- paste("Sede",paste0(input$grad_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      } else {
        sede <- paste("Facultad de",paste0(input$grad_fac_si,","),"Sede",paste0(input$grad_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      }
    }
  }
  
})

# * * grad_evol ---------------------------------------------------------------




#Serie evolución de graduados 

output$grad_evol_serie <- renderHighchart({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  Plot.Series(
    datos = Consolidado(),
    categoria = "TOTAL",
    titulo = "Evolución histórica del total de estudiantes graduados",
    labelY = "Número de graduados",
    libreria = "highcharter",
    estilo = list(hc.Credits = sede_actual(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
    ) %>% hc_tooltip(backgroundColor= "#232323")
    
  
  
})


#Extraer el último semestre registrado para la caja de información

ult_per <- reactive({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  last_year <- Consolidado() %>%  summarise(year = max(YEAR)) %>% pull(year)
  periodo <- Consolidado() %>% filter(YEAR == last_year) %>% summarise(periodo = max(SEMESTRE)) %>% pull(periodo)
  
  lista <- list(total = paste0(last_year,"-",periodo),
                last_year = last_year,
                last_period = periodo)
  
})


####--- info box total graduados UI ---###

output$total_box <- renderUI({
  
  
  
  div(class = "lines-background",
      div(class = "cont2 border-green green-maps",
          div(class = "info-tl",
              p(class = "info-title", "Total Graduados"),
              p(class = "info-subtitle info-sub-black",
                paste("Número de graduados histórico desde el 2009")
              )
          ),
          
          div(class = "info-num",
              div(class = "info-num-1",
                  p(id = "element",class = "num-m un-green",grad_evol_ib_h() + grad_evol_ib_m()),
                  p(class = "txt-m", "Número de graduados")
              )
          )
      )
  )
  
})

####--- info box total graduados por genero ---###

output$mh_box <- renderUI({
  
  
  
  div(class = "lines-background",
      div(class = "cont2 border-green green-maps",
          div(class = "info-tl",
              p(class = "info-title", "Graduados"),
              p(class ="info-subtitle","Número de graduados por sexo desde el 2009")
          ),
          
          div(class = "info-num",
              
              div(class = "info-num-1",
                  p(class = "num-m un-green",grad_evol_ib_m()),
                  p(class = "txt-m", "Mujeres")
              ),
              
              div(class = "info-num-2",
                  p(class = "num-h un-green",grad_evol_ib_h()),
                  p(class = "txt-h","Hombres")
              )
          )
      )
  )
  
})

# extraer número de graduados mujeres historico

grad_evol_ib_m <- reactive({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado() %>% filter(Variable == "SEXO",
                                    Clase == "Mujeres") %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

# extraer número graduados hombres historico

grad_evol_ib_h <- reactive({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado() %>% filter(Variable == "SEXO",
                                    Clase == "Hombres") %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

####--- info box graduados semestre actual por genero ---###}

output$mh_box_actual <- renderUI({
  
  div(class = "lines-background",
      div(
        class = "cont2 border-green green-maps",
        
        div(
          class = "info-tl",
          p(class = "info-title", paste("Graduados",ult_per()[[1]])),
          p(class = "info-subtitle info-sub-black",
            paste("Número de graduados por sexo 
                        semestre",ult_per()[[1]])),
          
          
        ),
        
        div(
          class = "info-num",
          div(
            class = "info-num-1",
            p(class = "num-m un-green",grad_evol_ib_ma()),
            p(class = "txt-m", "Mujeres")
          ),
          
          div(
            class = "info-num-2",
            p(class = "num-h un-green",grad_evol_ib_ha()),
            p(class = "txt-h","Hombres")
          )
        )
        
        
      ))
})

# extraer número de graduados mujeres semestre Anterior (ma) número

grad_evol_ib_ma <- reactive({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado() %>% filter(Variable == "SEXO",
                                    Clase == "Mujeres",
                                    YEAR == ult_per()[["last_year"]],
                                    SEMESTRE == ult_per()[["last_period"]]) %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

# extraer número graduados hombres semestre Anterior (ha)

grad_evol_ib_ha <- reactive({
  
  req(input$grad_sede_si %in% c("",as.character(unique(Graduados$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado() %>% filter(Variable == "SEXO",
                                    Clase == "Hombres",
                                    YEAR == ult_per()[["last_year"]],
                                    SEMESTRE == ult_per()[["last_period"]]) %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})


# * * grad_mod ----------------------------------------------------------------


#******* modalidad de formación serie ********#

output$grad_mod_serie <- renderHighchart({
  
  modalidad_general <- sort(unique(Graduados$TIPO_NIVEL))
  paleta_modalidad <- c("#f15a24","#8cc63f")
  color_modalidad <- tibble(modalidad_general,paleta_modalidad)
  
  modalidad_actual <- Consolidado() %>%  filter(Variable == "TIPO_NIVEL") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_modalidad %>% filter(modalidad_general %in% modalidad_actual) %>% 
    select(paleta_modalidad) %>% pull()
  
  Plot.Series(datos = Consolidado(),
              categoria = "TIPO_NIVEL",
              titulo = "Evolución del número de estudiantes graduados por modalidad de formación",
              labelX = "Periodo",
              labelY = "Número de graduados ",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
  
})

#outputOptions(output, "grad_mod_serie", suspendWhenHidden = FALSE)


#***** modalidad de formación torta ******#

output$grad_mod_torta <- renderHighchart({
  
  modalidad_general <- sort(unique(Graduados$TIPO_NIVEL))
  paleta_modalidad <- c("#f15a24","#8cc63f")
  color_modalidad <- tibble(modalidad_general,paleta_modalidad)
  
  modalidad_actual <- Consolidado() %>%  filter(Variable == "TIPO_NIVEL") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_modalidad %>% filter(modalidad_general %in% modalidad_actual) %>% 
    select(paleta_modalidad) %>% pull()
  
  
  Plot.Torta(datos = Consolidado(),
               categoria = "TIPO_NIVEL",
               titulo = "Distribución de graduados por <br> modalidad de formación <br>",
               ano = ult_per()[["last_year"]],
               periodo = ult_per()[["last_period"]],
               addPeriodo = T,
               col = colores_dinamico,
               libreria = "highcharter")
 
})

#***** modalidad de formación tabla ****##

output$grad_mod_tabla <- renderDataTable({
  
  Tabla(datos = Consolidado(),
        categoria = "TIPO_NIVEL",
        encabezado = "Total estudiantes graduados por modalidad de formación")
  })



# * * grad_nivel --------------------------------------------------------------



#***** nivel de formación serie *****#

output$grad_nivel_serie <- renderHighchart({
  
  req(input$grad_sede_si != "De La Paz" )
  
  nivel_general <- sort(unique(Graduados$NIVEL))
  paleta_nivel <- c("#6d6666","#fbb03b","#29abe2","#c1272d","#8cc63f")
  color_nivel <- tibble(nivel_general,paleta_nivel)
  
  nivel_actual <- Consolidado() %>%  filter(Variable == "NIVEL") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nivel %>% filter(nivel_general %in% nivel_actual) %>% 
    select(paleta_nivel) %>% pull()
  
  
  Plot.Series(datos = Consolidado(),
              categoria = "NIVEL",
              titulo = "Evolución del número de estudiantes graduados por nivel de formación",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})



#***** nivel de formación barras *****#
output$grad_nivel_barras <- renderPlotly({
  
  nivel_general <- sort(unique(Graduados$NIVEL))
  paleta_nivel <- c("#6d6666","#fbb03b","#29abe2","#c1272d","#8cc63f")
  color_nivel <- tibble(nivel_general,paleta_nivel)
  
  nivel_actual <- Consolidado() %>%  filter(Variable == "NIVEL") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nivel %>% filter(nivel_general %in% nivel_actual) %>% 
    select(paleta_nivel) %>% pull()
  
  Plot.Barras(datos     = Consolidado(),
              categoria = "NIVEL",
              ano       = ult_per()[["last_year"]],
              periodo   = ult_per()[["last_period"]],
              vertical  = F,
              ordinal   = T,
              libreria  = "plotly",
              titulo    = "Distribución de graduados por <br> nivel de formación",
              labelEje  = "Total",
              addPeriodo= T,
              colores = colores_dinamico,
              textInfo  = "Total de graduados")
  
})

#***** nivel de formación tabla *****#
output$grad_nivel_tabla <- renderDataTable({
  
  Tabla(datos = Consolidado(),
        categoria = "NIVEL",
        encabezado = "Total estudiantes graduados por nivel de formación")
  
})


# * * grad_sede --------------------------------------------------------------



#***** sede graduados serie *****#

output$grad_sede_serie <- renderHighchart({
  
  #Colores Sede
  
  sedes <- c("Amazonía","Bogotá","Caribe","De La Paz","Manizales","Medellín","Orinoquía","Palmira","Tumaco")
  paleta_sedes <- c("#29abe2","#8cc63f","#c1272d","#9e9ac8","#0071bc","#f15a24","#fbb03b","#93278f","#6d6666")
  color_sedes <- tibble(sedes,paleta_sedes)
  
  
  Plot.Series(datos = Consolidado(),
              categoria = "SEDE_NOMBRE_ADM",
              titulo = "Evolución del número de graduados por sede",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = color_sedes %>% select(paleta_sedes) %>% pull(),
              estilo = list(hc.Credits = sede_actual(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "grad_sede_serie", suspendWhenHidden = FALSE)

#***** sede barras *****#
output$grad_sede_barras <- renderPlotly({
  
  #Colores Sede
  
  sedes <- c("Amazonía","Bogotá","Caribe","De La Paz","Manizales","Medellín","Orinoquía","Palmira","Tumaco")
  paleta_sedes <- c("#29abe2","#8cc63f","#c1272d","#9e9ac8","#0071bc","#f15a24","#fbb03b","#93278f","#6d6666")
  color_sedes <- tibble(sedes,paleta_sedes)
  
  Plot.Barras(datos = Consolidado(),
              categoria = "SEDE_NOMBRE_ADM",
              vertical = F,
              ano = ult_per()[["last_year"]],
              titulo = "Distribución de graduados por <br> sedes de la universidad",
              periodo = ult_per()[["last_period"]],
              colores = color_sedes %>% select(paleta_sedes) %>% pull(),
              libreria = "plotly")
  
})

#***** sede tabla *****#
output$grad_sede_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado(),
        categoria = "SEDE_NOMBRE_ADM",
        encabezado = "Total estudiantes graduados por sedes de la universidad")
  
})


#  * * grad_lug_nac -------------------------------------------------------

Graduados_actual <- reactive({
  
  if(input$grad_sede_si != ""){
    Graduados_actual = Graduados %>% filter(SEDE_NOMBRE_ADM == input$grad_sede_si) %>% 
      filter(YEAR == max(YEAR)) %>% 
      filter(SEMESTRE == max(SEMESTRE)) %>%
      filter(!is.na(Departamento))
  } else {
    
    Graduados_actual = Graduados %>%  
      filter(YEAR == max(YEAR)) %>% 
      filter(SEMESTRE == max(SEMESTRE)) %>%
      filter(!is.na(Departamento))
  }
  
})

# Mapa por municipios

output$grad_lug_nac_pm <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual(),
    tipo = c("SiNoMpios"),
    titulo = paste("Graduados",ult_per()[["total"]]),
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})

# Mapa Total municipio

output$grad_lug_nac_tm <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual(),
    tipo = c("Mpios"),
    titulo = paste("Graduados",ult_per()[["total"]]),
    cortes = c(0,1,5,10,100,Inf),
    colores = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})

# Total por depto

output$grad_lug_nac_td <- renderLeaflet({
   
  colores = c("#ffffcc","#c2e699","#78c679","#31a354","#006837")
  
  Plot.Mapa(
    datos = Graduados_actual(),
    tipo = c("Deptos"),
    titulo = paste("Graduados",ult_per()[["total"]]),
    cortes = c(0,1,5,10,100,Inf),
    colores = colores[],
    colSedes = rep("red",9),
    colBorde = c("#7fff2e"),
    opacidad = 0.8
  )
  
})

# total depto y municipios

output$grad_lug_nac_tmd <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual(),
    tipo = c("DeptoMpio"),
    titulo = paste("Graduados",ult_per()[["total"]]),
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})


# * * grad_sexo ---------------------------------------------------------------

#***** sexo graduados serie *****#

output$grad_sexo_serie <- renderHighchart({
  
  #Colores Sexo
  
  sexo_general <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo_general,paleta_sexo)
  
  sexo_actual <- Consolidado() %>%  filter(Variable == "SEXO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_sexo %>% filter(sexo_general %in% sexo_actual) %>% 
    select(paleta_sexo) %>% pull()
  
  Plot.Series(datos = Consolidado(),
              categoria = "SEXO",
              titulo = "Evolución del número de graduados por sexo",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "grad_sexo_serie", suspendWhenHidden = FALSE)

#***** sexo torta *****#

output$grad_sexo_torta <- renderHighchart({
  
  #Colores Sexo
  
  sexo_general <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo_general,paleta_sexo)
  
  sexo_actual <- Consolidado() %>%  filter(Variable == "SEXO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_sexo %>% filter(sexo_general %in% sexo_actual) %>% 
    select(paleta_sexo) %>% pull()
  
  sexo <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo,paleta_sexo)
  
  Plot.Torta(datos = Consolidado(),
             categoria = "SEXO",
             titulo = "Distribución de graduados <br> por sexo <br>",
             ano = ult_per()[["last_year"]],
             periodo = ult_per()[["last_period"]],
             addPeriodo = T,
             col = colores_dinamico,
             libreria = "highcharter")
  
})



#***** sede tabla *****#
output$grad_sexo_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado(),
        categoria = "SEXO",
        encabezado = "Total graduados por sexo")
  
})



# * * grad_areas -----------------------------------------------------------

#***** areas graduados serie *****#

output$grad_areas_serie <- renderHighchart({
  
  #Colores Areas de admisión
  
  asnies_general <- sort(unique(Graduados$AREAC_SNIES))
  paleta_asnies <- c("#81337d","#41a6cb","#a03437","#176ba3","#cc6838","#e8ad5e","#94be58","#636b68")
  color_asnies <- tibble(asnies_general,paleta_asnies)
  
  asnies_actual <- Consolidado() %>%  filter(Variable == "AREAC_SNIES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_asnies %>% filter(asnies_general %in% asnies_actual) %>% 
    select(paleta_asnies) %>% pull()
  
  Plot.Series(datos = Consolidado(),
              categoria = "AREAC_SNIES",
              titulo = "Evolución del número de graduados por áreas del conocimiento SNIES",
              labelX = "Periodo",
              labelY = "Número de graduados", 
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "grad_areas_serie", suspendWhenHidden = FALSE)

#***** areas barras *****#
output$grad_areas_barra <- renderPlotly({
  
  asnies_general <- sort(unique(Graduados$AREAC_SNIES))
  paleta_asnies <- c("#81337d","#41a6cb","#a03437","#176ba3","#cc6838","#e8ad5e","#94be58","#636b68")
  color_asnies <- tibble(asnies_general,paleta_asnies)
  
  asnies_actual <- Consolidado() %>%  filter(Variable == "AREAC_SNIES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_asnies %>% filter(asnies_general %in% asnies_actual) %>% 
    select(paleta_asnies) %>% pull()
  
  
  Plot.Barras(datos = Consolidado(),
              categoria = "AREAC_SNIES",
              vertical = T,
              ordinal =  T,
              ano = ult_per()[["last_year"]],
              titulo = "Distribución de graduados por <br> áreas del conocimiento SNIES",
              periodo = ult_per()[["last_period"]],
              colores = colores_dinamico,
              libreria = "plotly")
  
})

#***** areas tabla *****#
output$grad_areas_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado(),
        categoria = "AREAC_SNIES",
        encabezado = "Total graduados por áreas del conocimiento SNIES")
  
})


# INTERFAZ PREGRADO --------------------------------------------------------------




# pre_sede_si ------------------------------------------------------------


output$pre_sede_si_ou <- renderUI({
  
  times <- input$pre_gen_bt
  
  div(id=letters[(times %% length(letters)) + 1],
      selectizeInput(inputId = 'pre_sede_si', 
                     label = 'Sede',
                     choices = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'),
                     options = list(
                       placeholder = 'Seleccione una sede',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
  )
  
})



# pre_fac_si -------------------------------------------------------------

output$pre_fac_si_ou <- renderPrint({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  
  
  if(!(is.null(input$pre_sede_si))){
    
    #***** Panel Condicional para filtro por facultad  según sede****#
    if(input$pre_sede_si %in% (unique(Graduados %>% filter(TIPO_NIVEL == "Pregrado") %>% 
                                     select(SEDE_NOMBRE_MAT))  %>% pull(SEDE_NOMBRE_MAT))){
      #**** selectInput  filtro por facultad ****#
      selectInput(
        inputId = "pre_fac_si",
        label = "Seleccione Facultad",
        choices = rbind("General",unique(Graduados %>% filter(SEDE_NOMBRE_MAT == "Bogotá") %>% 
                                        select(FACULTAD))),
        selected = "General"
      )
    }
    
  }
  
  
  
})

#*** selectinput filtro facultad dinámico según la sede escogida ***# 
observe({
  
  pre_sede <- input$pre_sede_si
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  
  if(!(is.null(pre_sede))){
    if(pre_sede != ""){
      
      updateSelectInput(
        session, 
        inputId = "pre_fac_si",
        label = paste("Facultades", pre_sede),
        choices = append(paste("Cifras Sede",input$pre_sede_si) ,unique(Graduados %>% filter(SEDE_NOMBRE_MAT == pre_sede) %>% 
                                            select(FACULTAD)) %>% arrange(FACULTAD)),
        selected = paste("Cifras Sede",input$pre_sede_si) 
      )
      
    }
  }
  
})  




# BD PRE -------------------------------------------------------


Consolidado_pre <- reactive({
  
  
  #Al inicializar el widget de la sede en NULL por un instante aparece un error
  #en ese instante el siguiente if identifica ese null para que no aparezca ese mensaje
  
  if(is.null(input$pre_sede_si)){
    
    sede <- ""
    
  }else{ 
    
    sede <- input$pre_sede_si
    
  }
  
  
  
  if(sede == "" | is.null(input$pre_fac_si)){
    
    Grad <- Graduados %>% filter(TIPO_NIVEL == "Pregrado") # Total matriculados a nivel nacional
    
    #Si el usuario ha escogido una facultad el if revisa si está
    #en las cifras generales de la sede o si está en alguna facultad
  } else if(!(input$pre_fac_si  %in% unique(Graduados %>% filter(SEDE_NOMBRE_MAT == input$pre_sede_si,
                                                                 TIPO_NIVEL == "Pregrado") %>% 
                                             select(FACULTAD) %>% pull(FACULTAD)))){
    
    Grad <- Graduados %>% filter(SEDE_NOMBRE_MAT == input$pre_sede_si,
                                 TIPO_NIVEL == "Pregrado") 
    
  } else {
    
    Grad <- Graduados %>% filter(SEDE_NOMBRE_MAT == input$pre_sede_si,
                                 FACULTAD == input$pre_fac_si,
                                 TIPO_NIVEL == "Pregrado")
    
  } 
    
    # Modificar niveles y crear factor para edad 
    
  Grad <- Grad %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
    mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
  Grad$CAT_EDAD <- factor(Grad$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))
  
 
  
    
    # Tabla agregada
    
    DT1 <- Agregar(Grad, 'SEDE_NOMBRE_ADM')
    DT2 <- Agregar(Grad, 'NACIONALIDAD')
    DT2 <- DT2 %>% filter(YEAR != 2009)
    DT3 <- Agregar(Grad, 'SEXO')
    DT4 <- Agregar(Grad, 'CAT_EDAD')
    DT4 <- DT4 %>% filter(YEAR != 2009)
    DT5 <- Agregar(Grad, 'ESTRATO')
    DT6 <- Agregar(Grad, 'MOD_ADM')
    DT7 <- Agregar(Grad, 'TIPO_ADM')
    DT8 <- Agregar(Grad, 'PAES')
    DT8 <- DT8 %>% filter(!is.na(Clase))
    DT9 <- Agregar(Grad, 'PEAMA')
    DT9 <- DT9 %>% filter(Clase != "Sin información")
    DT9 <- DT9 %>% filter(!YEAR %in% c(2009, 2010, 2011, 2012))
    DT10 <- Agregar(Grad, 'AREAC_SNIES')
    Total <- Totales(Grad)  
    
    
    Uperiodo <- Grad %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
      mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id)) %>% pull()
    
    if(Uperiodo < max(historic)){
      
      str_hist <- as.character(historic)
      years_left <- str_hist[historic > Uperiodo]
      
      DT1 <- add_ceros(DT1,'SEDE_NOMBRE_ADM',years_left)
      DT2 <- add_ceros(DT2,'NACIONALIDAD',years_left)
      DT3 <- add_ceros(DT3,'SEXO',years_left)
      DT4 <- add_ceros(DT4,'CAT_EDAD',years_left)
      DT5 <- add_ceros(DT5,'ESTRATO',years_left)
      DT6 <- add_ceros(DT6,'MOD_ADM',years_left)
      DT7 <- add_ceros(DT7,'TIPO_ADM',years_left)
      DT8 <- add_ceros(DT8,'PAES',years_left)
      DT9 <- add_ceros(DT9,'PEAMA',years_left)
      DT10 <- add_ceros(DT10,'AREAC_SNIES',years_left)
      Total <- add_ceros(Total,'TOTAL',years_left)
      
      }
    
    
    # Consolidado tabla agregada
    
    Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, Total)
    
    
    # Consolidado Estadística Matriculados 
    
    nuevo <- Agregado
    

  
})




# TABS PRE GEN ------------------------------------------------------


  
output$pre_tabset <- renderUI({
  
if(!(is.null(input$pre_sede_si))){
  if(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT)))){
    
    tabsetPanel(
      id = "pre_tabs",
      type = "pills",
      

# pre_evol_UI -------------------------------------------------------------

      
      tabPanel(
        title = "Evolución Graduados",
        
        
        
        #Series evolucion graduados
        br(),
        fluidRow(
          
          column(
            width = 12,
            
            wellPanel(
              style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
              highchartOutput("pre_evol_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                color.background = "#B1B2B0",
                                                                size = 1)
            )
            
          )
          
          
        ),
        
        fluidRow(
          column(
            width = 4,
            uiOutput("pre_total_box"),
            br()
          ),
          
          column(
            width = 4,
            uiOutput("pre_mh_box"),
            br()
          ),
          
          column(
            width = 4,
            uiOutput("pre_mh_box_actual"),
            br()
          )
        )
      ),
      

# pre_sede_UI --------------------------------------------------------------

     tabPanel(
       title = "Sede",
       
       br(),
       #Serie graduados por sede de formación
       fluidRow(
         column(
           width = 12,
           
           
           wellPanel(
             style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
             highchartOutput("pre_sede_serie") %>% withSpinner(type = 4, 
                                                               color = "#A61C31",
                                                               color.background = "#B1B2B0",
                                                               size = 1)
           ),
           
           hr()
           
           
         )
         
         
       ),
       
       
       
       fluidRow(
         
         column(
           width = 12,
           
           
           
           wellPanel(
             style =  "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
             plotlyOutput("pre_sede_barras", width = "auto", height = "auto",inline = F) %>% withSpinner(type = 4, 
                                                                                                         color = "#A61C31",
                                                                                                         color.background = "#B1B2B0",
                                                                                                         size = 1)
           ),
           
           hr()
           
           
           
         )
         
         
           
         ),
       
       
       fluidRow(
         column(
           width = 12,
           wellPanel(
             style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
             dataTableOutput("pre_sede_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                               color = "#A61C31",
                                                                               color.background = "#B1B2B0",
                                                                               size = 1)
           )
       )
         
         
         
         
         
         
       )
       
       
     ),
    


# pre_nac_UI --------------------------------------------------------------

tabPanel(
  title = "Nacionalidad",
  
  br(),
  #Serie graduados por nivel de formación
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_nac_serie") %>% withSpinner(type = 4, 
                                                        color = "#A61C31",
                                                        color.background = "#B1B2B0",
                                                        size = 1)
      ),
      hr()
      
      
    )
    
    
  ),
  
  fluidRow(
    
    column(
      width = 6,
      
      
      
      wellPanel(
        style =  "background-color: #fff; 
        border-color: #2c3e50; 
        box-shadow: 1px 1px 5px;
        height:471px;",
        highchartOutput("pre_nac_torta", width = "auto", height = "auto")%>% withSpinner(type = 4, 
                                                                                         color = "#A61C31",
                                                                                         color.background = "#B1B2B0",
                                                                                         size = 1)
      )
      
      
      
    ),
    
    column(
      width = 6,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_nac_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                         color = "#A61C31",
                                                                         color.background = "#B1B2B0",
                                                                         size = 1)
      )
      
    )
  ),
  
  
  ),


# pre_lug_UI -----------------------------------------------------------------

tabPanel(
  title = "Lugar de Nacimiento",
  id = "maps-container",
  br(),
  
  fluidRow(
    column(
      width = 12,
      div(class = "lines-background",
          wellPanel(
            style = 'background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;background: rgb(203,200,62);
background: -moz-linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
background: -webkit-linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
background: linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
filter: progid:DXImageTransform.Microsoft.gradient(startColorstr="#cbc83e",endColorstr="#0d865d",GradientType=1);;
              ',
            tabsetPanel(
              id = "tabs-maps",
              selected = "Por municipio",
              type = "pills",
              
              tabPanel(
                title = "Por municipio",
                br(),
                div(
                  leafletOutput("pre_lug_nac_pm", height = "115vh",
                                width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                 color.background = "#B1B2B0",
                                                                 size = 1)
                )
              ),
              
              tabPanel(
                title = "Total por municipio",
                br(),
                leafletOutput("pre_lug_nac_tm",height = "115vh", width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                   color.background = "#B1B2B0",
                                                                                                   size = 1)
              ),
              
              tabPanel(
                title = "Total por departamentos",
                br(),
                leafletOutput("pre_lug_nac_td", height = "115vh",
                              width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                               color.background = "#B1B2B0",
                                                               size = 1)
              ),
              
              tabPanel(
                title = "Total por departamentos y municipios",
                br(),
                leafletOutput("pre_lug_nac_tmd", height = "115vh",
                              width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                               color.background = "#B1B2B0",
                                                               size = 1)
              )
            )
          )
      )
    )
  )
),


# pre_sexo_UI ---------------------------------------------------------------
  
  
  #***** tab sede de formación sexo 
  tabPanel(
    
    title = "Sexo",
    
    br(),
    
    #Serie graduados por sexo
    fluidRow(
      column(
        width = 12,
        
        
        wellPanel(
          style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
          highchartOutput("pre_sexo_serie") %>% withSpinner(type = 4, 
                                                            color = "#A61C31",
                                                            color.background = "#B1B2B0",
                                                            size = 1)
        ),
        
        hr()
        
        
      )
      
      
    ),
    
   
    
    fluidRow(
      
      column(
        width = 6,
        
        
        
        wellPanel(
          style =  "background-color: #fff;
          border-color: #2c3e50; 
          box-shadow: 1px 1px 5px;
          height:454px;",
          highchartOutput("pre_sexo_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, 
                                                                                             color = "#A61C31",
                                                                                             color.background = "#B1B2B0",
                                                                                             size = 1)
        )
        
        
        
      ),
      
      column(
        width = 6,
        wellPanel(
          style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
          dataTableOutput("pre_sexo_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                           color = "#A61C31",
                                                                           color.background = "#B1B2B0",
                                                                           size = 1)
        )
        
      )
      
      
      
      
      
      
    )),#-----tab Sexo



# pre_edad_UI -------------------------------------------------------------

tabPanel(
  title = "Edad",
  
  br(),
  #Serie graduados por nivel de formación
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_edad_serie") %>% withSpinner(type = 4, 
                                                          color = "#A61C31",
                                                          color.background = "#B1B2B0",
                                                          size = 1)
      ),
      
      hr()
      
      
    )
    
    
  ),
 
  fluidRow(
    
    column(
      width = 12,
      
      
      
      wellPanel(
        style =  "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        plotlyOutput("pre_edad_barras", width = "auto", height = "auto",inline = F) %>% withSpinner(type = 4, 
                                                                                                    color = "#A61C31",
                                                                                                    color.background = "#B1B2B0",
                                                                                                    size = 1)
      ),
      
      hr()
      
      
      
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_edad_tabla", width = "100%")%>% withSpinner(type = 4, 
                                                                         color = "#A61C31",
                                                                         color.background = "#B1B2B0",
                                                                         size = 1)
      )
      
    )
  )
  
),#pre_edad_panel


# pre_estrado_UI -------------------------------------------------------------

tabPanel(
  title = "Estrato",
  
  br(),
  #Serie graduados por nivel de formación
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_estrato_serie") %>% withSpinner(type = 4, 
                                                             color = "#A61C31",
                                                             color.background = "#B1B2B0",
                                                             size = 1)
      )
      
      
    )
    
    
  ),
  
  hr(),
  
  fluidRow(
    
    column(
      width = 12,
      
      
      
      wellPanel(
        style =  "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        plotlyOutput("pre_estrato_barras", width = "auto", height = "auto",inline = F) %>% withSpinner(type = 4, 
                                                                                                      color = "#A61C31",
                                                                                                      color.background = "#B1B2B0",
                                                                                                      size = 1)
      )
      
      
      
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_estrato_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                             color = "#A61C31",
                                                                             color.background = "#B1B2B0",
                                                                             size = 1)
      )
      
    )
  )
  
),#pre_estrato_panel

# pre_mod_adm_UI ---------------------------------------------------------------


#***** tab sede de formación sexo 
tabPanel(
  
  title = "Modalidad de admisión",
  
  br(),
  
  #Serie graduados por sexo
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; 
        border-color: #2c3e50; 
        box-shadow: 1px 1px 5px;",
        highchartOutput("pre_mod_adm_serie") %>% withSpinner(type = 4, 
                                                             color = "#A61C31",
                                                             color.background = "#B1B2B0",
                                                             size = 1)
      ),
      
      hr()
      
      
    )
    
    
  ),
  
  
  fluidRow(
    
    column(
      width = 6,
      
      
      
      wellPanel(
        style =  "background-color: #fff; 
        border-color: #2c3e50; 
        box-shadow: 1px 1px 5px;
        height:471px;",
        highchartOutput("pre_mod_adm_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, 
                                                                                              color = "#A61C31",
                                                                                              color.background = "#B1B2B0",
                                                                                              size = 1)
      )
      
      
      
    ),
    
    column(
      width = 6,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_mod_adm_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                             color = "#A61C31",
                                                                             color.background = "#B1B2B0",
                                                                             size = 1)
      )
      
    )
    
    
    
    
    
    
  )),#-----tab mod admisión

# pre_tipo_adm_UI ---------------------------------------------------------------


#***** tab tipo de admisión
tabPanel(
  
  title = "Tipo de admisión",
  
  br(),
  
  #Serie graduados por tipo de admisión
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_tipo_adm_serie") %>% withSpinner(type = 4, 
                                                             color = "#A61C31",
                                                             color.background = "#B1B2B0",
                                                             size = 1)
      ),
      
      hr()
      
      
    )
    
    
  ),
  
  
  
  fluidRow(
    
    column(
      width = 6,
      
      
      
      wellPanel(
        style =  "background-color: #fff;
        border-color: #2c3e50; 
        box-shadow: 1px 1px 5px;
        height:471px;",
        highchartOutput("pre_tipo_adm_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, 
                                                                                               color = "#A61C31",
                                                                                               color.background = "#B1B2B0",
                                                                                               size = 1)
      )
      
      
      
    ),
    
    column(
      width = 6,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_tipo_adm_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                              color = "#A61C31",
                                                                              color.background = "#B1B2B0",
                                                                              size = 1)
      )
      
    )
    
    
    
    
    
    
  )),#-----tab tipo de adm

# pre_paes_UI -------------------------------------------------------------

tabPanel(
  title = "PAES",
  
  br(),
  #Serie graduados por nivel de formación
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_paes_serie") %>% withSpinner(type = 4, 
                                                          color = "#A61C31",
                                                          color.background = "#B1B2B0",
                                                          size = 1)
      ),
      
      hr()
      
      
    )
    
    
  ),
 
  
  fluidRow(
    
    column(
      width = 12,
      
      
      
      wellPanel(
        style =  "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        plotlyOutput("pre_paes_barras", width = "auto", height = "auto",inline = F) %>% withSpinner(type = 4, 
                                                                                                   color = "#A61C31",
                                                                                                   color.background = "#B1B2B0",
                                                                                                   size = 1)
      ),
      
      hr()
      
      
      
    )
  ),

  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_paes_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                         color = "#A61C31",
                                                                         color.background = "#B1B2B0",
                                                                         size = 1)
      )
      
    )
  )
  
),


# pre_peama_UI -------------------------------------------------------------

tabPanel(
  title = "PEAMA",
  
  br(),
  #Serie graduados por nivel de formación
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_peama_serie") %>% withSpinner(type = 4, 
                                                           color = "#A61C31",
                                                           color.background = "#B1B2B0",
                                                           size = 1)
      ),
      
      hr()
      
      
    )
    
    
  ),
  
  
  fluidRow(
    
    column(
      width = 12,
      
      
      
      wellPanel(
        style =  "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        plotlyOutput("pre_peama_barras", width = "auto", height = "auto",inline = F) %>% withSpinner(type = 4, 
                                                                                                     color = "#A61C31",
                                                                                                     color.background = "#B1B2B0",
                                                                                                     size = 1)
      ),
      
      hr()
      
      
      
    )
  ),
  
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_peama_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                           color = "#A61C31",
                                                                           color.background = "#B1B2B0",
                                                                           size = 1)
      )
      
    )
  )
  
),

# pre_areas_UI -------------------------------------------------------------

tabPanel(
  title = "Áreas del conocimiento",
  
  br(),
  #Serie graduados por nivel de formación
  fluidRow(
    column(
      width = 12,
      
      
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        highchartOutput("pre_areas_serie") %>% withSpinner(type = 4, 
                                                           color = "#A61C31",
                                                           color.background = "#B1B2B0",
                                                           size = 1)
      ),
      
      hr()
      
      
    )
    
    
  ),
  
  
  fluidRow(
    
    column(
      width = 12,
      
      
      
      wellPanel(
        style =  "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px;",
        plotlyOutput("pre_areas_barras", width = "auto", height = "auto",inline = F) %>% withSpinner(type = 4, 
                                                                                                     color = "#A61C31",
                                                                                                     color.background = "#B1B2B0",
                                                                                                     size = 1)
      ),
      
      hr()
      
      
      
    )
  ),
  
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
        dataTableOutput("pre_areas_tabla", width = "100%") %>% withSpinner(type = 4, 
                                                                           color = "#A61C31",
                                                                           color.background = "#B1B2B0",
                                                                           size = 1)
      )
      
    )
  )
  
)















)
    
    
  } else {
    # No información ----------------------------------------------------------
    withTags({
      div(class="jumbotron",
          h1("No información",
             class = "display-3"),
          p(class = "lead",
            "la sede aún no cuenta con cifras de graduados en pregrado")
      )
    })
  }
}
  

})

# * GRAFICAS --------------------------------------------------------------

#  Eliminar Panel Sede si se escoge alguna sede ----------------------------



observeEvent(input$pre_fac_si,{
  
  if(input$pre_fac_si == paste("Cifras Sede",input$pre_sede_si)){
    hideTab(inputId = "pre_tabs", target = "Sede")
  }
  
})


# Eliminar Panel Lugar de Información -------------------------------------

observeEvent(input$pre_fac_si, {
  
  if(input$pre_fac_si != paste("Cifras Sede",input$pre_sede_si)){
    hideTab(inputId = "pre_tabs", target = "Lugar de Nacimiento")
  }
  
})

# Eliminar panel Areas del conocimiento sede si escoge alguna facultad ---------------------------


observeEvent(input$pre_fac_si, {
  if(input$pre_fac_si != paste("Cifras Sede",input$pre_sede_si)){
    hideTab(inputId = "pre_tabs", target = "Áreas del conocimiento")
  }
  
})





# inputs ------------------------------------------------------------------

output$value_pre <- renderPrint({
  input$pre_sede_si
})

output$value2_pre <- renderPrint({
  input$pre_fac_si
})

output$value3_pre <- renderPrint({
  input$pre_prog_si
})


# * * sede_actual_pre ---------------------------------------------------------



sede_actual_pre <- reactive({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  if(!(is.null(input$pre_sede_si))){
    if(is.null(input$pre_fac_si)){
      
      if(input$pre_sede_si == "" | input$pre_gen_bt){
        sede <- "Cifras generales Universidad Nacional de Colombia (k: miles)."
      } else {
        sede <- paste("Sede",paste0(input$pre_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      }
      
    } else {
      
      if(input$pre_sede_si == ""){
        sede <- "Cifras generales Universidad Nacional de Colombia."
      } else if(input$pre_fac_si == paste("Cifras Sede",input$pre_sede_si)){
        sede <- paste("Sede",paste0(input$pre_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      } else {
        sede <- paste("Facultad de",paste0(input$pre_fac_si,","),"Sede",paste0(input$pre_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      }
    }
  }
  
})


# * * pre_evol ----------------------------------------------------------------

output$pre_evol_serie <- renderHighchart({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  
  Plot.Series(
    datos = Consolidado_pre(),
    categoria = "TOTAL",
    titulo = "Evolución histórica del total de estudiantes graduados en pregrado",
    labelY = "Número de graduados  ",
    libreria = "highcharter",
    estilo = list(hc.Credits = sede_actual_pre(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
  
})

outputOptions(output, "pre_evol_serie", priority = 1)

####--- info box total graduados UI ---###

output$pre_total_box <- renderUI({
  
  div(class = "lines-background",
      div(class = "cont2 border-green green-maps",
          div(class = "info-tl",
              p(class = "info-title", "Total Graduados"),
              p(class = "info-subtitle info-sub-black",
                paste("Número de graduados en pregrado histórico desde el 2009")
              )
          ),
          
          div(class = "info-num",
              div(class = "info-num-1",
                  p(id = "element",class = "num-m un-green",pre_evol_ib_h() + pre_evol_ib_m()),
                  p(class = "txt-m", "Número de graduados")
              )
          )
      )
  )
  
})

####--- info box total graduados por genero ---###

output$pre_mh_box <- renderUI({
  
  div(class = "lines-background",
      div(class = "cont2 border-green green-maps",
          div(class = "info-tl",
              p(class = "info-title", "Graduados"),
              p(class ="info-subtitle","Número de graduados en pregrado por sexo desde el 2009")
          ),
          
          div(class = "info-num",
              
              div(class = "info-num-1",
                  p(class = "num-m un-green",pre_evol_ib_m()),
                  p(class = "txt-m", "Mujeres")
              ),
              
              div(class = "info-num-2",
                  p(class = "num-h un-green",pre_evol_ib_h()),
                  p(class = "txt-h","Hombres")
              )
          )
      )
  )
  
})

# extraer número de graduados mujeres historico

pre_evol_ib_m <- reactive({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pre() %>% filter(Variable == "SEXO",
                                    Clase == "Mujeres") %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

# extraer número graduados hombres historico

pre_evol_ib_h <- reactive({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pre() %>% filter(Variable == "SEXO",
                                    Clase == "Hombres") %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

####--- info box graduados semestre actual por genero ---###}

output$pre_mh_box_actual <- renderUI({
  
  div(class = "lines-background",
      div(
        class = "cont2 border-green green-maps",
        
        div(
          class = "info-tl",
          p(class = "info-title", paste("Graduados",ult_per()[[1]])),
          p(class = "info-subtitle info-sub-black",
            paste("Número de graduados en pregrado por sexo 
                        semestre",ult_per()[[1]])),
          
          
        ),
        
        div(
          class = "info-num",
          div(
            class = "info-num-1",
            p(class = "num-m un-green",pre_evol_ib_ma()),
            p(class = "txt-m", "Mujeres")
          ),
          
          div(
            class = "info-num-2",
            p(class = "num-h un-green",pre_evol_ib_ha()),
            p(class = "txt-h","Hombres")
          )
        )
        
        
      ))
})

# extraer número de graduados mujeres semestre Anterior (ma) número

pre_evol_ib_ma <- reactive({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pre() %>% filter(Variable == "SEXO",
                                    Clase == "Mujeres",
                                    YEAR == ult_per()[["last_year"]],
                                    SEMESTRE == ult_per()[["last_period"]]) %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

# extraer número graduados hombres semestre Anterior (ha)

pre_evol_ib_ha <- reactive({
  
  req(input$pre_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Pregrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pre() %>% filter(Variable == "SEXO",
                                    Clase == "Hombres",
                                    YEAR == ult_per()[["last_year"]],
                                    SEMESTRE == ult_per()[["last_period"]]) %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

#outputOptions(output, "pre_evol_serie", suspendWhenHidden = FALSE)

# * * pre_sede --------------------------------------------------------------



#***** sede graduados serie *****#

output$pre_sede_serie <- renderHighchart({
  
  sedes <- c("Amazonía","Bogotá","Caribe","De La Paz","Manizales","Medellín","Orinoquía","Palmira","Tumaco")
  paleta_sedes <- c("#29abe2","#8cc63f","#c1272d","#9e9ac8","#0071bc","#f15a24","#fbb03b","#93278f","#6d6666")
  color_sedes <- tibble(sedes,paleta_sedes)
  
  Plot.Series(datos = Consolidado_pre(),
              categoria = "SEDE_NOMBRE_ADM",
              titulo = "Evolución de número de graduados en pregrado por sedes",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = color_sedes %>% select(paleta_sedes) %>% pull(),
              estilo = list(hc.Credits = sede_actual_pre(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_sede_serie", suspendWhenHidden = FALSE)

#***** sede barras *****#
output$pre_sede_barras <- renderPlotly({
  
  sedes <- c("Amazonía","Bogotá","Caribe","De La Paz","Manizales","Medellín","Orinoquía","Palmira","Tumaco")
  paleta_sedes <- c("#29abe2","#8cc63f","#c1272d","#9e9ac8","#0071bc","#f15a24","#fbb03b","#93278f","#6d6666")
  color_sedes <- tibble(sedes,paleta_sedes)
  
  Plot.Barras(datos = Consolidado_pre(),
              categoria = "SEDE_NOMBRE_ADM",
              vertical = F,
              ano = ult_per()[["last_year"]],
              periodo = ult_per()[["last_period"]],
              colores = color_sedes %>% select(paleta_sedes) %>% pull(),
              titulo = "Evolución del número de graduados <br> en pregrado por sedes",
              libreria = "plotly")
  
})

#***** sede tabla *****#
output$pre_sede_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pre(),
        categoria = "SEDE_NOMBRE_ADM",
        encabezado = "Total estudiantes en pregrado por sede ")
  
})



# * * pre_nac -------------------------------------------------------------

output$pre_nac_serie <- renderHighchart({
  
  nac_general <- sort(unique(Graduados$NACIONALIDAD))
  paleta_nac <- c("#8cc63f","#f15a24","#0071bc")
  color_nac <- tibble(nac_general,paleta_nac)
  
  nac_actual <- Consolidado_pre() %>%  filter(Variable == "NACIONALIDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nac %>% filter(nac_general %in% nac_actual) %>% 
    select(paleta_nac) %>% pull()
    
    Plot.Series(
      datos = Consolidado_pre(),
      categoria = 'NACIONALIDAD',
      titulo = "Evolución del número de graduados en pregrado según nacionalidad",
      labelY = "Número de graduados  ",
      libreria = "highcharter",
      colores= colores_dinamico,
      estilo = list(hc.Credits = sede_actual_pre(),
                    hc.Slider = FALSE,
                    hc.Tema = 1)
    ) %>% hc_tooltip(backgroundColor= "#232323")
    
  })


#outputOptions(output, "pre_nac_serie", suspendWhenHidden = FALSE)

output$pre_nac_torta <- renderHighchart({
  
  nac_general <- sort(unique(Graduados$NACIONALIDAD))
  paleta_nac <- c("#8cc63f","#f15a24","#0071bc")
  color_nac <- tibble(nac_general,paleta_nac)
  
  nac_actual <- Consolidado_pre() %>%  filter(Variable == "NACIONALIDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nac %>% filter(nac_general %in% nac_actual) %>% 
    select(paleta_nac) %>% pull()
  
  
  Plot.Torta(datos = Consolidado_pre(),
             categoria = 'NACIONALIDAD',
             titulo = "Distribución de graduados <br> en pregrado según nacionalidad <br>",
             ano = ult_per()[["last_year"]],
             periodo = ult_per()[["last_period"]],
             addPeriodo = T,
             colores= colores_dinamico,
             libreria = "highcharter")
  
  
  
})

# Sexo graduados pregrado

output$pre_nac_tabla <- renderDataTable({
  
  
  #Tabla sexo todas las sedes
  Tabla(datos = Consolidado_pre(),
        categoria = 'NACIONALIDAD',
        encabezado = 'Total estudiantes en pregrado por nacionalidad')
  
  
})








#  * * pre_lug_nac -------------------------------------------------------

Graduados_actual_pre <- reactive({
  
  if(input$pre_sede_si != ""){
    
    Graduados_actual = Graduados %>% filter(SEDE_NOMBRE_ADM == input$pre_sede_si,
                                            TIPO_NIVEL == "Pregrado") %>% 
      filter(YEAR == max(YEAR)) %>% 
      filter(SEMESTRE == max(SEMESTRE)) %>%
      filter(!is.na(Departamento))
    
  } else {
    
    Graduados_actual = Graduados %>%  
      filter(TIPO_NIVEL == "Pregrado") %>% 
      filter(YEAR == max(YEAR)) %>% 
      filter(SEMESTRE == max(SEMESTRE)) %>%
      filter(!is.na(Departamento))
  }
  
})
# Mapa por municipios

output$pre_lug_nac_pm <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pre(),
    tipo = c("SiNoMpios"),
    titulo = "Graduados 2020-2",
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})

# Mapa Total municipio

output$pre_lug_nac_tm <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pre(),
    tipo = c("Mpios"),
    titulo = "Graduados 2020-2",
    cortes = c(0,1,5,10,100,Inf),
    colores = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})

# Total por depto

output$pre_lug_nac_td <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pre(),
    tipo = c("Deptos"),
    titulo = "Graduados 2020-2",
    cortes = c(0,1,5,10,100,Inf),
    colores = c("#ffffcc","#c2e699","#78c679","#31a354","#006837"),
    colSedes = rep("red",9),
    colBorde = c("#7fff2e"),
    opacidad = 0.8
  )
  
})

# total depto y municipios

output$pre_lug_nac_tmd <- renderLeaflet({
  
  
  Plot.Mapa(
    datos = Graduados_actual_pre(),
    tipo = c("DeptoMpio"),
    titulo = "Graduados 2020-2",
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})



# * * pre_sexo ----------------------------------------------------------------



output$pre_sexo_serie <- renderHighchart({
  
  sexo_general <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo_general,paleta_sexo)
  
  sexo_actual <- Consolidado_pre() %>%  filter(Variable == "SEXO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_sexo %>% filter(sexo_general %in% sexo_actual) %>% 
    select(paleta_sexo) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pre(),
    categoria = "SEXO",
    titulo = "Evolución del número de graduados en pregrado por sexo",
    labelY = "Número de graduados",
    libreria = "highcharter",
    colores = colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pre(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_sexo_serie", suspendWhenHidden = FALSE)

output$pre_sexo_torta <- renderHighchart({
  
  sexo_general <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo_general,paleta_sexo)
  
  sexo_actual <- Consolidado_pre() %>%  filter(Variable == "SEXO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_sexo %>% filter(sexo_general %in% sexo_actual) %>% 
    select(paleta_sexo) %>% pull()
  
  
  Plot.Torta(datos = Consolidado_pre(),
             categoria = "SEXO",
             titulo = "Distribución de graduados <br> en pregrado por sexo <br>",
             ano = ult_per()[["last_year"]],
             periodo = ult_per()[["last_period"]],
             addPeriodo = T,
             col = colores_dinamico,
             libreria = "highcharter")
  
  
  
})

# Sexo graduados pregrado

output$pre_sexo_tabla <- renderDataTable({
  
  
  #Tabla sexo todas las sedes
  Tabla(datos = Consolidado_pre(),
        categoria = "Sexo",
        encabezado = "Total graduados en pregrado por sexo")
  
  
})

# * * pre_edad --------------------------------------------------------------



#***** edad serie *****#

output$pre_edad_serie <- renderHighchart({
  
  Grad <- Graduados %>% filter(TIPO_NIVEL == "Pregrado") %>% 
    mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
    mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
  
  Grad$CAT_EDAD <- factor(Grad$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))
  
  edad_general <- sort(Grad %>% select("CAT_EDAD") %>% unique() %>% pull())
  paleta_edad <- c("#8cc63f","#f15a24","#0071bc","#6d6666")
  color_edad <- tibble(edad_general,paleta_edad)
  
  edad_actual <- Consolidado_pre() %>%  filter(Variable == "CAT_EDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_edad %>% filter(edad_general %in% edad_actual) %>% 
    select(paleta_edad) %>% pull()
  
  Plot.Series(datos = Consolidado_pre(),
              categoria = "CAT_EDAD",
              titulo = "Evolución del número de graduados en pregrado por grupos de edad",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_edad_serie", suspendWhenHidden = FALSE)

#***** edad barras *****#
output$pre_edad_barras <- renderPlotly({
  
  Grad <- Graduados %>% filter(TIPO_NIVEL == "Pregrado") %>% 
    mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
    mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
  
  Grad$CAT_EDAD <- factor(Grad$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))
  
  edad_general <- sort(Grad %>% select("CAT_EDAD") %>% unique() %>% pull())
  paleta_edad <- c("#8cc63f","#f15a24","#0071bc","#6d6666")
  color_edad <- tibble(edad_general,paleta_edad)
  
  edad_actual <- Consolidado_pre() %>%  filter(Variable == "CAT_EDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_edad %>% filter(edad_general %in% edad_actual) %>% 
    select(paleta_edad) %>% pull()
  
  Plot.Barras(datos = Consolidado_pre(),
              categoria = "CAT_EDAD",
              vertical = F,
              ordinal = F,
              titulo = paste("Distribución de graduados en pregrado <br> por grupos de edad"),
              ano = ult_per()[["last_year"]],
              periodo = ult_per()[["last_period"]],
              colores = colores_dinamico,
              libreria = "plotly")
  
})

#***** edad tabla *****#
output$pre_edad_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pre(),
        categoria = "CAT_EDAD",
        encabezado = "Total graduados en pregrado por grupos de edad")
  
})




# * * pre_estrato --------------------------------------------------------------



#***** estrato serie *****#

output$pre_estrato_serie <- renderHighchart({
  
  estrato_general <- sort(Graduados %>% select("ESTRATO") %>% unique() %>% pull())
  paleta_estrato <- c("#8cc63f","#f15a24","#0071bc","#6d6666")
  color_estrato <- tibble(estrato_general,paleta_estrato)
  
  estrato_actual <- Consolidado_pre() %>%  filter(Variable == "ESTRATO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_estrato %>% filter(estrato_general %in% estrato_actual) %>% 
    select(paleta_estrato) %>% pull()
  
  Plot.Series(datos = Consolidado_pre(),
              categoria = 'ESTRATO',
              titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual_pre(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_estrato_serie", suspendWhenHidden = FALSE)

#***** edad barras *****#
output$pre_estrato_barras <- renderPlotly({
  
  estrato_general <- sort(Graduados %>% select("ESTRATO") %>% unique() %>% pull())
  paleta_estrato <- c("#8cc63f","#f15a24","#0071bc","#6d6666")
  color_estrato <- tibble(estrato_general,paleta_estrato)
  
  estrato_actual <- Consolidado_pre() %>%  filter(Variable == "ESTRATO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_estrato %>% filter(estrato_general %in% estrato_actual) %>% 
    select(paleta_estrato) %>% pull()
  
  Plot.Barras(datos = Consolidado_pre(),
              categoria = "ESTRATO",
              titulo = "Distribución de graduados en pregrado <br> por estrato socioeconómico",
              vertical = F,
              ano = ult_per()[["last_year"]],
              periodo = ult_per()[["last_period"]],
              colores = colores_dinamico,
              libreria = "plotly")
  
})

#***** edad tabla *****#
output$pre_estrato_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pre(),
        categoria = "ESTRATO",
        encabezado = "Total graduados en pregrado por estratos socioeconómicos")
  
})





# * * pre_mod_adm ----------------------------------------------------------------



output$pre_mod_adm_serie <- renderHighchart({
  
  modalidad_general <- sort(unique(Graduados$MOD_ADM))
  paleta_modalidad <- c("#f15a24","#8cc63f")
  color_modalidad <- tibble(modalidad_general,paleta_modalidad)
  
  modalidad_actual <- Consolidado_pre() %>%  filter(Variable == "MOD_ADM") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_modalidad %>% filter(modalidad_general %in% modalidad_actual) %>% 
    select(paleta_modalidad) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pre(),
    categoria = "MOD_ADM",
    titulo = "Evolución del número de graduados en pregrado 
    por modalidad de admisión",
    labelY = "Número de graduados",
    libreria = "highcharter",
    colores = colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pre(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_mod_adm_serie", suspendWhenHidden = FALSE)




output$pre_mod_adm_torta <- renderHighchart({

  modalidad_general <- sort(unique(Graduados$MOD_ADM))
  paleta_modalidad <- c("#f15a24","#8cc63f")
  color_modalidad <- tibble(modalidad_general,paleta_modalidad)
  
  modalidad_actual <- Consolidado_pre() %>%  filter(Variable == "MOD_ADM") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_modalidad %>% filter(modalidad_general %in% modalidad_actual) %>% 
    select(paleta_modalidad) %>% pull()
  
  Plot.Torta(datos = Consolidado_pre(),
             categoria = "MOD_ADM",
             titulo = "Distribución de graduados  en pregrado <br> según modalidad de admisión <br>",
             ano = ult_per()[["last_year"]],
             periodo = ult_per()[["last_period"]],
             addPeriodo = T,
             colores = colores_dinamico
             ,
             libreria = "highcharter")
  
  
  
})

# Sexo graduados pregrado

output$pre_mod_adm_tabla <- renderDataTable({
  
  
  #Tabla modalidad de admisión todas las sedes
  Tabla(datos = Consolidado_pre(),
        categoria = "MOD_ADM",
        encabezado = "Total graduados en pregrado según modalidad de admisión")
  
  
})


# * * pre_tipo_adm ----------------------------------------------------------------



output$pre_tipo_adm_serie <- renderHighchart({
  
  tipo_general <- sort(unique(Graduados$TIPO_ADM))
  paleta_tipo <- c("#f15a24","#6d6666","#117bc0","#8cc63f")
  color_tipo <- tibble(tipo_general,paleta_tipo)
  
  tipo_actual <- Consolidado_pre() %>%  filter(Variable == "TIPO_ADM") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_tipo %>% filter(tipo_general %in% tipo_actual) %>% 
    select(paleta_tipo) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pre(),
    categoria = "TIPO_ADM",
    titulo = "Evolución del número de graduados en pregrado por programa de admisión",
    labelY = "Número de graduados  ",
    libreria = "highcharter",
    colores = colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pre(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_tipo_adm_serie", suspendWhenHidden = FALSE)



output$pre_tipo_adm_torta <- renderHighchart({
  
  tipo_general <- sort(unique(Graduados$TIPO_ADM))
  paleta_tipo <- c("#f15a24","#6d6666","#117bc0","#8cc63f")
  color_tipo <- tibble(tipo_general,paleta_tipo)
  
  tipo_actual <- Consolidado_pre() %>%  filter(Variable == "TIPO_ADM") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_tipo %>% filter(tipo_general %in% tipo_actual) %>% 
    select(paleta_tipo) %>% pull()
  
  
  Plot.Torta(datos = Consolidado_pre(),
             categoria = "TIPO_ADM",
             titulo = "Distribución de graduados en pregrado <br> por programa de admisión <br>",
             ano = ult_per()[["last_year"]],
             periodo = ult_per()[["last_period"]],
             addPeriodo = T,
             colores = colores_dinamico,
             libreria = "highcharter")
  
  
  
})

# tipo adm graduados pregrado

output$pre_tipo_adm_tabla <- renderDataTable({
  
  
  #Tabla sexo todas las sedes
  Tabla(datos = Consolidado_pre(),
        categoria = "TIPO_ADM",
        encabezado = "Total graduados en pregrado por programa de admisión")
  
  
})

# * * pre_paes --------------------------------------------------------------



#***** paes serie *****#

output$pre_paes_serie <- renderHighchart({
  
  paes_general <- sort(unique(Graduados$PAES))
  paleta_paes <- c("#4982a4","#dfc38d","#666967","#ba6c56","#9fbf7e")
  color_paes <- tibble(paes_general,paleta_paes)
  
  paes_actual <- Consolidado_pre() %>%  filter(Variable == "PAES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_paes %>% filter(paes_general %in% paes_actual) %>% 
    select(paleta_paes) %>% pull()
  
  Plot.Series(datos = Consolidado_pre(),
              categoria = 'PAES',
              titulo = "Evolución del número de graduados en pregrado del programa PAES",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual_pre(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_paes_serie", suspendWhenHidden = FALSE)

#***** paes barras *****#
output$pre_paes_barras <- renderPlotly({
  
  paes_general <- sort(unique(Graduados$PAES))
  paleta_paes <- c("#4982a4","#dfc38d","#666967","#ba6c56","#9fbf7e")
  color_paes <- tibble(paes_general,paleta_paes)
  
  paes_actual <- Consolidado_pre() %>%  filter(Variable == "PAES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_paes %>% filter(paes_general %in% paes_actual) %>% 
    select(paleta_paes) %>% pull()
  
  Plot.Barras(datos = Consolidado_pre(),
              categoria = "PAES",
              titulo = "Distribución de graduados en pregrado <br> del programa PAES",
              vertical = T,
              ano = ult_per()[["last_year"]],
              periodo = ult_per()[["last_period"]],
              colores = colores_dinamico,
              libreria = "plotly")
  
})

#***** edad tabla *****#
output$pre_paes_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pre(),
        categoria = "PAES",
        encabezado = "Total graduados en pregrado por programa PAES")
  
})

# * * pre_peama --------------------------------------------------------------



#***** peama serie *****#

output$pre_peama_serie <- renderHighchart({
  
  peama_general <- sort(unique(Graduados$PEAMA))
  paleta_peama <- c("#29abe2","#c1272d","#fbb03b")
  color_peama <- tibble(peama_general,paleta_peama)
  
  peama_actual <- Consolidado_pre() %>%  filter(Variable == "PEAMA") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_peama %>% filter(peama_general %in% peama_actual) %>% 
    select(paleta_peama) %>% pull()
  
  
  
  Plot.Series(datos = Consolidado_pre(),
              categoria = 'PEAMA',
              titulo = "Evolución del número de graduados en pregrado del programa PEAMA",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = c("#29abe2","#c1272d","#fbb03b"),
              estilo = list(hc.Credits = sede_actual_pre(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_peama_serie", suspendWhenHidden = FALSE)

#***** paes barras *****#
output$pre_peama_barras <- renderPlotly({
  
  Plot.Barras(datos = Consolidado_pre(),
              categoria = "PEAMA",
              titulo = "Distribución de graduados en pregrado <br> del programa PEAMA",
              vertical = F,
              ano = ult_per()[["last_year"]],
              periodo = ult_per()[["last_period"]],
              colores = c("#29abe2","#c1272d","#fbb03b"),
              libreria = "plotly")
  
})

#***** edad tabla *****#
output$pre_peama_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pre(),
        categoria = "PEAMA",
        encabezado = "Total graduados en pregrado por programa PEAMA")
  
})




# * * pre_areas --------------------------------------------------------------



#***** areas serie *****#

output$pre_areas_serie <- renderHighchart({
  
  asnies_general <- sort(unique(Graduados$AREAC_SNIES))
  paleta_asnies <- c("#81337d","#41a6cb","#a03437","#176ba3","#cc6838","#e8ad5e","#94be58","#636b68")
  color_asnies <- tibble(asnies_general,paleta_asnies)
  
  asnies_actual <- Consolidado_pre() %>%  filter(Variable == "AREAC_SNIES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_asnies %>% filter(asnies_general %in% asnies_actual) %>% 
    select(paleta_asnies) %>% pull()
  
  Plot.Series(datos = Consolidado_pre(),
              categoria = 'AREAC_SNIES',
              titulo = "Evolución del número de graduados en pregrado por áreas del conocimiento SNIES",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual_pre(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pre_areas_serie", suspendWhenHidden = FALSE)

#***** areas barras *****#
output$pre_areas_barras <- renderPlotly({
  
  asnies_general <- sort(unique(Graduados$AREAC_SNIES))
  paleta_asnies <- c("#81337d","#41a6cb","#a03437","#176ba3","#cc6838","#e8ad5e","#94be58","#636b68")
  color_asnies <- tibble(asnies_general,paleta_asnies)
  
  asnies_actual <- Consolidado_pre() %>%  filter(Variable == "AREAC_SNIES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_asnies %>% filter(asnies_general %in% asnies_actual) %>% 
    select(paleta_asnies) %>% pull()
  
  Plot.Barras(datos = Consolidado_pre(),
              categoria = 'AREAC_SNIES',
              titulo = "Distribución de graduados en pregrado <br> por áreas del conocimiento SNIES",
              vertical = F,
              ano = ult_per()[["last_year"]],
              periodo = ult_per()[["last_period"]],
              colores = colores_dinamico,
              libreria = "plotly")
  
})

#***** areas tabla *****#
output$pre_areas_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pre(),
        categoria = 'AREAC_SNIES',
        encabezado = "Total graduados en pregrado por áreas del conocimiento SNIES")
  
})


















# INTERFAZ POSGRADO -------------------------------------------------------

# pos_sede_si ------------------------------------------------------------


output$pos_sede_si_ou <- renderUI({
  
  times <- input$pos_gen_bt
  
  div(id=letters[(times %% length(letters)) + 1],
      selectizeInput(inputId = 'pos_sede_si', 
                     label = 'Sede',
                     choices = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'),
                     options = list(
                       placeholder = 'Seleccione una sede',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
  )
  
})


# pos_fac_si -------------------------------------------------------------

output$pos_fac_si_ou <- renderPrint({
  
  req(input$pos_sede_si %in% unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))
  if(!(is.null(input$pos_sede_si))){
    
    #***** Panel Condicional para filtro por facultad  según sede****#
    if(input$pos_sede_si %in% (unique(Graduados %>% filter(TIPO_NIVEL == "Postgrado") %>% 
                                      select(SEDE_NOMBRE_MAT))  %>% pull(SEDE_NOMBRE_MAT))){
      #**** selectInput  filtro por facultad ****#
      selectInput(
        inputId = "pos_fac_si",
        label = "Seleccione Facultad",
        choices = rbind("Cifras Generales",unique(Graduados %>% filter(SEDE_NOMBRE_MAT == "Bogotá") %>% 
                                           select(FACULTAD))),
        selected = "Cifras Generales"
      )
    }
    
  }
  
  
  
})

#*** selectinput filtro facultad dinámico según la sede escogida ***# 
observe({
  
  pos_sede <- input$pos_sede_si
  req(input$pos_sede_si %in% unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))
  
  if(!(is.null(pos_sede))){
    if(pos_sede != ""){
      
      updateSelectInput(
        session, 
        inputId = "pos_fac_si",
        label = paste("Facultades", pos_sede),
        choices = append(paste("Cifras Sede", pos_sede),unique(Graduados %>% filter(SEDE_NOMBRE_MAT == pos_sede) %>% 
                                            select(FACULTAD)) %>% arrange(FACULTAD)),
        selected = paste("Cifras Sede", pos_sede)
      )
      
    }
  }
  
})  

# BD POST -----------------------------------------------------------------



Consolidado_pos <- reactive({
  
  
  #Al inicializar el widget de la sede en NULL por un instante aparece un error
  #en ese instante el siguiente if identifica ese null para que no aparezca ese mensaje
  
  if(is.null(input$pos_sede_si)){
    
    sede <- ""
    
  }else{ 
    
    sede <- input$pos_sede_si
    
  }
  
  
  
  if(sede == "" | is.null(input$pos_fac_si)){
    
    Grad <- Graduados %>% filter(TIPO_NIVEL == "Postgrado") # Total matriculados a nivel nacional
    
    #Si el usuario ha escogido una facultad el if revisa si está
    #en las cifras generales de la sede o si está en alguna facultad
  } else if(!(input$pos_fac_si  %in% unique(Graduados %>% filter(SEDE_NOMBRE_MAT == input$pos_sede_si,
                                                                 TIPO_NIVEL == "Postgrado") %>% 
                                            select(FACULTAD) %>% pull(FACULTAD)))){
    
    Grad <- Graduados %>% filter(SEDE_NOMBRE_MAT == input$pos_sede_si,
                                 TIPO_NIVEL == "Postgrado") 
    
  } else {
    
    Grad <- Graduados %>% filter(SEDE_NOMBRE_MAT == input$pos_sede_si,
                                 FACULTAD == input$pos_fac_si,
                                 TIPO_NIVEL == "Postgrado")
    
  }

    
    Grad <- Grad %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
    Grad$CAT_EDAD <- factor(Grad$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))
    
  
    
    # Tabla agregada
    
    
    
    DT1 <- Agregar(Grad, "SEDE_NOMBRE_MAT")
    DT2 <- Agregar(Grad, "NIVEL")
    DT3 <- Agregar(Grad, "NACIONALIDAD")
    DT3 <- DT3 %>% filter(YEAR != 2009)
    DT4 <- Agregar(Grad, "SEXO")
    DT5 <- Agregar(Grad, "CAT_EDAD")
    DT5 <- DT5 %>% filter(YEAR != 2009) 
    DT6 <- Agregar(Grad, "CONVENIO")
    DT6 <- DT6 %>% filter(!YEAR %in% c(2009:2018))
    DT7 <- Agregar(Grad, "TIP_CONVENIO")
    DT7 <- DT7 %>% filter(!YEAR %in% c(2009:2018) & Clase != "Sin información")
    DT8 <- Agregar(Grad, "AREAC_SNIES")
    Total <- Totales(Grad)  
    
    # Consolidado tabla agregada
    
    Uperiodo <- Grad %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
      mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id)) %>% pull()
    
    if(Uperiodo < max(historic)){
      
      str_hist <- as.character(historic)
      years_left <- str_hist[historic > Uperiodo]
      
      DT1 <- add_ceros(DT1,"SEDE_NOMBRE_MAT", years_left)
      DT2 <- add_ceros(DT2,"NIVEL",years_left)
      DT3 <- add_ceros(DT3,"NACIONALIDAD",years_left)
      DT4 <- add_ceros(DT4,"SEXO",years_left)
      DT5 <- add_ceros(DT5,"CAT_EDAD",years_left)
      DT6 <- add_ceros(DT6,"CONVENIO",years_left)
      DT7 <- add_ceros(DT7,"TIP_CONVENIO",years_left)
      DT8 <- add_ceros(DT8,"AREAC_SNIES",years_left)
      Total <- add_ceros(Total,'TOTAL',years_left)
      
    }
    
    Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)
    
    
  
    
    # Consolidado Estadística Matriculados 
    
    nuevo <- Agregado 
  
  
})


# TABS POS ----------------------------------------------------------------



output$pos_tabset <- renderUI({
  
  if(!(is.null(input$pos_sede_si))){
    if(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT)))){
      tabsetPanel(
        id = "pos_tabs",
        type = "pills",
        
        # pos_evol_UI -------------------------------------------------------------
        
        tabPanel(
          
          title = "Evolución Graduados",
          
          #Series evolucion graduados
          
          br(),
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_evol_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
                )
              
              )
            ),
          
          fluidRow(
            column(
              width = 4,
              uiOutput("pos_total_box"),
              br()
            ),
            
            column(
              width = 4,
              uiOutput("pos_mh_box"),
              br()
            ),
            
            column(
              width = 4,
              uiOutput("pos_mh_box_actual"),
              br()
            )
          )
          ),
        
        # pos_sede_UI --------------------------------------------------------------
        
        tabPanel(
          title = "Sede",
          br(),
          #Serie graduados por sede de formación
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_sede_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style =  "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                plotlyOutput("pos_sede_barras", width = "auto", 
                             height = "auto",inline = F) %>% withSpinner(type = 4, color = "#A61C31", 
                                                                         color.background = "#B1B2B0",
                                                                         size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px; ",
                dataTableOutput("pos_sede_tabla", width = "100%") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                  color.background = "#B1B2B0",
                                                                                  size = 1)
                )
              )
            )
          ),
        
        # pos_nivel_UI --------------------------------------------------------------
        
        tabPanel(
          title = "Nivel de formación",
          br(),
          #Serie graduados por nivel de formación
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_nivel_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                   color.background = "#B1B2B0",
                                                                   size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 6,
              
              wellPanel(
                style =  "background-color: #fff; 
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;
                height:471px;",
                highchartOutput("pos_nivel_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                    color.background = "#B1B2B0",
                                                                                                    size = 1)
                )
              ),
            
            column(
              width = 6,
              wellPanel(
                style = "background-color: #fff;
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px; ",
                dataTableOutput("pos_nivel_tabla", width = "100%") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                   color.background = "#B1B2B0",
                                                                                   size = 1)
                )
              )
            )
          ),
        
        # pos_nac_UI --------------------------------------------------------------
        
        tabPanel(
          title = "Nacionalidad",
          br(),
          #Serie graduados por nivel de formación
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff;
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;
                height:471px;",
                highchartOutput("pos_nac_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                 color.background = "#B1B2B0",
                                                                 size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 6,
              
              wellPanel(
                style =  "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_nac_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                  color.background = "#B1B2B0",
                                                                                                  size = 1)
                )
              ),
            
            column(
              width = 6,
              wellPanel(
                style = "background-color: #fff; border-color: #2c3e50; box-shadow: 1px 1px 5px; ",
                dataTableOutput("pos_nac_tabla", width = "100%") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                 color.background = "#B1B2B0",
                                                                                 size = 1)
                )
              )
            )
          ),
        
        # pos_lug_UI -----------------------------------------------------------------
        
        tabPanel(
          title = "Lugar de Nacimiento",
          id = "maps-container",
          br(),
          
          fluidRow(
            column(
              width = 12,
              div(class = "lines-background",
                  wellPanel(
                    style = 'background-color: #fff;
              border-color: #2c3e50; 
              box-shadow: 1px 1px 5px;background: rgb(203,200,62);
background: -moz-linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
background: -webkit-linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
background: linear-gradient(87deg, rgba(203,200,62,0.5) 0%, rgba(148,180,59,0.5) 50%, rgba(13,134,93,0.5) 100%);
filter: progid:DXImageTransform.Microsoft.gradient(startColorstr="#cbc83e",endColorstr="#0d865d",GradientType=1);;
              ',
                    tabsetPanel(
                      id = "tabs-maps",
                      selected = "Por municipio",
                      type = "pills",
                      
                      tabPanel(
                        title = "Por municipio",
                        br(),
                        div(
                          leafletOutput("pos_lug_nac_pm", height = "115vh",
                                        width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                         color.background = "#B1B2B0",
                                                                         size = 1)
                        )
                      ),
                      
                      tabPanel(
                        title = "Total por municipio",
                        br(),
                        leafletOutput("pos_lug_nac_tm",height = "115vh", width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                          color.background = "#B1B2B0",
                                                                                                          size = 1)
                      ),
                      
                      tabPanel(
                        title = "Total por departamentos",
                        br(),
                        leafletOutput("pos_lug_nac_td", height = "115vh",
                                      width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                       color.background = "#B1B2B0",
                                                                       size = 1)
                      ),
                      
                      tabPanel(
                        title = "Total por departamentos y municipios",
                        br(),
                        leafletOutput("pos_lug_nac_tmd", height = "115vh",
                                      width = "77vw")  %>% withSpinner(type = 4, color = "#A61C31", 
                                                                       color.background = "#B1B2B0",
                                                                       size = 1)
                      )
                    )
                  )
              )
            )
          )
        ),
        
        # pos_sexo_UI ---------------------------------------------------------------
        
        tabPanel(
          title = "Sexo",
          br(),
          #Serie graduados por sexo
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff;
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_sexo_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 6,
              wellPanel(
                style =  "background-color: #fff;
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;
                height:454px;",
                highchartOutput("pos_sexo_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                   color.background = "#B1B2B0",
                                                                                                   size = 1)
                )
              ),
            
            column(
              width = 6,
              
              wellPanel(
                style = "background-color: #fff;
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                dataTableOutput("pos_sexo_tabla", width = "100%") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                  color.background = "#B1B2B0",
                                                                                  size = 1)
                )
              )
            )
          ),
        
        # pos_edad_UI -------------------------------------------------------------
        
        tabPanel(
          title = "Edad",
          br(),
          #Serie graduados por nivel de formación
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_edad_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                  color.background = "#B1B2B0",
                                                                  size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style =  "background-color: #fff;
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;",
                plotlyOutput("pos_edad_barras", width = "auto", height = "auto",inline = F)%>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                           color.background = "#B1B2B0",
                                                                                                           size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px; ",
                dataTableOutput("pos_edad_tabla", width = "100%") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                  color.background = "#B1B2B0",
                                                                                  size = 1)
                )
              )
            )
          ),
        
        # pos_conv_UI -------------------------------------------------------------
        
        tabPanel(
          title = "Convenio",
          br(),
          #Serie graduados por convenio
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_conv_serie")%>% withSpinner(type = 4, color = "#A61C31", 
                                                                 color.background = "#B1B2B0",
                                                                 size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 6,
              wellPanel(
                style =  "background-color: #fff;
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px;
                height:454px;",
                highchartOutput("pos_conv_torta", width = "auto", height = "auto") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                                   color.background = "#B1B2B0",
                                                                                                   size = 1)
                )
              ),
            
            column(
              width = 6,
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                dataTableOutput("pos_conv_tabla", width = "100%") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                                  color.background = "#B1B2B0",
                                                                                  size = 1)
                )
              )
            )
          ),
        
        # pos_area_UI -------------------------------------------------------------
        
        tabPanel(
          title = "Áreas del conocimiento",
          
          br(),
          #Serie graduados por nivel de formación
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                highchartOutput("pos_areas_serie") %>% withSpinner(type = 4, color = "#A61C31", 
                                                                   color.background = "#B1B2B0",
                                                                   size = 1)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 12,
              
              wellPanel(
                style =  "background-color: #fff;
                border-color: #2c3e50;
                box-shadow: 1px 1px 5px;",
                plotlyOutput("pos_areas_barras", width = "auto", height = "auto",inline = F)
                ),
              hr()
              )
            ),
          
          fluidRow(
            column(
              width = 12,
              wellPanel(
                style = "background-color: #fff; 
                border-color: #2c3e50; 
                box-shadow: 1px 1px 5px; ",
                dataTableOutput("pos_areas_tabla", width = "100%")
                )
              )
            )
          )
        )
      
      
    } else {
      # ++ No información ----------------------------------------------------------
      withTags({
        div(class="jumbotron",
            h1("No información",
               class = "display-3"),
            p(class = "lead",
              "la sede aún no cuenta con cifras de graduados en postgrado")
        )
      })
    }
  }
  
  
})


# *GRAFICAS ----------------------------------------------------------------


# * * sede_actual_pos ---------------------------------------------------------



sede_actual_pos <- reactive({
  
  req(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))))
  
  if(!(is.null(input$pos_sede_si))){
    if(is.null(input$pos_fac_si)){
      
      if(input$pos_sede_si == "" | input$pos_gen_bt){
        sede <- "Cifras generales Universidad Nacional de Colombia (k: miles)."
      } else {
        sede <- paste("Sede",paste0(input$pos_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      }
      
    } else {
      
      if(input$pos_sede_si == ""){
        sede <- "Cifras generales Universidad Nacional de Colombia."
      } else if(input$pos_fac_si == paste("Cifras Sede",input$pos_sede_si)){
        sede <- paste("Sede",paste0(input$pos_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      } else {
        sede <- paste("Facultad de",paste0(input$pos_fac_si,","),"Sede",paste0(input$pos_sede_si,","),"Universidad Nacional de Colombia (k: miles).")
      }
    }
  }
  
})



# * * pos_evol ----------------------------------------------------------------

output$pos_evol_serie <- renderHighchart({
  
  req(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))))
  
  Plot.Series(
    datos = Consolidado_pos(),
    categoria = "TOTAL",
    titulo = "Evolución histórica del total de estudiantes graduados en postgrado",
    labelY = "Número de graduados  ",
    libreria = "highcharter",
    estilo = list(hc.Credits = sede_actual_pos(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
  
  
})

####--- info box total graduados UI ---###

output$pos_total_box <- renderUI({
  
  div(class = "lines-background",
      div(class = "cont2 border-green green-maps",
          div(class = "info-tl",
              p(class = "info-title", "Total Graduados"),
              p(class = "info-subtitle info-sub-black",
                paste("Número de graduados en postgrado histórico desde el 2009")
              )
          ),
          
          div(class = "info-num",
              div(class = "info-num-1",
                  p(id = "element",class = "num-m un-green",pos_evol_ib_h() + pos_evol_ib_m()),
                  p(class = "txt-m", "Número de graduados")
              )
          )
      )
  )
  
})

####--- info box total graduados por genero ---###

output$pos_mh_box <- renderUI({
  
  div(class = "lines-background",
      div(class = "cont2 border-green green-maps",
          div(class = "info-tl",
              p(class = "info-title", "Graduados"),
              p(class ="info-subtitle","Número de graduados en postgrado por sexo desde el 2009")
          ),
          
          div(class = "info-num",
              
              div(class = "info-num-1",
                  p(class = "num-m un-green",pos_evol_ib_m()),
                  p(class = "txt-m", "Mujeres")
              ),
              
              div(class = "info-num-2",
                  p(class = "num-h un-green",pos_evol_ib_h()),
                  p(class = "txt-h","Hombres")
              )
          )
      )
  )
  
})

# extraer número de graduados mujeres historico

pos_evol_ib_m <- reactive({
  
  req(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pos() %>% filter(Variable == "SEXO",
                                        Clase == "Mujeres") %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

# extraer número graduados hombres historico

pos_evol_ib_h <- reactive({
  
  req(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pos() %>% filter(Variable == "SEXO",
                                        Clase == "Hombres") %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

####--- info box graduados semestre actual por genero ---###}

output$pos_mh_box_actual <- renderUI({
  
  div(class = "lines-background",
      div(
        class = "cont2 border-green green-maps",
        
        div(
          class = "info-tl",
          p(class = "info-title", paste("Graduados",ult_per()[[1]])),
          p(class = "info-subtitle info-sub-black",
            paste("Número de graduados en postgrado por sexo 
                        semestre",ult_per()[[1]])),
          
          
        ),
        
        div(
          class = "info-num",
          div(
            class = "info-num-1",
            p(class = "num-m un-green",pos_evol_ib_ma()),
            p(class = "txt-m", "Mujeres")
          ),
          
          div(
            class = "info-num-2",
            p(class = "num-h un-green",pos_evol_ib_ha()),
            p(class = "txt-h","Hombres")
          )
        )
        
        
      ))
})

# extraer número de graduados mujeres semestre Anterior (ma) número

pos_evol_ib_ma <- reactive({
  
  req(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pos() %>% filter(Variable == "SEXO",
                                        Clase == "Mujeres",
                                        YEAR == ult_per()[["last_year"]],
                                        SEMESTRE == ult_per()[["last_period"]]) %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

# extraer número graduados hombres semestre Anterior (ha)

pos_evol_ib_ha <- reactive({
  
  req(input$pos_sede_si %in% c("",as.character(unique((Graduados %>% filter(TIPO_NIVEL == "Postgrado"))$SEDE_NOMBRE_MAT))))
  
  total <- Consolidado_pos() %>% filter(Variable == "SEXO",
                                        Clase == "Hombres",
                                        YEAR == ult_per()[["last_year"]],
                                        SEMESTRE == ult_per()[["last_period"]]) %>%
    summarise(grad = sum(Total)) %>%
    pull(grad)
  
})

#outputOptions(output, "pos_evol_serie", suspendWhenHidden = FALSE)


# * * pos_sede --------------------------------------------------------------



#***** sede graduados serie *****#

output$pos_sede_serie <- renderHighchart({
  
  
  sedes_general <- c("Amazonía","Bogotá","Caribe","De La Paz","Manizales","Medellín","Orinoquía","Palmira","Tumaco")
  paleta_sedes <- c("#29abe2","#8cc63f","#c1272d","#9e9ac8","#0071bc","#f15a24","#fbb03b","#93278f","#6d6666")
  color_sedes <- tibble(sedes_general,paleta_sedes)
  
  
  Plot.Series(datos = Consolidado_pos(),
              categoria = "SEDE_NOMBRE_MAT",
              titulo = "Evolución del número de graduados en postgrado por sedes",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = color_sedes %>% select(paleta_sedes) %>% pull(),
              estilo = list(hc.Credits = sede_actual_pos(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_sede_serie", suspendWhenHidden = FALSE)

#***** sede barras *****#
output$pos_sede_barras <- renderPlotly({
  
  sedes_general <- c("Amazonía","Bogotá","Caribe","De La Paz","Manizales","Medellín","Orinoquía","Palmira","Tumaco")
  paleta_sedes <- c("#29abe2","#8cc63f","#c1272d","#9e9ac8","#0071bc","#f15a24","#fbb03b","#93278f","#6d6666")
  color_sedes <- tibble(sedes_general,paleta_sedes)
  
  
  Plot.Barras(datos = Consolidado_pos(),
              categoria = "SEDE_NOMBRE_MAT",
              vertical = F,
              ano = 2020,
              titulo = "Distribución de graduados en postgrado <br> por sede de graduación",
              periodo = 1,
              colores = color_sedes %>% select(paleta_sedes) %>% pull(),
              libreria = "plotly")
  
})

#***** sede tabla *****#
output$pos_sede_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pos(),
        categoria = "SEDE_NOMBRE_MAT",
        encabezado = "Total estudiantes graduados en postgrado por sede de graduación")
  
})


# * * pos_nivel ----------------------------------------------------------------



output$pos_nivel_serie <- renderHighchart({
  
  nivel_general <- sort(unique(Graduados$NIVEL))
  paleta_nivel <- c("#6d6666","#fbb03b","#29abe2","#c1272d","#8cc63f")
  color_nivel <- tibble(nivel_general,paleta_nivel)
  
  nivel_actual <- Consolidado() %>%  filter(Variable == "NIVEL") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nivel %>% filter(nivel_general %in% nivel_actual) %>% 
    select(paleta_nivel) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pos(),
    categoria = "NIVEL",
    titulo = "Evolución del número de estudiantes graduados en postgrado por nivel de formación",
    labelY = "Número de graduados",
    libreria = "highcharter",
    colores = colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pos(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_nivel_serie", suspendWhenHidden = FALSE)

output$pos_nivel_torta <- renderHighchart({
  
  nivel_general <- sort(unique(Graduados$NIVEL))
  paleta_nivel <- c("#6d6666","#fbb03b","#29abe2","#c1272d","#8cc63f")
  color_nivel <- tibble(nivel_general,paleta_nivel)
  
  nivel_actual <- Consolidado() %>%  filter(Variable == "NIVEL") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nivel %>% filter(nivel_general %in% nivel_actual) %>% 
    select(paleta_nivel) %>% pull()
  
  
  Plot.Torta(datos = Consolidado_pos(),
             categoria = "NIVEL",
             titulo = "Distribución de graduados <br> en postgrado por niveles de formación <br>",
             ano = 2020,
             periodo = 1,
             addPeriodo = T,
             colores = colores_dinamico,
             libreria = "highcharter")
  
  
  
})

output$pos_nivel_tabla <- renderDataTable({
  
  
  #Tabla nacionalidad 
  Tabla(datos = Consolidado_pos(),
        categoria = "NIVEL",
        encabezado = "Total estudiantes graduados en postgrado por nivel de formación")
  
  
})

# * * pos_nac -------------------------------------------------------------

output$pos_nac_serie <- renderHighchart({
  
  nac_general <- sort(unique(Graduados$NACIONALIDAD))
  paleta_nac <- c("#8cc63f","#f15a24","#0071bc")
  color_nac <- tibble(nac_general,paleta_nac)
  
  nac_actual <- Consolidado_pos() %>%  filter(Variable == "NACIONALIDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nac %>% filter(nac_general %in% nac_actual) %>% 
    select(paleta_nac) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pos(),
    categoria = 'NACIONALIDAD',
    titulo = "Evolución del número de graduados en postgrado según nacionalidad",
    labelY = "Número de graduados  ",
    libreria = "highcharter",
    colores= colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pos(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_nac_serie", suspendWhenHidden = FALSE)


output$pos_nac_torta <- renderHighchart({
  
  nac_general <- sort(unique(Graduados$NACIONALIDAD))
  paleta_nac <- c("#8cc63f","#f15a24","#0071bc")
  color_nac <- tibble(nac_general,paleta_nac)
  
  nac_actual <- Consolidado_pos() %>%  filter(Variable == "NACIONALIDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_nac %>% filter(nac_general %in% nac_actual) %>% 
    select(paleta_nac) %>% pull()
  
  
  Plot.Torta(datos = Consolidado_pos(),
             categoria = 'NACIONALIDAD',
             titulo = "Distribución de graduados <br> en postgrado según nacionalidad <br>",
             ano = 2020,
             periodo = 1,
             addPeriodo = T,
             colores= colores_dinamico,
             libreria = "highcharter")
  
  
  
})

# nacionalidad graduados postgrado
output$pos_nac_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pos(),
        categoria = 'NACIONALIDAD',
        encabezado = 'Total estudiantes graduados en postgrado por nacionalidad')
  
  
})



#  * * pos_lug_nac -------------------------------------------------------

Graduados_actual_pos <- reactive({
  if(input$pos_sede_si != ""){
    
    Graduados_actual = Graduados %>% filter(SEDE_NOMBRE_ADM == input$pos_sede_si,
                                            TIPO_NIVEL == "Postgrado") %>% 
      filter(YEAR == max(YEAR)) %>% 
      filter(SEMESTRE == max(SEMESTRE)) %>%
      filter(!is.na(Departamento))
    
  } else {
    
    Graduados_actual = Graduados %>%  
      filter(TIPO_NIVEL == "Postgrado") %>% 
    filter(YEAR == max(YEAR)) %>% 
      filter(SEMESTRE == max(SEMESTRE)) %>%
      filter(!is.na(Departamento))
  }
})

# Mapa por municipios

output$pos_lug_nac_pm <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pos(),
    tipo = c("SiNoMpios"),
    titulo = "Graduados 2020-2",
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})

# Mapa Total municipio

output$pos_lug_nac_tm <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pos(),
    tipo = c("Mpios"),
    titulo = "Graduados 2020-2",
    cortes = c(0,1,5,10,100,Inf),
    colores = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})

# Total por depto

output$pos_lug_nac_td <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pos(),
    tipo = c("Deptos"),
    titulo = "Graduados 2020-2",
    cortes = c(0,1,5,10,100,Inf),
    colores = c("#ffffcc","#c2e699","#78c679","#31a354","#006837"),
    colSedes = rep("red",9),
    colBorde = c("#7fff2e"),
    opacidad = 0.8
  )
  
})

# total depto y municipios

output$pos_lug_nac_tmd <- renderLeaflet({
  
  Plot.Mapa(
    datos = Graduados_actual_pos(),
    tipo = c("DeptoMpio"),
    titulo = "Graduados 2020-2",
    colSedes = rep("red",9),
    opacidad = 0.8
  )
  
})





# * * pos_sexo ----------------------------------------------------------------



output$pos_sexo_serie <- renderHighchart({
  
  sexo_general <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo_general,paleta_sexo)
  
  sexo_actual <- Consolidado_pos() %>%  filter(Variable == "SEXO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_sexo %>% filter(sexo_general %in% sexo_actual) %>% 
    select(paleta_sexo) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pos(),
    categoria = "SEXO",
    titulo = "Evolución del número de graduados en postgrado por sexo",
    labelY = "Número de graduados  ",
    libreria = "highcharter",
    colores = colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pos(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_sexo_serie", suspendWhenHidden = FALSE)

output$pos_sexo_torta <- renderHighchart({
  
  sexo_general <- sort(unique(Graduados$SEXO))
  paleta_sexo <- c("#f15a24","#8cc63f")
  color_sexo <- tibble(sexo_general,paleta_sexo)
  
  sexo_actual <- Consolidado_pos() %>%  filter(Variable == "SEXO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_sexo %>% filter(sexo_general %in% sexo_actual) %>% 
    select(paleta_sexo) %>% pull()
  
  Plot.Torta(datos = Consolidado_pos(),
             categoria = "SEXO",
             titulo = "Distribución de graduados <br> en postgrado por sexo <br>",
             ano = 2020,
             periodo = 1,
             addPeriodo = T,
             col = colores_dinamico,
             libreria = "highcharter")
  
  
  
})


output$pos_sexo_tabla <- renderDataTable({
  

  Tabla(datos = Consolidado_pos(),
        categoria = "Sexo",
        encabezado = "Total graduados en postgrado por sexo")
  
  
})


# * * pos_edad --------------------------------------------------------------



#***** edad serie *****#

output$pos_edad_serie <- renderHighchart({
  
  Grad <- Graduados %>% filter(TIPO_NIVEL == "Postgrado") %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
  Grad$CAT_EDAD <- factor(Grad$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))
  
  
  edad_general <- sort(Grad %>% select("CAT_EDAD") %>% unique() %>% pull())
  paleta_edad <- c("#8cc63f","#0071bc","#f15a24","#fbb03b","#6d6666")
  color_edad <- tibble(edad_general,paleta_edad)
  
  edad_actual <- Consolidado_pos() %>%  filter(Variable == "CAT_EDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_edad %>% filter(edad_general %in% edad_actual) %>% 
    select(paleta_edad) %>% pull()
  
  Plot.Series(datos = Consolidado_pos(),
              categoria = "CAT_EDAD",
              titulo = "Evolución del número de graduados en postgrado por grupos de edad",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico, 
              estilo = list(hc.Credits = sede_actual_pos(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_edad_serie", suspendWhenHidden = FALSE)

#***** edad barras *****#
output$pos_edad_barras <- renderPlotly({
  
  Grad <- Graduados %>% filter(TIPO_NIVEL == "Postgrado") %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
  Grad$CAT_EDAD <- factor(Grad$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))
  
  
  edad_general <- sort(Grad %>% select("CAT_EDAD") %>% unique() %>% pull())
  paleta_edad <- c("#8cc63f","#0071bc","#f15a24","#fbb03b","#6d6666")
  color_edad <- tibble(edad_general,paleta_edad)
  
  edad_actual <- Consolidado_pos() %>%  filter(Variable == "CAT_EDAD") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_edad %>% filter(edad_general %in% edad_actual) %>% 
    select(paleta_edad) %>% pull()
  
  Plot.Barras(datos = Consolidado_pos(),
              categoria = "CAT_EDAD",
              vertical = T,
              ordinal = T,
              ano = 2020,
              periodo = 1,
              colores = colores_dinamico,
              titulo = "Distribución de graduados en postgrado <br> por grupos de edad",
              libreria = "plotly")
  
})

#***** edad tabla *****#
output$pos_edad_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pos(),
        categoria = "CAT_EDAD",
        encabezado = "Total graduados en postgrado por grupos de edad")
  
})


# * * pos_conv ----------------------------------------------------------------



output$pos_conv_serie <- renderHighchart({
  
  conv_general <- sort(unique(Graduados$CONVENIO))
  paleta_conv <- c("#8cc63f","#f15a24","#0071bc")
  color_conv <- tibble(conv_general,paleta_conv)
  
  conv_actual <- Consolidado_pos() %>%  filter(Variable == "CONVENIO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_conv %>% filter(conv_general %in% conv_actual) %>% 
    select(paleta_conv) %>% pull()
  
  Plot.Series(
    datos = Consolidado_pos(),
    categoria = "CONVENIO",
    titulo = "Evolución del número de estudiantes graduados en postgrado por convenios",
    labelY = "Número de graduados  ",
    libreria = "highcharter",
    colores = colores_dinamico,
    estilo = list(hc.Credits = sede_actual_pos(),
                  hc.Slider = FALSE,
                  hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_conv_serie", suspendWhenHidden = FALSE)

output$pos_conv_torta <- renderHighchart({
  
  conv_general <- sort(unique(Graduados$CONVENIO))
  paleta_conv <- c("#8cc63f","#f15a24","#0071bc")
  color_conv <- tibble(conv_general,paleta_conv)
  
  conv_actual <- Consolidado_pos() %>%  filter(Variable == "CONVENIO") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_conv %>% filter(conv_general %in% conv_actual) %>% 
    select(paleta_conv) %>% pull()
  
  
  Plot.Torta(datos = Consolidado_pos(),
             categoria = "CONVENIO",
             titulo = "Distribución de graduados <br> en postgrado en convenios <br>",
             ano = 2020,
             periodo = 1,
             addPeriodo = T,
             colores = colores_dinamico,
             libreria = "highcharter")
  
  
  
})

output$pos_conv_tabla <- renderDataTable({
  

  Tabla(datos = Consolidado_pos(),
        categoria = "CONVENIO",
        encabezado = "Total estudiantes graduados en postgrado por convenios")
  
  
})


# * * pos_area --------------------------------------------------------------



#***** area serie *****#

output$pos_areas_serie <- renderHighchart({
  
  asnies_general <- sort(unique(Graduados$AREAC_SNIES))
  paleta_asnies <- c("#81337d","#41a6cb","#a03437","#176ba3","#cc6838","#e8ad5e","#94be58","#636b68")
  color_asnies <- tibble(asnies_general,paleta_asnies)
  
  asnies_actual <- Consolidado_pos() %>%  filter(Variable == "AREAC_SNIES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_asnies %>% filter(asnies_general %in% asnies_actual) %>% 
    select(paleta_asnies) %>% pull()
  
  Plot.Series(datos = Consolidado_pos(),
              categoria = "AREAC_SNIES",
              titulo = "Evolución del número de graduados en postgrado por áreas del conocimiento SNIES",
              labelX = "Periodo",
              labelY = "Número de graduados",
              libreria = "highcharter",
              colores = colores_dinamico,
              estilo = list(hc.Credits = sede_actual_pos(),
                            hc.Slider = FALSE,
                            hc.Tema = 1)
  ) %>% hc_tooltip(backgroundColor= "#232323")
  
})

#outputOptions(output, "pos_areas_serie", suspendWhenHidden = FALSE)

#***** edad barras *****#
output$pos_areas_barras <- renderPlotly({
  
  asnies_general <- sort(unique(Graduados$AREAC_SNIES))
  paleta_asnies <- c("#81337d","#41a6cb","#a03437","#176ba3","#cc6838","#e8ad5e","#94be58","#636b68")
  color_asnies <- tibble(asnies_general,paleta_asnies)
  
  asnies_actual <- Consolidado_pos() %>%  filter(Variable == "AREAC_SNIES") %>%
    select(Clase) %>% unique() %>% pull()
  
  colores_dinamico <- color_asnies %>% filter(asnies_general %in% asnies_actual) %>% 
    select(paleta_asnies) %>% pull()
  
  Plot.Barras(datos = Consolidado_pos(),
              categoria = "AREAC_SNIES",
              vertical = F,
              ano = 2020,
              periodo = 1,
              colores = colores_dinamico,
              titulo = "Distribución de graduados en postgrado <br> por áreas del conocimiento SNIES",
              libreria = "plotly")
  
})

#***** edad tabla *****#
output$pos_areas_tabla <- renderDataTable({
  
  
  Tabla(datos = Consolidado_pos(),
        categoria = "AREAC_SNIES",
        encabezado = "Total graduados en postgrado por áreas del conocimiento SNIES")
  
})

#  Eliminar Panel Sede si se escoge alguna sede ----------------------------



observeEvent(input$pos_fac_si,{
  
  if(input$pos_fac_si == paste("Cifras Sede",input$pos_sede_si)){
    hideTab(inputId = "pos_tabs", target = "Sede")
  }
  
})


# Eliminar Panel Lugar de Información -------------------------------------

observeEvent(input$pos_fac_si, {
  
  if(input$pos_fac_si != paste("Cifras Sede",input$pos_sede_si)){
    hideTab(inputId = "pos_tabs", target = "Lugar de Nacimiento")
  }
  
})

# Eliminar panel Areas del conocimiento sede si escoge alguna facultad ---------------------------


observeEvent(input$pos_fac_si, {
  if(input$pos_fac_si != paste("Cifras Sede",input$pos_sede_si)){
    hideTab(inputId = "pos_tabs", target = "Áreas del conocimiento")
  }
  
})











}