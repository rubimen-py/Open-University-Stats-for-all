navbarPage(
  theme = "look.css",
  windowTitle = "UNAL Graduates",
  title = div(id = "titulo-navbar",p("UNAL Graduates")),
  id = "navbar",
  selected = "GRADUADOS (Graduates)",
  # página graduados general ------------------------------------------------
  tabPanel(
    title = "GRADUADOS (Graduates)",
    fluidRow(
      column(2,
             fluidRow(
               column(
                 width = 12,
                 # grad_gen_bt -------------------------------------------------------------
                 actionButton(inputId = "grad_gen_bt", 
                              label = "Cifras Generales",
                              style = "font-size:15px;
                              font-family:'Roboto Condensed', sans-serif;
                              padding:5px;",
                              width = "100%"
                              ),
                 
                 bsTooltip("grad_gen_bt","Cifras de la universidad en todas sus sedes.", 
                           placement = "bottom",
                           trigger = "hover",
                           options = NULL),
                 hr()
                 )
               ),
             fluidRow(
               column(
                 width = 12,
                 
                 wellPanel(
                 uiOutput('grad_sede_si_ou'),
                 uiOutput('grad_fac_si_ou')
                 
               )
             )),
             
             fluidRow(
               column(
                 width = 12,
                 wellPanel(
                   style = "background-color:#efefef;
                   color:black;",
                 p(
                   style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                   "The shiny app", strong("UNAL Graduates"), "from 
                   ", tags$span(style = "color:#94B43B;",
                     strong("Universidad Nacional de Colombia")),
                   "has the graphical representations such as time series, maps, pie charts, information tables... of the university's graduates since 2009."),
                 p(
                   style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                   "The button", tags$span(style = "color:#94B43B;",
                                strong("Ver metadatos (metadata)")),
                   "opens in a new window the meaning of each category displayed."),
                 p(
                   style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                   "you can filter the information by university campuses and faculties.", tags$span(style ="color:#b43b94;","'Sede' (campus)"), 
                   "y",tags$span(style = "color:#3b94b4;"," 'Facultad' (Faculty) "),"."
                 ),
                 p(
                   style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                   "The button", strong(style="color:#232323;",'Cifras Generales') ,"allows you to view university information without filters."
                 ),
                 
                 p(
                   style = "
                   color: black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                   "When choosing a ",tags$span(style="color:#b43b94;","sede (campus)"),"the filter of ",tags$span(style="color:#3b94b4;","facultades (faculties)"), "of that campus is shown."
                 ),
                 
                 p(
                   style = "
                   color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;
                   ",
                   "the choice", strong(style="color:#232323;",' "Cifras Sede ... " (campus stats ... ) '), "in the filter of",tags$span(style = "color:#3b94b4;","facultades (faculties)"),"allows you to view the campus statistics in all its faculties."
                 )
                 )
                 
               )
             )
             
             ),
      
      column(10,
             # tabs main panel ---------------------------------------------------------
             uiOutput("grad_tabset") %>% withSpinner(type = 4, color = "#A61C31", 
                                                     color.background = "#B1B2B0",
                                                     size = 1)
             )
      )
    ),
  # página graduados pregrado -----------------------------------------------
  tabPanel(
    title = "GRADUADOS PREGRADO (Undergraduate)",
    fluidRow(
      column(2,
             fluidRow(
               column(
                 width = 12,
                 # pre_gen_bt -------------------------------------------------------------
                 actionButton(inputId = "pre_gen_bt", 
                              label = "Cifras Generales",
                              style = "font-size:15px;
                              padding:5px;",
                              width = "100%"
                 ),
                 
                 bsTooltip("pre_gen_bt","Cifras de la universidad en todas sus sedes.", 
                           placement = "bottom",
                           trigger = "hover",
                           options = NULL),
                 hr()
                 )
               ),
             fluidRow(
               # pre_sede_si ------------------------------------------------------------
               column(
                 width = 12,
               wellPanel(
                 uiOutput('pre_sede_si_ou'),
                 uiOutput('pre_fac_si_ou'),
                 uiOutput('pre_prog_si_ou')
                 )
               )
               ),
             
             fluidRow(
               column(
                 width = 12,
                 wellPanel(
                   style = "background-color:#efefef;
                   color:black;",
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "The shiny app", strong("UNAL Graduates"), "from 
                   ", tags$span(style = "color:#94B43B;",
                                strong("Universidad Nacional de Colombia")),
                     "has the graphical representations such as time series, maps, pie charts, information tables... of the university's graduates since 2009."),
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "The button", tags$span(style = "color:#94B43B;",
                                             strong("Ver metadatos (metadata)")),
                     "opens in a new window the meaning of each category displayed."),
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "you can filter the information by university campuses and faculties.", tags$span(style ="color:#b43b94;","'Sede' (campus)"), 
                     "y",tags$span(style = "color:#3b94b4;"," 'Facultad' (Faculty) "),"."
                   ),
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "The button", strong(style="color:#232323;",'Cifras Generales') ,"allows you to view university information without filters."
                   ),
                   
                   p(
                     style = "
                   color: black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "When choosing a ",tags$span(style="color:#b43b94;","sede (campus)"),"the filter of ",tags$span(style="color:#3b94b4;","facultades (faculties)"), "of that campus is shown."
                   ),
                   
                   p(
                     style = "
                   color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;
                   ",
                     "the choice", strong(style="color:#232323;",' "Cifras Sede ... " (campus stats ... ) '), "in the filter of",tags$span(style = "color:#3b94b4;","facultades (faculties)"),"allows you to view the campus statistics in all its faculties."
                   )
                 )
                 
               )
             )
             ),
      
      column(10,
             # tabs main panel ---------------------------------------------------------
             uiOutput("pre_tabset") %>% withSpinner(type = 4, color = "#A61C31", 
                                                    color.background = "#B1B2B0",
                                                    size = 1)
             )
      )
    ),
  # página graduados posgrado ---------------------------------------------------------
  tabPanel(
    title = "GRADUADOS POSTGRADO (Postgraduate)",
    fluidRow(
      column(2,
             fluidRow(
               column(
                 width = 12,
                 # pos_gen_bt --------------------------------------------------------------
                 actionButton(inputId = "pos_gen_bt", 
                              label = "Cifras Generales",
                              style = "font-size:15px;
                              padding:5px;",
                              width = "100%"
                 ),
                 
                 bsTooltip("pos_gen_bt","Cifras de la universidad en todas sus sedes.", 
                           placement = "bottom",
                           trigger = "hover",
                           options = NULL),
                 hr()
                 )
               ),
             fluidRow(
               # pos_sede_si ------------------------------------------------------------
               
               wellPanel(
                 style = "padding-left: 10px; 
                 margin-left:13px;
                 margin-right:13px;",
                 uiOutput('pos_sede_si_ou'),
                 uiOutput('pos_fac_si_ou'),
                 uiOutput('pos_prog_si_ou')
                 )
               ),
             
             fluidRow(
               column(
                 width = 12,
                 wellPanel(
                   style = "background-color:#efefef;
                   color:black;",
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "The shiny app", strong("UNAL Graduates"), "from 
                   ", tags$span(style = "color:#94B43B;",
                                strong("Universidad Nacional de Colombia")),
                     "has the graphical representations such as time series, maps, pie charts, information tables... of the university's graduates since 2009."),
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "The button", tags$span(style = "color:#94B43B;",
                                             strong("Ver metadatos (metadata)")),
                     "opens in a new window the meaning of each category displayed."),
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "you can filter the information by university campuses and faculties.", tags$span(style ="color:#b43b94;","'Sede' (campus)"), 
                     "y",tags$span(style = "color:#3b94b4;"," 'Facultad' (Faculty) "),"."
                   ),
                   p(
                     style = "color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "The button", strong(style="color:#232323;",'Cifras Generales') ,"allows you to view university information without filters."
                   ),
                   
                   p(
                     style = "
                   color: black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;",
                     "When choosing a ",tags$span(style="color:#b43b94;","sede (campus)"),"the filter of ",tags$span(style="color:#3b94b4;","facultades (faculties)"), "of that campus is shown."
                   ),
                   
                   p(
                     style = "
                   color:black;
                   font-size:12px;
                   font-family:'Raleway',Tahoma,sans-serif;
                   ",
                     "the choice", strong(style="color:#232323;",' "Cifras Sede ... " (campus stats ... ) '), "in the filter of",tags$span(style = "color:#3b94b4;","facultades (faculties)"),"allows you to view the campus statistics in all its faculties."
                   )
                 )
                 
               )
             )
             ),
      
      column(10,
             # tabs main panel ---------------------------------------------------------
             uiOutput("pos_tabset") %>% withSpinner(type = 4, color = "#A61C31", 
                                                    color.background = "#B1B2B0",
                                                    size = 1)
             )
      )
    ),
  
    tags$script(HTML("var header = $('.navbar > .container-fluid');
                   header.append('<div style=\"float:right;\"><a id=\"metadatos-button\" href=\" http://estadisticas.unal.edu.co/menu-principal/cifras-generales/metadatos/cifras-generales/?tx_estadisticaunal_showprotocolo%5Bestadistica%5D=26&tx_estadisticaunal_showprotocolo%5Baction%5D=protocolo&tx_estadisticaunal_showprotocolo%5Bcontroller%5D=Estadistica&cHash=1b0e22d62926e55f3099a4677be3e9c1\" class=\"btn btn-success\" target=\"_blank\" >Ver Metadatos</a></div>');
                   console.log(header)"))
  
  
    
  
  
 
  
  
  
  
  
  
  )

