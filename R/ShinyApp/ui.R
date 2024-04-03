ui <- fluidPage(
  # Application title
  titlePanel("Multiomics Atlas of Biomarkers for Complex Diseases"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("disease", "Disease",
                  choices = c('CD', 'UC', 'RA', 'PSO', 'SLE',
                             'COPD', 'obesity', 'T2D',
                             'Atherosclerosis')),

      selectInput("omics", "Omics",
                  choices = c('proteomics', 'metabolomics', 'genomics')),

      radioButtons("patients", "Predictive Model",
                   choices = c('Incident' = "incident",
                               'Prevalent' = "prevalent"),
                   selected = "incident"),
      sliderInput("N",
                  "# Features",
                  min = 1,  max = 30, value = 5),
      helpText("Slide to change the number of molecules"),
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Feature Importance", plotOutput("plot1"),
                 style = 'overflow-y: scroll'), 
        tabPanel("AUC", plotOutput("plot2")) 
      ),
      fluidRow(column(12,
                      verbatimTextOutput('text1')
    ))
    )

  )
)

