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
    
    
    # Show Word Cloud
    mainPanel(fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
              ),
              fluidRow(column(12,
                              helpText(HTML('The figure on the left shows the coefficients for each biomarker. Biomarkers with positive coefficients may have disease-inducing roles, while negative coefficient indicate protective roles. 
                                        <br/>
                                        <br/>
                                       The figure on the right shows AUCs for different numbers of genomic, proteomic or metabolic potential biomarkers'))

              ))
    )

  )
)

