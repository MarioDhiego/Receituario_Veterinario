library(shiny)
library(shinydashboard)
library(DT)

# Define UI
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Receituario Veterinario",
    tags$li(class = "dropdown",
            a(href = "https://www.facebook.com/detranPARA",
              class = "fa fa-facebook",
              target = "_blank"
            )),
    tags$li(class = "dropdown",
            a(href = "https://www.instagram.com/detranpa_",
              class = "fa fa-instagram",
              target = "_blank"
            ))
  ),
  skin = "red",
  scrollToTop = TRUE,
  options = list(sidebarExpandOnHover = TRUE),
  sidebar = dashboardSidebar(
    minified = FALSE,
    collapsed = FALSE,
    sidebarMenu(
      menuItem("CADASTRO E PACIENTE", tabName = "cadastro_paciente"),
      menuItem("REMÉDIO", tabName = "remedio"),
      menuItem("FARMÁCIA", tabName = "farmacia"),
      menuItem("POSOLOGIA", tabName = "posologia"),
      menuItem("RECEITA", tabName = "receituario")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "cadastro_paciente",
              fluidRow(
                column(6,
                       radioButtons("tipo_paciente", "Tipo de Paciente:",
                                    choices = c("Novo Paciente", 
                                                "Paciente já Existente")),
                       conditionalPanel(
                         condition = "input.tipo_paciente == 'Novo Paciente'",
                         textInput("nome", "Nome do Paciente:"),
                         numericInput("idade", 
                                      "Idade:", value = NULL),
                         numericInput("altura", 
                                      "Altura (cm):", value = NULL),
                         actionButton("add_patient", "Adicionar Paciente")
                       ),
                       conditionalPanel(
                         condition = "input.tipo_paciente == 'Paciente já Existente'",
                         selectInput("paciente_existente", "Selecione o Paciente:",
                                     choices = c("Paciente A", 
                                                 "Paciente B", 
                                                 "Paciente C"))
                       )
                ),
                column(6,
                       textInput("nome_cadastro", "Proprietário:"),
                       selectInput("especie", "Espécie:",
                                   choices = c("Canino", 
                                               "Felino", 
                                               "Ave", 
                                               "Bovino", 
                                               "Equino",
                                               "Reptio")),
                       selectInput("raca", "Raça:",
                                   choices = c("Workichine", 
                                               "Pitbull", 
                                               "MangaLarga")),
                       dateInput("nascimento", "Data de Nascimento:"),
                       radioButtons("sexo", "Sexo:",
                                    choices = c("Masculino", 
                                                "Feminino")),
                       numericInput("peso", 
                                    "Peso (kg):", value = NULL)
                )
              )
      ),
      tabItem(tabName = "remedio",
              fluidRow(
                textInput("nome_remedio", "Nome/Tipo do Remedio:"),
                radioButtons("uso", "Uso:",
                             choices = c("Oral", 
                                         "Tôpico", 
                                         "Oftálmico",
                                         "Ambiente",
                                         "Externo", 
                                         "Inalário"))
              )
      ),
      tabItem(tabName = "farmacia",
              fluidRow(
                selectInput("tipo_farmacia", "Tipo de Farmácia:",
                            choices = c("Veterinária", 
                                        "Humana", 
                                        "Manipulação Veterinaria",
                                        "Manipulação Humana"))
              )
      ),
      tabItem(tabName = "posologia",
              fluidRow(
                textInput("dose", "Dose (mg):"),
                numericInput("frequencia", "Frequencia de Administracao:",
                             min = 1, 
                             max = 24, 
                             value = 1),
                selectInput("periodo", "Periodo de Administracao:",
                            choices = c("Horas", 
                                        "Dias", 
                                        "Semanas"))
              )
      ),
      tabItem(tabName = "receituario",
              fluidRow(
                DT::dataTableOutput("patient_table")
              )
      )
    )
  ),
  footer = dashboardFooter(
    left = tags$b("Dra. Zaíra de Sá"),
    right = tags$b("Medico Veterinário CRMV-PA:12.456")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize a reactive values object to store patient data
  patient_data <- reactiveValues(patients = data.frame(
    Nome = character(0),
    Idade = numeric(0),
    Altura = numeric(0)
  ))
  
  # Function to add patient to the database
  observeEvent(input$add_patient, {
    # Extracting inputs
    nome <- isolate(input$nome)
    idade <- isolate(input$idade)
    altura <- isolate(input$altura)
    
    # Add patient to the database
    patient_data$patients <- rbind(patient_data$patients, data.frame(
      Nome = nome,
      Idade = idade,
      Altura = altura
    ))
    
    # Clear input fields
    updateTextInput(session, "nome", value = "")
    updateNumericInput(session, "idade", value = NULL)
    updateNumericInput(session, "altura", value = NULL)
  })
  
  # Render the patient table
  output$patient_table <- DT::renderDataTable({
    DT::datatable(patient_data$patients, options = list(pageLength = 5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

