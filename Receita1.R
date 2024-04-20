
library(shiny)
#library(shinypdf)
library(pdftools)
library(tinytex)

# Define a UI for the application
ui <- fluidPage(
  titlePanel("Prescrição Veterinária"),
  tabsetPanel(
    tabPanel("Paciente",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("tipo_paciente", "Tipo de Paciente:",
                              choices = c("Novo Paciente", 
                                          "Paciente já Existente")),
                 conditionalPanel(
                   condition = "input.tipo_paciente == 'Novo Paciente'",
                   textInput("nome", "Nome do Paciente:"),
                   numericInput("idade", "Idade:", value = NULL),
                   numericInput("altura", "Altura (cm):", value = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.tipo_paciente == 'Paciente já existe'",
                   selectInput("paciente_existente", "Selecione o Paciente:",
                               choices = c("Paciente A", 
                                           "Paciente B", 
                                           "Paciente C"))
                 )
               ),
               mainPanel(
                 h3("Informações do Paciente")
               )
             )
    ),
    tabPanel("Cadastro",
             sidebarLayout(
               sidebarPanel(
                 textInput("nome_cadastro", "Nome:"),
                 selectInput("especie", "Espécie:",
                             choices = c("Canino", 
                                         "Felino", 
                                         "Ave", 
                                         "Bovino", 
                                         "Equino")),
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
               ),
               mainPanel(
                 h3("Cadastro do Paciente")
               )
             )
    ),
    tabPanel("Remédio",
             sidebarLayout(
               sidebarPanel(
                 textInput("nome_remedio", "Nome/Tipo do Remédio:"),
                 radioButtons("uso", "Uso:",
                              choices = c("Oral", 
                                          "Tópico", 
                                          "Oftálmico",
                                          "Ambiente",
                                          "Externo", 
                                          "Inalário"))
               ),
               mainPanel(
                 h3("Informações do Remédio")
               )
             )
    ),
    tabPanel("Posologia",
             sidebarLayout(
               sidebarPanel(
                 textInput("dose", "Dose (mg):"),
                 numericInput("frequencia", "Frequência de Administração:",
                              min = 1, max = 24, value = 1),
                 selectInput("periodo", "Período de Administração:",
                             choices = c("Horas", 
                                         "Dias", 
                                         "Semanas"))
               ),
               mainPanel(
                 h3("Posologia")
               )
             )
    ),
    tabPanel("Receituário")
    
    
  )
)

# Define the server logic
server <- function(input, output) {
  # Server logic goes here
}

# Run the application
shinyApp(ui = ui, server = server)
