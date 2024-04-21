#==============================================================================#
# Pacote Dashboard
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# Pacote Tabela Dinâmica
library(DT)

# Pacote Deploy
library(rsconnect)
library(reticulate)
library(renv)
library(rlang)
library(curl)
#==============================================================================#


#==============================================================================#
# Definir Interface do Usuário
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Receituário Web",
    tags$li(
      class = "dropdown",
      a(
        href = "https://www.facebook.com/detranPARA",
        class = "fa fa-facebook",
        target = "_blank"
      )
    ),
    tags$li(
      class = "dropdown",
      a(
        href = "https://www.instagram.com/detranpa_",
        class = "fa fa-instagram",
        target = "_blank"
      )
    ),
    tags$li(
      class = "dropdown",
      a(
        href = "https://twitter.com/DETRAN_PA",
        class = "fa fa-twitter",
        target = "_blank"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(href = "https://github.com/MarioDhiego",
             icon("github"), "Suporte", target = "_blank")
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("CADASTRO", tabName = "cadastro_proprietario"),
      menuItem("BASE CADASTRAL", tabName = "receituario"),
      menuItem("SINTOMAS", tabName = "sintomas"),
      menuItem("REMÉDIOS/POSOLOGIA", tabName = "med_farm_pos"),
      menuItem("ESTATÍSTICAS", tabName = "estatistica1"),
      menuItem("RECEITUÁRIO FINAL", tabName = "receitafim1")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "cadastro_proprietario",
              fluidRow(
                column(
                  6,
                  radioButtons(
                    "tipo_proprietario",
                    "PROPRIETÁRIO:",
                    choices = c("Novo Proprietário",
                                "Proprietário já Cadastrado")
                  ),
                  conditionalPanel(
                    condition = "input.tipo_proprietario == 'Novo Proprietário'",
                    textInput("nome", "NOME DO PROPRIETÁRIO:"),
                    selectInput("sexo", "Sexo:",
                                choices = c("Masculino", "Feminino")),
                    textInput("endereco", "ENDEREÇO:"),
                    textInput("cidade", "Cidade:"),
                    textInput("telefone", "CONTATO:"),
                    textInput("registrogeral", "RG:"),
                    actionButton("add_patient", "ADICIONAR NOVO", style = "color: #fff; background-color: #ff0000; border-color: #ff0000;")
                  ),
                  conditionalPanel(
                    condition = "input.tipo_proprietario == 'Proprietário já Cadastrado'",
                    selectInput(
                      "paciente_existente",
                      "Selecione o Proprietário:",
                      choices = c("Proprietario A",
                                  "Proprietario B",
                                  "Proprietario C")
                    )
                  )
                ),
                column(
                  6,
                  textInput("nome_cadastro", "PACIENTE:"),
                  selectInput(
                    "especie",
                    "ESPÉCIE:",
                    choices = c(
                      "Canino",
                      "Felino",
                      "Ramister",
                      "Ave",
                      "Bovino",
                      "Equino",
                      "Reptio"
                    )
                  ),
                  selectInput("sexo_animal", "SEXO ANIMAL:",
                              choices = c("Macho",
                                          "Fêmea")),
                  dateInput("nascimento", "DATA DE NASCIMENTO:"),
                  numericInput("peso", "PESO (kg):", value = NULL),
                  numericInput("altura", "ALTURA (cm):", value = NULL)
                )
              )),
      tabItem(tabName = "med_farm_pos",
              fluidRow(
                column(
                  6,
                  selectInput(
                    "nome_receita",
                    "RECEITA:",
                    choices = c("Comum",
                                "Controle Especial")
                  ),
                  selectInput(
                    "nome_remedio",
                    "REMÉDIO:",
                    choices = c(
                      "Oral",
                      "Tópico",
                      "Oftálmico",
                      "Ambiente",
                      "Externo",
                      "Inalário"
                    )
                  ),
                  selectInput(
                    "tipo_farmacia",
                    "FARMÁCIA:",
                    choices = c(
                      "Veterinária",
                      "Humana",
                      "Manipulação Veterinária",
                      "Manipulação Humana"
                    )
                  )
                  
                  
                ),
                column(
                  6,
                  textInput("dose", "DOSE (mg):"),
                  numericInput(
                    "frequencia",
                    "Frequencia de Administracao:",
                    min = 1,
                    max = 24,
                    value = 1
                  ),
                  selectInput(
                    "periodo",
                    "Periodo de Administracao:",
                    choices = c("Horas",
                                "Dias",
                                "Semanas")
                  )
                )
              )),
      tabItem(tabName = "receituario",
              fluidRow(DTOutput(
                "base_cadastral_table"
              ))),
      tabItem(tabName = "estatistica1",
              h2("Estatísticas")),
      tabItem(tabName = "receitafim1",
              h2("Receituário Final")),
      tabItem(tabName = "sintomas",
              h2("Sintomas"))
    )
  ),
  footer = dashboardFooter(
    left = tags$b("Dra. ZAÍRA DE CARVALHO DE SÁ"),
    right = tags$b("Medico Veterinário CRMV-PA:12.456")
  )
)

#==============================================================================#
# Definir Interface do Servidor 
server <- function(input, output, session) {
  # Initialize reactive values object to store patient data
  patient_data <- reactiveValues(
    patients = data.frame(
      Nome = character(0),
      Sexo = character(0),
      Endereco = character(0),
      Cidade = character(0),
      Telefone = character(0),
      Paciente = character(0),
      Espécie = character(0),
      Sexo_Animal = character(0),
      Nascimento = character(0),
      Peso = character(0)
    )
  )
  
  # Function to add patient to the database
  observeEvent(input$add_patient, {
    # Extract inputs
    nome <- isolate(input$nome)
    sexo <- isolate(input$sexo)
    endereco <- isolate(input$endereco)
    cidade <- isolate(input$cidade)
    telefone <- isolate(input$telefone)
    paciente <- isolate(input$nome_cadastro)
    especie <- isolate(input$especie)
    sexo_animal <- isolate(input$sexo_animal)
    nascimento <- isolate(input$nascimento)
    peso <- isolate(input$peso)
    
    # Add patient to the database
    patient_data$patients <-
      rbind(
        patient_data$patients,
        data.frame(
          Nome = nome,
          Sexo = sexo,
          Endereco = endereco,
          Cidade = cidade,
          Telefone = telefone,
          Paciente = paciente,
          Espécie = especie,
          Sexo_Animal = sexo_animal,
          Nascimento = nascimento,
          Peso = peso
        )
      )
    
    # Clear input fields
    updateTextInput(session, "nome", value = "")
    updateSelectInput(session, "sexo", selected = NULL)
    updateTextInput(session, "endereco", value = "")
    updateTextInput(session, "cidade", value = "")
    updateTextInput(session, "telefone", value = "")
    updateTextInput(session, "nome_cadastro", value = "")
    updateSelectInput(session, "especie", selected = NULL)
    updateSelectInput(session, "sexo_animal", selected = NULL)
    updateDateInput(session, "nascimento", value = NULL)
    updateNumericInput(session, "peso", value = NULL)
  })
  
  # Render the base cadastral table
  output$base_cadastral_table <- renderDT({
    datatable(
      patient_data$patients,
      filter = "top",
      #filter = "bottom",
      plugins = 'natural',
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        engthMenu = list(c(7, 50, 500, 2500, -1)),
        c(5, 50, 1000, 2500, "All"),
        pageLength = 7,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border compact stripe hover row-border order-column dt-body-right',
      editable = 'cell',
      colnames = c(
        'Proprietário',
        'Sexo',
        'Endereço',
        'Cidade',
        'Contato',
        'RG',
        'Paciente',
        'Espécie',
        'Animal',
        'Nascimento',
        'Peso',
        'Altura'
      ),
      caption = 'Tabela 1. Base de Dados sobre Vítimas Fatais por Sinistros de Trânsito em Rodovias Estaduais.'
    )
  })
}
#==============================================================================#


#==============================================================================#
# Run the application
shinyApp(ui = ui, server = server)
#==============================================================================#