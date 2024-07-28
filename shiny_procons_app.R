## Bibliotecas do projeto
library(shiny)
library(data.table)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)

## Fonte de dados
dados <- read.csv("dados_limpos.csv", encoding = 'UTF-8')

## Função para calcular a média de chamados por ano
media_chamados_ano <- dados %>%
  group_by(anocalendario) %>%
  summarise(qtd_chamados = n()) %>%
  summarise(medias_chamado_ano = mean(qtd_chamados)) %>%
  as.numeric()

## Custom CSS
custom_css <- "
body, .content-wrapper {
  background: linear-gradient(90deg, #0c2646 0, #204065 60%, #2a5788);
  color: #f8f9fa;
}
.info-box, .box {
  background: rgba(248, 249, 250, 0.15) !important;
  color: #f8f9fa !important;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  border: 1px solid rgba(248, 249, 250, 0.3);
}
.custom-icon {
  color: #f8f9fa !important;
}
"

## Função para personalizar gráficos Plotly
customize_plotly <- function(p) {
  p %>% layout(
    paper_bgcolor = 'rgba(12, 38, 70, 1)',
    plot_bgcolor = 'rgba(12, 38, 70, 1)',
    font = list(color = '#f8f9fa'),
    xaxis = list(tickfont = list(color = '#f8f9fa'), titlefont = list(color = '#f8f9fa')),
    yaxis = list(tickfont = list(color = '#f8f9fa'), titlefont = list(color = '#f8f9fa')),
    xaxis = list(showgrid = FALSE),
    yaxis = list(showgrid = FALSE)
  )
}

## Início do layout
cabecalho <- dashboardHeader(title = "PROCONS")

## Sidebar
barra_lateral <- dashboardSidebar(
  width = '230px', 
  sidebarMenu(
    menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
    menuItem('Informações', tabName = 'infos', icon = icon('info-circle'))
  )
)

## Corpo do dashboard
painel_principal <- dashboardBody(
  tags$head(tags$style(HTML(custom_css))),
  tabItems(
    tabItem(
      tabName = 'infos',
      h1('Informações'),
      infoBox(
        title = 'Contato', 
        icon = icon('envelope-square'),
        subtitle = 'Para mais informações e/ou feedback entre em contato: ricardoalexdel@gmail.com'
      )
    ),
    tabItem(
      tabName = 'dashboard',
      fluidRow(
        infoBox(title = '', subtitle = 'Registros', value = nrow(dados), icon = icon("database")),
        infoBox(title = '', subtitle = 'Reclamações por Ano', value = media_chamados_ano, icon = icon('list')),
        infoBoxOutput(outputId = 'qtdUF')
      ),
      fluidRow(
        column(
          width = 12,
          box(
            title = 'Filtros', width = '100%',
            column(
              width = 12,
              box(
                width = '100%', 
                awesomeCheckboxGroup(
                  inline = TRUE, 
                  inputId = 'select_UF', 
                  label = 'Estados:', 
                  choices = c('Todos', unique(dados$UF)), 
                  selected = 'Todos'
                )
              )
            ),
            column(
              width = 6,
              box(
                width = '100%', 
                dateRangeInput(
                  inputId = 'data_abertura',
                  label = 'Data Abertura',
                  format = 'dd-mm-yyyy',
                  start = min(as.Date(dados$DataAbertura)),
                  end = max(as.Date(dados$DataAbertura))
                )
              )
            ),
            column(
              width = 6,
              box(
                width = '100%',
                selectizeInput(
                  inputId ='assunto',
                  label = 'Descrição do assunto', 
                  choices = c('Todos', unique(dados$DescricaoAssunto)),
                  multiple = TRUE, options = list(maxItems = 5),
                  selected = 'Todos'
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = '100%',
            plotlyOutput(outputId = 'data', width = '100%'),
            verbatimTextOutput(outputId = 'descData')
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            width = '100%',
            plotlyOutput(outputId = 'atendida')
          )
        ),
        column(
          width = 6,
          box(
            width = '100%',
            plotlyOutput(outputId = 'atendidaAno')
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = '100%', title = 'Reclamações por UF',
            plotlyOutput(outputId = 'uf'),
            textOutput(outputId = 'descUf')
          )
        )
      )
    )
  )
)

ui <- dashboardPage(
  header = cabecalho, 
  sidebar = barra_lateral, 
  body = painel_principal
)

## Renderização dos gráficos
server <- function(input, output) {
  dados_selecionados <- reactive({
    data <- dados
    
    if(! 'Todos' %in% input$select_UF) {
      data <- data %>% filter(UF %in% input$select_UF)
    } 
    
    if(! 'Todos' %in% input$assunto) {
      data <- data %>% filter(DescricaoAssunto %in% input$assunto)
    }
    
    data <- data %>% filter(
      as.Date(DataAbertura) >= input$data_abertura[1] &
        as.Date(DataAbertura) <= input$data_abertura[2]
    )
    
    data
  })
  
  output$data <- renderPlotly({
    p <- ggplotly(
      data.frame(table(as.Date(dados_selecionados()$DataAbertura))) %>%
        rename(Data = Var1, Qtd = Freq) %>%
        ggplot(aes(as.Date(Data), Qtd)) +
        geom_line(color = "#E0FFFF", group = 1) +  # Cor da linha
        theme_minimal() +  # Mudança para o tema minimalista
        theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "#f8f9fa"),
          axis.text.y = element_text(color = "#f8f9fa"),
          axis.title.x = element_text(color = "#f8f9fa"),
          axis.title.y = element_text(color = "#f8f9fa"),
          plot.title = element_text(color = "#f8f9fa")
        ) +
        ggtitle('Quantidade de Reclamações por Ano-Mês') +
        scale_x_date(date_labels = '%b-%Y', breaks = '6 month')
    )
    customize_plotly(p)
  })
  
  output$uf <- renderPlotly({
    p <- ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>%
        rename(UF = Var1, Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF, Qtd), y = Qtd, text = paste('UF:', UF, "<br>", "Qtd:", Qtd))) +
        geom_bar(fill = "#E0FFFF", stat = 'identity') +
        coord_flip() +
        xlab('UF') +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "#f8f9fa"),
          axis.text.y = element_text(color = "#f8f9fa"),
          axis.title.x = element_text(color = "#f8f9fa"),
          axis.title.y = element_text(color = "#f8f9fa"),
          plot.title = element_text(color = "#f8f9fa")
        ) +
        ggtitle('Quantidade de reclamações por UF'),
      tooltip = 'text'
    )
    customize_plotly(p)
  })
  
  output$atendida <- renderPlotly({
    p <- ggplotly(
      ggplot(dados_selecionados()) +
        geom_bar(aes(x = Atendida), fill = "#E0FFFF", stat = 'count', color = "#f8f9fa") +
        ylab('Quantidade') +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = "#f8f9fa"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "#f8f9fa"),
          axis.text.y = element_text(color = "#f8f9fa"),
          axis.title.x = element_text(color = "#f8f9fa"),
          axis.title.y = element_text(color = "#f8f9fa"),
          plot.title = element_text(color = "#f8f9fa"),
          legend.title = element_text(color = "#f8f9fa"),
          legend.text = element_text(color = "#f8f9fa")
        ) +
        ggtitle('Quantidade de chamados atendidos')
    )
    customize_plotly(p)
  })
  
  output$atendidaAno <- renderPlotly({
    p <- ggplotly(
      data.frame(table(dados_selecionados()$anocalendario, dados_selecionados()$Atendida)) %>%
        rename(Ano = Var1, Atendidas = Var2, Qtd = Freq) %>%
        ggplot(aes(x = Ano, y = Qtd, fill = Atendidas)) +
        geom_bar(stat = 'identity', position = position_dodge2(), fill = "#E0FFFF") +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "#f8f9fa"),
          axis.text.y = element_text(color = "#f8f9fa"),
          axis.title.x = element_text(color = "#f8f9fa"),
          axis.title.y = element_text(color = "#f8f9fa"),
          plot.title = element_text(color = "#f8f9fa"),
          legend.title = element_text(color = "#f8f9fa"),
          legend.text = element_text(color = "#f8f9fa")
        ) +
        ggtitle('Quantidade de reclamações Atendidas(não) por Ano')
    )
    customize_plotly(p)
  })
  
  output$descData <- renderText({
    paste("Gráfico com a quantidade de reclamações feitas entre:", 
          min(dados_selecionados()$DataAbertura), '-', max(dados_selecionados()$DataAbertura))
  })
  
  output$descUf <- renderText({
    estados <- paste(unique(dados_selecionados()$UF), collapse = ', ')
    paste('Gráfico com a quantidade de reclamações feitas pelas UF: ', estados)
  })
  
  output$qtdUF <- renderValueBox({
    valueBox(
      value = length(unique(dados_selecionados()$UF)),
      subtitle = "UFs Selecionadas", icon = icon("map-location-dot", class = "custom-icon")
    )  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
