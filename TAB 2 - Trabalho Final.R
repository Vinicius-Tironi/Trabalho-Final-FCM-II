library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

# Dados para o app
Dados_2 <- read_csv("C:\\Users\\Vinicius\\Documents\\Vinícius\\Faculdade\\Ferramentas Modelagem\\Trabalho Final\\SQL Data.csv")
Dados_2 <- as.data.frame(Dados_2)


# UI - User Interface
ui <- fluidPage(
  
  # tema
  theme = shinytheme("darkly"),
  
  # Título da aplicação
  titlePanel("Educação e Deslocamento"),
  
  # Layout lateral
  sidebarLayout(
    sidebarPanel(
      # Selecionando raças - Selecionar somente um único valor
      checkboxGroupInput(inputId = "IdCor", selected = 'Amarela',
                  label = "Raça ou Cor:",
                  choices = sort(unique(Dados_2$cor)),
                  inline = FALSE
      ),
      # Selecionando as faixas etárias - Selecionar + de um valor
      checkboxGroupInput(inputId = "IdIdade", selected = '0 a 3 anos',
                     label = "Faixa Etária:",
                     choices = unique(Dados_2$idade),
                     #options = list(maxItems = 7)
      ),
      # Filtrando as diversas colunas - Fazer caixa de escolha
      checkboxGroupInput(inputId = "IdCol", selected = 'creche',
                         label = "Variáveis de estudo:",
                         choices = colnames(Dados_2)[seq(3,5)],
                         inline = FALSE
      ),
      
      actionButton(inputId = "IdExecutar",
                   label = "Consultar"
      )
    ),
    
    # Mostrar tudo em destaque gráfico, texto, tabela entre outros...
    mainPanel(
      # Texto inicial
      textOutput(outputId = "teste"),
      
      # Plot do gráfico de barras
      plotlyOutput(outputId = "IdGrafBarras"),
      
      # Plot do gráfico de Espalhamento
      plotlyOutput(outputId = "IdGrafEsp"),
      
      # Plot do boxplot
      plotlyOutput(outputId = "IdGrafBox")
    )
  )
)

# 


# Server
server <- function(input, output) {
  
  # Função responsável por executar quando o botão for clicado
#  output$teste <- eventReactive(input$IdExecutar,{)
 # })
  
  # Gráfico de Barras
  output$IdGrafBarras <- renderPlotly({
    ggplotly(Dados_2 %>% pivot_longer(3:5, names_to = 'estudo', values_to = 'valor') %>% 
               filter(estudo %in% input$IdCol) %>%
               group_by(cor, estudo) %>%
               summarise(valor = sum(valor, na.rm = TRUE)) %>%
               
               filter(cor %in% input$IdCor) %>%

               ggplot(aes(y = cor, x = valor, fill = estudo)) +
               geom_col() + 
               ylab("Raça") + xlab("Quantidade") + 
               ggtitle("Gráfico de Barras") + 
               theme_minimal() + 
               theme(plot.title = element_text(hjust = 0.5)) +
               scale_x_continuous(trans = "log")
    )
    
  })
  
  # Gráfico de Espalhamento
  output$IdGrafEsp <- renderPlotly({
    ggplotly(Dados_2 %>% pivot_longer(3:5, names_to = 'estudo', values_to = 'valor') %>% 
               filter(estudo %in% input$IdCol) %>%
               group_by(cor, estudo) %>%
               
               filter(idade %in% input$IdIdade) %>%
               
               ggplot(aes(x = valor, y = idade)) + 
               geom_point(aes(color = cor)) + 
               xlab("Quantidade") + ylab("Idade") +
               ggtitle("Espalhamento Linear") + 
               theme_minimal() + 
               scale_x_continuous(),
               axis.text.x=element_blank()
    )
    
  })
  
  # Gráfico Boxplot
  output$IdGrafBox <- renderPlotly({
    ggplotly(Dados_2 %>% pivot_longer(3:5, names_to = 'estudo', values_to = 'valor') %>% 
               filter(estudo %in% input$IdCol) %>%
               group_by(cor, estudo) %>%
               
               filter(cor %in% input$IdCor) %>%
               filter(estudo %in% input$IdCol) %>%
               
               ggplot(aes(x = cor, y = valor, fill = estudo)) + 
               geom_boxplot() + 
               xlab("Raça") + ylab("Quantidade") +
               ggtitle("Boxplot") + 
               theme_minimal() + 
               scale_y_continuous(trans = "log")
    )
    
  })
  
}

# Rodar a aplicação
shinyApp(ui = ui, server = server)