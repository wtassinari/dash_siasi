# ============================================
# DASHBOARD SIASI - VERSÃO 5 (COM ABA REGISTROS)
# ============================================

suppressWarnings({
  library(tidyverse)
  library(DT)
  library(scales)
  library(shiny)
  library(shinydashboard)
  library(plotly)
})

# ============================================
# CARREGAR E PREPARAR DADOS
# ============================================

# Registros Gerais
registros <- read.csv("registros.csv", sep=";", stringsAsFactors = FALSE)
names(registros) <- c("categorias", "frequencias")
registros$frequencias <- as.numeric(registros$frequencias)

# Dados de Nascimentos por Ano (com todas as categorias)
anonasc <- read.csv("frequencia_ano_nascimento.csv", sep=";")
anonasc_calc <- anonasc %>%
  mutate(
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2)
  )

# Dados de Nascimentos por DSEI
nascimentos_dsei <- read.csv("frequencia_ano_nascimento_dsei.csv", sep=";", stringsAsFactors = FALSE)
names(nascimentos_dsei) <- c("ds_dsei", "co_dsei_polo", "ano_nascimento", 
                             "frequencia_ativos", "frequencia_ativos_indigenas")

nascimentos_dsei_calc <- nascimentos_dsei %>%
  mutate(
    frequencia_ativos = as.numeric(frequencia_ativos),
    frequencia_ativos_indigenas = as.numeric(frequencia_ativos_indigenas),
    diferenca_nao_indigenas = frequencia_ativos - frequencia_ativos_indigenas,
    ano_num = as.numeric(as.character(ano_nascimento))
  ) %>%
  group_by(ds_dsei, co_dsei_polo) %>%
  mutate(
    perc_total_ativos = round(100 * frequencia_ativos / sum(frequencia_ativos, na.rm = TRUE), 2),
    perc_total_ativ_indig = round(100 * frequencia_ativos_indigenas / sum(frequencia_ativos_indigenas, na.rm = TRUE), 2)
  ) %>%
  ungroup() %>%
  arrange(ds_dsei, co_dsei_polo, ano_num) %>%
  group_by(ds_dsei, co_dsei_polo) %>%
  mutate(
    crescimento_abs_ativos = frequencia_ativos - lag(frequencia_ativos),
    crescimento_abs_ativ_indig = frequencia_ativos_indigenas - lag(frequencia_ativos_indigenas),
    crescimento_perc_ativos = round((frequencia_ativos - lag(frequencia_ativos)) / lag(frequencia_ativos) * 100, 2),
    crescimento_perc_ativ_indig = round((frequencia_ativos_indigenas - lag(frequencia_ativos_indigenas)) / lag(frequencia_ativos_indigenas) * 100, 2)
  ) %>%
  ungroup()

# Dados de Óbitos por Ano
obitos_ano <- read.csv("frequencia_ano_obitos.csv", sep=";", stringsAsFactors = FALSE)
names(obitos_ano) <- c("ano_obito", "frequencia_obitos", "percentual", 
                       "frequencia_acumulada", "percentual_acumulado")

obitos_ano_calc <- obitos_ano %>%
  mutate(
    frequencia_obitos = as.numeric(gsub(",", ".", gsub("\\.", "", frequencia_obitos))),
    percentual = as.numeric(gsub(",", ".", gsub("\\.", "", percentual)))
  ) %>%
  filter(!is.na(frequencia_obitos))

# Dados de Óbitos por DSEI
obitos_dsei <- read.csv("frequencia_ano_obitos_dsei.csv", sep=";", stringsAsFactors = FALSE)
names(obitos_dsei) <- c("ds_dsei", "co_dsei_polo", "ano_obito", 
                        "frequencia_simples", "percentual", 
                        "frequencia_acumulada", "percentual_acumulado")

obitos_dsei_clean <- obitos_dsei %>%
  mutate(
    frequencia_simples = as.numeric(gsub(",", ".", gsub("\\.", "", frequencia_simples))),
    percentual = as.numeric(gsub(",", ".", gsub("\\.", "", percentual))),
    frequencia_acumulada = as.numeric(gsub(",", ".", gsub("\\.", "", frequencia_acumulada))),
    percentual_acumulado = as.numeric(gsub(",", ".", gsub("\\.", "", percentual_acumulado))),
    ordem_ano = if_else(ano_obito == "Antes de 2000", 0, suppressWarnings(as.numeric(ano_obito)))
  ) %>%
  filter(!is.na(frequencia_simples), !is.na(ds_dsei), ds_dsei != "")

obitos_dsei_calc <- obitos_dsei_clean %>%
  filter(!is.na(ordem_ano)) %>%
  arrange(ds_dsei, co_dsei_polo, ordem_ano) %>%
  group_by(ds_dsei, co_dsei_polo) %>%
  mutate(
    perc_dsei = round((frequencia_simples / sum(frequencia_simples, na.rm = TRUE)) * 100, 2),
    crescimento_abs = frequencia_simples - lag(frequencia_simples),
    crescimento_perc = round((frequencia_simples - lag(frequencia_simples)) / lag(frequencia_simples) * 100, 2)
  ) %>%
  ungroup()

# Dados de População por Ano
populacao <- read.csv("populacao_por_ano.csv", sep=";")
names(populacao) <- c("ano", "ativos_e_indigenas", "somente_ativos")

populacao_calc <- populacao %>%
  arrange(ano) %>%
  mutate(
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2),
    crescimento_abs_ativos_indigenas = ativos_e_indigenas - lag(ativos_e_indigenas),
    crescimento_abs_somente_ativos = somente_ativos - lag(somente_ativos),
    crescimento_perc_ativos_indigenas = round((ativos_e_indigenas - lag(ativos_e_indigenas)) / lag(ativos_e_indigenas) * 100, 2),
    crescimento_perc_somente_ativos = round((somente_ativos - lag(somente_ativos)) / lag(somente_ativos) * 100, 2)
  )

# Dados de População por DSEI
populacao_dsei <- read.csv("populacao_por_ano_dsei_com_seq.csv", sep=";")
names(populacao_dsei) <- c("ds_dsei", "co_seq_dsei", "ano", "ativos_e_indigenas", "somente_ativos")

populacao_dsei_calc <- populacao_dsei %>%
  arrange(ds_dsei, ano) %>%
  group_by(ds_dsei) %>%
  mutate(
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2),
    crescimento_abs_ativos_indigenas = ativos_e_indigenas - lag(ativos_e_indigenas),
    crescimento_abs_somente_ativos = somente_ativos - lag(somente_ativos),
    crescimento_perc_ativos_indigenas = round((ativos_e_indigenas - lag(ativos_e_indigenas)) / lag(ativos_e_indigenas) * 100, 2),
    crescimento_perc_somente_ativos = round((somente_ativos - lag(somente_ativos)) / lag(somente_ativos) * 100, 2)
  ) %>%
  ungroup()

# ============================================
# UI DO SHINY
# ============================================

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard SIASI"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Registros Gerais", tabName = "registros", icon = icon("database")),
      menuItem("Nascimentos", tabName = "nascimentos", icon = icon("baby")),
      menuItem("Óbitos", tabName = "obitos", icon = icon("heart-broken")),
      menuItem("População", tabName = "populacao", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .box { border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .dataTables_wrapper { font-size: 12px; }
      "))
    ),
    
    tabItems(
      # ============================================
      # ABA: REGISTROS GERAIS
      # ============================================
      tabItem(tabName = "registros",
              fluidRow(
                box(title = "Registros Gerais da Base de Dados", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("registros_table"))
              )
      ),
      
      # ============================================
      # ABA: NASCIMENTOS
      # ============================================
      tabItem(tabName = "nascimentos",
              # Tabela Anual
              fluidRow(
                box(title = "Frequência de Nascimentos por Ano", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("nascimentos_ano_table"))
              ),
              # Gráfico Anual
              fluidRow(
                box(title = "Evolução de Nascimentos por Ano", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("nascimentos_ano_plot", height = "500px"))
              ),
              # Filtro DSEI
              fluidRow(
                box(title = "Filtro de DSEI", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("filtro_dsei_nascimentos"))
              ),
              # Tabela DSEI
              fluidRow(
                box(title = "Frequência de Nascimentos por DSEI e Ano", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("nascimentos_dsei_table"))
              ),
              # Gráfico DSEI
              fluidRow(
                box(title = "Série Temporal por DSEI", status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("nascimentos_dsei_plot", height = "500px"))
              )
      ),
      
      # ============================================
      # ABA: ÓBITOS
      # ============================================
      tabItem(tabName = "obitos",
              # Tabela Anual
              fluidRow(
                box(title = "Frequência de Óbitos por Ano", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("obitos_ano_table"))
              ),
              # Gráfico Anual
              fluidRow(
                box(title = "Evolução de Óbitos por Ano", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("obitos_ano_plot", height = "500px"))
              ),
              # Filtro DSEI
              fluidRow(
                box(title = "Filtro de DSEI", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("filtro_dsei_obitos"))
              ),
              # Tabela DSEI
              fluidRow(
                box(title = "Frequência de Óbitos por DSEI e Ano", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("obitos_dsei_table"))
              ),
              # Gráfico DSEI
              fluidRow(
                box(title = "Série Temporal por DSEI", status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("obitos_dsei_plot", height = "500px"))
              )
      ),
      
      # ============================================
      # ABA: POPULAÇÃO
      # ============================================
      tabItem(tabName = "populacao",
              # Tabela Anual
              fluidRow(
                box(title = "População por Ano", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("populacao_ano_table"))
              ),
              # Gráfico Anual
              fluidRow(
                box(title = "Evolução Populacional", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("populacao_ano_plot", height = "500px"))
              ),
              # Filtro DSEI
              fluidRow(
                box(title = "Filtro de DSEI", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("filtro_dsei_populacao"))
              ),
              # Tabela DSEI
              fluidRow(
                box(title = "População por DSEI e Ano", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("populacao_dsei_table"))
              ),
              # Gráfico DSEI
              fluidRow(
                box(title = "Série Temporal por DSEI", status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("populacao_dsei_plot", height = "500px"))
              )
      )
    )
  )
)

# ============================================
# SERVER DO SHINY
# ============================================

server <- function(input, output, session) {
  
  # ============================================
  # REGISTROS GERAIS
  # ============================================
  
  output$registros_table <- renderDT({
    datatable(registros,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("Categorias", "Frequências")
    ) %>%
      formatRound(columns = 2, digits = 0, mark = ".")
  })
  
  # ============================================
  # NASCIMENTOS
  # ============================================
  
  output$nascimentos_ano_table <- renderDT({
    datatable(anonasc_calc,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("Ano", "Ativos e Indígenas", "Somente Ativos", "Diferença", "% Ativ. Indig.", "% Só Ativos")
    ) %>%
      formatRound(columns = c(1:3), digits = 0, mark = ".") %>%
      formatRound(columns = c(4:5), digits = 2) %>%
      formatStyle(1, target = 'row', backgroundColor = styleEqual(c("Sem informação", "Até 2000"), c('#ffcccc', '#ffcccc')))
  })
  
  output$nascimentos_ano_plot <- renderPlotly({
    anonasc_numerico <- anonasc %>%
      filter(!ano_categoria %in% c("Sem informação", "Até 2000")) %>%
      mutate(ano_categoria = as.numeric(ano_categoria))
    
    plot_ly(data = anonasc_numerico) %>%
      add_trace(x = ~ano_categoria, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2),
                marker = list(color = '#e74c3c', size = 6)) %>%
      add_trace(x = ~ano_categoria, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#3498db', width = 2),
                marker = list(color = '#3498db', size = 6)) %>%
      layout(title = "Nascimentos: Ativos e Indígenas vs Somente Ativos",
             xaxis = list(title = "Ano", dtick = 2),
             yaxis = list(title = "Registros", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  # Filtro DSEI - Nascimentos
  output$filtro_dsei_nascimentos <- renderUI({
    dsei_choices <- nascimentos_dsei_calc %>%
      filter(!is.na(ano_num), ano_num >= 2000) %>%
      distinct(ds_dsei) %>%
      pull(ds_dsei) %>%
      sort()
    
    selectizeInput("nascimentos_dsei_select", "Selecione o DSEI:",
                   choices = dsei_choices,
                   selected = dsei_choices[1],
                   options = list(placeholder = 'Digite para buscar'))
  })
  
  # Tabela DSEI - Nascimentos (REATIVA)
  output$nascimentos_dsei_table <- renderDT({
    dsei_selecionado <- input$nascimentos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(NULL)
    }
    
    nascimentos_dsei_resumido <- nascimentos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      select(ds_dsei, ano_nascimento, frequencia_ativos_indigenas, frequencia_ativos,
             diferenca_nao_indigenas, perc_total_ativ_indig, perc_total_ativos,
             crescimento_perc_ativos, crescimento_perc_ativ_indig)
    
    datatable(nascimentos_dsei_resumido,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("DSEI", "Ano", "Ativ. Indig.", "Ativos", "Diferença", "% Indig.", "% Ativos", "Cresc. % Ativos", "Cresc. % Indig.")
    ) %>%
      formatRound(columns = c(3:5), digits = 0, mark = ".") %>%
      formatRound(columns = c(6:9), digits = 2)
  })
  
  # Gráfico DSEI - Nascimentos (REATIVO)
  output$nascimentos_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$nascimentos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- nascimentos_dsei_calc %>%
      filter(!is.na(ano_num), ano_num >= 2000, ds_dsei == dsei_selecionado)
    
    if (nrow(dados) == 0) {
      return(plotly_empty() %>% layout(title = "Sem dados disponíveis"))
    }
    
    plot_ly(dados) %>%
      add_trace(x = ~ano_num, y = ~frequencia_ativos_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano_num, y = ~frequencia_ativos, name = "Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = paste("Nascimentos -", dsei_selecionado),
             xaxis = list(title = "Ano", dtick = 5),
             yaxis = list(title = "Registros", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  # ============================================
  # ÓBITOS
  # ============================================
  
  output$obitos_ano_table <- renderDT({
    datatable(obitos_ano_calc %>% select(ano_obito, frequencia_obitos, percentual),
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("Ano", "Óbitos", "% Total")
    ) %>%
      formatRound(columns = c(2), digits = 0, mark = ".") %>%
      formatRound(columns = c(3), digits = 2)
  })
  
  output$obitos_ano_plot <- renderPlotly({
    obitos_filtrado <- obitos_ano_calc %>%
      filter(ano_obito != "Antes de 2000") %>%
      mutate(ano_obito = as.numeric(ano_obito)) %>%
      arrange(ano_obito)
    
    plot_ly(data = obitos_filtrado) %>%
      add_trace(x = ~ano_obito, y = ~frequencia_obitos, name = "Óbitos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2),
                marker = list(color = '#e74c3c', size = 6)) %>%
      layout(title = "Óbitos por Ano",
             xaxis = list(title = "Ano", dtick = 1),
             yaxis = list(title = "Óbitos", tickformat = ",.0f"),
             hovermode = "x unified")
  })
  
  # Filtro DSEI - Óbitos
  output$filtro_dsei_obitos <- renderUI({
    dsei_choices <- obitos_dsei_calc %>%
      distinct(ds_dsei) %>%
      pull(ds_dsei) %>%
      sort()
    
    selectizeInput("obitos_dsei_select", "Selecione o DSEI:",
                   choices = dsei_choices,
                   selected = dsei_choices[1],
                   options = list(placeholder = 'Digite para buscar'))
  })
  
  # Tabela DSEI - Óbitos (REATIVA)
  output$obitos_dsei_table <- renderDT({
    dsei_selecionado <- input$obitos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(NULL)
    }
    
    obitos_dsei_resumido <- obitos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      select(ds_dsei, ano_obito, frequencia_simples, perc_dsei, crescimento_abs, crescimento_perc)
    
    datatable(obitos_dsei_resumido,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("DSEI", "Ano", "Óbitos", "% DSEI", "Cresc. Abs.", "Cresc. %")
    ) %>%
      formatRound(columns = c(3:4), digits = 0, mark = ".") %>%
      formatRound(columns = c(5:6), digits = 2)
  })
  
  # Gráfico DSEI - Óbitos (REATIVO)
  output$obitos_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$obitos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- obitos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado, !is.na(ordem_ano), ordem_ano > 0)
    
    if (nrow(dados) == 0) {
      return(plotly_empty() %>% layout(title = "Sem dados disponíveis"))
    }
    
    plot_ly(dados) %>%
      add_trace(x = ~ordem_ano, y = ~frequencia_simples, name = "Óbitos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2),
                marker = list(size = 5)) %>%
      layout(title = paste("Óbitos -", dsei_selecionado),
             xaxis = list(title = "Ano", dtick = 1),
             yaxis = list(title = "Óbitos", tickformat = ",.0f"),
             hovermode = "x unified")
  })
  
  # ============================================
  # POPULAÇÃO
  # ============================================
  
  output$populacao_ano_table <- renderDT({
    datatable(populacao_calc,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("Ano", "Ativ. Indig.", "Só Ativos", "Diferença", "% Indig.", "% Ativos", "Cresc. Abs. Indig.", "Cresc. Abs. Ativos", "Cresc. % Indig.", "Cresc. % Ativos")
    ) %>%
      formatRound(columns = c(2:4, 7:8), digits = 0, mark = ".") %>%
      formatRound(columns = c(5:6, 9:10), digits = 2)
  })
  
  output$populacao_ano_plot <- renderPlotly({
    populacao_numerico <- populacao %>%
      filter(!is.na(ano)) %>%
      mutate(ano = as.numeric(ano))
    
    plot_ly(populacao_numerico) %>%
      add_trace(x = ~ano, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines", fill = 'tozeroy',
                fillcolor = 'rgba(231, 76, 60, 0.3)',
                line = list(color = '#e74c3c', width = 2)) %>%
      add_trace(x = ~ano, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines", fill = 'tonexty',
                fillcolor = 'rgba(52, 152, 219, 0.3)',
                line = list(color = '#3498db', width = 2)) %>%
      layout(title = "Evolução Populacional",
             xaxis = list(title = "Ano", dtick = 2),
             yaxis = list(title = "População", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  # Filtro DSEI - População
  output$filtro_dsei_populacao <- renderUI({
    dsei_choices <- populacao_dsei_calc %>%
      distinct(ds_dsei) %>%
      pull(ds_dsei) %>%
      sort()
    
    selectizeInput("populacao_dsei_select", "Selecione o DSEI:",
                   choices = dsei_choices,
                   selected = dsei_choices[1],
                   options = list(placeholder = 'Digite para buscar'))
  })
  
  # Tabela DSEI - População (REATIVA)
  output$populacao_dsei_table <- renderDT({
    dsei_selecionado <- input$populacao_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(NULL)
    }
    
    populacao_dsei_resumido <- populacao_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      select(ds_dsei, ano, ativos_e_indigenas, somente_ativos, diferenca,
             crescimento_perc_ativos_indigenas, crescimento_perc_somente_ativos)
    
    datatable(populacao_dsei_resumido,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollY = "300px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      colnames = c("DSEI", "Ano", "Ativ. Indig.", "Só Ativos", "Diferença", "Cresc. % Indig.", "Cresc. % Ativos")
    ) %>%
      formatRound(columns = c(3:5), digits = 0, mark = ".") %>%
      formatRound(columns = c(6:7), digits = 2)
  })
  
  # Gráfico DSEI - População (REATIVO)
  output$populacao_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$populacao_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- populacao_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado)
    
    if (nrow(dados) == 0) {
      return(plotly_empty() %>% layout(title = "Sem dados disponíveis"))
    }
    
    plot_ly(dados) %>%
      add_trace(x = ~ano, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = paste("População -", dsei_selecionado),
             xaxis = list(title = "Ano", dtick = 1),
             yaxis = list(title = "População", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
}

shinyApp(ui = ui, server = server)
