# ============================================
# DASHBOARD SIASI - VERSÃO 6 (CARDS + REGISTROS + LOGIN)
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
# CONFIGURAÇÃO DE SENHA
# ============================================
# A senha é lida de uma variável de ambiente, NUNCA fica escrita no código
# (importante já que o app.R vai para o GitHub).
# No Posit Connect: Configurações do app > Vars > adicionar SENHA_DASHBOARD.
# Localmente (RStudio): defina em um arquivo .Renviron (que NÃO deve ir pro Git).
SENHA_DASHBOARD <- Sys.getenv("SENHA_DASHBOARD", unset = "")

if (SENHA_DASHBOARD == "") {
  stop("A variável de ambiente SENHA_DASHBOARD não foi definida. Configure-a no Posit Connect (Vars) antes de publicar/rodar o app.")
}

# ============================================
# CARREGAR E PREPARAR DADOS
# ============================================

# Registros Gerais
registros <- read.csv("registros.csv", sep=";", stringsAsFactors = FALSE)
names(registros) <- c("categorias", "frequencias", "percentual_total_original")
registros$frequencias <- as.numeric(registros$frequencias)
registros$percentual_total_original <- as.numeric(registros$percentual_total_original)

# Linhas que representam o filtro "Apenas Aldeados" (para destacar na tabela)
categorias_aldeados <- registros$categorias[grepl("aldeados", registros$categorias, ignore.case = TRUE)]

# --- Valor externo: Total Original (não deduplicado) ---
total_original_externo <- 1394825

# --- Categorias que serão extraídas para os cards (marcadas em vermelho) ---
categorias_cards <- c(
  "Total original",
  "Total aldeados",
  "Somente ativos (aldeados)",
  "Somente indígenas (aldeados)",
  "Ativos e indígenas (aldeados)"
)

# Tabela filtrada sem os registros que viraram cards
registros_tabela <- registros %>%
  filter(!categorias %in% categorias_cards)

# Dados de Nascimentos por Ano (com todas as categorias)
anonasc <- read.csv("frequencia_ano_nascimento.csv", sep=";", stringsAsFactors = FALSE)
anonasc_calc <- anonasc %>%
  mutate(
    ano_categoria = as.character(ano_categoria),
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2)
  )

# Tabela filtrada: remove "Sem informação" e substitui "Até 2000" por "< 2000"
anonasc_table <- anonasc_calc %>%
  filter(ano_categoria != "Sem informação") %>%
  mutate(ano_categoria = ifelse(ano_categoria == "Até 2000", "< 2000", ano_categoria))

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

# --- Tela de login ---
login_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #ecf0f5; }
      .login-box {
        max-width: 380px;
        margin: 8% auto;
        padding: 30px;
        background: #ffffff;
        border-radius: 10px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.15);
        text-align: center;
      }
      .login-box h2 { color: #367fa9; margin-bottom: 20px; }
      .login-erro { color: #e74c3c; font-weight: bold; margin-top: 10px; }
    "))
  ),
  div(class = "login-box",
      h2("Dashboard SIASI"),
      icon("lock", style = "font-size: 40px; color: #367fa9; margin-bottom: 15px;"),
      passwordInput("senha_login", NULL, placeholder = "Digite a senha"),
      actionButton("btn_login", "Entrar", icon = icon("sign-in-alt"),
                   class = "btn-primary", style = "width: 100%;"),
      uiOutput("login_erro")
  )
)

# --- Conteúdo do dashboard (o que já existia) ---
dashboard_ui <- dashboardPage(
  dashboardHeader(
    title = "Dashboard SIASI",
    tags$li(class = "dropdown",
            actionButton("btn_logout", "Sair", icon = icon("sign-out-alt"),
                         style = "margin: 8px; background-color: #dd4b39; color: white; border: none;"))
  ),
  
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
        
        /* Estilos personalizados para os cards de registros */
        .registro-card {
          background: #ffffff;
          border-radius: 12px;
          padding: 20px 18px;
          text-align: center;
          box-shadow: 0 3px 12px rgba(0,0,0,0.1);
          border-top: 4px solid #e74c3c;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .registro-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 5px 18px rgba(0,0,0,0.15);
        }
        .registro-card .card-icon {
          font-size: 36px;
          color: #e74c3c;
          margin-bottom: 8px;
        }
        .registro-card .card-label {
          font-size: 13px;
          color: #555;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-bottom: 6px;
        }
        .registro-card .card-valor {
          font-size: 28px;
          font-weight: 700;
          color: #2c3e50;
          margin-bottom: 4px;
        }
        .registro-card .card-percentual {
          font-size: 15px;
          color: #e74c3c;
          font-weight: 600;
        }
      "))
    ),
    
    tabItems(
      # ============================================
      # ABA: REGISTROS GERAIS
      # ============================================
      tabItem(tabName = "registros",
              # --- CARDS COM OS REGISTROS DESTACADOS ---
              fluidRow(
                htmlOutput("cards_registros")
              ),
              # --- TABELA COM OS DEMAIS REGISTROS ---
              fluidRow(
                box(title = "Outros Registros da Base de Dados", status = "primary", solidHeader = TRUE, width = 12,
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

# --- UI final: alterna entre login e dashboard via server ---
ui <- uiOutput("pagina_principal")

# ============================================
# SERVER DO SHINY
# ============================================

server <- function(input, output, session) {
  
  # ============================================
  # AUTENTICAÇÃO
  # ============================================
  autenticado <- reactiveVal(FALSE)
  
  output$pagina_principal <- renderUI({
    if (autenticado()) {
      dashboard_ui
    } else {
      login_ui
    }
  })
  
  observeEvent(input$btn_login, {
    if (!is.null(input$senha_login) && input$senha_login == SENHA_DASHBOARD) {
      autenticado(TRUE)
      output$login_erro <- renderUI(NULL)
    } else {
      output$login_erro <- renderUI({
        div(class = "login-erro", "Senha incorreta. Tente novamente.")
      })
    }
  })
  
  observeEvent(input$btn_logout, {
    autenticado(FALSE)
    updateTextInput(session, "senha_login", value = "")
  })
  
  # ============================================
  # REGISTROS GERAIS — CARDS
  # ============================================
  
  output$cards_registros <- renderUI({
    # Mapeamento: categoria -> ícone FontAwesome
    # Nota: "Total original" do CSV será exibido como "Total Deduplicado"
    icone_map <- list(
      "Total original"                = "filter",
      "Total aldeados"                = "map-marker-alt",
      "Somente ativos (aldeados)"     = "clipboard-check",
      "Somente indígenas (aldeados)"  = "feather-alt",
      "Ativos e indígenas (aldeados)" = "users"
    )
    
    # Primeiro card: Total Original (valor externo)
    card_total_original <- {
      valor <- format(total_original_externo, big.mark = ".", decimal.mark = ",")
      percent_dedup <- sprintf("%.2f", 100 * registros$frequencias[registros$categorias == "Total original"] / total_original_externo)
      
      div(class = "registro-card",
          div(class = "card-icon", tags$i(class = "fas fa-database")),
          div(class = "card-label", "Total Original"),
          div(class = "card-valor", paste0("N = ", valor)),
          div(class = "card-percentual", paste0("Base (100%)") )
      )
    }
    
    # Cards restantes: do CSV
    cards_csv <- registros %>%
      filter(categorias %in% categorias_cards) %>%
      pull(categorias) %>%
      lapply(function(cat) {
        linha <- registros %>% filter(categorias == cat)
        valor <- format(linha$frequencias, big.mark = ".", decimal.mark = ",")
        
        # Se for "Total original", renomear para "Total Deduplicado"
        label <- ifelse(cat == "Total original", "Total Deduplicado", cat)
        icone <- icone_map[[cat]] %||% "circle"
        
        # Percentual: do CSV (baseado no total deduplicado)
        percent <- sprintf("%.2f", linha$percentual_total_original)
        
        div(class = "registro-card",
            div(class = "card-icon", tags$i(class = paste0("fas fa-", icone))),
            div(class = "card-label", label),
            div(class = "card-valor", paste0("N = ", valor)),
            div(class = "card-percentual", paste0(percent, "%"))
        )
      })
    
    # Combina: Total Original primeiro, depois os demais
    cards <- c(list(card_total_original), cards_csv)
    
    # Montar a grid: 5 cards em uma linha responsiva
    tagList(
      div(
        style = "display: flex; flex-wrap: wrap; gap: 16px; justify-content: center; padding: 10px 0;",
        cards
      )
    )
  })
  
  # ============================================
  # REGISTROS GERAIS — TABELA (sem os cards)
  # ============================================
  output$registros_table <- renderDT({
    datatable(registros_tabela,
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE,
        paging = FALSE
      ),
      extensions = c('Buttons'),
      rownames = FALSE,
      colnames = c("Categorias", "Frequências", "% do Total Original")
    ) %>%
      formatRound(columns = 2, digits = 0, mark = ".") %>%
      formatRound(columns = 3, digits = 2, mark = ".") %>%
      formatStyle(
        "categorias",
        target = "row",
        backgroundColor = styleEqual(categorias_aldeados[!categorias_aldeados %in% categorias_cards],
                                      rep("#eaf3ff", sum(!categorias_aldeados %in% categorias_cards)))
      )
  })
  
  # ============================================
  # NASCIMENTOS
  # ============================================
  
  output$nascimentos_ano_table <- renderDT({
    datatable(anonasc_table,
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
      formatStyle(1, target = 'row', backgroundColor = styleEqual(c("Sem informação", "< 2000"), c('#ffcccc', '#ffcccc')))
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
             xaxis = list(
               title = "Ano",
               tickangle = 45,
               tickvals = anonasc_numerico$ano_categoria,
               ticktext = as.character(as.integer(anonasc_numerico$ano_categoria)),
               tickfont = list(size = 9)
             ),
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
    
    # Consolidar anos < 2000 em "< 2000" e ordenar corretamente
    nascimentos_dsei_resumido <- nascimentos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      mutate(ano_nascimento = ifelse(ano_num < 2000, "< 2000", ano_nascimento)) %>%
      group_by(ds_dsei, ano_nascimento) %>%
      summarise(
        frequencia_ativos_indigenas = sum(frequencia_ativos_indigenas, na.rm = TRUE),
        frequencia_ativos = sum(frequencia_ativos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        diferenca_nao_indigenas = frequencia_ativos - frequencia_ativos_indigenas,
        ordem = ifelse(ano_nascimento == "< 2000", 0, as.numeric(ano_nascimento))
      ) %>%
      group_by(ds_dsei) %>%
      mutate(
        perc_total_ativ_indig = round(100 * frequencia_ativos_indigenas / sum(frequencia_ativos_indigenas, na.rm = TRUE), 2),
        perc_total_ativos = round(100 * frequencia_ativos / sum(frequencia_ativos, na.rm = TRUE), 2)
      ) %>%
      ungroup() %>%
      arrange(ordem) %>%
      mutate(
        crescimento_abs_ativos = frequencia_ativos - lag(frequencia_ativos),
        crescimento_abs_ativ_indig = frequencia_ativos_indigenas - lag(frequencia_ativos_indigenas),
        crescimento_perc_ativos = round((frequencia_ativos - lag(frequencia_ativos)) / lag(frequencia_ativos) * 100, 2),
        crescimento_perc_ativ_indig = round((frequencia_ativos_indigenas - lag(frequencia_ativos_indigenas)) / lag(frequencia_ativos_indigenas) * 100, 2)
      ) %>%
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
      formatRound(columns = c(6:9), digits = 2) %>%
      formatStyle(2, target = 'row', backgroundColor = styleEqual("< 2000", '#ffcccc'))
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
