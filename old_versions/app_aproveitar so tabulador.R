# ============================================
# DASHBOARD SIASI - VERSÃO 6 (CARDS + REGISTROS + LOGIN)
# ============================================

suppressWarnings({
  library(tidyverse)
  library(tidyr)
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
anonasc <- anonasc %>%
  mutate(
    ano_categoria = as.character(ano_categoria),
    ano_categoria = ifelse(ano_categoria == "Antes de 2000", "< 2000", ano_categoria)
  )

anonasc_calc <- anonasc %>%
  mutate(
    ano_categoria = as.character(ano_categoria),
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2)
  )

# Tabela filtrada: remove "Sem informação"
anonasc_table <- anonasc_calc %>%
  filter(ano_categoria != "Sem informação")

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
names(obitos_ano) <- c("ano_obito", "somente_ativos", "ativos_e_indigenas")
obitos_ano$ano_obito <- as.character(obitos_ano$ano_obito)
obitos_ano$ano_obito <- ifelse(obitos_ano$ano_obito == "Antes de 2000", "< 2000", obitos_ano$ano_obito)
obitos_ano$somente_ativos <- as.numeric(obitos_ano$somente_ativos)
obitos_ano$ativos_e_indigenas <- as.numeric(obitos_ano$ativos_e_indigenas)

obitos_ano_calc <- obitos_ano %>%
  mutate(
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2)
  )

# Tabela filtrada: remove "Sem informação"
obitos_ano_table <- obitos_ano_calc %>%
  filter(ano_obito != "Sem informação")

# Dados de Óbitos por DSEI
obitos_dsei <- read.csv("frequencia_ano_obitos_dsei.csv", sep=";", stringsAsFactors = FALSE)
# Selecionar apenas as 5 primeiras colunas necessárias
obitos_dsei <- obitos_dsei %>%
  select(1:5) %>%
  setNames(c("ds_dsei", "co_dsei_polo", "ano_obito", "somente_ativos", "ativos_e_indigenas"))

obitos_dsei_calc <- obitos_dsei %>%
  mutate(
    somente_ativos = as.numeric(somente_ativos),
    ativos_e_indigenas = as.numeric(ativos_e_indigenas),
    diferenca_nao_indigenas = somente_ativos - ativos_e_indigenas,
    ano_num = as.numeric(as.character(ano_obito))
  ) %>%
  group_by(ds_dsei, co_dsei_polo) %>%
  mutate(
    perc_total_ativos = round(100 * somente_ativos / sum(somente_ativos, na.rm = TRUE), 2),
    perc_total_ativ_indig = round(100 * ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE), 2)
  ) %>%
  ungroup() %>%
  arrange(ds_dsei, co_dsei_polo, ano_num) %>%
  group_by(ds_dsei, co_dsei_polo) %>%
  mutate(
    crescimento_abs_ativos = somente_ativos - lag(somente_ativos),
    crescimento_abs_ativ_indig = ativos_e_indigenas - lag(ativos_e_indigenas),
    crescimento_perc_ativos = round((somente_ativos - lag(somente_ativos)) / lag(somente_ativos) * 100, 2),
    crescimento_perc_ativ_indig = round((ativos_e_indigenas - lag(ativos_e_indigenas)) / lag(ativos_e_indigenas) * 100, 2)
  ) %>%
  ungroup()

# Dados de População por Ano
populacao <- read.csv("populacao2_por_ano.csv", sep=";", stringsAsFactors = FALSE)
names(populacao) <- c("ano", "ativos_e_indigenas", "somente_ativos")

populacao_calc <- populacao %>%
  mutate(
    ativos_e_indigenas = as.numeric(ativos_e_indigenas),
    somente_ativos = as.numeric(somente_ativos),
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2),
    crescimento_abs_ativos_indigenas = ativos_e_indigenas - lag(ativos_e_indigenas),
    crescimento_abs_somente_ativos = somente_ativos - lag(somente_ativos),
    crescimento_perc_ativos_indigenas = round((ativos_e_indigenas - lag(ativos_e_indigenas)) / lag(ativos_e_indigenas) * 100, 2),
    crescimento_perc_somente_ativos = round((somente_ativos - lag(somente_ativos)) / lag(somente_ativos) * 100, 2)
  )

# Dados de População por DSEI
populacao_dsei <- read.csv("populacao2_por_ano_dsei.csv", sep=";", stringsAsFactors = FALSE)
names(populacao_dsei) <- c("ds_dsei_aldeia", "co_seq_dsei", "ano", "ativos_e_indigenas", "somente_ativos")
populacao_dsei$ativos_e_indigenas <- as.numeric(populacao_dsei$ativos_e_indigenas)
populacao_dsei$somente_ativos <- as.numeric(populacao_dsei$somente_ativos)

# Dados do Tabulador (população por faixa etária)
tabulador_dados <- read.csv("tabela2000_2025.csv", sep=";", stringsAsFactors = FALSE)

# Definir ordem de idade_cat
idade_ordem <- c(
  "Até 3 meses",
  "De 3 meses a 6 meses",
  "De 6 meses a 1 ano",
  "1 a 4 anos",
  "5 a 9 anos",
  "10 a 14 anos",
  "15 a 19 anos",
  "20 a 24 anos",
  "25 a 29 anos",
  "30 a 34 anos",
  "35 a 39 anos",
  "40 a 44 anos",
  "45 a 49 anos",
  "50 a 54 anos",
  "55 a 59 anos",
  "60 a 64 anos",
  "65 a 69 anos",
  "70 a 74 anos",
  "75 a 79 anos",
  "80 anos ou mais"
)

# Factorizar idade_cat com a ordem correta
tabulador_dados$idade_cat <- factor(tabulador_dados$idade_cat, levels = idade_ordem, ordered = TRUE)

# Variáveis do tabulador (excluindo co_dsei_aldeia)
vars_tabulador <- c("ano", "idade_cat", "tp_sexo", "st_indigena", "ds_dsei_aldeia")

# Função para gerar tabela cruzada
gerar_crosstab <- function(dados, linha_var, coluna_var, tipo_valor = "abs") {
  
  # Preparar dados
  dados <- dados %>%
    mutate(
      !!linha_var := as.character(!!sym(linha_var)),
      !!coluna_var := as.character(!!sym(coluna_var))
    )
  
  # Agrupar e contar
  tab <- dados %>%
    group_by(!!sym(linha_var), !!sym(coluna_var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = coluna_var, values_from = n, values_fill = 0)
  
  # Renomear primeira coluna
  names(tab)[1] <- linha_var
  
  # Calcular totais por linha
  valor_cols <- setdiff(names(tab), linha_var)
  tab <- tab %>%
    mutate(Total = rowSums(across(all_of(valor_cols))))
  
  # Adicionar linha de total
  total_linha <- tab %>%
    summarise(across(all_of(valor_cols), sum), .groups = "drop") %>%
    mutate(!!linha_var := "Total", .before = 1)
  
  tab_completa <- bind_rows(tab, total_linha)
  valor_cols_com_total <- c(valor_cols, "Total")
  
  if (tipo_valor == "pct_linha") {
    tab_completa <- tab_completa %>%
      mutate(across(all_of(valor_cols_com_total), ~ round(.x / Total * 100, 1)))
  } else if (tipo_valor == "pct_coluna") {
    totais_coluna <- tab_completa[tab_completa[[linha_var]] == "Total",
                                   valor_cols_com_total] %>% unlist()
    tab_completa <- tab_completa %>%
      mutate(across(all_of(valor_cols_com_total),
                    ~ round(.x / totais_coluna[cur_column()] * 100, 1)))
  } else if (tipo_valor == "pct_total") {
    grande_total <- tab_completa$Total[tab_completa[[linha_var]] == "Total"]
    tab_completa <- tab_completa %>%
      mutate(across(all_of(valor_cols_com_total), ~ round(.x / grande_total * 100, 1)))
  }
  
  # Reordenar as colunas finais conforme a ordem definida
  colunas_finais <- c(linha_var, valor_cols_com_total)
  tab_completa <- tab_completa[, colunas_finais]
  
  tab_completa
}

populacao_dsei_calc <- populacao_dsei %>%
  arrange(ds_dsei_aldeia, ano) %>%
  group_by(ds_dsei_aldeia) %>%
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
      menuItem("População", tabName = "populacao", icon = icon("users")),
      menuItem("Tabulador", tabName = "tabulador", icon = icon("table"))
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
        
        /* Estilos para filtros */
        .filtro-container {
          margin-bottom: 18px;
          padding: 12px;
          background: #f9f9f9;
          border-radius: 5px;
          border-left: 4px solid #2196F3;
        }
        .filtro-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 10px;
        }
        .filtro-header h5 {
          margin: 0;
          font-weight: bold;
          color: #333;
          font-size: 14px;
        }
        .filtro-buttons {
          display: flex;
          gap: 5px;
        }
        .filtro-buttons .btn {
          padding: 4px 8px;
          font-size: 11px;
        }
        /* Grid para checkboxes */
        .filtro-checkboxes {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
          gap: 8px;
          padding: 8px 0;
        }
        .filtro-checkboxes .checkbox {
          margin: 0;
          padding: 3px 0;
        }
        .filtro-checkboxes .checkbox label {
          font-size: 12px;
          margin-bottom: 0;
          font-weight: 400;
        }
        .filtro-checkboxes .checkbox input[type='checkbox'] {
          margin-right: 5px;
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
                box(title = "Detalhamento de Registros", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("tabela_registros"))
              )
      ),
      
      # ============================================
      # ABA: NASCIMENTOS
      # ============================================
      tabItem(tabName = "nascimentos",
              fluidRow(
                box(title = "Nascimentos por Ano", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("nascimentos_plot"))
              ),
              fluidRow(
                box(title = "Tabela de Nascimentos por Ano", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("nascimentos_table"))
              ),
              fluidRow(
                box(title = "Nascimentos por DSEI", status = "primary", solidHeader = TRUE, width = 12,
                    column(3, selectInput("nascimentos_dsei_select", "Selecione um DSEI", choices = c(""))),
                    column(9, plotlyOutput("nascimentos_dsei_plot"))
                )
              ),
              fluidRow(
                box(title = "Tabela de Nascimentos por DSEI", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("nascimentos_dsei_table"))
              )
      ),
      
      # ============================================
      # ABA: ÓBITOS
      # ============================================
      tabItem(tabName = "obitos",
              fluidRow(
                box(title = "Óbitos por Ano", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("obitos_plot"))
              ),
              fluidRow(
                box(title = "Tabela de Óbitos por Ano", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("obitos_table"))
              ),
              fluidRow(
                box(title = "Óbitos por DSEI", status = "primary", solidHeader = TRUE, width = 12,
                    column(3, selectInput("obitos_dsei_select", "Selecione um DSEI", choices = c(""))),
                    column(9, plotlyOutput("obitos_dsei_plot"))
                )
              ),
              fluidRow(
                box(title = "Tabela de Óbitos por DSEI", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("obitos_dsei_table"))
              )
      ),
      
      # ============================================
      # ABA: POPULAÇÃO
      # ============================================
      tabItem(tabName = "populacao",
              fluidRow(
                box(title = "População por Ano", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("populacao_plot"))
              ),
              fluidRow(
                box(title = "Tabela de População por Ano", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("populacao_table"))
              ),
              fluidRow(
                box(title = "População por DSEI", status = "primary", solidHeader = TRUE, width = 12,
                    column(3, selectInput("populacao_dsei_select", "Selecione um DSEI", choices = c(""))),
                    column(9, plotlyOutput("populacao_dsei_plot"))
                )
              ),
              fluidRow(
                box(title = "Tabela de População por DSEI", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("populacao_dsei_table"))
              )
      ),
      
      # ============================================
      # ABA: TABULADOR
      # ============================================
      tabItem(tabName = "tabulador",
              fluidRow(
                box(title = "Configuração do Tabulador", status = "primary", solidHeader = TRUE, width = 12,
                    column(3, selectInput("tab_linha", "Variável de Linha", 
                                         choices = c("ano", "idade_cat", "tp_sexo", "st_indigena", "ds_dsei_aldeia"), 
                                         selected = "ds_dsei_aldeia")),
                    column(3, selectInput("tab_coluna", "Variável de Coluna", 
                                         choices = c("ano", "idade_cat", "tp_sexo", "st_indigena", "ds_dsei_aldeia"), 
                                         selected = "idade_cat")),
                    column(3, selectInput("tab_estrato", "Estratificação (opcional)", 
                                         choices = c("Nenhuma" = "nenhuma"), selected = "nenhuma")),
                    column(3, radioButtons("tab_tipo_valor", "Conteúdo",
                                         choices = c("Frequência" = "abs", 
                                                    "% Linha" = "pct_linha",
                                                    "% Coluna" = "pct_coluna",
                                                    "% Total" = "pct_total"),
                                         selected = "abs", inline = TRUE))
                )
              ),
              fluidRow(
                box(title = "Filtros", status = "info", solidHeader = TRUE, width = 12,
                    uiOutput("tab_filtros_ui"))
              ),
              fluidRow(
                box(title = "Tabelas Cruzadas", status = "success", solidHeader = TRUE, width = 12,
                    downloadButton("tab_baixar_csv", "Baixar CSV"),
                    hr(),
                    uiOutput("tab_conteudo_tabelas"))
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
    } else {
      output$login_erro <- renderUI({
        div(class = "login-erro", "Senha incorreta!")
      })
    }
  })
  
  observeEvent(input$btn_logout, {
    autenticado(FALSE)
    output$login_erro <- renderUI({})
  })
  
  # ============================================
  # REGISTROS GERAIS
  # ============================================
  
  output$cards_registros <- renderUI({
    cards_html <- lapply(categorias_cards, function(cat) {
      valor <- registros %>% filter(categorias == cat) %>% pull(frequencias)
      percentual <- registros %>% filter(categorias == cat) %>% pull(percentual_total_original)
      
      div(class = "col-md-2",
          div(class = "registro-card",
              div(class = "card-icon", icon("users")),
              div(class = "card-label", cat),
              div(class = "card-valor", format(valor, big.mark = ".", decimal.mark = ",")),
              div(class = "card-percentual", paste0(round(percentual, 2), "%"))
          )
      )
    })
    
    do.call(fluidRow, cards_html)
  })
  
  output$tabela_registros <- renderDT({
    datatable(registros_tabela,
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
              colnames = c("Categoria", "Frequência", "% Total Original")
    ) %>%
      formatCurrency(columns = 2, currency = "", interval = 3, mark = ".", digits = 0)
  })
  
  # ============================================
  # NASCIMENTOS
  # ============================================
  
  output$nascimentos_plot <- renderPlotly({
    plot_ly(anonasc_table) %>%
      add_trace(x = ~ano_categoria, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano_categoria, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = "Nascimentos por Ano",
             xaxis = list(title = "Ano"),
             yaxis = list(title = "Nascimentos", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  output$nascimentos_table <- renderDT({
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
              colnames = c("Ano", "Ativos e Indígenas", "Somente Ativos", "Diferença", "% Ativos Indig.", "% Somente Ativos")
    ) %>%
      formatRound(columns = c(2:5), digits = 0, mark = ".") %>%
      formatRound(columns = c(6:7), digits = 2)
  })
  
  observe({
    dsei_choices <- sort(unique(nascimentos_dsei_calc$ds_dsei))
    updateSelectInput(session, "nascimentos_dsei_select", choices = dsei_choices, selected = dsei_choices[1])
  })
  
  output$nascimentos_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$nascimentos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- nascimentos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado)
    
    if (nrow(dados) == 0) {
      return(plotly_empty() %>% layout(title = "Sem dados disponíveis"))
    }
    
    plot_ly(dados) %>%
      add_trace(x = ~ano_nascimento, y = ~frequencia_ativos_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano_nascimento, y = ~frequencia_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = paste("Nascimentos -", dsei_selecionado),
             xaxis = list(title = "Ano", dtick = 1),
             yaxis = list(title = "Nascimentos", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  output$nascimentos_dsei_table <- renderDT({
    dsei_selecionado <- input$nascimentos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(datatable(data.frame()))
    }
    
    dados <- nascimentos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      select(ds_dsei, co_dsei_polo, ano_nascimento, frequencia_ativos_indigenas, frequencia_ativos,
             diferenca_nao_indigenas, perc_total_ativ_indig, perc_total_ativos)
    
    datatable(dados,
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
              colnames = c("DSEI", "Código", "Ano", "Ativ. Indig.", "Só Ativos", "Diferença", "Cresc. % Indig.", "Cresc. % Ativos")
    ) %>%
      formatRound(columns = c(4:6), digits = 0, mark = ".") %>%
      formatRound(columns = c(7:8), digits = 2)
  })
  
  # ============================================
  # ÓBITOS
  # ============================================
  
  output$obitos_plot <- renderPlotly({
    plot_ly(obitos_ano_table) %>%
      add_trace(x = ~ano_obito, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano_obito, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = "Óbitos por Ano",
             xaxis = list(title = "Ano"),
             yaxis = list(title = "Óbitos", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  output$obitos_table <- renderDT({
    datatable(obitos_ano_table,
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
              colnames = c("Ano", "Ativos e Indígenas", "Somente Ativos", "Diferença", "% Ativos Indig.", "% Somente Ativos")
    ) %>%
      formatRound(columns = c(2:5), digits = 0, mark = ".") %>%
      formatRound(columns = c(6:7), digits = 2)
  })
  
  observe({
    dsei_choices <- sort(unique(obitos_dsei_calc$ds_dsei))
    updateSelectInput(session, "obitos_dsei_select", choices = dsei_choices, selected = dsei_choices[1])
  })
  
  output$obitos_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$obitos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- obitos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado)
    
    if (nrow(dados) == 0) {
      return(plotly_empty() %>% layout(title = "Sem dados disponíveis"))
    }
    
    plot_ly(dados) %>%
      add_trace(x = ~ano_obito, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano_obito, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = paste("Óbitos -", dsei_selecionado),
             xaxis = list(title = "Ano", dtick = 1),
             yaxis = list(title = "Óbitos", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  output$obitos_dsei_table <- renderDT({
    dsei_selecionado <- input$obitos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(datatable(data.frame()))
    }
    
    dados <- obitos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      select(ds_dsei, co_dsei_polo, ano_obito, ativos_e_indigenas, somente_ativos,
             diferenca_nao_indigenas, perc_total_ativ_indig, perc_total_ativos)
    
    datatable(dados,
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
              colnames = c("DSEI", "Código", "Ano", "Ativ. Indig.", "Só Ativos", "Diferença", "Cresc. % Indig.", "Cresc. % Ativos")
    ) %>%
      formatRound(columns = c(4:6), digits = 0, mark = ".") %>%
      formatRound(columns = c(7:8), digits = 2)
  })
  
  # ============================================
  # POPULAÇÃO
  # ============================================
  
  output$populacao_plot <- renderPlotly({
    plot_ly(populacao_calc) %>%
      add_trace(x = ~ano, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = "População por Ano",
             xaxis = list(title = "Ano", dtick = 1),
             yaxis = list(title = "População", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
  })
  
  output$populacao_table <- renderDT({
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
              colnames = c("Ano", "Ativos e Indígenas", "Somente Ativos", "Diferença", "% Ativos Indig.", "% Somente Ativos", "Cresc. Abs. Indig.", "Cresc. Abs. Ativos", "Cresc. % Indig.", "Cresc. % Ativos")
    ) %>%
      formatRound(columns = c(2:5), digits = 0, mark = ".") %>%
      formatRound(columns = c(6:10), digits = 2)
  })
  
  observe({
    dsei_choices <- sort(unique(populacao_dsei_calc$ds_dsei_aldeia))
    updateSelectInput(session, "populacao_dsei_select", choices = dsei_choices, selected = dsei_choices[1])
  })
  
  output$populacao_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$populacao_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- populacao_dsei_calc %>%
      filter(ds_dsei_aldeia == dsei_selecionado)
    
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
  
  output$populacao_dsei_table <- renderDT({
    dsei_selecionado <- input$populacao_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(datatable(data.frame()))
    }
    
    dados <- populacao_dsei_calc %>%
      filter(ds_dsei_aldeia == dsei_selecionado) %>%
      select(ds_dsei_aldeia, ano, ativos_e_indigenas, somente_ativos,
             diferenca, perc_col_ativos_indigenas, perc_col_somente_ativos,
             crescimento_abs_ativos_indigenas, crescimento_abs_somente_ativos,
             crescimento_perc_ativos_indigenas, crescimento_perc_somente_ativos)
    
    datatable(dados,
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
  
  # ============================================
  # TABULADOR
  # ============================================
  
  # Impede escolher a mesma variável em linha e coluna
  observeEvent(input$tab_linha, {
    escolhas_coluna <- setdiff(vars_tabulador, input$tab_linha)
    atual <- isolate(input$tab_coluna)
    selecionado <- if (atual %in% escolhas_coluna) atual else escolhas_coluna[1]
    updateSelectInput(session, "tab_coluna", choices = escolhas_coluna, selected = selecionado)
    
    # Atualizar opções de estrato
    outras <- setdiff(vars_tabulador, c(input$tab_linha, input$tab_coluna))
    escolhas_estrato <- c("Nenhuma" = "nenhuma", setNames(outras, outras))
    updateSelectInput(session, "tab_estrato", choices = escolhas_estrato, selected = "nenhuma")
  })
  
  # Painel de filtros dinâmico com checkboxGroupInput em grid
  output$tab_filtros_ui <- renderUI({
    tagList(
      lapply(vars_tabulador, function(v) {
        # Ordenar choices corretamente para idade_cat
        if (v == "idade_cat") {
          choices_list <- intersect(idade_ordem, unique(tabulador_dados[[v]]))
        } else {
          choices_list <- sort(unique(tabulador_dados[[v]]))
        }
        
        tagList(
          div(
            class = "filtro-container",
            div(
              class = "filtro-header",
              h5(v),
              div(
                class = "filtro-buttons",
                actionButton(paste0("btn_all_", v), "Selecionar Tudo", class = "btn-sm btn-success"),
                actionButton(paste0("btn_none_", v), "Remover Tudo", class = "btn-sm btn-danger")
              )
            ),
            div(
              class = "filtro-checkboxes",
              checkboxGroupInput(
                inputId = paste0("tab_filtro_", v),
                label = NULL,
                choices = choices_list,
                selected = choices_list,
                inline = TRUE
              )
            )
          )
        )
      })
    )
  })
  
  # Botões "Selecionar Tudo" e "Remover Tudo" para cada variável
  lapply(vars_tabulador, function(v) {
    observeEvent(input[[paste0("btn_all_", v)]], {
      if (v == "idade_cat") {
        choices_list <- intersect(idade_ordem, unique(tabulador_dados[[v]]))
      } else {
        choices_list <- sort(unique(tabulador_dados[[v]]))
      }
      updateCheckboxGroupInput(session, paste0("tab_filtro_", v), selected = choices_list)
    })
    
    observeEvent(input[[paste0("btn_none_", v)]], {
      updateCheckboxGroupInput(session, paste0("tab_filtro_", v), selected = character(0))
    })
  })
  
  # Dados filtrados
  tab_dados_filtrados <- reactive({
    df <- tabulador_dados
    for (v in vars_tabulador) {
      sel <- input[[paste0("tab_filtro_", v)]]
      req(sel)
      df <- df[df[[v]] %in% sel, ]
    }
    df
  })
  
  # Tabela única reativa
  tab_tabela_unica_reativa <- reactive({
    req(input$tab_linha, input$tab_coluna, input$tab_linha != input$tab_coluna)
    gerar_crosstab(tab_dados_filtrados(), input$tab_linha, input$tab_coluna, input$tab_tipo_valor)
  })
  
  output$tab_tabela_unica <- renderDT({
    tab <- tab_tabela_unica_reativa()
    datatable(tab,
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/pt-BR.json'),
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollY = "400px",
                scrollCollapse = TRUE,
                scrollX = TRUE,
                paging = FALSE
              ),
              extensions = c('Buttons', 'Scroller'),
              rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tab), digits = 1)
  })
  
  output$tab_conteudo_tabelas <- renderUI({
    tagList(
      DTOutput("tab_tabela_unica")
    )
  })
  
  # Download CSV
  output$tab_baixar_csv <- downloadHandler(
    filename = function() {
      paste0("tabulador_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      tab <- tab_tabela_unica_reativa()
      write.csv2(tab, file, row.names = FALSE)
    }
  )
}

# ============================================
# EXECUTAR APP
# ============================================

shinyApp(ui = ui, server = server)
