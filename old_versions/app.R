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
 library(shinyWidgets)
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
    ano_categoria = ifelse(ano_categoria == "Até 2000", "< 2000", ano_categoria)
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

# ============================================
# DADOS DE ÓBITOS POR ANO (ADAPTADO)
# ============================================
obitos_ano <- read.csv("frequencia_ano_obitos.csv", sep=";", stringsAsFactors = FALSE)
names(obitos_ano) <- c("ano_categoria", "somente_ativos", "ativos_e_indigenas")

obitos_ano_calc <- obitos_ano %>%
  mutate(
    ano_categoria = as.character(ano_categoria),
    somente_ativos = as.numeric(gsub(",", ".", gsub("\\.", "", somente_ativos))),
    ativos_e_indigenas = as.numeric(gsub(",", ".", gsub("\\.", "", ativos_e_indigenas)))
  ) %>%
  filter(!is.na(somente_ativos), !is.na(ativos_e_indigenas)) %>%
  mutate(
    diferenca = somente_ativos - ativos_e_indigenas,
    perc_col_ativos_e_indigenas = round((ativos_e_indigenas / sum(ativos_e_indigenas, na.rm = TRUE)) * 100, 2),
    perc_col_somente_ativos = round((somente_ativos / sum(somente_ativos, na.rm = TRUE)) * 100, 2)
  )

# Tabela filtrada: remove "Sem informação" e substitui "Antes de 2000" por "< 2000"
obitos_ano_table <- obitos_ano_calc %>%
  filter(ano_categoria != "Sem informação") %>%
  mutate(ano_categoria = ifelse(ano_categoria == "Antes de 2000", "< 2000", ano_categoria))

# ============================================
# DADOS DE ÓBITOS POR DSEI (ADAPTADO)
# ============================================
obitos_dsei <- read.csv("frequencia_ano_obitos_dsei.csv", sep=";", stringsAsFactors = FALSE)
names(obitos_dsei) <- c("ds_dsei", "co_dsei_polo", "ano_obito",
                        "frequencia_ativos", "frequencia_indigenas",
                        "percentual_ativos", "frequencia_acumulada_ativos", "percentual_acumulado_ativos")

obitos_dsei_clean <- obitos_dsei %>%
  mutate(
    frequencia_ativos = as.numeric(gsub(",", ".", gsub("\\.", "", frequencia_ativos))),
    frequencia_indigenas = as.numeric(gsub(",", ".", gsub("\\.", "", frequencia_indigenas))),
    ano_num = if_else(ano_obito == "Antes de 2000", 0, suppressWarnings(as.numeric(ano_obito)))
  ) %>%
  filter(!is.na(frequencia_ativos), !is.na(frequencia_indigenas), !is.na(ds_dsei), ds_dsei != "")

obitos_dsei_calc <- obitos_dsei_clean %>%
  filter(!is.na(ano_num)) %>%
  arrange(ds_dsei, co_dsei_polo, ano_num) %>%
  group_by(ds_dsei, co_dsei_polo) %>%
  mutate(
    diferenca_nao_indigenas = frequencia_ativos - frequencia_indigenas,
    perc_total_ativos = round(100 * frequencia_ativos / sum(frequencia_ativos, na.rm = TRUE), 2),
    perc_total_indigenas = round(100 * frequencia_indigenas / sum(frequencia_indigenas, na.rm = TRUE), 2),
    crescimento_abs_ativos = frequencia_ativos - lag(frequencia_ativos),
    crescimento_abs_indigenas = frequencia_indigenas - lag(frequencia_indigenas),
    crescimento_perc_ativos = round((frequencia_ativos - lag(frequencia_ativos)) / lag(frequencia_ativos) * 100, 2),
    crescimento_perc_indigenas = round((frequencia_indigenas - lag(frequencia_indigenas)) / lag(frequencia_indigenas) * 100, 2)
  ) %>%
  ungroup()

# Dados de População por Ano
populacao <- read.csv("populacao2_por_ano.csv", sep=";")
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
populacao_dsei <- read.csv("populacao2_por_ano_dsei.csv", sep=";")
names(populacao_dsei) <- c("ds_dsei", "co_seq_dsei", "ano", "ativos_e_indigenas", "somente_ativos")

# ============================================
# CARREGAR DADOS DO TABULADOR
# ============================================
tabulador_dados <- read.csv("tabela2000_2025.csv", sep=";", stringsAsFactors = FALSE, encoding = "UTF-8")
tabulador_dados$frequencia <- as.numeric(tabulador_dados$frequencia)
tabulador_dados <- tabulador_dados[!is.na(tabulador_dados$frequencia), ]

# Converter ano para character para compatibilidade com outras variáveis
tabulador_dados$ano <- as.character(tabulador_dados$ano)

# Definir ordem correta de idade_cat
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
tabulador_dados$idade_cat <- factor(tabulador_dados$idade_cat, levels = idade_ordem, ordered = TRUE)

vars_tabulador <- c("ano", "idade_cat", "tp_sexo", "st_indigena", "ds_dsei_aldeia")

# Função auxiliar para gerar tabela cruzada
gerar_crosstab <- function(df, linha_var, coluna_var, tipo_valor) {
  # Converter coluna de linha para character se for factor
  if (is.factor(df[[linha_var]])) {
    df[[linha_var]] <- as.character(df[[linha_var]])
  }
  if (is.factor(df[[coluna_var]])) {
    df[[coluna_var]] <- as.character(df[[coluna_var]])
  }
  
  tab <- df %>%
    group_by(.data[[linha_var]], .data[[coluna_var]]) %>%
    summarise(freq = sum(frequencia), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = all_of(coluna_var),
                       values_from = freq, values_fill = 0)
  
  valor_cols <- setdiff(names(tab), linha_var)
  
  # Se a coluna_var eh idade_cat, reordenar as colunas conforme a ordem cronologica
  if (coluna_var == "idade_cat") {
    # Manter apenas as colunas que existem na tabela
    valor_cols_ordenado <- intersect(idade_ordem, valor_cols)
    # Adicionar qualquer coluna que nao esteja em idade_ordem (por seguranca)
    valor_cols_ordenado <- c(valor_cols_ordenado, setdiff(valor_cols, idade_ordem))
    valor_cols <- valor_cols_ordenado
  } else {
    valor_cols <- sort(valor_cols)
  }
  
  tab$Total <- rowSums(tab[valor_cols])
  
  total_linha <- tab %>%
    summarise(across(all_of(c(valor_cols, "Total")), sum)) %>%
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
      # ABA: ÓBITOS (ADAPTADA)
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
    # Manter a coluna como character para exibição correta de "< 2000"
    tabela_display <- anonasc_table %>%
      mutate(ano_categoria = as.character(ano_categoria))
    
    datatable(tabela_display,
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
      formatStyle(1, target = 'row', backgroundColor = styleEqual("< 2000", '#ffcccc'))
  })
  
  output$nascimentos_ano_plot <- renderPlotly({
    anonasc_numerico <- anonasc %>%
      filter(ano_categoria != "< 2000") %>%
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
  # ÓBITOS (ADAPTADO)
  # ============================================
  
  output$obitos_ano_table <- renderDT({
    # Reordenar colunas: Somente Ativos primeiro, depois Ativos e Indígenas
    tabela_obitos <- obitos_ano_table %>%
      select(ano_categoria, somente_ativos, ativos_e_indigenas, diferenca, perc_col_somente_ativos, perc_col_ativos_e_indigenas)
    
    datatable(tabela_obitos,
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
      colnames = c("Ano", "Somente Ativos", "Ativos e Indígenas", "Diferença", "% Só Ativos", "% Ativ. Indig.")
    ) %>%
      formatRound(columns = c(2:4), digits = 0, mark = ".") %>%
      formatRound(columns = c(5:6), digits = 2) %>%
      formatStyle(1, target = 'row', backgroundColor = styleEqual("< 2000", '#ffcccc'))
  })
  
  output$obitos_ano_plot <- renderPlotly({
    obitos_numerico <- obitos_ano_calc %>%
      filter(ano_categoria != "< 2000") %>%
      mutate(ano_categoria = as.numeric(ano_categoria)) %>%
      arrange(ano_categoria)
    
    plot_ly(data = obitos_numerico) %>%
      add_trace(x = ~ano_categoria, y = ~somente_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#3498db', width = 2),
                marker = list(color = '#3498db', size = 6)) %>%
      add_trace(x = ~ano_categoria, y = ~ativos_e_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2),
                marker = list(color = '#e74c3c', size = 6)) %>%
      layout(title = "Óbitos: Ativos e Indígenas vs Somente Ativos",
             xaxis = list(
               title = "Ano",
               tickangle = 45,
               tickvals = obitos_numerico$ano_categoria,
               ticktext = as.character(as.integer(obitos_numerico$ano_categoria)),
               tickfont = list(size = 9)
             ),
             yaxis = list(title = "Óbitos", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
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
  
  # Tabela DSEI - Óbitos (REATIVA - ADAPTADA)
  output$obitos_dsei_table <- renderDT({
    dsei_selecionado <- input$obitos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(NULL)
    }
    
    # Consolidar anos < 2000 em "< 2000" e ordenar corretamente
    obitos_dsei_resumido <- obitos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado) %>%
      mutate(ano_obito = ifelse(ano_num == 0, "< 2000", ano_obito)) %>%
      group_by(ds_dsei, ano_obito) %>%
      summarise(
        frequencia_ativos = sum(frequencia_ativos, na.rm = TRUE),
        frequencia_indigenas = sum(frequencia_indigenas, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        diferenca_nao_indigenas = frequencia_ativos - frequencia_indigenas,
        ordem = ifelse(ano_obito == "< 2000", 0, as.numeric(ano_obito))
      ) %>%
      group_by(ds_dsei) %>%
      mutate(
        perc_total_ativos = round(100 * frequencia_ativos / sum(frequencia_ativos, na.rm = TRUE), 2),
        perc_total_indigenas = round(100 * frequencia_indigenas / sum(frequencia_indigenas, na.rm = TRUE), 2)
      ) %>%
      ungroup() %>%
      arrange(ordem) %>%
      mutate(
        crescimento_abs_ativos = frequencia_ativos - lag(frequencia_ativos),
        crescimento_abs_indigenas = frequencia_indigenas - lag(frequencia_indigenas),
        crescimento_perc_ativos = round((frequencia_ativos - lag(frequencia_ativos)) / lag(frequencia_ativos) * 100, 2),
        crescimento_perc_indigenas = round((frequencia_indigenas - lag(frequencia_indigenas)) / lag(frequencia_indigenas) * 100, 2)
      ) %>%
      select(ds_dsei, ano_obito, frequencia_ativos, frequencia_indigenas,
             diferenca_nao_indigenas, perc_total_ativos, perc_total_indigenas,
             crescimento_perc_ativos, crescimento_perc_indigenas)
    
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
      colnames = c("DSEI", "Ano", "Só Ativos", "Ativ. Indig.", "Diferença", "% Só Ativos", "% Ativ. Indig.", "Cresc. % Ativos", "Cresc. % Indig.")
    ) %>%
      formatRound(columns = c(3:5), digits = 0, mark = ".") %>%
      formatRound(columns = c(6:9), digits = 2) %>%
      formatStyle(2, target = 'row', backgroundColor = styleEqual("< 2000", '#ffcccc'))
  })
  
  # Gráfico DSEI - Óbitos (REATIVO - ADAPTADO)
  output$obitos_dsei_plot <- renderPlotly({
    dsei_selecionado <- input$obitos_dsei_select
    
    if (is.null(dsei_selecionado) || dsei_selecionado == "") {
      return(plotly_empty() %>% layout(title = "Selecione um DSEI"))
    }
    
    dados <- obitos_dsei_calc %>%
      filter(ds_dsei == dsei_selecionado, !is.na(ano_num), ano_num > 0)
    
    if (nrow(dados) == 0) {
      return(plotly_empty() %>% layout(title = "Sem dados disponíveis"))
    }
    
    plot_ly(dados) %>%
      add_trace(x = ~ano_num, y = ~frequencia_indigenas, name = "Ativos e Indígenas",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2980b9', width = 2),
                marker = list(size = 5)) %>%
      add_trace(x = ~ano_num, y = ~frequencia_ativos, name = "Somente Ativos",
                type = "scatter", mode = "lines+markers",
                line = list(color = '#e74c3c', width = 2, dash = "dash"),
                marker = list(size = 5)) %>%
      layout(title = paste("Óbitos -", dsei_selecionado),
             xaxis = list(title = "Ano", dtick = 5),
             yaxis = list(title = "Óbitos", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
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
  

  
  # Painel de filtros dinâmico com pickerInput
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
          h5(v),
          pickerInput(
            inputId = paste0("tab_filtro_", v),
            label = NULL,
            choices = choices_list,
            selected = choices_list,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Remover Tudo",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "Nenhum selecionado",
              size = 10,
              `live-search` = TRUE,
              `live-search-placeholder` = "Buscar..."
            )
          ),
          hr()
        )
      })
    )
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
    sufixo <- if (input$tab_tipo_valor != "abs") " (%)" else " (frequência)"
    datatable(
      tab, rownames = FALSE,
      options = list(pageLength = 20, dom = 't', ordering = FALSE),
      caption = paste0("Linhas: ", input$tab_linha, "  |  Colunas: ", input$tab_coluna, sufixo)
    )
  })
  
  # Decide o que mostrar: 1 tabela ou N tabelas estratificadas
  output$tab_conteudo_tabelas <- renderUI({
    req(input$tab_estrato)
    if (input$tab_estrato == "nenhuma") {
      DTOutput("tab_tabela_unica")
    } else {
      df <- tab_dados_filtrados()
      valores_estrato <- sort(unique(df[[input$tab_estrato]]))
      tagList(
        lapply(valores_estrato, function(v) {
          id <- paste0("tab_tabela_estrato_", make.names(v))
          tagList(
            h4(paste0(input$tab_estrato, ": ", v)),
            DTOutput(id),
            br()
          )
        })
      )
    }
  })
  
  # Gera dinamicamente uma renderDT para cada categoria de estratificação
  observe({
    req(input$tab_estrato)
    if (input$tab_estrato != "nenhuma") {
      df <- tab_dados_filtrados()
      valores_estrato <- sort(unique(df[[input$tab_estrato]]))
      estrato_var <- input$tab_estrato
      
      lapply(valores_estrato, function(v) {
        local({
          valor_local <- v
          id <- paste0("tab_tabela_estrato_", make.names(valor_local))
          output[[id]] <- renderDT({
            df_sub <- df[df[[estrato_var]] == valor_local, ]
            tab <- gerar_crosstab(df_sub, input$tab_linha, input$tab_coluna, input$tab_tipo_valor)
            sufixo <- if (input$tab_tipo_valor != "abs") " (%)" else " (frequência)"
            datatable(
              tab, rownames = FALSE,
              options = list(pageLength = 20, dom = 't', ordering = FALSE),
              caption = paste0("Linhas: ", input$tab_linha, "  |  Colunas: ", input$tab_coluna, sufixo)
            )
          })
        })
      })
    }
  })
  
  # Exportação
  tab_tabelas_para_exportar <- reactive({
    req(input$tab_linha, input$tab_coluna, input$tab_estrato)
    df <- tab_dados_filtrados()
    
    if (input$tab_estrato == "nenhuma") {
      tab <- gerar_crosstab(df, input$tab_linha, input$tab_coluna, input$tab_tipo_valor)
      tab <- tab %>% mutate(Estrato = "Todos", .before = 1)
      return(tab)
    }
    
    valores_estrato <- sort(unique(df[[input$tab_estrato]]))
    tabelas <- lapply(valores_estrato, function(v) {
      df_sub <- df[df[[input$tab_estrato]] == v, ]
      tab <- gerar_crosstab(df_sub, input$tab_linha, input$tab_coluna, input$tab_tipo_valor)
      tab %>% mutate(Estrato = paste0(input$tab_estrato, ": ", v), .before = 1)
    })
    bind_rows(tabelas)
  })
  
  output$tab_baixar_csv <- downloadHandler(
    filename = function() {
      sufixo_estrato <- if (input$tab_estrato == "nenhuma") "" else paste0("_por_", input$tab_estrato)
      paste0("tabulador_", input$tab_linha, "_x_", input$tab_coluna, sufixo_estrato,
             "_", input$tab_tipo_valor, ".csv")
    },
    content = function(file) {
      write.csv2(tab_tabelas_para_exportar(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui = ui, server = server)
