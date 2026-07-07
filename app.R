# ============================================================
# PAINEL DE ACIDENTES ESCORPIÔNICOS — RIDE-DF
# Shiny app para exibição em TV (1920x1080), modo kiosk (sem
# menus, sem filtros, sem sidebar, sem scroll), com rotação
# automática entre os municípios da RIDE-DF (10s cada).
#
# Estrutura pensada para reuso: para outro agravo/ano, basta
# alterar os parâmetros no topo do arquivo (AGRAVO, ANO_PAINEL).
# ============================================================

# ---- Pacotes ----
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(sf)

# ============================================================
# PARÂMETROS GERAIS (ajuste aqui para reutilizar o painel)
# ============================================================
DATA_PATH              <- "dados/animais_tratado.rds"
SHAPE_PATH             <- "dados/shapefile_ride.rds"
AGRAVO                 <- "Escorpi"              # trecho (sem acento) buscado em TP_ACIDENT_DESC
TITULO                 <- "ACIDENTES ESCORPIÔNICOS"
ANO_PAINEL             <- 2026                   # fixo: população de referência é de 2025
POP_REF_ANO             <- 2025                  # ano de referência da estimativa populacional exibida no cabeçalho
SEGUNDOS_POR_MUNICIPIO <- 10
SEGUNDOS_OVERVIEW      <- 20                     # duração da tela de visão geral da RIDE-DF

# ============================================================
# FUNÇÕES AUXILIARES
# ============================================================

# remove acentuação para permitir comparações robustas de texto
sem_acento <- function(x) {
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

# agrupa idade em anos nas 7 faixas exigidas pela pirâmide etária
bucket_idade <- function(idade) {
  cut(
    idade,
    breaks = c(-Inf, 9, 19, 29, 39, 49, 59, Inf),
    labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
    right  = TRUE
  )
}

# placeholder visual quando não há notificações no período
grafico_vazio <- function(msg = "Sem notificações no período") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, color = "#1a0033", size = 6) +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", color = NA))
}

# renderiza a variação % com seta e cor: mais casos que ano anterior = ruim
# (vermelho, seta pra cima); menos casos = bom (verde, seta pra baixo).
# Inf = o ano anterior tinha ZERO casos no período e o atual passou a ter —
# isso é tratado como alerta ("NOVO"), não como percentual (que seria
# matematicamente infinito e sem sentido de exibir).
renderizar_variacao <- function(v) {
  if (is.na(v)) return(tags$span("N/D", style = "color:#1a0033;"))
  if (is.infinite(v)) return(tags$span("\u25B2 NOVO", style = "color:#C62828;"))
  cor  <- if (v > 0) "#C62828" else if (v < 0) "#2E7D32" else "#1a0033"
  seta <- if (v > 0) "\u25B2" else if (v < 0) "\u25BC" else "\u25AC"
  tags$span(
    sprintf("%s %s%%", seta, number(abs(v), accuracy = 0.1, decimal.mark = ",")),
    style = paste0("color:", cor, ";")
  )
}

# ============================================================
# CARREGAMENTO E PREPARAÇÃO DOS DADOS
# (roda uma única vez, quando o app sobe)
#
# Pré-requisito: `animais_tratado.rds` precisa ter a coluna `ride_df`
# (TRUE/FALSE), criada no script de tratamento a partir da API oficial
# do IBGE (regiões integradas de desenvolvimento) — ver
# `adicionar_ride_df.R`. Isso evita depender de lista de nomes na mão.
# ============================================================
animais <- readRDS(DATA_PATH)

if (!"ride_df" %in% names(animais)) {
  stop(
    "A base não tem a coluna `ride_df`. Rode o trecho `adicionar_ride_df.R` ",
    "no seu script de tratamento antes de gerar o .rds."
  )
}

# população por município/ano: usa a base COMPLETA (não só o agravo),
# assim municípios sem nenhuma notificação de escorpião no ano ainda
# aparecem no painel com a população correta.
pop_lookup <- animais %>%
  distinct(municipio, cod_municipio, ANO_BASE, populacao) %>%
  filter(!is.na(populacao))

# base filtrada: apenas o agravo de interesse + municípios da RIDE-DF
base_agravo <- animais %>%
  filter(str_detect(sem_acento(TP_ACIDENT_DESC), regex(AGRAVO, ignore_case = TRUE))) %>%
  filter(ride_df)

ano_padrao <- if (is.null(ANO_PAINEL)) max(base_agravo$ANO_BASE, na.rm = TRUE) else ANO_PAINEL

# ============================================================
# ANO/SEMANA EPIDEMIOLÓGICA — centralizado aqui uma única vez
# (extraído de SEM_NOT, que é a fonte confiável, diferente de
# ANO_BASE que é só o ano do arquivo baixado — ver o bug que já
# corrigimos antes). Todo gráfico/indicador que precisa de
# semana epidemiológica usa essas duas colunas, em vez de cada
# um reimplementar o parsing.
# ============================================================
base_agravo <- base_agravo %>%
  mutate(
    ano_sem = suppressWarnings(as.integer(str_sub(as.character(SEM_NOT), 1, 4))),
    semana  = suppressWarnings(as.integer(str_sub(as.character(SEM_NOT), 5, 6)))
  )

ANO_ANTERIOR <- ANO_PAINEL - 1

# anos usados como referência histórica no canal endêmico (todos os anos
# presentes na base que são anteriores ao ano do painel)
ANOS_HISTORICOS <- base_agravo$ano_sem[!is.na(base_agravo$ano_sem) & base_agravo$ano_sem < ANO_PAINEL]
ANOS_HISTORICOS <- sort(unique(ANOS_HISTORICOS))

# data/hora em que o .rds foi gerado pela última vez (usado no rodapé —
# reflete a última execução bem-sucedida do etl_animais.R)
ULTIMA_ATUALIZACAO <- file.info(DATA_PATH)$mtime

# semana epidemiológica mais recente com notificação no ano corrente —
# usada como "corte" para comparar de forma justa com o mesmo período do
# ano anterior (comparar ano fechado com ano em andamento não faria sentido)
semana_max_atual <- base_agravo %>%
  filter(ano_sem == ANO_PAINEL, !is.na(semana), semana >= 1, semana <= 53) %>%
  pull(semana)
semana_max_atual <- if (length(semana_max_atual) == 0) NA_integer_ else max(semana_max_atual, na.rm = TRUE)

# título com semana/ano corrente ao lado — evita repetir "2026" solto no
# canto direito do cabeçalho, que ficava sem contexto
TITULO_COMPLETO <- sprintf(
  "%s — Semana %s/%d",
  TITULO,
  if (is.na(semana_max_atual)) "N/D" else sprintf("%02d", semana_max_atual),
  ANO_PAINEL
)

# rótulo do card de variação — deixa explícito até qual semana a
# comparação vai, pra não ficar um "vs 2025" solto sem contexto
ROTULO_VARIACAO <- if (is.na(semana_max_atual)) {
  sprintf("Variação vs %d", ANO_ANTERIOR)
} else {
  sprintf("Até sem. %02d vs. %d", semana_max_atual, ANO_ANTERIOR)
}

# ---- Variação % de casos vs. mesmo período do ano anterior ----
# (mesmas semanas epidemiológicas 1..semana_max_atual, nos dois anos)
# `municipio_filtro = NULL` calcula para a RIDE-DF inteira.
#
# Retorno:
#   NA_real_  -> não dá pra calcular (sem semana de corte disponível)
#   Inf       -> ano anterior tinha ZERO casos e o atual tem 1+ (foco novo/
#                emergência — sinal de alerta, não "sem dado")
#   0         -> os dois anos com zero casos no período (estável, sem sinal)
#   demais    -> variação percentual normal
calcular_variacao <- function(df, municipio_filtro = NULL) {
  if (is.na(semana_max_atual)) return(NA_real_)

  base <- df
  if (!is.null(municipio_filtro)) base <- base %>% filter(municipio == municipio_filtro)

  casos_atual <- base %>%
    filter(ano_sem == ANO_PAINEL, semana <= semana_max_atual) %>%
    nrow()
  casos_anterior <- base %>%
    filter(ano_sem == ANO_ANTERIOR, semana <= semana_max_atual) %>%
    nrow()

  if (casos_anterior == 0) {
    return(if (casos_atual > 0) Inf else 0)
  }
  ((casos_atual - casos_anterior) / casos_anterior) * 100
}

# municípios efetivamente exibidos = os marcados como RIDE-DF que existem
# na base (ordem alfabética). Usa a base completa (não só o agravo) para
# não perder município com zero casos de escorpião no ano.
municipios_disponiveis <- animais %>%
  filter(ride_df) %>%
  distinct(municipio) %>%
  arrange(municipio) %>%
  pull(municipio)

N_MUNICIPIOS <- length(municipios_disponiveis)

if (N_MUNICIPIOS == 0) {
  stop("Nenhum município com ride_df == TRUE foi encontrado na base.")
}

# ============================================================
# SHAPEFILE DA RIDE-DF (para o mapa da tela de visão geral)
# Gerado à parte por `etl_shapefile_ride.R` — não faz parte do
# ETL mensal, já que a malha municipal quase não muda.
# ============================================================
if (!file.exists(SHAPE_PATH)) {
  stop(
    "Não encontrei `", SHAPE_PATH, "`. Rode `etl_shapefile_ride.R` antes de ",
    "iniciar o painel — ele gera esse arquivo (só precisa rodar 1x)."
  )
}

shapefile_ride <- readRDS(SHAPE_PATH)

if (!"cod_municipio" %in% names(shapefile_ride)) {
  stop("O shapefile não tem a coluna `cod_municipio`. Rode `etl_shapefile_ride.R` novamente.")
}

# ============================================================
# RESUMO AGREGADO DA RIDE-DF (tela de visão geral)
# Calculado uma vez no carregamento do app — ANO_PAINEL é fixo,
# então esse resumo não muda durante a execução.
# ============================================================
municipios_ride_base <- animais %>%
  filter(ride_df) %>%
  distinct(municipio, cod_municipio)

casos_por_municipio <- base_agravo %>%
  filter(ANO_BASE == ano_padrao) %>%
  group_by(municipio) %>%
  summarise(
    casos  = n(),
    obitos = sum(obito_agravo, na.rm = TRUE),
    graves = sum(grave, na.rm = TRUE),
    .groups = "drop"
  )

pop_ano_padrao <- pop_lookup %>%
  filter(ANO_BASE == ano_padrao) %>%
  distinct(municipio, populacao)

# uma linha por município da RIDE-DF, mesmo os com zero casos no ano
resumo_ride <- municipios_ride_base %>%
  left_join(casos_por_municipio, by = "municipio") %>%
  mutate(
    casos  = coalesce(casos, 0L),
    obitos = coalesce(obitos, 0),
    graves = coalesce(graves, 0)
  ) %>%
  left_join(pop_ano_padrao, by = "municipio") %>%
  mutate(incidencia = if_else(!is.na(populacao) & populacao > 0, (casos / populacao) * 10000, NA_real_))

indicadores_ride <- list(
  casos      = sum(resumo_ride$casos),
  obitos     = sum(resumo_ride$obitos),
  graves     = sum(resumo_ride$graves),
  incidencia = {
    pop_total   <- sum(resumo_ride$populacao, na.rm = TRUE)
    casos_total <- sum(resumo_ride$casos)
    if (pop_total > 0) (casos_total / pop_total) * 10000 else NA_real_
  },
  letalidade = {
    casos_total <- sum(resumo_ride$casos)
    if (casos_total > 0) (sum(resumo_ride$obitos) / casos_total) * 100 else 0
  },
  populacao_total = sum(resumo_ride$populacao, na.rm = TRUE),
  variacao = calcular_variacao(base_agravo)
)

# ============================================================
# CORES E TEMA GRÁFICO
# ============================================================
COR_MASC <- "#2C7FB8"
COR_FEM  <- "#D63384"
COR_DESTAQUE <- "#FDB913"
COR_LEVE <- "#2E7D32"
COR_MODERADO <- "#F9A825"
COR_GRAVE <- "#C62828"

tema_painel <- theme_minimal(base_size = 18) +
  theme(
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.position   = "bottom",
    legend.text       = element_text(color = "#1a0033", size = 13),
    text              = element_text(color = "#1a0033"),
    axis.text         = element_text(color = "#1a0033", size = 13),
    panel.grid.major  = element_line(color = "gray85"),
    panel.grid.minor  = element_blank()
  )

# tema para os gráficos donut (Sexo, Classificação): legenda compacta na
# lateral direita, para não "roubar" a altura do círculo como acontecia
# com a legenda embaixo
tema_donut <- tema_painel +
  theme(
    legend.position   = "right",
    legend.direction  = "vertical",
    legend.key.size   = unit(0.4, "cm"),
    legend.text       = element_text(color = "#1a0033", size = 12),
    legend.spacing.y  = unit(0.05, "cm"),
    plot.margin       = margin(0, 0, 0, 0)
  )

# ============================================================
# TELA 1 — VISÃO GERAL DA RIDE-DF (mapa + indicadores agregados)
# ============================================================
ui_overview <- function() {
  div(class = "grid-overview",
      div(class = "grafico-box mapa-wrapper",
          div(class = "titulo-grafico", "Mapa da RIDE-DF — Incidência (/10 mil hab.)"),
          plotOutput("mapa_ride")),

      div(class = "lado-direito",
          div(class = "stats-row",
              div(class = "stat-flat", div(class = "valor", textOutput("card_ride_casos", inline = TRUE)),
                  div(class = "rotulo", "Casos")),
              div(class = "stat-flat", div(class = "valor", textOutput("card_ride_incidencia", inline = TRUE)),
                  div(class = "rotulo", "Incidência /10 mil hab.")),
              div(class = "stat-flat", div(class = "valor", textOutput("card_ride_letalidade", inline = TRUE)),
                  div(class = "rotulo", "Letalidade")),
              div(class = "stat-flat", div(class = "valor", textOutput("card_ride_obitos", inline = TRUE)),
                  div(class = "rotulo", "Óbitos")),
              div(class = "stat-flat", div(class = "valor", textOutput("card_ride_graves", inline = TRUE)),
                  div(class = "rotulo", "Casos graves")),
              div(class = "stat-flat", div(class = "valor", uiOutput("card_ride_variacao", inline = TRUE)),
                  div(class = "rotulo", ROTULO_VARIACAO))
          ),

          div(class = "grafico-box",
              div(class = "titulo-grafico", "Municípios com Maior Incidência (Top 10, mín. 5 casos)"),
              plotOutput("grafico_ranking_ride")),

          div(class = "grafico-box",
              div(class = "titulo-grafico", "Canal Endêmico — Notificações por Semana Epidemiológica"),
              plotOutput("grafico_semanal_ride"))
      )
  )
}

# ============================================================
# TELA 2 — DETALHE DO MUNICÍPIO (rotação de 10s)
# ============================================================
ui_municipio <- function() {
  tagList(
    div(class = "linha-cards",
        div(class = "card", div(class = "valor", textOutput("card_casos", inline = TRUE)),
            div(class = "rotulo", "Casos")),
        div(class = "card", div(class = "valor", textOutput("card_incidencia", inline = TRUE)),
            div(class = "rotulo", "Incidência /10 mil hab.")),
        div(class = "card", div(class = "valor", textOutput("card_letalidade", inline = TRUE)),
            div(class = "rotulo", "Letalidade")),
        div(class = "card", div(class = "valor", textOutput("card_obitos", inline = TRUE)),
            div(class = "rotulo", "Óbitos")),
        div(class = "card", div(class = "valor", textOutput("card_graves", inline = TRUE)),
            div(class = "rotulo", "Casos graves")),
        div(class = "card", div(class = "valor", uiOutput("card_variacao", inline = TRUE)),
            div(class = "rotulo", ROTULO_VARIACAO))
    ),

    div(class = "linha-graficos",
        div(class = "grafico-box", div(class = "titulo-grafico", "Distribuição por Sexo"),
            plotOutput("grafico_sexo")),
        div(class = "grafico-box", div(class = "titulo-grafico", "Pirâmide Etária (Sexo x Idade)"),
            plotOutput("grafico_piramide")),
        div(class = "grafico-box", div(class = "titulo-grafico", "Distribuição por Raça/Cor"),
            plotOutput("grafico_raca"))
    ),

    div(class = "linha-graficos",
        div(class = "grafico-box", div(class = "titulo-grafico", "Classificação de Gravidade"),
            plotOutput("grafico_classificacao")),
        div(class = "grafico-box", div(class = "titulo-grafico", "Tempo até o Atendimento"),
            plotOutput("grafico_tempo")),
        div(class = "grafico-box", div(class = "titulo-grafico", "Local da Picada"),
            plotOutput("grafico_local"))
    ),

    div(class = "linha-tempo",
        div(class = "grafico-box", div(class = "titulo-grafico", "Canal Endêmico — Notificações por Semana"),
            plotOutput("grafico_semanal"))
    )
  )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  title = TITULO,
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%; margin: 0; padding: 0;
        overflow: hidden;
        background-color: #FFFFFF;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      * { box-sizing: border-box; }

      .cabecalho {
        display: flex; justify-content: space-between; align-items: center;
        background-color: #1a0033; color: white; padding: 0 30px;
        height: 7vh;
      }
      .cabecalho h1 { font-size: 2.1vw; margin: 0; font-weight: 700; letter-spacing: 1px; }
      .cabecalho .info { font-size: 1.35vw; font-weight: 500; white-space: nowrap; }

      .linha-cards {
        display: flex; justify-content: space-between; gap: 12px;
        padding: 10px 20px; height: 12vh;
      }
      .card {
        flex: 1; border-radius: 12px; color: white;
        background: linear-gradient(135deg, #2b0a4d, #4a1073);
        display: flex; flex-direction: column; justify-content: center; align-items: center;
      }
      .card .valor { font-size: 2.15vw; font-weight: 800; line-height: 1.1; }
      .card .rotulo { font-size: 0.95vw; text-transform: uppercase; opacity: 0.85; margin-top: 4px; text-align:center; }

      .linha-graficos { display: flex; gap: 10px; padding: 6px 20px; height: 26vh; }
      .grafico-box {
        flex: 1; background-color: #FFFFFF; border: 1px solid #E0E0E0; border-radius: 10px;
        padding: 4px 8px; display: flex; flex-direction: column;
      }
      .linha-tempo { padding: 6px 20px 14px 20px; height: 25vh; }
      .linha-tempo .grafico-box { height: 100%; }

      .titulo-grafico {
        color: #1a0033; font-size: 1.05vw; text-align: center;
        font-weight: 600; margin: 2px 0 0 0;
      }
      .grafico-box .shiny-plot-output {
        flex: 1; width: 100% !important; height: 100% !important;
      }

      /* ---- Tela de visão geral (mapa ocupa metade da tela) ---- */
      .grid-overview {
        display: flex;
        gap: 10px;
        padding: 10px 20px;
        height: 89vh;
      }
      .mapa-wrapper { flex: 1 1 50%; height: 100%; }

      .rodape {
        height: 3vh; display: flex; align-items: center; justify-content: center;
        background-color: #F5F5F5; color: #666; font-size: 0.85vw;
        border-top: 1px solid #E0E0E0;
      }

      .lado-direito {
        flex: 1 1 50%;
        height: 100%;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      .stats-row { display: flex; gap: 8px; height: 17%; }
      .lado-direito .grafico-box { flex: 1; }

      .stat-flat {
        flex: 1; background-color: #FFFFFF; border: 1px solid #E0E0E0;
        border-left: 5px solid #7A1FA2; border-radius: 8px;
        display: flex; flex-direction: column; justify-content: center; align-items: center;
        padding: 4px;
      }
      .stat-flat .valor { color: #1a0033; font-weight: 800; font-size: 1.5vw; line-height: 1.1; }
      .stat-flat .rotulo {
        color: #666; font-size: 0.72vw; text-transform: uppercase;
        margin-top: 2px; text-align: center;
      }
    "))
  ),

  div(class = "cabecalho",
      h1(TITULO_COMPLETO),
      div(class = "info", textOutput("info_cabecalho", inline = TRUE))
  ),

  uiOutput("conteudo_principal"),

  div(class = "rodape",
      sprintf(
        "Fonte: SINAN/DATASUS | Atualizado em %s",
        format(ULTIMA_ATUALIZACAO, "%d/%m/%Y às %H:%M")
      )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Estado da rotação automática ----
  # modo "overview": tela de visão geral da RIDE-DF (SEGUNDOS_OVERVIEW)
  # modo "municipio": rotação por município (SEGUNDOS_POR_MUNICIPIO cada)
  # ciclo completo: overview -> município 1 -> ... -> município N -> overview -> ...
  estado <- reactiveValues(modo = "overview", indice = 1, contador = SEGUNDOS_OVERVIEW)

  observe({
    invalidateLater(1000, session)
    isolate({
      novo_contador <- estado$contador - 1

      if (novo_contador > 0) {
        estado$contador <- novo_contador
        return(NULL)
      }

      if (estado$modo == "overview") {
        # acabou a visão geral -> começa a rotação pelos municípios
        estado$modo     <- "municipio"
        estado$indice   <- 1
        estado$contador <- SEGUNDOS_POR_MUNICIPIO
      } else if (estado$indice >= N_MUNICIPIOS) {
        # terminou o ciclo de municípios -> volta pra visão geral
        estado$modo     <- "overview"
        estado$contador <- SEGUNDOS_OVERVIEW
      } else {
        estado$indice   <- estado$indice + 1
        estado$contador <- SEGUNDOS_POR_MUNICIPIO
      }
    })
  })

  # ---- Alterna entre as duas telas ----
  output$conteudo_principal <- renderUI({
    if (estado$modo == "overview") ui_overview() else ui_municipio()
  })

  municipio_atual <- reactive({
    municipios_disponiveis[estado$indice]
  })

  # ============================================================
  # OBJETO REATIVO CENTRAL: dados do município exibido no momento
  # Toda a lógica de cabeçalho/cards/gráficos parte daqui.
  # ============================================================
  dados_municipio <- reactive({
    base_agravo %>%
      filter(municipio == municipio_atual(), ANO_BASE == ano_padrao)
  })

  populacao_atual <- reactive({
    valor <- pop_lookup %>%
      filter(municipio == municipio_atual(), ANO_BASE == ano_padrao) %>%
      pull(populacao)
    if (length(valor) == 0) NA_real_ else valor[1]
  })

  # ============================================================
  # INDICADORES CENTRALIZADOS (alimentam cabeçalho e cards)
  # ============================================================
  indicadores <- reactive({
    df  <- dados_municipio()
    pop <- populacao_atual()

    casos  <- nrow(df)
    obitos <- sum(df$obito_agravo, na.rm = TRUE)
    graves <- sum(df$grave, na.rm = TRUE)

    incidencia <- if (!is.na(pop) && pop > 0) (casos / pop) * 10000 else NA_real_
    letalidade <- if (casos > 0) (obitos / casos) * 100 else 0
    variacao   <- calcular_variacao(base_agravo, municipio_filtro = municipio_atual())

    list(
      casos      = casos,
      obitos     = obitos,
      graves     = graves,
      incidencia = incidencia,
      letalidade = letalidade,
      populacao  = pop,
      variacao   = variacao
    )
  })

  # ---- Cabeçalho ----
  output$info_cabecalho <- renderText({
    if (estado$modo == "overview") {
      pop_fmt <- number(indicadores_ride$populacao_total, big.mark = ".", accuracy = 1)
      sprintf("RIDE-DF | Visão Geral | Pop. %s | %ds", pop_fmt, estado$contador)
    } else {
      ind <- indicadores()
      pop_fmt <- if (!is.na(ind$populacao)) {
        number(ind$populacao, big.mark = ".", accuracy = 1)
      } else {
        "N/D"
      }
      sprintf(
        "%s | Pop. %s (ref. %d) | %02d/%02d | %ds",
        municipio_atual(), pop_fmt, POP_REF_ANO, estado$indice, N_MUNICIPIOS, estado$contador
      )
    }
  })

  # ---- Cards ----
  output$card_casos <- renderText({
    number(indicadores()$casos, big.mark = ".")
  })

  output$card_incidencia <- renderText({
    v <- indicadores()$incidencia
    if (is.na(v)) "N/D" else number(v, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
  })

  output$card_letalidade <- renderText({
    paste0(number(indicadores()$letalidade, accuracy = 0.1, decimal.mark = ","), "%")
  })

  output$card_obitos <- renderText({
    number(indicadores()$obitos, big.mark = ".")
  })

  output$card_graves <- renderText({
    number(indicadores()$graves, big.mark = ".")
  })

  output$card_variacao <- renderUI({
    renderizar_variacao(indicadores()$variacao)
  })

  # ============================================================
  # TELA DE VISÃO GERAL — cards agregados da RIDE-DF
  # (vêm de `indicadores_ride`, calculado 1x no carregamento,
  # já que ANO_PAINEL é fixo)
  # ============================================================
  output$card_ride_casos <- renderText({
    number(indicadores_ride$casos, big.mark = ".")
  })

  output$card_ride_incidencia <- renderText({
    v <- indicadores_ride$incidencia
    if (is.na(v)) "N/D" else number(v, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
  })

  output$card_ride_letalidade <- renderText({
    paste0(number(indicadores_ride$letalidade, accuracy = 0.1, decimal.mark = ","), "%")
  })

  output$card_ride_obitos <- renderText({
    number(indicadores_ride$obitos, big.mark = ".")
  })

  output$card_ride_graves <- renderText({
    number(indicadores_ride$graves, big.mark = ".")
  })

  output$card_ride_variacao <- renderUI({
    renderizar_variacao(indicadores_ride$variacao)
  })

  # ---- Mapa coroplético (incidência por município) ----
  output$mapa_ride <- renderPlot({
    mapa_dados <- shapefile_ride %>%
      left_join(resumo_ride %>% select(cod_municipio, incidencia, casos), by = "cod_municipio")

    ggplot(mapa_dados) +
      geom_sf(aes(fill = incidencia), color = "white", linewidth = 0.3) +
      coord_sf(expand = FALSE) +
      scale_fill_gradient(
        low = "#FDE9B6", high = "#7A1FA2", na.value = "gray90",
        name = "Incid. /10 mil hab."
      ) +
      tema_painel +
      theme(
        axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        # `panel.grid = element_blank()` sozinho não sobrescrevia a grade que
        # `tema_painel` já tinha definido explicitamente em panel.grid.major —
        # precisa zerar major e minor especificamente.
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "bottom", legend.key.width = unit(1.1, "cm")
      )
  }, bg = "transparent")

  # ---- Ranking dos municípios com maior incidência (não maior volume) ----
  # Município com mais casos em termos absolutos é sempre Brasília (mais
  # gente) — o que interessa pra vigilância é onde a picada é
  # proporcionalmente mais frequente. Exige um mínimo de casos pra taxa não
  # ficar instável (1 caso num município pequeno dispararia a incidência
  # sem isso representar um padrão real).
  output$grafico_ranking_ride <- renderPlot({
    CASOS_MINIMOS_RANKING <- 5

    tab <- resumo_ride %>%
      filter(!is.na(incidencia), casos >= CASOS_MINIMOS_RANKING) %>%
      slice_max(incidencia, n = 10, with_ties = FALSE) %>%
      mutate(
        municipio_curto = str_remove(municipio, " - [A-Z]{2}$"),
        municipio_curto = factor(municipio_curto, levels = rev(municipio_curto))
      )

    if (nrow(tab) == 0) return(grafico_vazio(sprintf("Nenhum município com %d+ casos", CASOS_MINIMOS_RANKING)))

    ggplot(tab, aes(x = municipio_curto, y = incidencia)) +
      geom_col(fill = COR_DESTAQUE) +
      coord_flip() +
      geom_text(
        aes(label = paste0(number(incidencia, accuracy = 0.1, decimal.mark = ","), " (", casos, " casos)")),
        hjust = -0.05, color = "#1a0033", size = 4.3
      ) +
      scale_y_continuous(labels = number_format(decimal.mark = ","), limits = c(0, max(tab$incidencia) * 1.4)) +
      tema_painel +
      theme(axis.title = element_blank())
  }, bg = "transparent")

  # ---- Notificações por semana epidemiológica — RIDE-DF inteira ----
  # canal endêmico: faixa mín-máx histórica (ANOS_HISTORICOS) atrás da
  # linha do ano corrente, pra dar contexto se o valor da semana é normal
  output$grafico_semanal_ride <- renderPlot({
    df_atual <- base_agravo %>%
      filter(!is.na(semana), semana >= 1, semana <= 53, ano_sem == ano_padrao) %>%
      count(semana) %>%
      complete(semana = 1:53, fill = list(n = 0))

    if (length(ANOS_HISTORICOS) == 0) {
      return(
        ggplot(df_atual, aes(x = semana, y = n)) +
          geom_line(color = COR_DESTAQUE, linewidth = 1.2) +
          geom_point(color = COR_DESTAQUE, size = 2) +
          scale_x_continuous(breaks = seq(1, 53, 4)) +
          labs(x = "Semana epidemiológica", y = "Notificações") +
          tema_painel
      )
    }

    df_historico <- base_agravo %>%
      filter(ano_sem %in% ANOS_HISTORICOS, !is.na(semana), semana >= 1, semana <= 53) %>%
      count(ano_sem, semana) %>%
      complete(ano_sem = ANOS_HISTORICOS, semana = 1:53, fill = list(n = 0)) %>%
      group_by(semana) %>%
      summarise(minimo = min(n), mediana = median(n), maximo = max(n), .groups = "drop")

    ggplot() +
      geom_ribbon(data = df_historico, aes(x = semana, ymin = minimo, ymax = maximo),
                  fill = "gray70", alpha = 0.4) +
      geom_line(data = df_historico, aes(x = semana, y = mediana),
                color = "gray45", linewidth = 0.7, linetype = "dashed") +
      geom_line(data = df_atual, aes(x = semana, y = n), color = COR_DESTAQUE, linewidth = 1.2) +
      geom_point(data = df_atual, aes(x = semana, y = n), color = COR_DESTAQUE, size = 2) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      labs(
        x = "Semana epidemiológica", y = "Notificações",
        caption = sprintf(
          "Faixa cinza: mín-máx %d-%d   |   - - -  mediana %d-%d   |   linha cheia: %d",
          min(ANOS_HISTORICOS), max(ANOS_HISTORICOS),
          min(ANOS_HISTORICOS), max(ANOS_HISTORICOS), ano_padrao
        )
      ) +
      tema_painel +
      theme(plot.caption = element_text(color = "#1a0033", size = 12, hjust = 0.5))
  }, bg = "transparent")

  # ============================================================
  # GRÁFICOS — segunda linha (perfil demográfico)
  # ============================================================

  output$grafico_sexo <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    tab <- df %>%
      mutate(sexo = case_when(
        str_starts(sem_acento(CS_SEXO_DESC), "Masc") ~ "Masculino",
        str_starts(sem_acento(CS_SEXO_DESC), "Fem")   ~ "Feminino",
        TRUE ~ "Ignorado"
      )) %>%
      count(sexo) %>%
      mutate(pct = n / sum(n))

    ggplot(tab, aes(x = "", y = pct, fill = sexo)) +
      geom_col(width = 1, color = "#0D0D0D") +
      coord_polar(theta = "y") +
      geom_text(aes(label = percent(pct, accuracy = 1)),
                position = position_stack(vjust = 0.5), color = "white", size = 6) +
      scale_fill_manual(values = c("Masculino" = COR_MASC, "Feminino" = COR_FEM, "Ignorado" = "gray50")) +
      tema_donut +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(), legend.title = element_blank())
  }, bg = "transparent")

  output$grafico_piramide <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    faixas_ordem <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+")

    tab <- df %>%
      mutate(
        faixa = bucket_idade(idade_anos),
        sexo  = case_when(
          str_starts(sem_acento(CS_SEXO_DESC), "Masc") ~ "Masculino",
          str_starts(sem_acento(CS_SEXO_DESC), "Fem")   ~ "Feminino",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(faixa), !is.na(sexo)) %>%
      count(faixa, sexo) %>%
      mutate(n_plot = if_else(sexo == "Masculino", -n, n))

    if (nrow(tab) == 0) return(grafico_vazio())

    ggplot(tab, aes(x = factor(faixa, levels = faixas_ordem), y = n_plot, fill = sexo)) +
      geom_col(width = 0.75) +
      coord_flip() +
      scale_fill_manual(values = c("Masculino" = COR_MASC, "Feminino" = COR_FEM)) +
      scale_y_continuous(labels = function(x) number(abs(x), big.mark = ".")) +
      tema_painel +
      theme(axis.title = element_blank(), legend.title = element_blank())
  }, bg = "transparent")

  output$grafico_raca <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    ordem_raca <- c("Branca", "Preta", "Parda", "Amarela", "Indigena", "Ignorado")

    tab <- df %>%
      mutate(raca_norm = case_when(
        str_detect(sem_acento(CS_RACA_DESC), regex("Branca", ignore_case = TRUE))   ~ "Branca",
        str_detect(sem_acento(CS_RACA_DESC), regex("Preta", ignore_case = TRUE))    ~ "Preta",
        str_detect(sem_acento(CS_RACA_DESC), regex("Parda", ignore_case = TRUE))    ~ "Parda",
        str_detect(sem_acento(CS_RACA_DESC), regex("Amarela", ignore_case = TRUE))  ~ "Amarela",
        str_detect(sem_acento(CS_RACA_DESC), regex("Indigena", ignore_case = TRUE)) ~ "Indigena",
        TRUE ~ "Ignorado"
      )) %>%
      count(raca_norm) %>%
      mutate(pct = n / sum(n),
             raca_norm = factor(raca_norm, levels = rev(ordem_raca)))

    ggplot(tab, aes(x = raca_norm, y = pct)) +
      geom_col(fill = COR_DESTAQUE) +
      coord_flip() +
      geom_text(aes(label = percent(pct, accuracy = 0.1)), hjust = -0.15, color = "#1a0033", size = 5) +
      scale_y_continuous(labels = percent, limits = c(0, max(tab$pct, na.rm = TRUE) * 1.3)) +
      tema_painel +
      theme(axis.title = element_blank())
  }, bg = "transparent")

  # ============================================================
  # GRÁFICOS — terceira linha (características do acidente)
  # ============================================================

  output$grafico_classificacao <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    tab <- df %>%
      mutate(classe = case_when(
        str_detect(sem_acento(TRA_CLASSI_DESC), regex("Leve", ignore_case = TRUE))     ~ "Leve",
        str_detect(sem_acento(TRA_CLASSI_DESC), regex("Moderad", ignore_case = TRUE))  ~ "Moderado",
        str_detect(sem_acento(TRA_CLASSI_DESC), regex("Grave", ignore_case = TRUE))    ~ "Grave",
        TRUE ~ "Ignorado"
      )) %>%
      count(classe) %>%
      mutate(pct = n / sum(n))

    ggplot(tab, aes(x = "", y = pct, fill = classe)) +
      geom_col(width = 1, color = "#0D0D0D") +
      coord_polar(theta = "y") +
      geom_text(aes(label = percent(pct, accuracy = 1)),
                position = position_stack(vjust = 0.5), color = "white", size = 6) +
      scale_fill_manual(values = c("Leve" = COR_LEVE, "Moderado" = COR_MODERADO,
                                    "Grave" = COR_GRAVE, "Ignorado" = "gray50")) +
      tema_donut +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(), legend.title = element_blank())
  }, bg = "transparent")

  output$grafico_tempo <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    # ordem clínica padrão do SINAN; categorias fora dessa lista entram no final
    ordem_tempo <- c("0 a 1 hora", "1 a 3 horas", "4 a 6 horas", "7 a 12 horas",
                      "13 a 24 horas", "Mais de 24 horas", "Ignorado")

    tab <- df %>%
      filter(!is.na(ANT_TEMPO_DESC)) %>%
      count(ANT_TEMPO_DESC, sort = TRUE)

    niveis <- union(intersect(ordem_tempo, tab$ANT_TEMPO_DESC),
                     setdiff(tab$ANT_TEMPO_DESC, ordem_tempo))

    tab <- tab %>% mutate(ANT_TEMPO_DESC = factor(ANT_TEMPO_DESC, levels = rev(niveis)))

    ggplot(tab, aes(x = ANT_TEMPO_DESC, y = n)) +
      geom_col(fill = COR_DESTAQUE) +
      coord_flip() +
      geom_text(aes(label = number(n, big.mark = ".")), hjust = -0.15, color = "#1a0033", size = 5) +
      scale_y_continuous(labels = number_format(big.mark = "."), limits = c(0, max(tab$n) * 1.25)) +
      tema_painel +
      theme(axis.title = element_blank())
  }, bg = "transparent")

  output$grafico_local <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    tab <- df %>%
      filter(!is.na(ANT_LOCA_1_DESC)) %>%
      count(ANT_LOCA_1_DESC, sort = TRUE) %>%
      slice_max(n, n = 8) %>%
      mutate(ANT_LOCA_1_DESC = factor(ANT_LOCA_1_DESC, levels = rev(ANT_LOCA_1_DESC)))

    ggplot(tab, aes(x = ANT_LOCA_1_DESC, y = n)) +
      geom_col(fill = COR_DESTAQUE) +
      coord_flip() +
      geom_text(aes(label = number(n, big.mark = ".")), hjust = -0.15, color = "#1a0033", size = 5) +
      scale_y_continuous(labels = number_format(big.mark = "."), limits = c(0, max(tab$n) * 1.25)) +
      tema_painel +
      theme(axis.title = element_blank())
  }, bg = "transparent")

  # ============================================================
  # GRÁFICO — quarta linha (evolução temporal)
  # ============================================================

  output$grafico_semanal <- renderPlot({
    df <- dados_municipio()
    if (nrow(df) == 0) return(grafico_vazio())

    # ano_sem/semana já vêm centralizados de base_agravo (herdados via
    # dados_municipio()); ver comentário na criação dessas colunas sobre o
    # porquê de não usar ANO_BASE aqui.
    df_atual <- df %>%
      filter(!is.na(semana), semana >= 1, semana <= 53, ano_sem == ano_padrao) %>%
      count(semana) %>%
      complete(semana = 1:53, fill = list(n = 0))

    if (length(ANOS_HISTORICOS) == 0) {
      return(
        ggplot(df_atual, aes(x = semana, y = n)) +
          geom_line(color = COR_DESTAQUE, linewidth = 1.2) +
          geom_point(color = COR_DESTAQUE, size = 2.5) +
          scale_x_continuous(breaks = seq(1, 53, 4)) +
          labs(x = "Semana epidemiológica", y = "Notificações") +
          tema_painel
      )
    }

    # canal endêmico: faixa mín-máx histórica (ANOS_HISTORICOS) desse
    # município específico, atrás da linha do ano corrente
    df_historico <- base_agravo %>%
      filter(municipio == municipio_atual(), ano_sem %in% ANOS_HISTORICOS,
             !is.na(semana), semana >= 1, semana <= 53) %>%
      count(ano_sem, semana) %>%
      complete(ano_sem = ANOS_HISTORICOS, semana = 1:53, fill = list(n = 0)) %>%
      group_by(semana) %>%
      summarise(minimo = min(n), mediana = median(n), maximo = max(n), .groups = "drop")

    ggplot() +
      geom_ribbon(data = df_historico, aes(x = semana, ymin = minimo, ymax = maximo),
                  fill = "gray70", alpha = 0.4) +
      geom_line(data = df_historico, aes(x = semana, y = mediana),
                color = "gray45", linewidth = 0.7, linetype = "dashed") +
      geom_line(data = df_atual, aes(x = semana, y = n), color = COR_DESTAQUE, linewidth = 1.2) +
      geom_point(data = df_atual, aes(x = semana, y = n), color = COR_DESTAQUE, size = 2.5) +
      scale_x_continuous(breaks = seq(1, 53, 4)) +
      labs(
        x = "Semana epidemiológica", y = "Notificações",
        caption = sprintf(
          "Faixa cinza: mín-máx %d-%d   |   - - -  mediana %d-%d   |   linha cheia: %d",
          min(ANOS_HISTORICOS), max(ANOS_HISTORICOS),
          min(ANOS_HISTORICOS), max(ANOS_HISTORICOS), ano_padrao
        )
      ) +
      tema_painel +
      theme(plot.caption = element_text(color = "#1a0033", size = 12, hjust = 0.5))
  }, bg = "transparent")
}

# ============================================================
# EXECUÇÃO
# ============================================================
shinyApp(ui = ui, server = server)
