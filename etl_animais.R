# ============================================================
# ETL — Acidentes por Animais Peçonhentos (SINAN)
# Baixa os dados preliminares, aplica os tratamentos, marca a
# RIDE-DF e salva o .rds final que o painel lê.
#
# Pensado para rodar sozinho, sem supervisão, via Agendador de
# Tarefas do Windows. Por isso:
#   - tudo relevante vai pro log (dados/etl_log.txt)
#   - a gravação final é ATÔMICA (grava num arquivo temporário e
#     só troca pelo definitivo se TUDO deu certo — assim o painel
#     nunca lê um .rds pela metade)
#   - erro em UM ano de download não derruba o processo inteiro
# ============================================================

library(read.dbc)
library(dplyr)
library(purrr)
library(curl)
library(lubridate)
library(sidrar)
library(httr)
library(jsonlite)
library(stringr)

# ---- Parâmetros ----
DIRETORIO_DADOS   <- "dados"
ARQUIVO_FINAL     <- file.path(DIRETORIO_DADOS, "animais_tratado.rds")
ARQUIVO_LOG       <- file.path(DIRETORIO_DADOS, "etl_log.txt")
ANOS_SINAN        <- 2023:2026   # anos de notificação baixados do SINAN
ANO_POPULACAO     <- 2025        # ano da estimativa populacional (IBGE/SIDRA)

if (!dir.exists(DIRETORIO_DADOS)) dir.create(DIRETORIO_DADOS, recursive = TRUE)

# ---- Log simples em arquivo (mantém histórico entre execuções) ----
log_msg <- function(...) {
  linha <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(...))
  cat(linha, "\n")
  cat(linha, "\n", file = ARQUIVO_LOG, append = TRUE)
}

log_msg("========== INÍCIO DO ETL ==========")

resultado_etl <- tryCatch({

  options(timeout = 600)

  # ---- 1. Download SINAN (preliminar) ----
  # Anos "fechados" (mais antigos que o ano anterior) praticamente não mudam
  # mais — não faz sentido baixar de novo todo mês. Só forçamos o download
  # do ano atual e do anterior (notificação atrasada pode cair no anterior).
  # Para os demais, se já existe um .dbc local, reaproveita; se não existe
  # (primeira execução), baixa também.
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  anos_sempre_baixar <- c(ano_atual - 1, ano_atual)

  baixar_sinan_prelim <- function(agravo, ano, diretorio = file.path(DIRETORIO_DADOS, "sinan")) {
    ano2 <- substr(as.character(ano), 3, 4)
    arquivo <- paste0(agravo, "BR", ano2, ".dbc")
    if (!dir.exists(diretorio)) dir.create(diretorio, recursive = TRUE)
    destino <- file.path(diretorio, arquivo)

    precisa_baixar <- (ano %in% anos_sempre_baixar) || !file.exists(destino)

    if (precisa_baixar) {
      url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/", arquivo)
      log_msg("Baixando: ", arquivo, if (ano %in% anos_sempre_baixar) " (ano corrente/anterior)" else " (sem cache local)")
      erro_download <- tryCatch({
        curl_download(url = url, destfile = destino)
        FALSE
      }, error = function(e) {
        log_msg("AVISO — falha ao baixar ", arquivo, ": ", conditionMessage(e))
        TRUE
      })
      if (erro_download && !file.exists(destino)) {
        return(NULL)  # nunca teve esse arquivo, nem o antigo — pula o ano
      }
      if (erro_download) {
        log_msg("Usando cache local de ", arquivo, " (download falhou, mas já existia)")
      }
    } else {
      log_msg("Reaproveitando cache local: ", arquivo, " (ano fechado, já baixado antes)")
    }

    tryCatch({
      read.dbc(destino) %>% mutate(ANO_BASE = ano)
    }, error = function(e) {
      log_msg("AVISO — erro ao ler ", arquivo, ": ", conditionMessage(e))
      NULL
    })
  }

  animais <- map_dfr(ANOS_SINAN, \(ano) baixar_sinan_prelim(agravo = "ANIM", ano = ano))

  if (nrow(animais) == 0) {
    stop("Nenhum arquivo do SINAN foi baixado com sucesso. Abortando ETL sem tocar no .rds atual.")
  }
  log_msg("Download concluído: ", nrow(animais), " registros brutos.")

  # ---- 2. Seleção de variáveis ----
  animais_sel <- animais %>%
    select(
      "DT_NOTIFIC", "DT_SIN_PRI", "SEM_NOT", "ANT_DT_ACI", "ANT_UF", "ANT_MUNIC_",
      "SG_UF", "ID_MN_RESI", "CS_SEXO", "CS_RACA", "NU_IDADE_N", "ANT_TEMPO_",
      "ANT_LOCA_1", "TP_ACIDENT", "TRA_CLASSI", "DOENCA_TRA", "COM_CHOQUE",
      "EVOLUCAO", "DT_OBITO", "ANO_BASE"
    )

  # ---- 3. Dicionários ----
  dic_tp_acidente <- tibble(
    TP_ACIDENT = c("1", "2", "3", "4", "5", "6", "9"),
    TP_ACIDENT_DESC = c("Serpente", "Aranha", "Escorpiao", "Lagarta", "Abelha", "Outros", "Ignorado")
  )
  dic_classificacao <- tibble(
    TRA_CLASSI = c("1", "2", "3", "9"),
    TRA_CLASSI_DESC = c("Leve", "Moderado", "Grave", "Ignorado")
  )
  dic_evolucao <- tibble(
    EVOLUCAO = c("1", "2", "3", "9"),
    EVOLUCAO_DESC = c("Cura", "Obito pelo agravo", "Obito por outra causa", "Ignorado")
  )
  dic_tempo <- tibble(
    ANT_TEMPO_ = c("1", "2", "3", "4", "5", "6", "9"),
    ANT_TEMPO_DESC = c("0 a 1 hora", "1 a 3 horas", "3 a 6 horas", "6 a 12 horas",
                        "12 a 24 horas", "24 horas ou mais", "Ignorado")
  )
  dic_sexo <- tibble(
    CS_SEXO = c("M", "F", "I"),
    CS_SEXO_DESC = c("Masculino", "Feminino", "Ignorado")
  )
  dic_doenca_trabalho <- tibble(
    DOENCA_TRA = c("1", "2", "9"),
    DOENCA_TRA_DESC = c("Sim", "Nao", "Ignorado")
  )
  dic_choque <- tibble(
    COM_CHOQUE = c("1", "2", "9"),
    COM_CHOQUE_DESC = c("Sim", "Nao", "Ignorado")
  )
  dic_raca <- tibble(
    CS_RACA = c("1", "2", "3", "4", "5", "9"),
    CS_RACA_DESC = c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Ignorado")
  )
  dic_local_picada <- tibble(
    ANT_LOCA_1 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "99"),
    ANT_LOCA_1_DESC = c("Cabeca", "Tronco", "Braco", "Antebraco", "Mao", "Dedo da mao",
                         "Coxa", "Perna", "Pe", "Dedo do pe", "Ignorado")
  )

  # ---- 4. Enriquecimento ----
  animais_tratado <- animais_sel %>%
    mutate(
      TP_ACIDENT = as.character(TP_ACIDENT), TRA_CLASSI = as.character(TRA_CLASSI),
      EVOLUCAO = as.character(EVOLUCAO), ANT_TEMPO_ = as.character(ANT_TEMPO_),
      ANT_LOCA_1 = as.character(ANT_LOCA_1), CS_SEXO = as.character(CS_SEXO),
      CS_RACA = as.character(CS_RACA), DOENCA_TRA = as.character(DOENCA_TRA),
      COM_CHOQUE = as.character(COM_CHOQUE)
    ) %>%
    left_join(dic_tp_acidente, by = "TP_ACIDENT") %>%
    left_join(dic_classificacao, by = "TRA_CLASSI") %>%
    left_join(dic_evolucao, by = "EVOLUCAO") %>%
    left_join(dic_tempo, by = "ANT_TEMPO_") %>%
    left_join(dic_sexo, by = "CS_SEXO") %>%
    left_join(dic_doenca_trabalho, by = "DOENCA_TRA") %>%
    left_join(dic_choque, by = "COM_CHOQUE") %>%
    left_join(dic_raca, by = "CS_RACA") %>%
    left_join(dic_local_picada, by = "ANT_LOCA_1") %>%
    mutate(
      idade_anos = case_when(
        NU_IDADE_N >= 4000 & NU_IDADE_N < 5000 ~ as.numeric(NU_IDADE_N - 4000),
        NU_IDADE_N >= 3000 & NU_IDADE_N < 4000 ~ round((NU_IDADE_N - 3000) / 12, 2),
        NU_IDADE_N >= 2000 & NU_IDADE_N < 3000 ~ round((NU_IDADE_N - 2000) / 365, 2),
        NU_IDADE_N >= 1000 & NU_IDADE_N < 2000 ~ round((NU_IDADE_N - 1000) / (24 * 365), 4),
        TRUE ~ NA_real_
      ),
      obito_agravo = case_when(
        EVOLUCAO == "2" ~ TRUE,
        EVOLUCAO %in% c("1", "3", "9") ~ FALSE,
        TRUE ~ NA
      ),
      grave = case_when(
        TRA_CLASSI == "3" ~ TRUE,
        TRA_CLASSI %in% c("1", "2", "9") ~ FALSE,
        TRUE ~ NA
      )
    )

  log_msg("Tratamento/enriquecimento concluído: ", nrow(animais_tratado), " registros.")

  # ---- 5. População (SIDRA/IBGE) ----
  pop <- get_sidra(api = paste0("/t/6579/n6/all/v/9324/p/", ANO_POPULACAO))

  pop_tratada <- pop %>%
    transmute(
      cod_municipio = as.numeric(substr(`Município (Código)`, 1, 6)),
      municipio = Município,
      populacao = Valor
    )

  animais_tratado <- animais_tratado %>%
    mutate(
      cod_municipio_sinan = as.numeric(as.character(ID_MN_RESI)),
      cod_municipio = case_when(
        cod_municipio_sinan >= 539900 & cod_municipio_sinan <= 539999 ~ 530010,
        TRUE ~ cod_municipio_sinan
      )
    ) %>%
    left_join(pop_tratada, by = "cod_municipio") %>%
    mutate(
      faixa_etaria = case_when(
        idade_anos < 10 ~ "0-9", idade_anos < 20 ~ "10-19", idade_anos < 30 ~ "20-29",
        idade_anos < 40 ~ "30-39", idade_anos < 50 ~ "40-49", idade_anos < 60 ~ "50-59",
        idade_anos < 70 ~ "60-69", idade_anos < 80 ~ "70-79", idade_anos < 90 ~ "80-89",
        idade_anos >= 90 ~ "90+", TRUE ~ NA_character_
      ),
      faixa_etaria = factor(faixa_etaria, levels = c(
        "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"
      ))
    )

  log_msg("População (ref. ", ANO_POPULACAO, ") anexada: ",
          sum(!is.na(animais_tratado$populacao)), " de ", nrow(animais_tratado), " linhas com valor.")

  # ---- 6. RIDE-DF via API do IBGE ----
  ride_raw <- fromJSON(
    "https://servicodados.ibge.gov.br/api/v1/localidades/regioes-integradas-de-desenvolvimento",
    flatten = FALSE
  )
  ride_df_idx <- which(str_detect(ride_raw$nome, regex("Distrito Federal", ignore_case = TRUE)))
  if (length(ride_df_idx) == 0) stop("RIDE-DF não encontrada na API do IBGE.")

  municipios_ride_df <- ride_raw$municipios[[ride_df_idx]]
  cod_sinan_ride_df <- as.numeric(substr(as.character(municipios_ride_df$id), 1, 6))

  animais_tratado <- animais_tratado %>%
    mutate(ride_df = cod_municipio %in% cod_sinan_ride_df)

  n_municipios_ride <- animais_tratado %>% filter(ride_df) %>% distinct(municipio) %>% nrow()
  log_msg("RIDE-DF: ", n_municipios_ride, " municípios presentes na base (de ",
          length(cod_sinan_ride_df), " oficiais segundo o IBGE).")

  # ---- 7. Checagens mínimas antes de salvar ----
  if (nrow(animais_tratado) < 1000) {
    stop("A base tratada ficou suspeitosamente pequena (< 1000 linhas). Abortando gravação.")
  }
  if (n_municipios_ride == 0) {
    stop("Nenhum município da RIDE-DF encontrado na base tratada. Abortando gravação.")
  }

  # ---- 8. Gravação ATÔMICA ----
  # grava num arquivo temporário e só substitui o definitivo se der tudo certo,
  # assim o painel (que pode estar lendo o .rds a qualquer momento) nunca
  # encontra um arquivo pela metade
  arquivo_temp <- paste0(ARQUIVO_FINAL, ".tmp")
  saveRDS(animais_tratado, arquivo_temp)
  file.rename(arquivo_temp, ARQUIVO_FINAL)

  log_msg("Arquivo final salvo com sucesso em: ", ARQUIVO_FINAL)
  TRUE

}, error = function(e) {
  log_msg("ERRO — ETL abortado: ", conditionMessage(e))
  log_msg("O .rds anterior NÃO foi alterado (painel continua funcionando com o dado antigo).")
  FALSE
})

log_msg("========== FIM DO ETL (sucesso = ", resultado_etl, ") ==========")

# código de saída pro Windows saber se deu certo (usado no .bat de agendamento)
quit(status = if (isTRUE(resultado_etl)) 0 else 1, save = "no")
