# ============================================================
# ETL — Shapefile dos municípios da RIDE-DF
# Roda raramente (malha municipal do IBGE quase não muda) —
# NÃO faz parte do agendamento mensal do etl_animais.R.
# ============================================================

library(geobr)
library(dplyr)
library(sf)
library(httr)
library(jsonlite)
library(stringr)
library(rmapshaper)  # para simplificar a geometria (arquivo menor, render mais rápido)

DIRETORIO_DADOS <- "dados"
ARQUIVO_SAIDA   <- file.path(DIRETORIO_DADOS, "shapefile_ride.rds")

# ---- 1. Lista oficial de municípios da RIDE-DF (mesma fonte do app.R) ----
ride_raw <- fromJSON(
  "https://servicodados.ibge.gov.br/api/v1/localidades/regioes-integradas-de-desenvolvimento",
  flatten = FALSE
)
ride_df_idx <- which(str_detect(ride_raw$nome, regex("Distrito Federal", ignore_case = TRUE)))
municipios_ride_df <- ride_raw$municipios[[ride_df_idx]]

cod_ibge_ride_df  <- as.numeric(municipios_ride_df$id)          # 7 dígitos (IBGE)
cod_sinan_ride_df <- as.numeric(substr(as.character(cod_ibge_ride_df), 1, 6))  # 6 dígitos (bate com cod_municipio)

# ---- 2. Baixa a malha municipal (Goiás + Minas Gerais + DF) via geobr ----
# geobr usa código IBGE de 7 dígitos (code_muni)
malha_go <- read_municipality(code_muni = "GO", year = 2022)
malha_mg <- read_municipality(code_muni = "MG", year = 2022)
malha_df <- read_municipality(code_muni = "DF", year = 2022)

malha_completa <- bind_rows(malha_go, malha_mg, malha_df)

# ---- 3. Filtra só os municípios da RIDE-DF e junta o código de 6 dígitos ----
shapefile_ride <- malha_completa %>%
  filter(code_muni %in% cod_ibge_ride_df) %>%
  mutate(cod_municipio = as.numeric(substr(as.character(code_muni), 1, 6))) %>%
  select(cod_municipio, name_muni, geom)

# ---- 4. Simplifica a geometria (menos vértices = render bem mais rápido numa TV) ----
shapefile_ride <- ms_simplify(shapefile_ride, keep = 0.05, keep_shapes = TRUE)

message(
  "Shapefile RIDE-DF: ", nrow(shapefile_ride), " municípios encontrados de ",
  length(cod_ibge_ride_df), " oficiais."
)

# ---- 5. Salva ----
if (!dir.exists(DIRETORIO_DADOS)) dir.create(DIRETORIO_DADOS, recursive = TRUE)
saveRDS(shapefile_ride, ARQUIVO_SAIDA)

message("Salvo em: ", ARQUIVO_SAIDA)
