# ============================================================
# CONFIGURAÇÃO INICIAL DO GITHUB — rodar 1x só, manualmente
# Conecta a pasta do projeto ao repositório e sobe os arquivos
# pela primeira vez (código + dados).
# ============================================================

library(gert)

PASTA_PROJETO <- "C:/Users/gdmal/OneDrive/Desktop/05_SINAN/painel"
REPO_URL      <- "https://github.com/gdmalvao/paineis_SINAN.git"

# ---- Confirma que o token está disponível ----
if (Sys.getenv("GITHUB_PAT") == "") {
  stop(
    "GITHUB_PAT não está configurado. Rode usethis::edit_r_environ(), ",
    "adicione a linha GITHUB_PAT=seu_token, salve e reinicie o R antes ",
    "de rodar este script."
  )
}

# ---- .gitignore: evita subir lixo/cache pro repositório ----
gitignore_path <- file.path(PASTA_PROJETO, ".gitignore")
writeLines(
  c(
    "chrome_kiosk_profile/",  # perfil do Chrome kiosk (criado pelo iniciar_painel.bat)
    "dados/sinan/",           # .dbc brutos baixados do DATASUS (cache, recriado a cada ETL)
    "dados/etl_log.txt",      # log local, não precisa versionar
    ".Rproj.user/",           # estado interno do RStudio
    ".Rhistory",
    ".RData"
  ),
  gitignore_path
)
message("Criado: ", gitignore_path)

# ---- Inicializa o repositório local (se ainda não for um) ----
if (!dir.exists(file.path(PASTA_PROJETO, ".git"))) {
  git_init(PASTA_PROJETO)
  message("Repositório git inicializado em: ", PASTA_PROJETO)
} else {
  message("Pasta já é um repositório git, pulando git_init().")
}

# ---- Configura nome/email do autor dos commits (exigido pelo git) ----
# Troque pelo seu nome/e-mail se quiser que os commits fiquem com essa
# identificação (não precisa ser um e-mail real de verdade).
git_config_set("user.name", "Painel SINAN ETL", repo = PASTA_PROJETO)
git_config_set("user.email", "painel-etl@local", repo = PASTA_PROJETO)

# ---- Conecta ao repositório remoto (se ainda não estiver conectado) ----
remotes_atuais <- git_remote_list(repo = PASTA_PROJETO)
if (!"origin" %in% remotes_atuais$name) {
  git_remote_add(url = REPO_URL, name = "origin", repo = PASTA_PROJETO)
  message("Remote 'origin' adicionado: ", REPO_URL)
} else {
  message("Remote 'origin' já existe, pulando.")
}

# ---- Adiciona tudo (respeitando o .gitignore) e faz o primeiro commit ----
git_add(".", repo = PASTA_PROJETO)
status <- git_status(repo = PASTA_PROJETO)

if (nrow(status) > 0) {
  git_commit(
    "Configuração inicial do painel + dados",
    repo = PASTA_PROJETO
  )
  message("Commit criado com ", nrow(status), " arquivo(s).")
} else {
  message("Nada novo pra commitar.")
}

# ---- Envia pro GitHub ----
# Se o repositório no GitHub foi criado com README/licença (não vazio),
# isso pode dar conflito de "histórias não relacionadas" — nesse caso,
# me avise que ajusto o comando.
git_push(remote = "origin", repo = PASTA_PROJETO)

message("Pronto! Confira em: https://github.com/gdmalvao/paineis_SINAN")
