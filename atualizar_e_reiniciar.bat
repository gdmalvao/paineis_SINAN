@echo off
REM ============================================================
REM Roda o ETL (baixa + trata + salva) e, SE E SOMENTE SE deu
REM certo, reinicia o painel para ele carregar o .rds novo.
REM Se o ETL falhar, o painel continua rodando com o dado antigo
REM (nada e derrubado) — confira dados\etl_log.txt para ver o erro.
REM ============================================================

set RSCRIPT="C:\PROGRA~1\R\R-45~1.2\bin\x64\Rscript.exe"
set PASTA_APP=C:\Users\gdmal\OneDrive\Desktop\05_SINAN\painel

cd /d %PASTA_APP%
%RSCRIPT% etl_animais.R

if %ERRORLEVEL% EQU 0 (
    echo ETL OK — reiniciando o painel...

    REM fecha o Shiny e o Chrome em modo kiosk atuais
    taskkill /FI "WINDOWTITLE eq painel_shiny*" /T /F >nul 2>&1
    taskkill /IM chrome.exe /F >nul 2>&1

    REM aguarda tudo fechar de fato antes de subir de novo
    timeout /t 3 /nobreak

    call "%PASTA_APP%\iniciar_painel.bat"
) else (
    echo ETL FALHOU — painel NAO foi reiniciado. Veja dados\etl_log.txt
)
