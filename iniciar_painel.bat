@echo off
REM ============================================================
REM Inicia o painel: sobe o Shiny em background numa porta fixa
REM e abre o Chrome em modo kiosk apontando pra ela.
REM ============================================================

set RSCRIPT="C:\PROGRA~1\R\R-45~1.2\bin\x64\Rscript.exe"
set PASTA_APP=C:\Users\gdmal\OneDrive\Desktop\05_SINAN\painel
set PORTA=3838
set CHROME="C:\Program Files\Google\Chrome\Application\chrome.exe"

REM sobe o Shiny em background (janela minimizada, sem abrir navegador sozinho)
start "painel_shiny" /min %RSCRIPT% -e "shiny::runApp('%PASTA_APP%', host='127.0.0.1', port=%PORTA%, launch.browser=FALSE)"

REM espera o Shiny responder de verdade antes de abrir o navegador, em vez
REM de um tempo fixo — a base e grande (1,27 milhao de linhas + shapefile)
REM e o tempo de carregamento varia (mais lento na primeira vez do dia).
REM Tenta a cada 3s, por ate 3 minutos (60 tentativas).
set MAX_TENTATIVAS=60
set /a TENTATIVA=0

:esperar_shiny
set /a TENTATIVA+=1
powershell -Command "try { (New-Object Net.Sockets.TcpClient).Connect('127.0.0.1', %PORTA%); exit 0 } catch { exit 1 }" >nul 2>&1
if %ERRORLEVEL% EQU 0 goto shiny_pronto
if %TENTATIVA% GEQ %MAX_TENTATIVAS% (
    echo AVISO: Shiny nao respondeu apos 3 minutos. Abrindo mesmo assim...
    goto shiny_pronto
)
timeout /t 3 /nobreak >nul
goto esperar_shiny

:shiny_pronto
REM folga extra pro Shiny terminar de montar a primeira pagina depois que a
REM porta ja esta aceitando conexao (a porta abre um pouco antes da pagina
REM estar 100% pronta)
timeout /t 3 /nobreak >nul

REM abre em modo kiosk (tela cheia, sem barra de endereco).
REM --user-data-dir cria um PERFIL SEPARADO do Chrome, so pro painel — isso
REM garante que o --kiosk sempre funcione de verdade, mesmo que seu Chrome
REM pessoal ja esteja aberto (senao o Chrome ignora --kiosk e so abre uma
REM aba na janela normal existente, que foi o que aconteceu no seu teste).
set PERFIL_KIOSK=%PASTA_APP%\chrome_kiosk_profile
start "" %CHROME% --kiosk --new-window --user-data-dir="%PERFIL_KIOSK%" --no-first-run --disable-session-crashed-bubble --disable-infobars http://127.0.0.1:%PORTA%
