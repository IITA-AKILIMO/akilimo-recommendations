@echo off
setlocal enabledelayedexpansion

REM --------------------------------------------------------------------------
REM  dev.bat  - hot-reload development server for akilimo-recommendations
REM
REM  Watches R/, api.R, net/*.css and data/input/translations.csv.
REM  Restarts Rscript api.R automatically whenever a watched file changes.
REM
REM  Requires watchexec:
REM    winget install watchexec.watchexec
REM    scoop install watchexec          (alternative)
REM    https://github.com/watchexec/watchexec/releases
REM --------------------------------------------------------------------------

REM Check watchexec is available
watchexec --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: watchexec not found.
    echo Install it with:
    echo   winget install watchexec.watchexec
    echo   scoop install watchexec
    echo   or download from https://github.com/watchexec/watchexec/releases
    exit /b 1
)

REM Check Rscript is on PATH
Rscript --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Rscript not found on PATH.
    exit /b 1
)

REM Change into project root so all paths are relative (avoids backslash issues)
if defined WORKDIR (
    cd /d "%WORKDIR%"
) else (
    cd /d "%~dp0"
)

REM The price database is kept between runs: on start it is migrated and
REM updated from the seed CSVs, keeping prices refreshed from the API.
REM To start from a clean database, delete data\input\akilimo_compute.sqlite.

echo Akilimo dev server - auto-restart enabled
echo Root    : %CD%
echo Watching: R\  api.R  net\  data\input\translations.csv
echo ------------------------------------------------------
echo.

REM Absolute watch paths: watchexec 2.7 on Windows fails with
REM "cannot find the file specified" for relative --watch paths
watchexec ^
  --watch "%CD%\R" ^
  --watch "%CD%\api.R" ^
  --watch "%CD%\.env" ^
  --watch "%CD%\net" ^
  --watch "%CD%\data\input\translations.csv" ^
  --exts R,csv,css ^
  --shell=none ^
  --restart ^
  --clear ^
  --debounce 1s ^
  -- Rscript api.R
