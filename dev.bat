@echo off
setlocal enabledelayedexpansion

REM ──────────────────────────────────────────────────────────────────────────
REM  dev.bat  — hot-reload development server for akilimo-recommendations
REM
REM  Watches R/, api.R, net/*.css and data/input/translations.csv.
REM  Restarts Rscript api.R automatically whenever a watched file changes.
REM
REM  Requires watchexec:
REM    winget install watchexec.watchexec
REM    scoop install watchexec          (alternative)
REM    https://github.com/watchexec/watchexec/releases
REM ──────────────────────────────────────────────────────────────────────────

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

echo Akilimo dev server — auto-restart enabled
echo Root    : %CD%
echo Watching: R\  api.R  net\  data\input\translations.csv
echo ──────────────────────────────────────────────────────
echo.

watchexec ^
  --watch R ^
  --watch api.R ^
  --watch .env ^
  --watch net ^
  --watch data\input\translations.csv ^
  --exts R,csv,css ^
  --shell=none ^
  --restart ^
  --clear ^
  --debounce 1000 ^
  -- Rscript api.R
