@echo off
setlocal enabledelayedexpansion

REM ==========================================================================
REM  Akilimo Recommendation Engine - full environment setup (Windows)
REM
REM  Windows equivalent of setup.sh. Checks the tools you must install
REM  yourself, installs WeasyPrint, R packages and Poetry, creates the .env
REM  files and downloads the runtime data from OSF (default) or Zenodo.
REM
REM  Usage:
REM    setup.bat               full setup (tools check + R + Python + data)
REM    setup.bat --no-data     skip the data download
REM    setup.bat --no-system   skip the tool checks and the WeasyPrint install
REM
REM  setup.sh installs system libraries with apt. Here, missing tools (R,
REM  Rtools, Python, Java, the GTK3 runtime for WeasyPrint) are installed
REM  with winget. If winget is missing, install them by hand. A fresh
REM  Python install needs a new terminal before it is on PATH.
REM ==========================================================================

set "INSTALL_SYSTEM=1"
set "DOWNLOAD_DATA=1"

:parse_args
if "%~1"=="" goto args_done
if /i "%~1"=="--no-system" set "INSTALL_SYSTEM=0"
if /i "%~1"=="--no-data"   set "DOWNLOAD_DATA=0"
if /i "%~1"=="--help" goto usage
if /i "%~1"=="-h"     goto usage
shift
goto parse_args

:usage
echo Usage: %~nx0 [--no-system] [--no-data]
exit /b 0

:args_done

REM Work from the project root, wherever the script is started from
cd /d "%~dp0"
set "ROOT=%CD%"

REM --------------------------------------------------------------------------
REM  1. Tools and WeasyPrint
REM --------------------------------------------------------------------------

if "%INSTALL_SYSTEM%"=="0" (
    echo [INFO]  Skipping tool checks and WeasyPrint install ^(--no-system^).
    goto r_packages
)

echo [INFO]  Checking required tools...

set "HAVE_WINGET=1"
where winget >nul 2>&1
if errorlevel 1 set "HAVE_WINGET=0"

REM R -------------------------------------------------------------------------
where Rscript >nul 2>&1
if errorlevel 1 (
    if "!HAVE_WINGET!"=="0" (
        echo [ERROR] Rscript not found and winget is not available. Install R from https://cran.r-project.org 1>&2
        exit /b 1
    )
    echo [INFO]  Installing R and Rtools with winget...
    winget install --id RProject.R -e --silent --accept-package-agreements --accept-source-agreements
    winget install --id RProject.Rtools -e --silent --accept-package-agreements --accept-source-agreements
    call :add_r_to_path
    where Rscript >nul 2>&1
    if errorlevel 1 (
        echo [ERROR] R was installed but Rscript is not on PATH yet. Open a new terminal and run setup.bat again. 1>&2
        exit /b 1
    )
)

REM Python --------------------------------------------------------------------
where python >nul 2>&1
if errorlevel 1 (
    if "!HAVE_WINGET!"=="0" (
        echo [ERROR] python not found and winget is not available. Install Python 3.10+ from https://www.python.org 1>&2
        exit /b 1
    )
    echo [INFO]  Installing Python with winget...
    winget install --id Python.Python.3.12 -e --silent --accept-package-agreements --accept-source-agreements
    echo [ERROR] Python was installed. Open a new terminal so PATH updates, then run setup.bat again. 1>&2
    exit /b 1
)

REM Java (older mail code; setup.sh installs openjdk-21) ------------------------
where java >nul 2>&1
if errorlevel 1 (
    if "!HAVE_WINGET!"=="1" (
        echo [INFO]  Installing Java 21 with winget...
        winget install --id Microsoft.OpenJDK.21 -e --silent --accept-package-agreements --accept-source-agreements
    ) else (
        echo [WARN]  Java not found; install OpenJDK 21 if you need the old mail code. 1>&2
    )
)

REM WeasyPrint and its GTK3 runtime -----------------------------------------
echo [INFO]  Installing WeasyPrint ^(latest, from requirements.txt^)...
python -m pip install --disable-pip-version-check --user -r "%ROOT%\requirements.txt"
if errorlevel 1 (
    echo [ERROR] Could not install WeasyPrint. 1>&2
    exit /b 1
)
where weasyprint >nul 2>&1
if errorlevel 1 (
    echo [WARN]  'weasyprint' is not on PATH yet; add the pip scripts folder to PATH. 1>&2
) else (
    echo [OK]    WeasyPrint ready.
)

where libgobject-2.0-0.dll >nul 2>&1
if errorlevel 1 (
    if "!HAVE_WINGET!"=="1" (
        echo [INFO]  Installing the GTK3 runtime WeasyPrint needs...
        winget install --id tschoonj.GTKForWindows -e --silent --accept-package-agreements --accept-source-agreements
        if errorlevel 1 (
            echo [WARN]  GTK3 install failed. See https://doc.courtbouillon.org/weasyprint/stable/first_steps.html#windows 1>&2
        )
    ) else (
        echo [WARN]  GTK3 runtime not found. See https://doc.courtbouillon.org/weasyprint/stable/first_steps.html#windows 1>&2
    )
)

goto r_packages

:add_r_to_path
REM Put the newest installed R on PATH for this window only
for /d %%D in ("%ProgramFiles%\R\R-*") do set "R_BIN=%%~fD\bin"
if defined R_BIN set "PATH=!R_BIN!;%PATH%"
exit /b 0

:r_packages

REM --------------------------------------------------------------------------
REM  2. R packages
REM --------------------------------------------------------------------------

echo [INFO]  Installing R packages...
where Rscript >nul 2>&1
if errorlevel 1 (
    echo [ERROR] 'Rscript' is not installed or not in PATH. 1>&2
    exit /b 1
)

Rscript "%ROOT%\install_packages.R"
if errorlevel 1 (
    echo [ERROR] R package installation failed. 1>&2
    exit /b 1
)
echo [OK]    R packages ready.

REM R API environment file
if not exist "%ROOT%\.env" (
    copy /y "%ROOT%\.env.example" "%ROOT%\.env" >nul
    echo [INFO]  Created .env from .env.example - edit it to set AKILIMO_ROOT, Plivo, and email credentials.
)

REM --------------------------------------------------------------------------
REM  3. Python / Poetry setup
REM --------------------------------------------------------------------------

echo [INFO]  Setting up Python data scripts...

where poetry >nul 2>&1
if errorlevel 1 (
    echo [WARN]  Poetry not found. Installing with pip... 1>&2
    python -m pip install --disable-pip-version-check --user poetry
    if errorlevel 1 (
        echo [ERROR] Could not install Poetry. 1>&2
        exit /b 1
    )
)

REM pip --user puts poetry.exe in a Scripts folder that may not be on PATH yet;
REM "python -m poetry" works either way, so use it when poetry is not found.
set "POETRY=poetry"
where poetry >nul 2>&1
if errorlevel 1 set "POETRY=python -m poetry"

pushd "%ROOT%\scripts"

if not exist ".env" (
    copy /y ".env.example" ".env" >nul
    echo [INFO]  Created scripts\.env from .env.example.
    echo [INFO]  Edit scripts\.env to set OSF_NODE_ID ^(OSF^) or ZENODO_RECORD_ID ^(Zenodo^).
)

%POETRY% install --no-interaction
if errorlevel 1 (
    popd
    echo [ERROR] poetry install failed. 1>&2
    exit /b 1
)
echo [OK]    Python dependencies installed.

REM --------------------------------------------------------------------------
REM  4. Download runtime data
REM --------------------------------------------------------------------------

if "%DOWNLOAD_DATA%"=="1" (
    echo [INFO]  Downloading runtime data ^(OSF by default; use --source zenodo to switch^)...
    %POETRY% run setup-data
    if errorlevel 1 (
        popd
        echo [ERROR] Data download failed. 1>&2
        exit /b 1
    )
    echo [OK]    Runtime data downloaded and extracted.
) else (
    echo [INFO]  Skipping data download ^(--no-data^).
    echo [INFO]  Run later: cd scripts ^&^& poetry run setup-data
)

popd

REM --------------------------------------------------------------------------
REM  Done
REM --------------------------------------------------------------------------

echo.
echo ======================================================================
echo   Setup complete.
echo.
echo   Start the API:
echo     Rscript api.R          ^(or dev.bat for hot-reload^)
echo.
echo   Run tests:
echo     Rscript tests/test_small.R
echo ======================================================================

endlocal
exit /b 0
