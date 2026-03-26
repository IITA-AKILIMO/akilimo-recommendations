@echo off
setlocal enabledelayedexpansion

REM Try common Rscript locations
set RSCRIPT_CMD=

for %%C in (
    "C:\Program Files\R\bin\Rscript.exe"
    "C:\Program Files\R\R-current\bin\Rscript.exe"
    "C:\R\bin\Rscript.exe"
) do (
    if exist %%~C (
        set RSCRIPT_CMD=%%~C
        goto :found
    )
)

REM Try PATH lookup
for /f "usebackq tokens=*" %%P in (`where Rscript.exe 2^>nul`) do (
    if exist %%P (
        set RSCRIPT_CMD=%%P
        goto :found
    )
)

REM If still not found, assume Rscript.exe is in the same folder as api.R
set SCRIPT_DIR=%~dp0
if exist "%SCRIPT_DIR%Rscript.exe" (
    set RSCRIPT_CMD=%SCRIPT_DIR%Rscript.exe
    goto :found
)

echo ERROR: Rscript not found
exit /b 1

:found
REM Use WORKDIR env variable if set, otherwise default to script location
if defined WORKDIR (
    set TARGET_DIR=%WORKDIR%
) else (
    set TARGET_DIR=%~dp0
)

echo Using Rscript at: %RSCRIPT_CMD%
echo Working directory: %TARGET_DIR%
"%RSCRIPT_CMD%" "%TARGET_DIR%api.R"