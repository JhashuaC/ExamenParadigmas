@echo off
setlocal

set start=%time%
echo ========================================
echo ===   Iniciando Pipeline Fortran+Java ===
echo ========================================
echo Inicio: %start%

echo ========================================
echo ===  Compilando y ejecutando Fortran ===
echo ========================================

cd fortran
:: borrar el exe viejo si existe (evita "Acceso denegado")
if exist ..\compute_stats.exe del ..\compute_stats.exe
gfortran compute_stats.f90 -o ..\compute_stats.exe
..\compute_stats.exe input.csv stats.csv errors.log
cd ..

echo ===================================
echo ===  Ejecutando Java con Maven  ===
echo ===================================

cd java\java
mvn exec:java -Dexec.mainClass="main.Main" -Dexec.args="..\..\fortran\stats.csv in\rules.json"
cd ..\..

set end=%time%
echo Fin: %end%

:: Calcular duración en segundos usando PowerShell
:: Calcular duración en formato mm:ss usando PowerShell
for /f "tokens=* usebackq" %%a in (`powershell -NoProfile -Command ^
    "$d=(New-TimeSpan -Start '%start%' -End '%end%'); '{0:D2}:{1:D2}' -f $d.Minutes,$d.Seconds"`) do set duration=%%a

echo ================================
echo ===  Pipeline Finalizado     ===
echo ===  Tiempo total: %duration% min:seg
echo ================================


pause
endlocal
