@echo off
echo =======================================
echo ===  Compilando y ejecutando Fortran ===
echo =======================================

:: Ir a la carpeta fortran
cd fortran

:: Compilar el programa de estadísticas
gfortran compute_stats.f90 -o ..\compute_stats.exe

:: Ejecutar el binario (input.csv -> stats.csv + errors.log)
..\compute_stats.exe input.csv stats.csv errors.log

:: Volver a la carpeta raíz
cd ..

echo ===================================
echo ===  Ejecutando Java con Maven  ===
echo ===================================

cd java\java

:: Ejecutar la clase principal de Java con Maven
mvn exec:java -Dexec.mainClass="main.Main" -Dexec.args="..\..\fortran\stats.csv ..\..\fortran\rules.json"

cd ..\..

echo ================================
echo ===  Pipeline Finalizado 😀  ===
echo ================================
pause
