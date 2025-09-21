
package main;
import java.util.*;

public class Main {
    public static void main(String[] args) {
        // 1. Leer métricas desde stats.csv
        List<MetricaColumna> metricas = CSVReader.leerStats("out/stats.csv");

        // 2. Leer reglas desde rules.json
        List<ReglaCalidad> reglas = JSONRulesReader.leerReglas("in/rules.json");

        // 3. Generar reportes
        ReportGenerator.generarReportes(metricas, reglas);

        System.out.println("Reportes generados en carpeta out/");
    }
}
