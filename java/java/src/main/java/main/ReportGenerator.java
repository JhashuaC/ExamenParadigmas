package main;
import java.io.*;
import java.util.*;

public class ReportGenerator {

    public static void generarReportes(List<MetricaColumna> metricas, List<ReglaCalidad> reglas) {
        try (PrintWriter txt = new PrintWriter(new FileWriter("out/reporte.txt"));
             PrintWriter csv = new PrintWriter(new FileWriter("out/reporte_resumen.csv"))) {

            csv.println("variable,estado");

            for (MetricaColumna m : metricas) {
                txt.println("Variable: " + m.getNombreVariable());
                for (ReglaCalidad r : reglas) {
                    String resultado = m.evaluarCalidad(r);
                    txt.println("  - " + resultado);
                    csv.println(m.getNombreVariable() + "," + resultado);
                }
                txt.println();
            }
        } catch (Exception e) {
            System.err.println("Error al generar reportes: " + e.getMessage());
        }
    }
}
