package main;

import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.List;

import metricas.MetricaColumna;
import reglas.ReglaCalidad;

/**
 * Clase encargada de generar los reportes de salida
 * a partir de las métricas estadísticas y las reglas
 * de calidad aplicadas.
 *
 * Produce dos archivos de salida:
 * <ul>
 *   <li><b>reporte.txt</b>: descripción detallada de la evaluación.</li>
 *   <li><b>reporte_resumen.csv</b>: estado de cada variable en formato CSV.</li>
 * </ul>
 *
 * Estos reportes permiten interpretar de manera textual
 * y resumida el cumplimiento o incumplimiento de las reglas
 * sobre cada métrica procesada.
 */
public class ReportGenerator {

    /**
     * Genera los reportes de salida en formato TXT y CSV
     * a partir de una lista de métricas y de reglas de calidad.
     *
     * @param metricas Lista de métricas calculadas que representan las variables estadísticas.
     * @param reglas Lista de reglas de calidad que serán aplicadas a cada métrica.
     */
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
