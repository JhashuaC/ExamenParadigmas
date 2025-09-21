package main;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Clase encargada de leer el archivo CSV generado por el programa en Fortran
 * y convertir sus filas en objetos {@link MetricaColumna}.
 *
 * El archivo debe tener la siguiente estructura:
 * 
 * variable,media,mediana,desvio,outliers
 * col1,49.9663,49.7895,2.9672,0
 * col2,54.8463,54.3795,25.6384,0
 * ...
 *
 * Cada fila se convierte en una instancia de {@link MetricaNormal}.
 */
public class CSVReader {

    /**
     * Lee un archivo CSV con estadísticas calculadas y devuelve una lista de métricas.
     *
     * @param ruta Ruta del archivo CSV a leer (por ejemplo "out/stats.csv").
     * @return Lista de objetos {@link MetricaColumna} construidos a partir del archivo.
     */
    public static List<MetricaColumna> leerStats(String ruta) {
        List<MetricaColumna> metricas = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(ruta))) {
            String linea = br.readLine(); // cabecera
            while ((linea = br.readLine()) != null) {
                String[] partes = linea.split(",");
                String nombre = partes[0].trim();
                double media = Double.parseDouble(partes[1].trim());
                double mediana = Double.parseDouble(partes[2].trim());
                double desviacion = Double.parseDouble(partes[3].trim());
                int outliers = Integer.parseInt(partes[4].trim());
                metricas.add(new MetricaNormal(nombre, media, mediana, desviacion, outliers));
            }
        } catch (Exception e) {
            System.err.println("Error al leer stats.csv: " + e.getMessage());
        }
        return metricas;
    }
}
