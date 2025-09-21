package main;
import java.io.*;
import java.util.*;



public class CSVReader {

    public static List<MetricaColumna> leerStats(String ruta) {
        List<MetricaColumna> metricas = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(ruta))) {
            String linea = br.readLine(); // saltar cabecera
            while ((linea = br.readLine()) != null) {
                String[] partes = linea.split(",");
                String nombre = partes[0];
                double media = Double.parseDouble(partes[1]);
                double mediana = Double.parseDouble(partes[2]);
                double desviacion= Double.parseDouble(partes[3]);
                int outliers = Integer.parseInt(partes[4]);

            
                metricas.add(new MetricaNormal(nombre, media, mediana, desviacion, outliers));
            }
        } catch (Exception e) {
            System.err.println("Error al leer stats.csv: " + e.getMessage());
        }
        return metricas;
    }
}
