package main;
// Interfaz que define cómo validar una métrica

public interface ReglaCalidad {
    String validar(MetricaColumna metrica);
}
