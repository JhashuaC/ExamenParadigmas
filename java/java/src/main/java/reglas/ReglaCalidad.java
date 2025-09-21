package reglas;
// Interfaz que define cómo validar una métrica

import metricas.MetricaColumna;

public interface ReglaCalidad {
    String validar(MetricaColumna metrica);
}
