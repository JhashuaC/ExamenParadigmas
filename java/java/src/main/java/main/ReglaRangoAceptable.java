package main;

/**
 * Implementación de {@link ReglaCalidad} que valida
 * si la media de una métrica se encuentra dentro de
 * un rango aceptable definido por un mínimo y un máximo.
 *
 * Esta regla permite verificar que los valores medios
 * de las variables estén en un intervalo esperado,
 * facilitando la detección de desviaciones.
 */
public class ReglaRangoAceptable implements ReglaCalidad {
    private double min;
    private double max;

    /**
     * Constructor de la clase ReglaRangoAceptable.
     *
     * @param min Valor mínimo permitido para la media.
     * @param max Valor máximo permitido para la media.
     */
    public ReglaRangoAceptable(double min, double max) {
        this.min = min;
        this.max = max;
    }

    /**
     * Evalúa si la media de la métrica está dentro del rango definido.
     *
     * @param metrica Objeto {@link MetricaColumna} que contiene las estadísticas a validar.
     * @return Cadena con el resultado: 
     *         "OK - Media dentro del rango" si cumple,
     *         "FALLA - Media fuera del rango" en caso contrario.
     */
    @Override
    public String validar(MetricaColumna metrica) {
        if (metrica.getMedia() >= min && metrica.getMedia() <= max) {
            return "OK - Media dentro del rango";
        } else {
            return "FALLA - Media fuera del rango";
        }
    }
}
