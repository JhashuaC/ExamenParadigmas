package main;
// Verifica si la media está dentro de un rango definido



public class ReglaRangoAceptable implements ReglaCalidad {
    private double min;
    private double max;

    public ReglaRangoAceptable(double min, double max) {
        this.min = min;
        this.max = max;
    }

    @Override
    public String validar(MetricaColumna metrica) {
        if (metrica.getMedia() >= min && metrica.getMedia() <= max) {
            return "OK - Media dentro del rango";
        } else {
            return "FALLA - Media fuera del rango";
        }
    }
}
