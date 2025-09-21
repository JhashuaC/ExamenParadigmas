package main;
// Subclase que representa métricas sin ajustes especiales


public class MetricaNormal extends MetricaColumna {

    public MetricaNormal(String nombreVariable, double media, double mediana, double desviacionEstandar, int outliers) {
        super(nombreVariable, media, mediana, desviacionEstandar, outliers);
    }

    @Override
    public String evaluarCalidad(ReglaCalidad regla) {
        return regla.validar(this);
    }
}
