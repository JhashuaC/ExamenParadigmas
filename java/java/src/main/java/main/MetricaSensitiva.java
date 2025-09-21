package main;
// Subclase que podría dar más importancia a la desviación o a outliers



public class MetricaSensitiva extends MetricaColumna {

    public MetricaSensitiva(String nombreVariable, double media, double mediana, double desviacionEstandar, int outliers) {
        super(nombreVariable, media, mediana, desviacionEstandar, outliers);
    }

    @Override
    public String evaluarCalidad(ReglaCalidad regla) {
        // Se podría aplicar alguna lógica diferente, por ejemplo:
        if (outliers > 0) {
            return "Variable " + nombreVariable + " sensible: " + regla.validar(this);
        }
        return regla.validar(this);
    }
}
