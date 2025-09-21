package reglas;
// Verifica que no haya outliers

import metricas.MetricaColumna;

public class ReglaSinOutliers implements ReglaCalidad {

    @Override
    public String validar(MetricaColumna metrica) {
        if (metrica.getOutliers() == 0) {
            return "OK - Sin outliers";
        } else {
            return "FALLA - Existen " + metrica.getOutliers() + " outliers";
        }
    }
}
