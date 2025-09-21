package main;

/**
 * Subclase concreta de {@link MetricaColumna} que representa
 * métricas en las que la presencia de outliers u otros factores
 * puede influir de manera más significativa en la evaluación.
 *
 * A diferencia de {@link MetricaNormal}, esta clase puede aplicar
 * un tratamiento especial cuando se detectan valores atípicos,
 * marcando la variable como "sensible".
 */
public class MetricaSensitiva extends MetricaColumna {

    /**
     * Constructor de la clase MetricaSensitiva.
     *
     * @param nombreVariable Nombre de la variable estadística.
     * @param media Valor promedio de la columna.
     * @param mediana Valor central de la columna.
     * @param desviacionEstandar Medida de dispersión de los datos.
     * @param outliers Cantidad de valores atípicos detectados.
     */
    public MetricaSensitiva(String nombreVariable, double media, double mediana, double desviacionEstandar, int outliers) {
        super(nombreVariable, media, mediana, desviacionEstandar, outliers);
    }

    /**
     * Evalúa la calidad de la métrica utilizando la regla indicada.
     * Si existen outliers, la respuesta incluye una marca de sensibilidad.
     *
     * @param regla Implementación de {@link ReglaCalidad} aplicada a esta métrica.
     * @return Cadena de texto con el resultado de la validación, destacando la sensibilidad si aplica.
     */
    @Override
    public String evaluarCalidad(ReglaCalidad regla) {
        if (outliers > 0) {
            return "Variable " + nombreVariable + " sensible: " + regla.validar(this);
        }
        return regla.validar(this);
    }
}
