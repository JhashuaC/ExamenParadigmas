package metricas;

import reglas.ReglaCalidad;

/**
 * Subclase concreta de {@link MetricaColumna} que representa
 * métricas sin ajustes especiales en su evaluación.
 *
 * Esta clase se utiliza cuando no es necesario aplicar ponderaciones
 * adicionales o lógica especial para la interpretación de las métricas.
 * 
 * Cada instancia de {@code MetricaNormal} puede ser evaluada
 * mediante una regla de calidad implementada por {@link ReglaCalidad}.
 */
public class MetricaNormal extends MetricaColumna {

    /**
     * Constructor de la clase MetricaNormal.
     *
     * @param nombreVariable Nombre de la variable estadística.
     * @param media Valor promedio de la columna.
     * @param mediana Valor central de la columna.
     * @param desviacionEstandar Medida de dispersión de los datos.
     * @param outliers Cantidad de valores atípicos detectados.
     */
    public MetricaNormal(String nombreVariable, double media, double mediana, double desviacionEstandar, int outliers) {
        super(nombreVariable, media, mediana, desviacionEstandar, outliers);
    }

    /**
     * Evalúa la calidad de la métrica utilizando la regla indicada.
     *
     * @param regla Implementación de {@link ReglaCalidad} aplicada a esta métrica.
     * @return Cadena de texto con el resultado de la validación.
     */
    @Override
    public String evaluarCalidad(ReglaCalidad regla) {
        return regla.validar(this);
    }
}
