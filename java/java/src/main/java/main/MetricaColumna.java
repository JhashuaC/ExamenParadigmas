package main;

/**
 * Clase abstracta que modela las métricas básicas de una columna
 * obtenidas del procesamiento estadístico en Fortran.
 *
 * Cada métrica representa una variable con sus estadísticas
 * (media, mediana, desviación estándar y número de outliers).
 *
 * Esta clase sirve como base para subclases especializadas
 * ({@link MetricaNormal}, {@link MetricaSensitiva})
 * que implementan la evaluación de calidad mediante
 * reglas de negocio representadas por {@link ReglaCalidad}.
 */
public abstract class MetricaColumna {
    protected String nombreVariable;
    protected double media;
    protected double mediana;
    protected double desviacionEstandar;
    protected int outliers;

    /**
     * Constructor de la clase abstracta MetricaColumna.
     *
     * @param nombreVariable Nombre de la variable estadística.
     * @param media Valor promedio de la columna.
     * @param mediana Valor central de la columna.
     * @param desviacionEstandar Medida de dispersión de los datos.
     * @param outliers Cantidad de valores atípicos detectados.
     */
    public MetricaColumna(String nombreVariable, double media, double mediana, double desviacionEstandar, int outliers) {
        this.nombreVariable = nombreVariable;
        this.media = media;
        this.mediana = mediana;
        this.desviacionEstandar = desviacionEstandar;
        this.outliers = outliers;
    }

    public String getNombreVariable() { return nombreVariable; }
    public double getMedia() { return media; }
    public double getMediana() { return mediana; }
    public double getDesviacionEstandar() { return desviacionEstandar; }
    public int getOutliers() { return outliers; }

    /**
     * Método abstracto que permite evaluar la calidad de la métrica
     * utilizando una regla de validación.
     *
     * @param regla Implementación de {@link ReglaCalidad} que define la validación.
     * @return Resultado de la evaluación en formato String.
     */
    public abstract String evaluarCalidad(ReglaCalidad regla);
}
