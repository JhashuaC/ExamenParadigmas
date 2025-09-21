package main;
// Clase abstracta que modela las métricas básicas de una columna


public abstract class MetricaColumna {
    protected String nombreVariable;
    protected double media;
    protected double mediana;
    protected double desviacionEstandar;
    protected int outliers;

    // Constructor
    public MetricaColumna(String nombreVariable, double media, double mediana, double desviacionEstandar, int outliers) {
        this.nombreVariable = nombreVariable;
        this.media = media;
        this.mediana = mediana;
        this.desviacionEstandar = desviacionEstandar;
        this.outliers = outliers;
    }

    // Métodos getters
    public String getNombreVariable() { return nombreVariable; }
    public double getMedia() { return media; }
    public double getMediana() { return mediana; }
    public double getDesviacionEstandar() { return desviacionEstandar; }
    public int getOutliers() { return outliers; }

    // Método abstracto que las subclases deben implementar
    public abstract String evaluarCalidad(ReglaCalidad regla);
}
