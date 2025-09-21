package main;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

/**
 * Clase encargada de leer un archivo JSON de reglas de calidad
 * y convertirlo en una lista de objetos que implementan {@link ReglaCalidad}.
 *
 * El archivo JSON debe tener la siguiente estructura:
 *
 * {
 *   "reglas": [
 *     { "tipo": "RangoAceptable", "min": 10, "max": 60 },
 *     { "tipo": "SinOutliers" }
 *   ]
 * }
 *
 * Soporta actualmente dos tipos de reglas:
 * - {@link ReglaRangoAceptable}: valida que la media esté en un rango definido.
 * - {@link ReglaSinOutliers}: valida que no existan outliers.
 */
public class JSONRulesReader {

    /**
     * Lee un archivo JSON y devuelve una lista de reglas de calidad.
     *
     * @param ruta Ruta del archivo JSON a leer (por ejemplo "in/rules.json").
     * @return Lista de objetos {@link ReglaCalidad} construidos a partir del JSON.
     */
    public static List<ReglaCalidad> leerReglas(String ruta) {
        List<ReglaCalidad> reglas = new ArrayList<>();
        try {
            Gson gson = new Gson();
            JsonObject obj = gson.fromJson(new FileReader(ruta), JsonObject.class);
            JsonArray arr = obj.getAsJsonArray("reglas");

            for (JsonElement el : arr) {
                JsonObject regla = el.getAsJsonObject();
                String tipo = regla.get("tipo").getAsString();

                switch (tipo) {
                    case "RangoAceptable":
                        double min = regla.get("min").getAsDouble();
                        double max = regla.get("max").getAsDouble();
                        reglas.add(new ReglaRangoAceptable(min, max));
                        break;
                    case "SinOutliers":
                        reglas.add(new ReglaSinOutliers());
                        break;
                }
            }
        } catch (Exception e) {
            System.err.println("Error al leer rules.json: " + e.getMessage());
        }
        return reglas;
    }
}
