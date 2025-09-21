package main;
import com.google.gson.*;
import java.io.*;
import java.util.*;

public class JSONRulesReader {

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
