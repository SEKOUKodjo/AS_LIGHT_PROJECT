from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import joblib
import numpy as np

app = FastAPI(
    title="API Prédiction Qualité de l'Air",
    description="Prédit l'indice de qualité de l'air à partir de données météo et environnementales.",
    version="1.0"
)

# --- Chargement du modèle Lasso ---
model = joblib.load("models/lasso.pkl")

# --- 49 features dans l'ordre exact d'entraînement ---
FEATURE_NAMES = [
    "temperature_2m_max", "temperature_2m_min", "temperature_2m_mean",
    "apparent_temperature_mean", "precipitation_sum", "rain_sum",
    "precip_log", "wind_speed_10m_max", "wind_gusts_10m_max",
    "shortwave_radiation_sum", "et0_fao_evapotranspiration", "sunshine_duration",
    "daylight_duration", "precipitation_hours", "isa", "isa_scaled",
    "temp_amplitude", "sunshine_ratio", "wind_gust_ratio", "wind_dir_sin",
    "wind_dir_cos", "is_harmattan", "is_dry_season", "is_stagnant",
    "is_no_rain", "month_sin", "month_cos", "dayofyear_sin", "dayofyear_cos",
    "year", "temp_lag1", "temp_lag3", "temp_lag7", "wind_lag1", "wind_lag3",
    "wind_lag7", "rain_lag1", "rain_lag3", "rain_lag7", "isa_lag1",
    "isa_lag3", "isa_lag7", "temp_roll7", "wind_roll7", "precip_roll7",
    "latitude", "longitude", "city_enc", "region_enc"
]


# --- Schéma de requête ---
class AirQualityRequest(BaseModel):
    temperature_2m_max: float
    temperature_2m_min: float
    temperature_2m_mean: float
    apparent_temperature_mean: float
    precipitation_sum: float
    rain_sum: float
    precip_log: float
    wind_speed_10m_max: float
    wind_gusts_10m_max: float
    shortwave_radiation_sum: float
    et0_fao_evapotranspiration: float
    sunshine_duration: float
    daylight_duration: float
    precipitation_hours: float
    isa: float
    isa_scaled: float
    temp_amplitude: float
    sunshine_ratio: float
    wind_gust_ratio: float
    wind_dir_sin: float
    wind_dir_cos: float
    is_harmattan: int       # 0 ou 1
    is_dry_season: int      # 0 ou 1
    is_stagnant: int        # 0 ou 1
    is_no_rain: int         # 0 ou 1
    month_sin: float
    month_cos: float
    dayofyear_sin: float
    dayofyear_cos: float
    year: int
    temp_lag1: float
    temp_lag3: float
    temp_lag7: float
    wind_lag1: float
    wind_lag3: float
    wind_lag7: float
    rain_lag1: float
    rain_lag3: float
    rain_lag7: float
    isa_lag1: float
    isa_lag3: float
    isa_lag7: float
    temp_roll7: float
    wind_roll7: float
    precip_roll7: float
    latitude: float
    longitude: float
    city_enc: float
    region_enc: float


# --- Schéma de réponse ---
class AirQualityResponse(BaseModel):
    prediction: float
    qualite: str
    features_recues: int


def interpreter_qualite(valeur: float) -> str:
    """Interprète l'indice prédit en catégorie qualité de l'air."""
    if valeur <= 50:
        return "Bonne 🟢"
    elif valeur <= 100:
        return "Modérée 🟡"
    elif valeur <= 150:
        return "Mauvaise pour groupes sensibles 🟠"
    elif valeur <= 200:
        return "Mauvaise 🔴"
    elif valeur <= 300:
        return "Très mauvaise 🟣"
    else:
        return "Dangereuse ⚫"


# --- Routes ---
@app.get("/")
def root():
    return {
        "message": "API Prédiction Qualité de l'Air opérationnelle ✅",
        "modele": "Lasso",
        "nb_features": len(FEATURE_NAMES),
        "doc": "/docs"
    }


@app.get("/features")
def get_features():
    """Retourne la liste des 49 features attendues avec leur importance."""
    importance = {
        "is_no_rain": 0.2216, "precip_log": 0.1299, "precipitation_sum": 0.1233,
        "isa": 0.1095, "rain_sum": 0.1029, "isa_scaled": 0.1024,
        "precipitation_hours": 0.0869, "is_harmattan": 0.0468,
        "is_dry_season": 0.0407, "temperature_2m_max": 0.0128
    }
    return {
        "nb_features": len(FEATURE_NAMES),
        "features": FEATURE_NAMES,
        "top_10_importance": importance
    }


@app.post("/predict", response_model=AirQualityResponse)
def predict(data: AirQualityRequest):
    """Prédit l'indice de qualité de l'air."""
    try:
        values = [getattr(data, feat) for feat in FEATURE_NAMES]
        X = np.array(values).reshape(1, -1)

        prediction = float(model.predict(X)[0])
        qualite = interpreter_qualite(prediction)

        return AirQualityResponse(
            prediction=round(prediction, 4),
            qualite=qualite,
            features_recues=len(values)
        )

    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Erreur de prédiction : {str(e)}")


@app.post("/predict/batch")
def predict_batch(data: list[AirQualityRequest]):
    """Prédit l'indice de qualité de l'air pour plusieurs observations à la fois."""
    try:
        X = np.array([[getattr(d, feat) for feat in FEATURE_NAMES] for d in data])
        predictions = model.predict(X).tolist()

        return {
            "nb_predictions": len(predictions),
            "resultats": [
                {
                    "index": i,
                    "prediction": round(p, 4),
                    "qualite": interpreter_qualite(p)
                }
                for i, p in enumerate(predictions)
            ]
        }
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Erreur batch : {str(e)}")
