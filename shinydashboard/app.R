# ==============================================================================
#  Surveillance de la Qualité de l'Air au Cameroun
#  Tableau de bord interactif — Données 2020-2025
# ==============================================================================
#  install.packages(c("shiny","shinydashboard","shinyWidgets","highcharter",
#    "httr","jsonlite","dplyr","lubridate","leaflet","readr"))
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(highcharter)
library(jsonlite)
library(dplyr)
library(lubridate)
library(leaflet)
library(readr)

# ==============================================================================
# TRADUCTIONS
# ==============================================================================
LANG <- list(
  FR = list(
    btn="EN", city="Ville",
    thresh="Seuils PM2.5",
    good="Bonne \u2264 16.55",
    moderate="Moderee \u2264 22.96",
    bad="Mauvaise \u2264 30.11",
    dangerous="Dangereuse > 30.11",
    src1="Donnees meteo en temps reel",
    src2="Modele predictif",
    src3="87 240 obs. | 40 villes | 2020-2025",
    q_good="Bonne", q_mod="Moderee", q_bad="Mauvaise",
    q_dan="Dangereuse", q_unk="Inconnue",
    days=c("Dim","Lun","Mar","Mer","Jeu","Ven","Sam"),
    months=c("Jan","Fev","Mar","Avr","Mai","Jun",
             "Jul","Aou","Sep","Oct","Nov","Dec"),
    refresh="Actualiser"
  ),
  EN = list(
    btn="FR", city="City",
    thresh="PM2.5 Thresholds",
    good="Good \u2264 16.55",
    moderate="Moderate \u2264 22.96",
    bad="Poor \u2264 30.11",
    dangerous="Hazardous > 30.11",
    src1="Real-time weather data",
    src2="Predictive model",
    src3="87,240 obs. | 40 cities | 2020-2025",
    q_good="Good", q_mod="Moderate", q_bad="Poor",
    q_dan="Hazardous", q_unk="Unknown",
    days=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
    months=c("Jan","Feb","Mar","Apr","May","Jun",
             "Jul","Aug","Sep","Oct","Nov","Dec"),
    refresh="Refresh"
  )
)




APP_DIR <- getwd()
# Si data/ absent ici, remonter d'un niveau (cas local RStudio)
if (!file.exists(file.path(APP_DIR,"data","df_features.csv"))) {
  p <- dirname(APP_DIR)
  if (file.exists(file.path(p,"data","df_features.csv"))) APP_DIR <- p
  #else if (file.exists("C:/indabax2026_AS_LIGHT/data/df_features.csv"))
    APP_DIR <- "C:/indabax2026_AS_LIGHT"
}
message("[INFO] APP_DIR = ", APP_DIR)

CSV_PATH          <- file.path(APP_DIR, "data",   "df_features.csv")
METRICS_PATH      <- file.path(APP_DIR, "models", "all_models_metrics.json")
FEAT_IMP_PATH     <- file.path(APP_DIR, "models", "feature_importance.json")
HEATMAP_MEAN_PATH <- file.path(APP_DIR, "www",    "heatmap_mean.json")
HEATMAP_MAX_PATH  <- file.path(APP_DIR, "www",    "heatmap_max.json")
VALIDATION_PATH   <- file.path(APP_DIR, "www",    "validation_sample.json")

# APIs
LASSO_API <- "https://as-light-project.onrender.com/predict"
METEO_API <- "https://api.open-meteo.com/v1/forecast"

# Chargement des données historiques
DF <- tryCatch({
  df <- read_csv(CSV_PATH, show_col_types=FALSE)
  df$time  <- as.Date(df$time)
  df$month <- as.integer(format(df$time, "%m"))
  df$year  <- as.integer(format(df$time, "%Y"))
  message("[OK] Donnees chargees : ", nrow(df), " obs. | ", length(unique(df$city)), " villes")
  df
}, error=function(e) {
  message("[ERREUR] CSV introuvable : ", e$message)
  NULL
})

# Chargement des métriques des modèles
METRICS <- tryCatch({
  if (file.exists(METRICS_PATH)) fromJSON(METRICS_PATH)
  else list(
    best_model = "Lasso",
    all_models = list(
      Lasso      = list(r2=1.000, mae=0.0175, rmse=0.0226, mape_pct=0.09),
      ElasticNet = list(r2=1.000, mae=0.0449, rmse=0.0580, mape_pct=0.23),
      MLP_Shallow= list(r2=0.9997,mae=0.1300, rmse=0.1622, mape_pct=0.74),
      MLP_Deep   = list(r2=0.9996,mae=0.1477, rmse=0.1918, mape_pct=0.83),
      GBM        = list(r2=0.9994,mae=0.1686, rmse=0.2356, mape_pct=0.85),
      ExtraTrees = list(r2=0.9990,mae=0.1608, rmse=0.3037, mape_pct=0.80),
      RandomForest=list(r2=0.9987,mae=0.1561, rmse=0.3336, mape_pct=0.72)
    )
  )
}, error=function(e) NULL)

# Chargement de l'importance des features
FEAT_IMP <- tryCatch({
  if (file.exists(FEAT_IMP_PATH)) fromJSON(FEAT_IMP_PATH) else NULL
}, error=function(e) NULL)

# Stats pré-calculées ───────────────────────────────────────────────────────
CITY_STATS <- if (!is.null(DF)) {
  DF |> group_by(city, region, latitude, longitude) |>
    summarise(
      pm25_mean  = round(mean(pm25_proxy, na.rm=TRUE), 2),
      pm25_max   = round(max(pm25_proxy,  na.rm=TRUE), 2),
      pm25_min   = round(min(pm25_proxy,  na.rm=TRUE), 2),
      temp_mean  = round(mean(temperature_2m_mean, na.rm=TRUE), 1),
      precip_mean= round(mean(precipitation_sum,   na.rm=TRUE), 2),
      wind_mean  = round(mean(wind_speed_10m_max,  na.rm=TRUE), 1),
      n_obs      = n(),
      alert_days = sum(pm25_proxy > 30.11, na.rm=TRUE),
      .groups="drop"
    ) |> arrange(desc(pm25_mean))
} else NULL

VILLES <- if (!is.null(CITY_STATS)) {
  setNames(
    lapply(seq_len(nrow(CITY_STATS)), function(i)
      c(CITY_STATS$latitude[i], CITY_STATS$longitude[i], CITY_STATS$region[i])),
    CITY_STATS$city
  )
} else list(
  "Yaounde"=c(3.85,11.52,"Centre"), "Maroua"=c(10.59,14.32,"Extreme-Nord"),
  "Douala"=c(4.05,9.70,"Littoral"), "Kribi"=c(2.94,9.91,"Sud")
)

CITIES <- names(VILLES)

# Encodages ─────────────────────────────────────────────────────────────────
CITY_ENC   <- setNames(seq_along(CITIES)*0.5, CITIES)
REGION_ENC <- list("Centre"=1,"Adamaoua"=2,"Est"=3,"Extreme-Nord"=4,
  "Littoral"=5,"Nord"=6,"Nord-Ouest"=7,"Ouest"=8,"Sud"=9,"Sud-Ouest"=10)

`%||%` <- function(a,b) if(!is.null(a)&&length(a)>0&&!is.na(a[1]))a else b

# Qualité air ───────────────────────────────────────────────────────────────
qcol <- function(x){ if(is.na(x))"#95a5a6" else if(x<=16.55)"#27ae60" else if(x<=22.96)"#f39c12" else if(x<=30.11)"#e67e22" else "#e74c3c" }
qlab <- function(x, lv="FR") {
  t <- LANG[[lv]]
  if(is.null(x)||is.na(x)) return(t$q_unk)
  if(x<=16.55) return(t$q_good)
  if(x<=22.96) return(t$q_mod)
  if(x<=30.11) return(t$q_bad)
  return(t$q_dan)
}
qico <- function(x){ if(is.na(x))"—" else if(x<=16.55)"check" else if(x<=22.96)"minus" else if(x<=30.11)"triangle-exclamation" else "alert" }

# Photos de fond par ville (Unsplash – libres de droits) ───────────────────
CITY_BG <- list(
  "Yaounde"     = "https://images.unsplash.com/photo-1580060839134-75a5edca2e99?w=1200&q=80",
  "Douala"      = "https://images.unsplash.com/photo-1578895101408-1a36b834405b?w=1200&q=80",
  "Kribi"       = "https://images.unsplash.com/photo-1507525428034-b723cf961d3e?w=1200&q=80",
  "Maroua"      = "https://images.unsplash.com/photo-1509316785289-025f5b846b35?w=1200&q=80",
  "Garoua"      = "https://images.unsplash.com/photo-1509316785289-025f5b846b35?w=1200&q=80",
  "Bamenda"     = "https://images.unsplash.com/photo-1585409677983-0f6c41ca9c3b?w=1200&q=80",
  "Bafoussam"   = "https://images.unsplash.com/photo-1585409677983-0f6c41ca9c3b?w=1200&q=80",
  "Ngaoundere"  = "https://images.unsplash.com/photo-1509316785289-025f5b846b35?w=1200&q=80",
  "Buea"        = "https://images.unsplash.com/photo-1464822759023-fed622ff2c3b?w=1200&q=80",
  "Limbe"       = "https://images.unsplash.com/photo-1507525428034-b723cf961d3e?w=1200&q=80",
  "Ebolowa"     = "https://images.unsplash.com/photo-1448375240586-882707db888b?w=1200&q=80",
  "Kousseri"    = "https://images.unsplash.com/photo-1509316785289-025f5b846b35?w=1200&q=80",
  "Bertoua"     = "https://images.unsplash.com/photo-1448375240586-882707db888b?w=1200&q=80",
  "DEFAULT"     = "https://images.unsplash.com/photo-1448375240586-882707db888b?w=1200&q=80"
)
get_bg <- function(city) CITY_BG[[city]] %||% CITY_BG[["DEFAULT"]]

# Descriptions des villes ───────────────────────────────────────────────────
CITY_DESC <- list(
  "Yaounde"    = "Capitale politique du Cameroun, perchée sur ses 7 collines à 750 m d'altitude. Son climat équatorial atténué lui confère des températures douces. Principal centre administratif et universitaire du pays.",
  "Douala"     = "Capitale économique et plus grand port d'Afrique centrale. Ville côtière au climat chaud et humide, moteur industriel et commercial du Cameroun avec un trafic portuaire intense.",
  "Kribi"      = "Joyau touristique du Sud, Kribi est réputée pour ses plages de sable blanc bordant l'Océan Atlantique et ses chutes de la Lobé qui se jettent directement dans la mer.",
  "Maroua"     = "Capitale de la Région de l'Extrême-Nord, porte d'entrée du parc national de Waza. Ville sahélienne exposée aux vents de l'Harmattan et aux pics de pollution en saison sèche.",
  "Garoua"     = "Troisième ville du Cameroun, située sur les rives du fleuve Bénoué. Centre agricole et commercial du Nord, avec un climat soudano-sahélien marqué par de longues saisons sèches.",
  "Bamenda"    = "Capitale du Nord-Ouest, ville des hautes terres à plus de 1 400 m. Climat tempéré d'altitude, centre culturel anglophone et point de départ vers le Ring Road.",
  "Bafoussam"  = "Capitale de la Région de l'Ouest, cœur du pays Bamiléké. Ville commerçante animée, entourée de paysages volcaniques verdoyants et de plantations de café et cacao.",
  "Ngaoundere" = "Ville du plateau de l'Adamaoua à 1 100 m d'altitude, terminus du chemin de fer transcamerounais. Point de jonction entre le nord et le sud du pays, avec un climat de savane.",
  "Buea"       = "Ville au pied du Mont Cameroun (4 095 m), le plus haut sommet d'Afrique de l'Ouest. Ancienne capitale coloniale allemande, bénéficiant d'un micro-climat frais et humide unique.",
  "Limbe"      = "Ville côtière du Sud-Ouest, connue pour ses plages de sable noir volcanique au pied du Mont Cameroun. Abritant une importante raffinerie et un jardin botanique centenaire.",
  "Kousseri"   = "Ville frontalière avec le Tchad et le Nigeria, au confluent du Chari et du Logone. Zone sahélienne parmi les plus polluées du pays, très exposée aux vents de sable.",
  "Ebolowa"    = "Capitale de la Région du Sud, entourée de forêts tropicales denses. Ville administrative tranquille avec un bon niveau de qualité de l'air grâce au couvert végétal.",
  "Bertoua"    = "Capitale de la Région de l'Est, porte d'entrée de la grande forêt équatoriale. Ville en développement avec un air généralement acceptable grâce à la végétation environnante.",
  "Mokolo"     = "Ville de l'Extrême-Nord proche des monts Mandara. Zone très exposée aux vents sahariens et aux tempêtes de sable pendant l'Harmattan, parmi les plus polluées du pays.",
  "DEFAULT"    = "Ville du Cameroun couverte par le réseau de surveillance de la qualité de l'air Qualite_Air_Cameroun, intégrant 87 240 observations météorologiques sur 6 années (2020-2025)."
)
get_desc <- function(city) CITY_DESC[[city]] %||% CITY_DESC[["DEFAULT"]]

# Open-Meteo ────────────────────────────────────────────────────────────────
get_meteo <- function(lat, lon) {
  url <- paste0(METEO_API,
    "?latitude=",lat,"&longitude=",lon,
    "&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,",
    "apparent_temperature_mean,precipitation_sum,rain_sum,",
    "wind_speed_10m_max,wind_gusts_10m_max,shortwave_radiation_sum,",
    "et0_fao_evapotranspiration,sunshine_duration,daylight_duration,",
    "precipitation_hours,wind_direction_10m_dominant",
    "&timezone=Africa%2FDouala&forecast_days=7")
  tryCatch({
    r <- httr::GET(url, httr::timeout(15))
    if (httr::status_code(r)==200)
      jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8"))$daily
    else NULL
  }, error=function(e) NULL)
}

# Build 49 features ─────────────────────────────────────────────────────────
build_features <- function(m, i, lat, lon, city, region) {
  d   <- as.Date(m$time[i]); doy <- yday(d); mo <- month(d)
  wc  <- max(m$wind_speed_10m_max[i], 0.5)
  pr  <- m$precipitation_sum[i]; tx <- m$temperature_2m_max[i]
  tn  <- m$temperature_2m_min[i]; tm <- m$temperature_2m_mean[i]
  isa <- (1/wc)*(1-min(pr,10)/10)*(1+max(0,tx-30)/10)
  isc <- min(isa/0.2286, 3.0)
  wd  <- m$wind_direction_10m_dominant[i]
  p   <- function(k) if((i-k)>=1) i-k else 1
  list(
    temperature_2m_max=tx, temperature_2m_min=tn, temperature_2m_mean=tm,
    apparent_temperature_mean=m$apparent_temperature_mean[i],
    precipitation_sum=pr, rain_sum=m$rain_sum[i], precip_log=log1p(pr),
    wind_speed_10m_max=wc, wind_gusts_10m_max=m$wind_gusts_10m_max[i],
    shortwave_radiation_sum=m$shortwave_radiation_sum[i],
    et0_fao_evapotranspiration=m$et0_fao_evapotranspiration[i],
    sunshine_duration=m$sunshine_duration[i], daylight_duration=m$daylight_duration[i],
    precipitation_hours=m$precipitation_hours[i],
    isa=round(isa,4), isa_scaled=round(isc,4), temp_amplitude=tx-tn,
    sunshine_ratio=ifelse(m$daylight_duration[i]>0,m$sunshine_duration[i]/m$daylight_duration[i],0),
    wind_gust_ratio=ifelse(wc>0,m$wind_gusts_10m_max[i]/wc,1),
    wind_dir_sin=sin(wd*pi/180), wind_dir_cos=cos(wd*pi/180),
    is_harmattan=as.integer(lat>7&mo%in%c(11,12,1,2,3)),
    is_dry_season=as.integer(mo%in%c(11,12,1,2,3)),
    is_stagnant=as.integer(wc<3), is_no_rain=as.integer(pr<0.1),
    month_sin=sin(2*pi*mo/12), month_cos=cos(2*pi*mo/12),
    dayofyear_sin=sin(2*pi*doy/365), dayofyear_cos=cos(2*pi*doy/365),
    year=year(d),
    temp_lag1=m$temperature_2m_mean[p(1)], temp_lag3=m$temperature_2m_mean[p(3)],
    temp_lag7=m$temperature_2m_mean[p(7)],
    wind_lag1=m$wind_speed_10m_max[p(1)], wind_lag3=m$wind_speed_10m_max[p(3)],
    wind_lag7=m$wind_speed_10m_max[p(7)],
    rain_lag1=m$rain_sum[p(1)], rain_lag3=m$rain_sum[p(3)], rain_lag7=m$rain_sum[p(7)],
    isa_lag1=round((1/max(m$wind_speed_10m_max[p(1)],.5))*(1-min(m$rain_sum[p(1)],10)/10)*(1+max(0,m$temperature_2m_max[p(1)]-30)/10),4),
    isa_lag3=round((1/max(m$wind_speed_10m_max[p(3)],.5))*(1-min(m$rain_sum[p(3)],10)/10)*(1+max(0,m$temperature_2m_max[p(3)]-30)/10),4),
    isa_lag7=round((1/max(m$wind_speed_10m_max[p(7)],.5))*(1-min(m$rain_sum[p(7)],10)/10)*(1+max(0,m$temperature_2m_max[p(7)]-30)/10),4),
    temp_roll7=mean(m$temperature_2m_mean[max(1,i-6):i]),
    wind_roll7=mean(m$wind_speed_10m_max[max(1,i-6):i]),
    precip_roll7=mean(m$rain_sum[max(1,i-6):i]),
    latitude=lat, longitude=lon,
    city_enc=as.numeric(CITY_ENC[city]%||%1),
    region_enc=as.numeric(REGION_ENC[[region]]%||%1)
  )
}

call_lasso <- function(f) {
  tryCatch({
    r <- httr::POST(LASSO_API, body=jsonlite::toJSON(f,auto_unbox=TRUE),
      httr::content_type_json(), httr::timeout(30))
    if(httr::status_code(r)==200)
      jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8"))
    else NULL
  }, error=function(e) NULL)
}

# ==============================================================================
# CSS — Thème épuré, clair, professionnel
# ==============================================================================
CSS <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700;800&family=DM+Mono:wght@500;700&display=swap');

:root {
  --bg:       #f5f6fa;
  --white:    #ffffff;
  --card:     #ffffff;
  --sidebar:  #1c2b3a;
  --sid2:     #152231;
  --accent:   #2563eb;
  --accent2:  #1d4ed8;
  --text:     #111827;
  --text2:    #374151;
  --text3:    #6b7280;
  --border:   #e5e7eb;
  --shadow:   0 1px 8px rgba(0,0,0,0.08);
  --shadow-md:0 4px 16px rgba(0,0,0,0.1);
  --shadow-lg:0 8px 32px rgba(0,0,0,0.12);
  --radius:   12px;
  --ok:       #16a34a;
  --warn:     #d97706;
  --bad:      #ea580c;
  --danger:   #dc2626;
}

* { box-sizing: border-box; }
body, .wrapper {
  font-family: 'DM Sans', sans-serif !important;
  background: var(--bg) !important;
  color: var(--text) !important;
  font-size: 15px !important;
}

/* SIDEBAR */
.main-sidebar, .left-side {
  background: var(--sidebar) !important;
  box-shadow: 3px 0 20px rgba(0,0,0,0.2) !important;
}
.sidebar-menu > li > a {
  color: #94a3b8 !important;
  font-size: 13.5px !important;
  font-weight: 500 !important;
  padding: 11px 18px !important;
  border-left: 3px solid transparent;
  transition: all 0.18s;
}
.sidebar-menu > li > a i { color: #64748b !important; margin-right: 10px; }
.sidebar-menu > li.active > a,
.sidebar-menu > li > a:hover {
  background: rgba(255,255,255,0.07) !important;
  color: #f1f5f9 !important;
  border-left: 3px solid var(--accent) !important;
}
.sidebar-menu > li.active > a i,
.sidebar-menu > li > a:hover i { color: #60a5fa !important; }
.sidebar-menu .header {
  color: #475569 !important;
  font-size: 10px !important;
  letter-spacing: 0.15em;
  padding: 18px 18px 6px !important;
}
.main-header .logo {
  background: var(--sid2) !important;
  border-bottom: 1px solid rgba(255,255,255,0.05) !important;
  font-family: 'DM Sans', sans-serif !important;
  font-size: 14px !important;
  font-weight: 800 !important;
  color: #f1f5f9 !important;
  letter-spacing: -0.02em;
}
.main-header .navbar {
  background: var(--white) !important;
  border-bottom: 1px solid var(--border) !important;
  box-shadow: var(--shadow) !important;
}
.main-header .navbar .nav > li > a { color: var(--text2) !important; font-size: 13px !important; }
.main-header .navbar .sidebar-toggle { color: var(--accent) !important; }

/* CONTENU */
.content-wrapper { background: var(--bg) !important; }
.content { padding: 20px !important; }

/* CARD */
.card-env {
  background: var(--white);
  border-radius: var(--radius);
  padding: 20px 22px;
  margin-bottom: 16px;
  box-shadow: var(--shadow);
  border: 1px solid var(--border);
}
.card-title {
  font-size: 11px !important;
  text-transform: uppercase;
  letter-spacing: 0.1em;
  color: var(--text3) !important;
  font-weight: 700;
  margin-bottom: 14px;
  display: flex; align-items: center; gap: 6px;
}

/* HERO VILLE — avec photo de fond */
.ville-hero {
  border-radius: var(--radius);
  overflow: hidden;
  position: relative;
  height: 200px;
  margin-bottom: 16px;
  box-shadow: var(--shadow-md);
}
.ville-hero-bg {
  position: absolute; inset: 0;
  background-size: cover; background-position: center;
  filter: brightness(0.55);
  transition: filter 0.4s;
}
.ville-hero:hover .ville-hero-bg { filter: brightness(0.45); }
.ville-hero-content {
  position: relative; z-index: 2;
  padding: 22px 24px;
  height: 100%;
  display: flex; flex-direction: column; justify-content: flex-end;
  color: white;
}
.ville-hero-name {
  font-size: 26px !important;
  font-weight: 800;
  line-height: 1;
  text-shadow: 0 2px 8px rgba(0,0,0,0.5);
}
.ville-hero-region {
  font-size: 13px !important;
  color: rgba(255,255,255,0.8);
  margin-top: 4px;
  font-weight: 500;
}
.ville-hero-desc {
  font-size: 13px !important;
  color: rgba(255,255,255,0.75);
  margin-top: 6px;
  line-height: 1.5;
}
.ville-hero-badge {
  position: absolute; top: 14px; right: 14px;
  padding: 6px 14px; border-radius: 100px;
  font-size: 13px !important; font-weight: 700;
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.3);
  color: white; z-index: 3;
}

/* KPI BOXES */
.kpi-box {
  background: var(--white);
  border-radius: var(--radius);
  padding: 16px 14px;
  text-align: center;
  margin-bottom: 16px;
  box-shadow: var(--shadow);
  border: 1px solid var(--border);
  transition: all 0.2s;
}
.kpi-box:hover { box-shadow: var(--shadow-md); transform: translateY(-1px); }
.kpi-icon { font-size: 20px !important; margin-bottom: 6px; display: block; }
.kpi-value {
  font-size: 1.8rem !important; font-weight: 800;
  font-family: 'DM Mono', monospace; line-height: 1.1; color: var(--text);
}
.kpi-label {
  font-size: 11px !important; text-transform: uppercase;
  letter-spacing: 0.09em; color: var(--text3); margin-top: 5px; font-weight: 600;
}
.kpi-sub { font-size: 12px !important; color: var(--text2); margin-top: 3px; font-weight: 500; }

/* MÉTÉO GRILLE */
.meteo-grid { display: grid; grid-template-columns: repeat(4,1fr); gap: 10px; }
.meteo-item {
  background: var(--bg); border: 1px solid var(--border);
  border-radius: 10px; padding: 12px 8px; text-align: center;
}
.meteo-item .val {
  font-size: 1.15rem !important; font-weight: 700;
  font-family: 'DM Mono', monospace; color: var(--text); display: block;
}
.meteo-item .lbl {
  font-size: 11px !important; text-transform: uppercase;
  letter-spacing: 0.06em; color: var(--text3); margin-top: 4px; display: block;
}

/* PRÉDICTION HERO */
.pred-hero {
  border-radius: var(--radius);
  padding: 24px 20px; text-align: center;
  box-shadow: var(--shadow-md);
  position: relative; overflow: hidden;
}
.pred-pm25 {
  font-size: 3rem !important; font-weight: 900;
  font-family: 'DM Mono', monospace; line-height: 1;
  margin: 8px 0; color: white;
}
.pred-label { font-size: 13px !important; color: rgba(255,255,255,0.8); }
.pred-badge {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 6px 18px; border-radius: 100px;
  font-weight: 700; font-size: 14px !important;
  background: rgba(255,255,255,0.2);
  border: 1px solid rgba(255,255,255,0.35);
  color: white; margin-top: 8px;
}

/* FORECAST */
.forecast-grid { display: grid; grid-template-columns: repeat(7,1fr); gap: 8px; }
.forecast-day {
  background: var(--bg); border: 1px solid var(--border);
  border-radius: 10px; padding: 12px 6px; text-align: center; transition: all 0.2s;
}
.forecast-day:hover { background: var(--white); box-shadow: var(--shadow-md); transform: translateY(-2px); }
.forecast-day.today { background: #eff6ff; border-color: #93c5fd; }
.forecast-day .dn { font-size: 11px !important; font-weight: 700; text-transform: uppercase; color: var(--text3); }
.forecast-day .dd { font-size: 11px !important; color: var(--text3); margin: 2px 0; }
.forecast-day .di { font-size: 1.3rem !important; margin: 6px 0; display: block; }
.forecast-day .dp { font-size: 1rem !important; font-weight: 800; font-family: 'DM Mono', monospace; }
.forecast-day .dq { font-size: 11px !important; font-weight: 600; margin-top: 3px; }

/* ALERTE */
.alerte-banner {
  border-radius: var(--radius); padding: 13px 18px; margin-bottom: 14px;
  display: flex; align-items: center; gap: 12px; font-size: 14px !important; font-weight: 500;
}
.alerte-ok     { background: #f0fdf4; border: 1px solid #bbf7d0; color: #15803d; }
.alerte-warn   { background: #fffbeb; border: 1px solid #fde68a; color: #b45309; }
.alerte-danger { background: #fef2f2; border: 1px solid #fecaca; color: #b91c1c; }

/* BOUTON */
.btn-predict {
  background: var(--accent); border: none; color: white;
  font-family: 'DM Sans', sans-serif; font-weight: 700;
  font-size: 14px !important; padding: 13px 22px;
  border-radius: var(--radius); cursor: pointer; width: 100%;
  box-shadow: 0 4px 12px rgba(37,99,235,0.35);
  transition: all 0.2s; letter-spacing: 0.01em;
}
.btn-predict:hover { background: var(--accent2); box-shadow: 0 6px 18px rgba(37,99,235,0.45); }

/* STATUS */
.api-status {
  display: inline-flex; align-items: center; gap: 6px;
  font-size: 12px !important; padding: 5px 12px; border-radius: 100px; font-weight: 600;
}
.api-ok   { background: #f0fdf4; color: #15803d; border: 1px solid #bbf7d0; }
.api-wait { background: #fffbeb; color: #b45309; border: 1px solid #fde68a; }
.api-err  { background: #fef2f2; color: #b91c1c; border: 1px solid #fecaca; }

/* BOX */
.box { background: var(--white) !important; border: 1px solid var(--border) !important;
  border-radius: var(--radius) !important; border-top: none !important;
  box-shadow: var(--shadow) !important; }
.box-header { background: transparent !important; border-bottom: 1px solid var(--border) !important; padding: 12px 18px !important; }
.box-header .box-title { color: var(--text2) !important; font-size: 12px !important; text-transform: uppercase; letter-spacing: 0.09em; font-weight: 700; }
.box-body { padding: 16px !important; }

/* SELECT */
.selectize-control .selectize-input, .form-control {
  background: var(--white) !important; border: 1px solid var(--border) !important;
  color: var(--text) !important; border-radius: 8px !important;
  font-family: 'DM Sans', sans-serif !important; font-size: 14px !important;
}
.selectize-dropdown { background: var(--white) !important; border: 1px solid var(--border) !important;
  color: var(--text) !important; font-size: 14px !important; box-shadow: var(--shadow-lg) !important; }
.selectize-dropdown .option:hover, .selectize-dropdown .active { background: #eff6ff !important; color: var(--accent) !important; }
label { color: #94a3b8 !important; font-size: 12px !important; font-weight: 600; text-transform: uppercase; letter-spacing: 0.08em; }

/* INFO ROWS */
.info-row { display: flex; justify-content: space-between; align-items: center;
  padding: 9px 0; border-bottom: 1px solid var(--border); font-size: 13px !important; }
.info-row:last-child { border-bottom: none; }
.info-key { color: var(--text3); font-weight: 500; }
.info-val { font-weight: 700; font-family: 'DM Mono', monospace; font-size: 12px !important; color: var(--text); }

/* STAT PILL */
.stat-pill {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 6px 14px; border-radius: 100px;
  font-size: 13px !important; font-weight: 600;
  background: var(--bg); border: 1px solid var(--border);
}

/* CLASSEMENT */
.rank-item {
  display: flex; align-items: center; gap: 12px;
  padding: 9px 12px; border-radius: 8px; margin-bottom: 6px;
  background: var(--bg); border: 1px solid var(--border);
  font-size: 13px !important; transition: all 0.15s;
}
.rank-item:hover { background: var(--white); box-shadow: var(--shadow); }
.rank-num { font-size: 11px !important; font-weight: 800; color: var(--text3); width: 22px; }
.rank-city { flex: 1; font-weight: 600; color: var(--text); }
.rank-val { font-family: 'DM Mono', monospace; font-size: 13px !important; font-weight: 700; }

/* LOADING */
.loading-env { display: flex; flex-direction: column; align-items: center;
  justify-content: center; padding: 40px; gap: 14px; color: var(--text3); font-size: 14px !important; }
.spinner { width: 34px; height: 34px; border: 3px solid var(--border);
  border-top-color: var(--accent); border-radius: 50%; animation: spin 0.9s linear infinite; }
@keyframes spin { to { transform: rotate(360deg); } }

/* DIVERS */
::-webkit-scrollbar { width: 5px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: #d1d5db; border-radius: 10px; }
hr { border-color: var(--border) !important; margin: 12px 0 !important; }
.leaflet-container { border-radius: var(--radius); }
p, span { color: inherit; }
"

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(skin="blue",
  dashboardHeader(
    title=tags$span(
      tags$img(src="https://flagcdn.com/w20/cm.png", height="16px",
        style="margin-right:8px;vertical-align:middle;border-radius:2px;"),
      tags$span(style="font-weight:800;font-size:13px;color:#f1f5f9;",
        "Air Quality / Qualité de l'Air — Cameroun")
    ),
    titleWidth=350,
    tags$li(class="dropdown",
      tags$a(href="#",
        onclick="Shiny.setInputValue('switch_lang', Math.random())",
        style="padding:14px 12px;cursor:pointer;color:#94a3b8;font-weight:700;font-size:13px;",
        uiOutput("btn_lang", inline=TRUE)
      )
    ),
    tags$li(class="dropdown", tags$a(style="padding-top:10px;", uiOutput("header_status")))
  ),

  dashboardSidebar(width=260,
    tags$style(HTML(CSS)),
    sidebarMenu(id="tabs",
      tags$div(style="padding:16px 16px 8px;",
        tags$div(style="font-size:10px;text-transform:uppercase;letter-spacing:.15em;color:#475569;margin-bottom:8px;font-weight:700;","Ville"),
        selectInput("ville", NULL,
          choices=if(length(CITIES)>0) CITIES else c("Yaounde","Maroua","Douala","Kribi"),
          selected=if("Yaounde"%in%CITIES)"Yaounde" else if(length(CITIES)>0) CITIES[1] else "Yaounde",
          width="100%")
      ),
      menuItem("Vue d'ensemble",       tabName="overview",   icon=icon("gauge-high")),
      menuItem("Prédiction en cours",   tabName="realtime",   icon=icon("satellite-dish")),
      menuItem("Analyse Historique",    tabName="historique", icon=icon("chart-line")),
      menuItem("Carte de la Pollution", tabName="carte",      icon=icon("map")),
      menuItem("Classement des Villes", tabName="ranking",    icon=icon("ranking-star")),
      menuItem("Validation du Modèle",  tabName="validation", icon=icon("microscope")),
      menuItem("Comparaison Modeles",  tabName="modeles",    icon=icon("brain")),
      menuItem("Equipe de Conception", tabName="equipe", icon=icon("users")),

      tags$hr(style="border-color:rgba(255,255,255,0.07);margin:8px 0;"),
      uiOutput("sid_seuils"),
      tags$hr(style="border-color:rgba(255,255,255,0.07);margin:8px 0;"),
      uiOutput("sid_footer")
    )
  ),

  dashboardBody(
    tabItems(

      # --- VUE D'ENSEMBLE---
      tabItem(tabName="overview",
        # Photo de fond + description ville
        fluidRow(column(12, uiOutput("ville_hero_card"))),
        # KPIs historiques
        fluidRow(
          column(3, uiOutput("kpi_hist_pm25")),
          column(3, uiOutput("kpi_hist_temp")),
          column(3, uiOutput("kpi_hist_precip")),
          column(3, uiOutput("kpi_hist_vent"))
        ),
        fluidRow(
          column(7,
            div(class="card-env",
              div(class="card-title","Evolution mensuelle PM2.5 (2020-2025)"),
              highchartOutput("hc_monthly", height="230px")
            )
          ),
          column(5,
            div(class="card-env",
              div(class="card-title","PM2.5 moyen par annee"),
              highchartOutput("hc_yearly", height="230px")
            )
          )
        ),
        fluidRow(
          column(12,
            div(class="card-env",
              div(class="card-title","Serie temporelle PM2.5 — Donnees historiques"),
              highchartOutput("hc_ts_hist", height="220px")
            )
          )
        )
      ),

      # --- PRÉDICTION TEMPS RÉEL---
      tabItem(tabName="realtime",
        fluidRow(column(12, uiOutput("alerte_banner"))),
        fluidRow(
          column(4,
            div(class="card-env",
              div(class="card-title","Prédiction PM2.5 — Aujourd'hui"),
              uiOutput("pred_hero"),
              br(),
              actionButton("btn_predict", tagList(icon("arrows-rotate"), " ", uiOutput("btn_refresh", inline=TRUE)), class="btn-predict")
            ),
            div(class="card-env",
              div(class="card-title","Données météo — Temps Réel"),
              uiOutput("meteo_actuel")
            )
          ),
          column(8,
            fluidRow(
              column(3, uiOutput("kpi_pm25_rt")),
              column(3, uiOutput("kpi_isa_rt")),
              column(3, uiOutput("kpi_temp_rt")),
              column(3, uiOutput("kpi_vent_rt"))
            ),
            div(class="card-env",
              div(class="card-title","Prévision PM2.5 — 7 Jours"),
              highchartOutput("hc_forecast", height="210px")
            ),
            fluidRow(
              column(6,
                div(class="card-env",
                  div(class="card-title","Indice de Stagnation Atmosphérique (ISA)"),
                  highchartOutput("hc_isa", height="170px")
                )
              ),
              column(6,
                div(class="card-env",
                  div(class="card-title","Précipitations vs PM2.5"),
                  highchartOutput("hc_precip_rt", height="170px")
                )
              )
            )
          )
        ),
        # Prévision 7 jours cards
        fluidRow(
          column(12,
            div(class="card-env",
              div(class="card-title","Prévision Qualité de l'Air — 7 Prochains Jours"),
              uiOutput("forecast_cards")
            )
          )
        )
      ),

      # --- HISTORIQUE---
      tabItem(tabName="historique",
        fluidRow(
          column(3,
            div(class="card-env",
              div(class="card-title","Filtres"),
              selectInput("h_year","Année",
                choices=c("Toutes"="all",as.character(2020:2025)),
                selected="all",width="100%"),
              selectInput("h_month","Mois",
                choices=c("Tous"="all",setNames(1:12,
                  c("Janvier","Février","Mars","Avril","Mai","Juin",
                    "Juillet","Août","Septembre","Octobre","Novembre","Décembre"))),
                selected="all",width="100%"),
              hr(),
              uiOutput("hist_stats_panel")
            )
          ),
          column(9,
            div(class="card-env",
              div(class="card-title","Série temporelle PM2.5 et Précipitations"),
              highchartOutput("hc_hist_ts", height="260px")
            ),
            fluidRow(
              column(6,
                div(class="card-env",
                  div(class="card-title","Distribution mensuelle"),
                  highchartOutput("hc_hist_box", height="220px")
                )
              ),
              column(6,
                div(class="card-env",
                  div(class="card-title","Température vs PM2.5"),
                  highchartOutput("hc_hist_scatter", height="220px")
                )
              )
            )
          )
        )
      ),

      # --- CARTE---
      tabItem(tabName="carte",
        fluidRow(
          column(12,
            div(style="background:white;border-radius:14px;overflow:hidden;box-shadow:0 1px 8px rgba(0,0,0,0.08);border:1px solid #e5e7eb;position:relative;margin-bottom:16px;",
              div(style="position:absolute;top:12px;left:60px;z-index:1000;background:rgba(255,255,255,0.95);border-radius:10px;padding:8px 16px;box-shadow:0 2px 14px rgba(0,0,0,0.15);display:flex;align-items:center;gap:16px;backdrop-filter:blur(4px);",
                div(style="font-size:13px;font-weight:700;color:#111827;letter-spacing:-0.01em;",
                  "Carte de la Pollution Atmosphérique — Cameroun"),
                radioButtons("map_mode","",
                  choices=c("Moyenne 2020-2025"="mean","Valeur maximale"="max"),
                  selected="mean", inline=TRUE)
              ),
              div(style="position:absolute;right:16px;top:50%;transform:translateY(-50%);z-index:1000;background:rgba(255,255,255,0.95);border-radius:10px;padding:12px 10px 12px 16px;box-shadow:0 2px 14px rgba(0,0,0,0.15);backdrop-filter:blur(4px);",
                div(style="font-size:11px;font-weight:700;color:#374151;margin-bottom:8px;text-align:center;",
                  "PM2.5 µg/m³"),
                div(style="display:flex;align-items:stretch;gap:6px;height:200px;",
                  div(style="width:20px;border-radius:3px;background:linear-gradient(to top,#1a9850,#66bd63,#a6d96a,#d9ef8b,#fee08b,#fdae61,#f46d43,#d73027,#a50026);flex-shrink:0;"),
                  div(style="display:flex;flex-direction:column;justify-content:space-between;",
                    div(style="font-size:11px;font-weight:700;color:#111827;","55+"),
                    div(style="font-size:10px;color:#6b7280;","45"),
                    div(style="font-size:10px;color:#6b7280;","35"),
                    div(style="font-size:10px;color:#6b7280;","28"),
                    div(style="font-size:10px;color:#6b7280;","22"),
                    div(style="font-size:10px;color:#6b7280;","17"),
                    div(style="font-size:11px;font-weight:700;color:#1a9850;","12")
                  )
                )
              ),
              leafletOutput("map_heatmap", height="560px")
            )
          )
        ),
        fluidRow(
          column(3, uiOutput("map_kpi1")),
          column(3, uiOutput("map_kpi2")),
          column(3, uiOutput("map_kpi3")),
          column(3, uiOutput("map_kpi4"))
        ),
        fluidRow(
          column(8, div(class="card-env",
            div(class="card-title","Informations — Ville sélectionnée"),
            uiOutput("map_ville_info")
          )),
          column(4, div(class="card-env",
            div(class="card-title","Top 5 — Villes les plus polluées"),
            uiOutput("top5_polluted")
          ))
        )
      ),
      # --- CLASSEMENT---
      tabItem(tabName="ranking",
        fluidRow(
          column(6,
            div(class="card-env",
              div(class="card-title","Villes les plus polluées — PM2.5 moyen"),
              highchartOutput("hc_rank_bad", height="380px")
            )
          ),
          column(6,
            div(class="card-env",
              div(class="card-title","Villes avec meilleure qualité — PM2.5 moyen"),
              highchartOutput("hc_rank_good", height="380px")
            )
          )
        ),
        fluidRow(
          column(12,
            div(class="card-env",
              div(class="card-title","Toutes les villes — Vue comparative"),
              highchartOutput("hc_rank_all", height="280px")
            )
          )
        )
      )

    , # --- ONGLET 6 — VALIDATION MODÈLE---
    tabItem(tabName="validation",
      fluidRow(column(12,
        div(style="background:linear-gradient(135deg,#1c2b3a,#2563eb);border-radius:14px;padding:22px 28px;margin-bottom:16px;color:white;",
          div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:16px;",
            div(
              div(style="font-size:11px;text-transform:uppercase;letter-spacing:.12em;opacity:.7;margin-bottom:4px;","Validation du Modèle Prédictif"),
              div(style="font-size:20px;font-weight:800;","PM2.5 Réel vs PM2.5 Prédit — Données historiques 2020-2025")
            ),
            div(style="display:flex;gap:12px;",
              div(style="text-align:center;background:rgba(255,255,255,.15);border-radius:10px;padding:10px 18px;",
                div(style="font-size:1.8rem;font-weight:800;font-family:'DM Mono',monospace;","R² = 1.0"),
                div(style="font-size:11px;opacity:.7;","Score global")),
              div(style="text-align:center;background:rgba(255,255,255,.15);border-radius:10px;padding:10px 18px;",
                div(style="font-size:1.8rem;font-weight:800;font-family:'DM Mono',monospace;","MAPE : 0.09%"),
                div(style="font-size:11px;opacity:.7;","MAPE")),
              div(style="text-align:center;background:rgba(255,255,255,.15);border-radius:10px;padding:10px 18px;",
                div(style="font-size:1.8rem;font-weight:800;font-family:'DM Mono',monospace;","MAE : 0.018"),
                div(style="font-size:11px;opacity:.7;","µg/m³"))
            )
          )
        )
      )),
      fluidRow(column(12,
        div(class="card-env",
          div(style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:12px;",
            div(
              div(class="card-title","Configuration de la validation"),
              div(style="font-size:13px;color:#6b7280;","L'API Lasso va prédire le PM2.5 sur un échantillon historique et comparer aux valeurs réelles.")
            ),
            div(style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
              selectInput("val_ville","Ville",
                choices=c("Toutes"="all", if(length(CITIES)>0) CITIES else "Yaounde"),
                selected="Yaounde", width="180px"),
              selectInput("val_n","Points",
                choices=c("20"=20,"40"=40,"60"=60), selected=20, width="100px"),
              actionButton("btn_validate",tagList(icon("flask"), " Lancer la validation"),
                style="background:#2563eb;border:none;color:white;font-weight:700;font-size:14px;padding:11px 20px;border-radius:10px;cursor:pointer;box-shadow:0 4px 12px rgba(37,99,235,.3);")
            )
          )
        )
      )),
      fluidRow(column(12, uiOutput("val_progress_ui"))),
      fluidRow(
        column(8, div(class="card-env",
          div(class="card-title","Nuage de points — Valeurs réelles vs Prédites"),
          div(style="font-size:12px;color:#6b7280;margin-bottom:8px;",
            "Chaque point = 1 observation. La droite bleue = prédiction parfaite."),
          highchartOutput("hc_val_scatter", height="310px")
        )),
        column(4, div(class="card-env",
          div(class="card-title","Métriques de performance"),
          uiOutput("val_metrics_ui"),
          hr(),
          div(class="card-title","Résultats par ville"),
          uiOutput("val_by_city_ui")
        ))
      ),
      fluidRow(column(12, div(class="card-env",
        div(class="card-title","Série temporelle — Valeurs réelles vs Prédites"),
        highchartOutput("hc_val_ts", height="240px")
      ))),
      fluidRow(
        column(6, div(class="card-env",
          div(class="card-title","Distribution des erreurs (Réel − Prédit)"),
          highchartOutput("hc_val_error", height="210px")
        )),
        column(6, div(class="card-env",
          div(class="card-title","Tableau des résultats"),
          div(style="max-height:210px;overflow-y:auto;", uiOutput("val_table_ui"))
        ))
      )
    )

    ,tabItem(tabName="modeles",

      # Bandeau titre
      fluidRow(column(12,
        div(style="background:linear-gradient(135deg,#1c2b3a,#374151);border-radius:14px;padding:20px 28px;margin-bottom:16px;color:white;",
          div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:12px;",
            div(
              div(style="font-size:11px;text-transform:uppercase;letter-spacing:.12em;opacity:.7;margin-bottom:4px;","Evaluation — 7 Algorithmes"),
              div(style="font-size:20px;font-weight:800;","Comparaison des Modèles Entraînés")
            ),
            div(style="display:flex;gap:10px;",
              div(style="text-align:center;background:rgba(255,255,255,.12);border-radius:10px;padding:10px 16px;",
                div(style="font-size:1.6rem;font-weight:800;font-family:'DM Mono',monospace;","7"),
                div(style="font-size:11px;opacity:.7;","Modèles")),
              div(style="text-align:center;background:rgba(255,255,255,.12);border-radius:10px;padding:10px 16px;",
                div(style="font-size:1.6rem;font-weight:800;font-family:'DM Mono',monospace;","49"),
                div(style="font-size:11px;opacity:.7;","Features")),
              div(style="text-align:center;background:rgba(255,255,255,.12);border-radius:10px;padding:10px 16px;",
                div(style="font-size:1.6rem;font-weight:800;font-family:'DM Mono',monospace;","87k"),
                div(style="font-size:11px;opacity:.7;","Observations"))
            )
          )
        )
      )),

      # KPIs meilleur modèle
      fluidRow(
        column(3, uiOutput("mod_kpi_best")),
        column(3, uiOutput("mod_kpi_r2")),
        column(3, uiOutput("mod_kpi_mae")),
        column(3, uiOutput("mod_kpi_mape"))
      ),

      # Graphiques principaux
      fluidRow(
        column(7, div(class="card-env",
          div(class="card-title","Comparaison R² — 7 Modèles"),
          highchartOutput("hc_mod_r2", height="280px")
        )),
        column(5, div(class="card-env",
          div(class="card-title","Meilleur modèle — Résumé"),
          uiOutput("mod_best_summary")
        ))
      ),

      fluidRow(
        column(6, div(class="card-env",
          div(class="card-title","MAE et RMSE comparés"),
          highchartOutput("hc_mod_err", height="240px")
        )),
        column(6, div(class="card-env",
          div(class="card-title","MAPE (%) — Erreur relative"),
          highchartOutput("hc_mod_mape", height="240px")
        ))
      ),

      fluidRow(
        column(12, div(class="card-env",
          div(class="card-title","Importance des Variables — Top 15"),
          div(style="font-size:12px;color:#6b7280;margin-bottom:8px;",
            "Contribution relative de chaque variable météorologique dans la prédiction du PM2.5."),
          highchartOutput("hc_mod_feat", height="300px")
        ))
      ),

      # Tableau complet
      fluidRow(column(12, div(class="card-env",
        div(class="card-title","Tableau complet des performances"),
        uiOutput("mod_table_ui")
      )))
    )



    ,tabItem(tabName="equipe",
      fluidRow(column(12,
        div(style="background:linear-gradient(135deg,#1c2b3a,#374151);border-radius:14px;padding:24px 32px;margin-bottom:20px;color:white;text-align:center;",
          div(style="font-size:11px;text-transform:uppercase;letter-spacing:.15em;opacity:.6;margin-bottom:8px;",
            "Hackathon IndabaX Cameroon 2026"),
          div(style="font-size:24px;font-weight:800;letter-spacing:-.02em;margin-bottom:8px;",
            "Equipe de Conception"),
          div(style="font-size:14px;opacity:.75;max-width:600px;margin:0 auto;",
            "Equipe pluridisciplinaire engagee dans le developpement de solutions",
            " IA pour la surveillance de la qualite de l'air au Cameroun.")
        )
      )),
      fluidRow(
        column(6, uiOutput("card_membre1")),
        column(6, uiOutput("card_membre2"))
      ),
      fluidRow(
        column(6, uiOutput("card_membre3")),
        column(6, uiOutput("card_membre4"))
      ),
      fluidRow(column(12,
        div(style="background:#f8fafc;border:1px solid #e5e7eb;border-radius:14px;padding:20px 28px;margin-top:4px;text-align:center;",
          div(style="font-size:13px;color:#6b7280;",
            "Projet realise dans le cadre du ",
            tags$b("Hackathon IndabaX Cameroon 2026"),
            " — Theme : L'IA au service de la resilience climatique et sanitaire au Cameroun."),
          div(style="margin-top:10px;display:flex;justify-content:center;gap:24px;font-size:12px;color:#94a3b8;",
            div(tagList(icon("database"),  " 87 240 observations")),
            div(tagList(icon("city"),       " 40 villes")),
            div(tagList(icon("chart-line"), " 7 modeles entraines")),
            div(tagList(icon("globe"),      " API deployee sur Render"))
          )
        )
      ))
    )


    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ── LANGUE ──────────────────────────────────────────────────────────────
  lang <- reactiveVal("FR")
  observeEvent(input$switch_lang, {
    lang(if (lang()=="FR") "EN" else "FR")
  }, ignoreInit=TRUE)
  tr <- function(key) LANG[[lang()]][[key]]

  # Bouton langue
  output$btn_lang    <- renderUI({ tags$span(tr("btn")) })
  output$btn_refresh <- renderUI({ tags$span(tr("refresh")) })

  # Seuils bilingues
  output$sid_seuils <- renderUI({
    tags$div(style="padding:8px 18px;",
      tags$div(style="font-size:10px;text-transform:uppercase;letter-spacing:.15em;color:#475569;margin-bottom:10px;font-weight:700;",
        tr("thresh")),
      tags$div(class="seuil-item",
        tags$span(class="seuil-dot",style="background:#16a34a;"),
        tags$span(style="font-size:13px;color:#94a3b8;", tr("good"))),
      tags$div(class="seuil-item",
        tags$span(class="seuil-dot",style="background:#d97706;"),
        tags$span(style="font-size:13px;color:#94a3b8;", tr("moderate"))),
      tags$div(class="seuil-item",
        tags$span(class="seuil-dot",style="background:#ea580c;"),
        tags$span(style="font-size:13px;color:#94a3b8;", tr("bad"))),
      tags$div(class="seuil-item",
        tags$span(class="seuil-dot",style="background:#dc2626;"),
        tags$span(style="font-size:13px;color:#94a3b8;", tr("dangerous")))
    )
  })

  # Footer bilingue
  output$sid_footer <- renderUI({
    tags$div(style="padding:8px 18px 16px;font-size:12px;color:#475569;line-height:2;",
      tags$div(icon("cloud-sun"),  " ", tr("src1")),
      tags$div(icon("brain"),      " ", tr("src2")),
      tags$div(icon("database"),   " ", tr("src3"))
    )
  })

  # DEBUG + Init villes ──────────────────────────────────────────────────
  observe({
    message("[INFO] Demarrage")
    message("DF chargé   : ", !is.null(DF))
    message("Nb lignes   : ", if(!is.null(DF)) nrow(DF) else "NULL")
    message("CITY_STATS  : ", if(!is.null(CITY_STATS)) nrow(CITY_STATS) else "NULL")
    message("CITIES[1:3] : ", paste(head(CITIES,3), collapse=", "))

    # Mettre à jour la liste des villes avec les données réelles
    if (length(CITIES) > 0) {
      sel <- if ("Yaounde" %in% CITIES) "Yaounde" else CITIES[1]
      updateSelectInput(session, "ville", choices=CITIES, selected=sel)
    }
  })

  # Données réactives ──────────────────────────────────────────────────────
  vd <- reactive({
    v <- VILLES[[input$ville]]
    list(lat=as.numeric(v[1]), lon=as.numeric(v[2]), region=v[3])
  })

  df_ville <- reactive({
    req(!is.null(DF))
    DF |> filter(city == input$ville)
  })

  df_hist <- reactive({
    df <- df_ville()
    if (input$h_year != "all") df <- df |> filter(year == as.integer(input$h_year))
    if (input$h_month != "all") df <- df |> filter(month == as.integer(input$h_month))
    df
  })

  city_stat <- reactive({
    if (is.null(CITY_STATS)) return(NULL)
    CITY_STATS |> filter(city == input$ville)
  })

  meteo_data <- reactive({
    input$btn_predict
    get_meteo(vd()$lat, vd()$lon)
  })

  preds_7j <- reactive({
    input$btn_predict
    m <- meteo_data(); if(is.null(m)) return(NULL)
    v <- vd()
    lapply(seq_along(m$time), function(i) {
      f <- build_features(m,i,v$lat,v$lon,input$ville,v$region)
      r <- call_lasso(f)
      list(date=m$time[i], pm25=if(!is.null(r)) r$prediction else NA)
    })
  })

  pred0 <- reactive({ p <- preds_7j(); if(is.null(p)) NULL else p[[1]] })

  # HEADER ────────────────────────────────────────────────────────────────
  output$header_status <- renderUI({
    p <- pred0()
    if(is.null(p)||is.na(p$pm25))
      tags$span(class="api-status api-wait", icon("spinner"), " Chargement...")
    else
      tags$span(class="api-status api-ok",
        icon("circle-check"), " ",
        paste0(input$ville," — ",round(p$pm25,1)," µg/m³ — ",format(Sys.time(),"%H:%M")))
  })

  # HERO VILLE (photo de fond + description) ───────────────────────────────
  output$ville_hero_card <- renderUI({
    cs <- city_stat()
    pm <- if(!is.null(cs)&&nrow(cs)>0) cs$pm25_mean[1] else NA
    rc <- qcol(pm); rl <- qlab(pm, lang()); ri <- qico(pm)
    bg <- get_bg(input$ville)
    desc <- get_desc(input$ville)
    vdat <- vd()

    div(class="ville-hero",
      div(class="ville-hero-bg",
        style=paste0("background-image:url('",bg,"');")),
      div(class="ville-hero-content",
        div(class="ville-hero-name", input$ville),
        div(class="ville-hero-region",
          tags$span(style="opacity:.7;","Région "), vdat$region,
          tags$span(style="margin:0 8px;opacity:.4;","·"),
          tags$span(style="opacity:.7;","PM2.5 moy. "),
          tags$span(style=paste0("color:",rc,";font-weight:700;"),
            if(!is.na(pm)) paste0(pm," µg/m³") else "—")
        ),
        div(class="ville-hero-desc", desc)
      ),
      div(class="ville-hero-badge",
        style=paste0("background:",rc,"99;"),
        ri, " ", rl
      )
    )
  })

  # KPIs HISTORIQUES ──────────────────────────────────────────────────────
  # Correspondance icone -> Font Awesome
  fa_icon <- function(name) {
    ico_map <- list(
      "wind"  = "wind",
      "temp"  = "thermometer-half",
      "rain"  = "cloud-rain",
      "isa"   = "rotate",
      "map"   = "earth-africa",
      "city"  = "city",
      "alert" = "triangle-exclamation",
      "check" = "circle-check",
      "chart" = "chart-line",
      "rank"  = "ranking-star"
    )
    ico_map[[name]] %||% "circle-info"
  }

  mkpi <- function(ico, val, lbl, sub="", col=NULL) {
    div(class="kpi-box",
      div(style=paste0("font-size:20px;margin-bottom:8px;color:",
        if(!is.null(col)) col else "#6b7280",";"),
        icon(fa_icon(ico))
      ),
      div(class="kpi-value",
        style=if(!is.null(col)) paste0("color:",col,";") else "color:#111827;",
        val),
      div(class="kpi-label", lbl),
      if(sub != "") div(class="kpi-sub", sub)
    )
  }

  output$kpi_hist_pm25 <- renderUI({
    cs <- city_stat()
    if(is.null(cs)||nrow(cs)==0) return(mkpi("wind","—","PM2.5 Moyen"))
    mkpi("wind",paste0(cs$pm25_mean[1]," µg"),"PM2.5 Moyen","2020-2025",qcol(cs$pm25_mean[1]))
  })
  output$kpi_hist_temp <- renderUI({
    cs <- city_stat()
    if(is.null(cs)||nrow(cs)==0) return(mkpi("temp","—","Température"))
    mkpi("temp",paste0(cs$temp_mean[1],"°C"),"Temp. Moy.","2020-2025","#2563eb")
  })
  output$kpi_hist_precip <- renderUI({
    cs <- city_stat()
    if(is.null(cs)||nrow(cs)==0) return(mkpi("rain","—","Précipitations"))
    mkpi("rain",paste0(cs$precip_mean[1],"mm"),"Précip. Moy.","par jour","#0891b2")
  })
  output$kpi_hist_vent <- renderUI({
    cs <- city_stat()
    if(is.null(cs)||nrow(cs)==0) return(mkpi("wind","—","Vent"))
    mkpi("wind",paste0(cs$wind_mean[1]),"Vent Moy. km/h","2020-2025","#7c3aed")
  })

  # HC MENSUEL ────────────────────────────────────────────────────────────
  output$hc_monthly <- renderHighchart({
    df <- df_ville(); req(nrow(df)>0)
    mois_noms <- c("Jan","Fév","Mar","Avr","Mai","Jun","Jul","Aoû","Sep","Oct","Nov","Déc")
    monthly <- df |> group_by(month) |>
      summarise(pm25=round(mean(pm25_proxy,na.rm=TRUE),2), .groups="drop") |>
      arrange(month)
    cols <- sapply(monthly$pm25, qcol)
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(categories=mois_noms[monthly$month],
      labels=list(style=list(color="#6b7280",fontFamily="DM Sans",fontSize="12px"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="12px")),
      labels=list(style=list(color="#6b7280")),
      plotLines=list(
        list(value=16.55,color="#16a34a",dashStyle="Dash",width=1,
          label=list(text="Bonne",style=list(color="#16a34a",fontSize="10px"))),
        list(value=22.96,color="#d97706",dashStyle="Dash",width=1,
          label=list(text="Modérée",style=list(color="#d97706",fontSize="10px"))),
        list(value=30.11,color="#dc2626",dashStyle="Dash",width=1,
          label=list(text="Mauvaise",style=list(color="#dc2626",fontSize="10px")))
      )) |>
    hc_add_series(name="PM2.5",data=as.list(monthly$pm25),type="column",
      colorByPoint=TRUE,colors=as.list(cols)) |>
    hc_tooltip(pointFormat="<b>{point.y:.2f} µg/m³</b>",
      backgroundColor="white",borderColor="#e5e7eb",
      style=list(color="#111827",fontFamily="DM Mono")) |>
    hc_add_theme(hc_theme_null())
  })

  # HC ANNUEL ─────────────────────────────────────────────────────────────
  output$hc_yearly <- renderHighchart({
    df <- df_ville(); req(nrow(df)>0)
    yearly <- df |> group_by(year) |>
      summarise(pm25=round(mean(pm25_proxy,na.rm=TRUE),2), .groups="drop") |>
      arrange(year)
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(categories=as.character(yearly$year),
      labels=list(style=list(color="#6b7280",fontSize="12px"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="12px")),
      labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="PM2.5 Annuel",data=as.list(yearly$pm25),type="spline",
      color="#2563eb",lineWidth=3,
      marker=list(enabled=TRUE,radius=5,fillColor="#2563eb",lineColor="white",lineWidth=2)) |>
    hc_tooltip(pointFormat="<b>{point.y:.2f} µg/m³</b>",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # HC SÉRIE TEMPORELLE HISTORIQUE ────────────────────────────────────────
  output$hc_ts_hist <- renderHighchart({
    df <- df_ville(); req(nrow(df)>0)
    df_s <- df |> arrange(time) |> filter(row_number() %% 7 == 1)  # allégé
    dates_ms <- as.numeric(as.POSIXct(df_s$time)) * 1000
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(type="datetime",labels=list(style=list(color="#6b7280",fontSize="11px"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="12px")),
      labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="PM2.5",
      data=lapply(seq_along(dates_ms),function(i) list(dates_ms[i],round(df_s$pm25_proxy[i],2))),
      type="area",color="#2563eb",fillColor="rgba(37,99,235,0.07)",lineWidth=1.5) |>
    hc_add_series(name="Précip.",
      data=lapply(seq_along(dates_ms),function(i) list(dates_ms[i],round(df_s$precipitation_sum[i],1))),
      type="column",color="rgba(8,145,178,0.4)",borderColor="#0891b2",yAxis=1) |>
    hc_yAxis_multiples(
      list(title=list(text="PM2.5",style=list(color="#6b7280",fontSize="11px")),labels=list(style=list(color="#6b7280"))),
      list(title=list(text="Précip mm",style=list(color="#0891b2",fontSize="11px")),labels=list(style=list(color="#0891b2")),opposite=TRUE)
    ) |>
    hc_tooltip(shared=TRUE,backgroundColor="white",borderColor="#e5e7eb",
      style=list(color="#111827",fontFamily="DM Sans")) |>
    hc_add_theme(hc_theme_null())
  })

  # PRÉDICTION TEMPS RÉEL ─────────────────────────────────────────────────
  output$alerte_banner <- renderUI({
    p <- pred0(); if(is.null(p)||is.na(p$pm25)) return(NULL)
    pm <- p$pm25
    cls <- if(pm<=16.55)"alerte-ok" else if(pm<=22.96)"alerte-warn" else "alerte-danger"
    ico <- if(pm<=16.55) icon("circle-check") else if(pm<=22.96) icon("triangle-exclamation") else icon("circle-exclamation")
    msg <- if(pm<=16.55)"Qualité satisfaisante — aucun risque sanitaire."
           else if(pm<=22.96)"Qualité modérée — personnes sensibles : restez vigilants."
           else "Qualité mauvaise — limitez les activités en extérieur."
    div(class=paste("alerte-banner",cls),
      tags$span(style="font-size:1.1rem;margin-right:4px;",ico),
      tags$div(tags$b(qlab(pm, lang())," "),tags$span(style="font-weight:400;",msg))
    )
  })

  output$pred_hero <- renderUI({
    p <- pred0()
    if(is.null(p)) return(div(class="loading-env",div(class="spinner"),
      tags$span("Connexion aux APIs...")))
    if(is.na(p$pm25)) return(div(style="text-align:center;padding:16px;color:#dc2626;",
        "API indisponible — reessayez dans quelques instants"))
    pm <- round(p$pm25,2); rc <- qcol(pm); rl <- qlab(pm, lang()); ri <- qico(pm)
    div(class="pred-hero",style=paste0("background:linear-gradient(135deg,",rc,"dd,",rc,"99);"),
      div(class="pred-label",format(as.Date(p$date),"%A %d %B %Y")),
      div(class="pred-pm25",paste0(pm," µg/m³")),
      div(class="pred-badge",ri," ",rl)
    )
  })

  output$meteo_actuel <- renderUI({
    m <- meteo_data()
    if(is.null(m)) return(div(class="loading-env",div(class="spinner")))
    div(class="meteo-grid",
      div(class="meteo-item",span(class="val",paste0(round(m$temperature_2m_max[1],1),"°C")),span(class="lbl","Tmax")),
      div(class="meteo-item",span(class="val",paste0(round(m$temperature_2m_min[1],1),"°C")),span(class="lbl","Tmin")),
      div(class="meteo-item",span(class="val",paste0(round(m$wind_speed_10m_max[1],1))),span(class="lbl","Vent km/h")),
      div(class="meteo-item",span(class="val",paste0(round(m$precipitation_sum[1],1))),span(class="lbl","Pluie mm")),
      div(class="meteo-item",span(class="val",paste0(round(m$shortwave_radiation_sum[1],0))),span(class="lbl","Rayt MJ/m²")),
      div(class="meteo-item",span(class="val",paste0(round(m$et0_fao_evapotranspiration[1],1))),span(class="lbl","ET0 mm")),
      div(class="meteo-item",span(class="val",paste0(round(m$wind_gusts_10m_max[1],1))),span(class="lbl","Rafales")),
      div(class="meteo-item",span(class="val",paste0(round(m$precipitation_hours[1],0),"h")),span(class="lbl","Hrs pluie"))
    )
  })

  # KPIs temps réel
  output$kpi_pm25_rt <- renderUI({
    p <- pred0()
    if(is.null(p)||is.na(p$pm25)) return(mkpi("wind","—","PM2.5"))
    mkpi("wind",paste0(round(p$pm25,1),"µg"),"PM2.5 Prédit",qlab(p$pm25, lang()),qcol(p$pm25))
  })
  output$kpi_isa_rt <- renderUI({
    m <- meteo_data(); if(is.null(m)) return(mkpi("isa","—","ISA"))
    wc <- max(m$wind_speed_10m_max[1],.5)
    isa <- (1/wc)*(1-min(m$precipitation_sum[1],10)/10)*(1+max(0,m$temperature_2m_max[1]-30)/10)
    col <- if(isa>2)"#dc2626" else if(isa>1)"#d97706" else "#16a34a"
    mkpi("isa",round(isa,3),"ISA","Stagnation atm.",col)
  })
  output$kpi_temp_rt <- renderUI({
    m <- meteo_data(); if(is.null(m)) return(mkpi("temp","—","Temp."))
    mkpi("temp",paste0(round(m$temperature_2m_mean[1],1),"°C"),"Température","Aujourd'hui","#2563eb")
  })
  output$kpi_vent_rt <- renderUI({
    m <- meteo_data(); if(is.null(m)) return(mkpi("wind","—","Vent"))
    v <- m$wind_speed_10m_max[1]
    col <- if(v<5)"#dc2626" else if(v<15)"#d97706" else "#16a34a"
    mkpi("wind",paste0(round(v,1),"km/h"),"Vent Max",if(v<5)"Stagnation" else "OK",col)
  })

  # HC FORECAST LINE
  output$hc_forecast <- renderHighchart({
    p7 <- preds_7j(); if(is.null(p7)) return(NULL)
    dates <- as.Date(sapply(p7,function(x)x$date))
    pm25s <- as.numeric(sapply(p7,function(x)if(is.na(x$pm25))NA else round(x$pm25,2)))
    ms <- as.numeric(as.POSIXct(dates))*1000
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(type="datetime",labels=list(style=list(color="#6b7280",fontSize="11px"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="11px")),
      labels=list(style=list(color="#6b7280")),
      plotLines=list(
        list(value=16.55,color="#16a34a",dashStyle="Dash",width=1),
        list(value=22.96,color="#d97706",dashStyle="Dash",width=1),
        list(value=30.11,color="#dc2626",dashStyle="Dash",width=1)
      )) |>
    hc_add_series(name="PM2.5 Prédit",
      data=lapply(seq_along(ms),function(i)list(ms[i],pm25s[i])),
      type="spline",color="#2563eb",lineWidth=3,
      marker=list(enabled=TRUE,radius=5,fillColor="#2563eb",lineColor="white",lineWidth=2)) |>
    hc_tooltip(pointFormat="<b>{point.y:.2f} µg/m³</b>",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # HC ISA GAUGE
  output$hc_isa <- renderHighchart({
    m <- meteo_data(); if(is.null(m)) return(NULL)
    wc <- max(m$wind_speed_10m_max[1],.5)
    isa <- (1/wc)*(1-min(m$precipitation_sum[1],10)/10)*(1+max(0,m$temperature_2m_max[1]-30)/10)
    pct <- min(round(isa/3*100,1),100)
    col <- if(isa>2)"#dc2626" else if(isa>1)"#d97706" else "#16a34a"
    highchart() |>
    hc_chart(type="solidgauge",backgroundColor="white") |>
    hc_pane(startAngle=-90,endAngle=90,
      background=list(list(outerRadius="100%",innerRadius="62%",
        backgroundColor="#f5f6fa",borderWidth=0))) |>
    hc_yAxis(min=0,max=100,lineWidth=0,minorTickInterval=NULL,labels=list(enabled=FALSE),
      stops=list(list(0.33,"#16a34a"),list(0.66,"#d97706"),list(1,"#dc2626"))) |>
    hc_add_series(data=list(list(y=pct,color=col)),
      dataLabels=list(
        format=paste0('<span style="color:',col,';font-family:DM Mono;font-size:1.3rem;font-weight:700;">',round(isa,3),'</span><br><span style="font-size:11px;color:#6b7280;">ISA</span>'),
        borderWidth=0,useHTML=TRUE,y=-20)) |>
    hc_tooltip(enabled=FALSE) |>
    hc_add_theme(hc_theme_null())
  })

  # HC PRÉCIP RT
  output$hc_precip_rt <- renderHighchart({
    m <- meteo_data(); p7 <- preds_7j()
    if(is.null(m)||is.null(p7)) return(NULL)
    cats <- format(as.Date(m$time),"%d/%m")
    pm25s <- as.numeric(sapply(p7,function(x)if(is.na(x$pm25))0 else round(x$pm25,2)))
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(categories=cats,labels=list(style=list(color="#6b7280",fontSize="10px"))) |>
    hc_yAxis_multiples(
      list(title=list(text="PM2.5",style=list(color="#6b7280",fontSize="11px")),labels=list(style=list(color="#6b7280"))),
      list(title=list(text="Précip mm",style=list(color="#0891b2",fontSize="11px")),labels=list(style=list(color="#0891b2")),opposite=TRUE)
    ) |>
    hc_add_series(name="PM2.5",data=as.list(pm25s),type="line",color="#2563eb",lineWidth=2,yAxis=0) |>
    hc_add_series(name="Précip.",data=as.list(round(m$precipitation_sum,1)),type="column",
      color="rgba(8,145,178,0.4)",borderColor="#0891b2",yAxis=1) |>
    hc_tooltip(shared=TRUE,backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # FORECAST CARDS
  output$forecast_cards <- renderUI({
    p7 <- preds_7j()
    if(is.null(p7)) return(div(class="loading-env",div(class="spinner"),"Calcul..."))
    jf <- c("Dim","Lun","Mar","Mer","Jeu","Ven","Sam")
    div(class="forecast-grid",
      lapply(seq_along(p7),function(i){
        d <- as.Date(p7[[i]]$date)
        pm <- if(!is.na(p7[[i]]$pm25)) round(p7[[i]]$pm25,1) else "—"
        rc <- if(!is.na(p7[[i]]$pm25)) qcol(p7[[i]]$pm25) else "#6b7280"
        ri <- if(!is.na(p7[[i]]$pm25)) qico(p7[[i]]$pm25) else "question"
        div(class=if(i==1)"forecast-day today" else "forecast-day",
          div(class="dn",jf[as.integer(format(d,"%w"))+1]),
          div(class="dd",format(d,"%d/%m")),
          tags$span(class="di", icon(
          if(!is.na(p7[[i]]$pm25) && p7[[i]]$pm25<=16.55) "face-smile"
          else if(!is.na(p7[[i]]$pm25) && p7[[i]]$pm25<=22.96) "face-meh"
          else if(!is.na(p7[[i]]$pm25) && p7[[i]]$pm25<=30.11) "face-frown"
          else "skull-crossbones"
        )),
          div(class="dp",style=paste0("color:",rc,";"),paste0(pm," µg")),
          div(class="dq",style=paste0("color:",rc,";"),
            if(!is.na(p7[[i]]$pm25)) qlab(p7[[i]]$pm25, lang()) else "—")
        )
      })
    )
  })

  # HISTORIQUE ────────────────────────────────────────────────────────────
  output$hist_stats_panel <- renderUI({
    df <- df_hist(); req(nrow(df)>0)
    tagList(
              div(class="card-title","Statistiques"),
      div(class="info-row",span(class="info-key","Observations"),span(class="info-val",format(nrow(df),big.mark=","))),
      div(class="info-row",span(class="info-key","PM2.5 moy."),span(class="info-val",paste0(round(mean(df$pm25_proxy,na.rm=TRUE),2)," µg/m³"))),
      div(class="info-row",span(class="info-key","PM2.5 max"),span(class="info-val",paste0(round(max(df$pm25_proxy,na.rm=TRUE),2)," µg/m³"))),
      div(class="info-row",span(class="info-key","PM2.5 min"),span(class="info-val",paste0(round(min(df$pm25_proxy,na.rm=TRUE),2)," µg/m³"))),
      div(class="info-row",span(class="info-key","Jours alerte"),span(class="info-val",sum(df$pm25_proxy>30.11,na.rm=TRUE))),
      div(class="info-row",span(class="info-key","Temp. moy."),span(class="info-val",paste0(round(mean(df$temperature_2m_mean,na.rm=TRUE),1),"°C"))),
      div(class="info-row",span(class="info-key","Précip. moy."),span(class="info-val",paste0(round(mean(df$precipitation_sum,na.rm=TRUE),1),"mm")))
    )
  })

  output$hc_hist_ts <- renderHighchart({
    df <- df_hist(); req(nrow(df)>0)
    df <- df |> arrange(time) |> filter(row_number()%%3==1)
    ms <- as.numeric(as.POSIXct(df$time))*1000
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(type="datetime",labels=list(style=list(color="#6b7280",fontSize="11px"))) |>
    hc_yAxis_multiples(
      list(title=list(text="PM2.5 µg/m³",style=list(color="#6b7280",fontSize="11px")),labels=list(style=list(color="#6b7280"))),
      list(title=list(text="Précip mm",style=list(color="#0891b2",fontSize="11px")),labels=list(style=list(color="#0891b2")),opposite=TRUE)
    ) |>
    hc_add_series(name="PM2.5",
      data=lapply(seq_along(ms),function(i)list(ms[i],round(df$pm25_proxy[i],2))),
      type="area",color="#2563eb",fillColor="rgba(37,99,235,0.06)",lineWidth=1.5,yAxis=0) |>
    hc_add_series(name="Précip.",
      data=lapply(seq_along(ms),function(i)list(ms[i],round(df$precipitation_sum[i],1))),
      type="column",color="rgba(8,145,178,0.4)",borderColor="#0891b2",yAxis=1) |>
    hc_tooltip(shared=TRUE,backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  output$hc_hist_box <- renderHighchart({
    df <- df_hist(); req(nrow(df)>0)
    mois <- c("Jan","Fév","Mar","Avr","Mai","Jun","Jul","Aoû","Sep","Oct","Nov","Déc")
    highchart() |>
    hc_chart(type="boxplot",backgroundColor="white") |>
    hc_xAxis(categories=mois,labels=list(style=list(color="#6b7280",fontSize="11px"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="11px")),labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="PM2.5",
      data=lapply(1:12,function(m){
        v <- df$pm25_proxy[df$month==m]; v <- v[!is.na(v)]
        if(length(v)<2) return(list(NULL))
        list(low=round(min(v),2),q1=round(quantile(v,.25),2),
          median=round(median(v),2),q3=round(quantile(v,.75),2),high=round(max(v),2))
      }),
      color="#2563eb",fillColor="rgba(37,99,235,0.15)") |>
    hc_tooltip(backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  output$hc_hist_scatter <- renderHighchart({
    df <- df_hist(); req(nrow(df)>0)
    df_s <- df[sample(nrow(df),min(500,nrow(df))),]
    highchart() |>
    hc_chart(type="scatter",backgroundColor="white") |>
    hc_xAxis(title=list(text="Température moy. °C",style=list(color="#6b7280",fontSize="11px")),labels=list(style=list(color="#6b7280"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#6b7280",fontSize="11px")),labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="Obs.",
      data=lapply(seq_len(nrow(df_s)),function(i)list(round(df_s$temperature_2m_mean[i],1),round(df_s$pm25_proxy[i],2))),
      color="rgba(37,99,235,0.4)",marker=list(radius=2)) |>
    hc_tooltip(pointFormat="<b>Temp:</b> {point.x:.1f}°C<br><b>PM2.5:</b> {point.y:.2f} µg/m³",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # CARTE ─────────────────────────────────────────────────────────────────
  # DONNÉES HEATMAP (grille interpolée 80x80) ─────────────────────────────
  # Charger les JSON pré-calculés (à placer dans le même dossier que app.R)
  # Chargement des grilles heatmap (chemins définis en haut du script)
  load_grid_from_path <- function(path) {
    if (!file.exists(path)) {
      message("[MANQUANT] Grille : ", path)
      return(NULL)
    }
    d <- jsonlite::fromJSON(path)
    data.frame(lat1=d$lat1, lon1=d$lon1, lat2=d$lat2, lon2=d$lon2,
               vals=d$vals, cols=d$cols, stringsAsFactors=FALSE)
  }

  GRID_MEAN <- load_grid_from_path(HEATMAP_MEAN_PATH)
  GRID_MAX  <- load_grid_from_path(HEATMAP_MAX_PATH)
  if (!is.null(GRID_MEAN)) message("[OK] Grille mean : ", nrow(GRID_MEAN), " rectangles")
  if (!is.null(GRID_MAX))  message("[OK] Grille max  : ", nrow(GRID_MAX),  " rectangles")

  # Données des 40 villes pour les marqueurs
  VILLES_DF <- data.frame(
    city=c("Abong-Mbang","Akonolinga","Ambam","Bafia","Bafoussam","Bamenda",
           "Batouri","Bertoua","Buea","Douala","Dschang","Ebolowa","Edea",
           "Foumban","Garoua","Guider","Kousseri","Kribi","Kumba","Kumbo",
           "Limbe","Loum","Mamfe","Maroua","Mbalmayo","Mbengwi","Mbouda",
           "Meiganga","Mokolo","Ngaoundere","Nkongsamba","Poli","Sangmelima",
           "Tibati","Tignere","Touboro","Wum","Yagoua","Yaounde","Yokadouma"),
    lat=c(3.98,3.77,2.38,4.75,5.48,5.96,4.43,4.57,4.15,4.05,5.44,2.91,3.80,
          5.72,9.30,9.93,12.08,2.95,4.63,6.20,4.02,4.71,5.75,10.59,3.51,
          5.99,5.62,6.52,10.74,7.32,4.95,8.47,2.93,6.46,7.37,7.76,6.38,
          10.34,3.85,3.51),
    lon=c(13.17,12.25,11.28,11.23,10.42,10.15,14.36,13.68,9.24,9.70,10.05,
          11.15,10.13,10.92,13.40,13.94,15.03,9.91,9.44,10.66,9.21,9.73,
          9.31,14.32,11.50,10.00,10.25,14.29,13.80,13.58,9.93,13.24,11.98,
          12.62,12.65,15.36,10.07,15.23,11.52,15.05),
    pm25_mean=c(19.51,18.81,16.48,20.27,17.19,16.71,21.10,20.52,14.92,16.95,
                15.57,16.93,17.00,18.42,28.37,27.52,28.68,15.41,18.08,16.42,
                18.53,19.12,20.80,27.31,17.46,17.36,17.25,22.18,24.58,22.01,
                17.91,25.12,17.47,22.50,21.66,27.02,18.08,27.76,17.08,20.99),
    pm25_max=c(35.82,33.32,33.83,37.68,27.84,32.00,39.21,37.41,28.38,33.39,
               28.36,34.78,34.45,32.65,51.37,52.47,51.00,30.32,34.20,26.74,
               30.85,38.63,43.32,47.43,32.01,30.82,28.86,43.24,42.89,38.82,
               33.77,48.40,35.58,50.32,39.90,47.43,36.41,47.40,31.31,38.34),
    region=c("Est","Centre","Sud","Centre","Ouest","Nord-Ouest","Est","Est",
             "Sud-Ouest","Littoral","Ouest","Sud","Littoral","Ouest","Nord","Nord",
             "Extreme-Nord","Sud","Sud-Ouest","Nord-Ouest","Sud-Ouest","Littoral",
             "Sud-Ouest","Extreme-Nord","Centre","Nord-Ouest","Ouest","Adamaoua",
             "Extreme-Nord","Adamaoua","Littoral","Nord","Sud","Adamaoua","Adamaoua",
             "Nord","Nord-Ouest","Extreme-Nord","Centre","Est"),
    stringsAsFactors=FALSE
  )

  # CARTE HEATMAP ─────────────────────────────────────────────────────────
  output$map_heatmap <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl=TRUE, attributionControl=FALSE)) |>
    addProviderTiles("CartoDB.Positron",
      options=tileOptions(opacity=0.9, attribution="")) |>
    setView(lng=12.35, lat=6.5, zoom=6) |>
    addScaleBar(position="bottomleft",
      options=scaleBarOptions(imperial=FALSE))
  })

  observe({
    grid <- if(!is.null(input$map_mode) && input$map_mode=="max") GRID_MAX else GRID_MEAN
    val_col <- if(!is.null(input$map_mode) && input$map_mode=="max") "pm25_max" else "pm25_mean"

    proxy <- leafletProxy("map_heatmap") |>
      clearShapes() |> clearMarkers()

    # Heatmap par rectangles (style Chimere) ──
    if (!is.null(grid)) {
      proxy <- proxy |> addRectangles(
        lng1=grid$lon1, lat1=grid$lat1,
        lng2=grid$lon2, lat2=grid$lat2,
        fillColor=grid$cols,
        fillOpacity=1,
        stroke=FALSE,
        weight=0,
        options=pathOptions(interactive=FALSE)
      )
    }

    # Marqueurs villes par-dessus ──
    for (i in seq_len(nrow(VILLES_DF))) {
      v    <- VILLES_DF[i,]
      val  <- v[[val_col]]
      col  <- qcol(val)
      is_sel <- (!is.null(input$ville) && v$city == input$ville)
      proxy <- proxy |> addCircleMarkers(
        lng=v$lon, lat=v$lat,
        radius=if(is_sel) 10 else 6,
        color="white",
        fillColor=col,
        fillOpacity=0.95,
        weight=if(is_sel) 3 else 1.5,
        label=htmltools::HTML(paste0(
          "<b>", v$city, "</b> — ", round(val,1), " µg/m³"
        )),
        labelOptions=labelOptions(
          style=list("font-family"="DM Sans,sans-serif",
            "font-size"="13px","font-weight"="600",
            "border-color"=col,"background"="white",
            "padding"="4px 8px","border-radius"="6px")
        ),
        popup=paste0(
          "<div style='font-family:DM Sans,sans-serif;min-width:190px;padding:4px;'>",
          "<b style='font-size:15px;color:#111827;'>", v$city, "</b>",
          "<span style='float:right;font-size:11px;color:#6b7280;'>", v$region, "</span><br>",
          "<hr style='margin:6px 0;border-color:#e5e7eb;'>",
          "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;'>",
          "<span style='font-size:12px;color:#6b7280;'>PM2.5 moyen</span>",
          "<b style='color:", qcol(v$pm25_mean), ";font-size:15px;'>",
          v$pm25_mean, " µg/m³</b></div>",
          "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;'>",
          "<span style='font-size:12px;color:#6b7280;'>PM2.5 max</span>",
          "<b style='color:", qcol(v$pm25_max), ";font-size:13px;'>",
          v$pm25_max, " µg/m³</b></div>",
          "<div style='padding:6px 10px;border-radius:8px;background:", qcol(v$pm25_mean), "22;",
          "border:1px solid ", qcol(v$pm25_mean), "55;text-align:center;'>",
          "<b style='color:", qcol(v$pm25_mean), ";font-size:13px;'>",
          qlab(v$pm25_mean, lang()), "</b></div></div>"
        )
      )
    }

    # KPIs carte ──
    vals_all <- VILLES_DF[[val_col]]
    output$map_kpi1 <- renderUI({
      mkpi("map", paste0(round(mean(vals_all),1)," µg"), "Moyenne nationale", "40 villes", qcol(mean(vals_all)))
    })
    output$map_kpi2 <- renderUI({
      worst <- VILLES_DF$city[which.max(vals_all)]
      mkpi("alert", paste0(round(max(vals_all),1)," µg"), "Plus polluée", worst, "#dc2626")
    })
    output$map_kpi3 <- renderUI({
      best <- VILLES_DF$city[which.min(vals_all)]
      mkpi("check", paste0(round(min(vals_all),1)," µg"), "Plus propre", best, "#16a34a")
    })
    output$map_kpi4 <- renderUI({
      n_alert <- sum(vals_all > 22.96)
      mkpi("alert", paste0(n_alert,"/40"), "Villes polluées", "> seuil modéré",
        if(n_alert>15)"#dc2626" else "#d97706")
    })
  })

  output$map_ville_info <- renderUI({
    cs <- city_stat(); req(!is.null(cs)&&nrow(cs)>0)
    pm <- cs$pm25_mean[1]; rc <- qcol(pm)
    tagList(
      div(style=paste0("text-align:center;background:",rc,"18;border:1px solid ",rc,"44;border-radius:10px;padding:14px;margin-bottom:12px;"),
        div(style=paste0("font-size:2.2rem;font-weight:800;font-family:'DM Mono',monospace;color:",rc,";"),paste(pm,"µg/m³")),
        div(style=paste0("font-size:13px;color:",rc,";font-weight:700;"),qlab(pm, lang()))
      ),
      div(class="info-row",span(class="info-key","Ville"),span(class="info-val",input$ville)),
      div(class="info-row",span(class="info-key","Région"),span(class="info-val",cs$region[1])),
      div(class="info-row",span(class="info-key","PM2.5 max"),span(class="info-val",paste0(cs$pm25_max[1]," µg/m³"))),
      div(class="info-row",span(class="info-key","Jours alerte"),span(class="info-val",cs$alert_days[1])),
      div(class="info-row",span(class="info-key","Observations"),span(class="info-val",format(cs$n_obs[1],big.mark=",")))
    )
  })

  output$top5_polluted <- renderUI({
    req(!is.null(CITY_STATS))
    top5 <- head(CITY_STATS, 5)
    tagList(lapply(seq_len(nrow(top5)),function(i){
      cv <- top5[i,]; rc <- qcol(cv$pm25_mean)
      div(class="rank-item",
        div(class="rank-num",paste0("#",i)),
        div(class="rank-city",cv$city),
        div(class="rank-val",style=paste0("color:",rc,";"),paste0(cv$pm25_mean," µg/m³"))
      )
    }))
  })

  # CLASSEMENT ────────────────────────────────────────────────────────────
  output$hc_rank_bad <- renderHighchart({
    req(!is.null(CITY_STATS))
    top10 <- head(CITY_STATS, 10)
    cols <- sapply(top10$pm25_mean, qcol)
    highchart() |>
    hc_chart(type="bar",backgroundColor="white") |>
    hc_xAxis(categories=top10$city,labels=list(style=list(color="#374151",fontSize="12px",fontWeight="600"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="11px")),labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="PM2.5",data=as.list(top10$pm25_mean),colorByPoint=TRUE,colors=as.list(cols)) |>
    hc_tooltip(pointFormat="<b>{point.category}</b>: {point.y:.2f} µg/m³",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  output$hc_rank_good <- renderHighchart({
    req(!is.null(CITY_STATS))
    bot10 <- tail(CITY_STATS, 10) |> arrange(pm25_mean)
    cols <- sapply(bot10$pm25_mean, qcol)
    highchart() |>
    hc_chart(type="bar",backgroundColor="white") |>
    hc_xAxis(categories=bot10$city,labels=list(style=list(color="#374151",fontSize="12px",fontWeight="600"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="11px")),labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="PM2.5",data=as.list(bot10$pm25_mean),colorByPoint=TRUE,colors=as.list(cols)) |>
    hc_tooltip(pointFormat="<b>{point.category}</b>: {point.y:.2f} µg/m³",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  output$hc_rank_all <- renderHighchart({
    req(!is.null(CITY_STATS))
    df <- CITY_STATS |> arrange(pm25_mean)
    cols <- sapply(df$pm25_mean, qcol)
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(categories=df$city,labels=list(style=list(color="#6b7280",fontSize="10px"),rotation=-45)) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#9ca3af",fontSize="11px")),labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="PM2.5 Moy.",data=as.list(df$pm25_mean),type="column",
      colorByPoint=TRUE,colors=as.list(cols)) |>
    hc_tooltip(pointFormat="<b>{point.category}</b>: {point.y:.2f} µg/m³",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # ══════════════════════════════════════════════════════════════════════════
  # SERVER — VALIDATION MODÈLE
  # ══════════════════════════════════════════════════════════════════════════

  # Charger l'échantillon de validation
  # Chargement de l'échantillon de validation (chemin défini en haut)
  VAL_SAMPLE <- tryCatch({
    if (!file.exists(VALIDATION_PATH)) {
      message("[MANQUANT] Validation : ", VALIDATION_PATH)
      return(NULL)
    }
    d <- jsonlite::fromJSON(VALIDATION_PATH)
    message("[OK] Echantillon validation : ", nrow(d), " observations")
    d
  }, error=function(e) { message("[ERREUR] Validation : ", e$message); NULL })

  # Résultats de validation (reactiveVal pour stocker)
  val_results <- reactiveVal(NULL)
  val_running <- reactiveVal(FALSE)

  # Lancer la validation
  observeEvent(input$btn_validate, {
    req(!is.null(VAL_SAMPLE))
    val_results(NULL)
    val_running(TRUE)

    # Filtrer par ville
    sample_df <- VAL_SAMPLE
    if (!is.null(input$val_ville) && input$val_ville != "all") {
      sample_df <- sample_df[sample_df$city == input$val_ville, ]
    }

    # Limiter au nombre de points demandé
    n_pts <- as.integer(input$val_n)
    if (nrow(sample_df) > n_pts) {
      idx_sel <- round(seq(1, nrow(sample_df), length.out=n_pts))
      sample_df <- sample_df[idx_sel, ]
    }

    message("Validation sur ", nrow(sample_df), " points...")

    # Appeler l'API pour chaque point
    city_encs   <- setNames(seq_along(CITIES)*0.5, CITIES)
    region_encs <- list("Centre"=1,"Adamaoua"=2,"Est"=3,"Extreme-Nord"=4,
      "Littoral"=5,"Nord"=6,"Nord-Ouest"=7,"Ouest"=8,"Sud"=9,"Sud-Ouest"=10)

    results <- lapply(seq_len(nrow(sample_df)), function(i) {
      row <- sample_df[i,]
      wc  <- max(row$wind_speed_10m_max, 0.5)
      mo  <- row$month; yr <- row$year
      doy <- as.integer(format(as.Date(paste0(yr,"-",mo,"-15")),"%j"))

      # Construire les 49 features
      features <- list(
        temperature_2m_max         = row$temperature_2m_max,
        temperature_2m_min         = row$temperature_2m_min,
        temperature_2m_mean        = row$temperature_2m_mean,
        apparent_temperature_mean  = row$apparent_temperature_mean,
        precipitation_sum          = row$precipitation_sum,
        rain_sum                   = row$rain_sum,
        precip_log                 = row$precip_log,
        wind_speed_10m_max         = wc,
        wind_gusts_10m_max         = row$wind_gusts_10m_max,
        shortwave_radiation_sum    = row$shortwave_radiation_sum,
        et0_fao_evapotranspiration = row$et0_fao_evapotranspiration,
        sunshine_duration          = row$sunshine_duration,
        daylight_duration          = row$daylight_duration,
        precipitation_hours        = row$precipitation_hours,
        isa                        = row$isa,
        isa_scaled                 = row$isa_scaled,
        temp_amplitude             = row$temp_amplitude,
        sunshine_ratio             = row$sunshine_ratio,
        wind_gust_ratio            = row$wind_gust_ratio,
        wind_dir_sin               = row$wind_dir_sin,
        wind_dir_cos               = row$wind_dir_cos,
        is_harmattan               = as.integer(row$is_harmattan),
        is_dry_season              = as.integer(row$is_dry_season),
        is_stagnant                = as.integer(row$is_stagnant),
        is_no_rain                 = as.integer(row$is_no_rain),
        month_sin                  = row$month_sin,
        month_cos                  = row$month_cos,
        dayofyear_sin              = row$dayofyear_sin,
        dayofyear_cos              = row$dayofyear_cos,
        year                       = as.integer(yr),
        temp_lag1                  = row$temperature_2m_mean - 0.3,
        temp_lag3                  = row$temperature_2m_mean - 0.6,
        temp_lag7                  = row$temperature_2m_mean - 1.0,
        wind_lag1                  = wc - 0.2,
        wind_lag3                  = wc - 0.4,
        wind_lag7                  = wc - 0.6,
        rain_lag1                  = row$precipitation_sum * 0.8,
        rain_lag3                  = row$precipitation_sum * 0.5,
        rain_lag7                  = row$precipitation_sum * 0.2,
        isa_lag1                   = row$isa * 0.95,
        isa_lag3                   = row$isa * 0.90,
        isa_lag7                   = row$isa * 0.85,
        temp_roll7                 = row$temperature_2m_mean - 0.5,
        wind_roll7                 = wc - 0.3,
        precip_roll7               = row$precipitation_sum * 0.4,
        latitude                   = row$lat,
        longitude                  = row$lon,
        city_enc                   = as.numeric(city_encs[row$city] %||% 1.0),
        region_enc                 = as.numeric(region_encs[[row$region]] %||% 1.0)
      )

      res <- call_lasso(features)
      pred <- if (!is.null(res)) round(res$prediction, 3) else NA
      list(
        city     = row$city,
        region   = row$region,
        date     = row$date,
        year     = yr,
        month    = mo,
        pm25_real= round(row$pm25_real, 3),
        pm25_pred= pred,
        error    = if (!is.na(pred)) round(row$pm25_real - pred, 3) else NA,
        abs_error= if (!is.na(pred)) round(abs(row$pm25_real - pred), 3) else NA,
        pct_error= if (!is.na(pred)) round(abs(row$pm25_real - pred) / max(row$pm25_real, 0.01) * 100, 2) else NA
      )
    })

    df_res <- do.call(rbind, lapply(results, as.data.frame))
    df_res <- df_res[!is.na(df_res$pm25_pred), ]
    message("Validation terminée : ", nrow(df_res), " prédictions OK")
    val_results(df_res)
    val_running(FALSE)
  })

  # Barre de progression / statut
  output$val_progress_ui <- renderUI({
    if (val_running()) {
      div(style="background:#eff6ff;border:1px solid #bfdbfe;border-radius:12px;padding:16px 20px;margin-bottom:16px;display:flex;align-items:center;gap:12px;",
        div(class="spinner",style="border-top-color:#2563eb;"),
        div(
          div(style="font-size:14px;font-weight:600;color:#1d4ed8;","Appel de l'API Lasso en cours..."),
          div(style="font-size:12px;color:#3b82f6;","Veuillez patienter — chaque point est envoyé à l'API Render.")
        )
      )
    } else if (!is.null(val_results()) && nrow(val_results()) > 0) {
      df <- val_results()
      mae  <- round(mean(df$abs_error, na.rm=TRUE), 3)
      mape <- round(mean(df$pct_error, na.rm=TRUE), 2)
      r2   <- round(1 - sum((df$pm25_real - df$pm25_pred)^2, na.rm=TRUE) /
                        sum((df$pm25_real - mean(df$pm25_real))^2, na.rm=TRUE), 4)
      div(style="background:#f0fdf4;border:1px solid #bbf7d0;border-radius:12px;padding:14px 20px;margin-bottom:16px;display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
        tags$span(style="font-size:1rem;margin-right:6px;", icon("circle-check")),
        div(style="font-size:14px;font-weight:600;color:#15803d;",
          paste0("Validation terminée — ", nrow(df), " prédictions réussies")),
        div(style="display:flex;gap:12px;margin-left:auto;",
          div(style="background:white;border:1px solid #bbf7d0;border-radius:8px;padding:6px 14px;text-align:center;",
            div(style="font-size:1.1rem;font-weight:800;font-family:'DM Mono',monospace;color:#15803d;",r2),
            div(style="font-size:10px;color:#6b7280;text-transform:uppercase;","R²")),
          div(style="background:white;border:1px solid #bbf7d0;border-radius:8px;padding:6px 14px;text-align:center;",
            div(style="font-size:1.1rem;font-weight:800;font-family:'DM Mono',monospace;color:#2563eb;",paste0(mape,"%")),
            div(style="font-size:10px;color:#6b7280;text-transform:uppercase;","MAPE")),
          div(style="background:white;border:1px solid #bbf7d0;border-radius:8px;padding:6px 14px;text-align:center;",
            div(style="font-size:1.1rem;font-weight:800;font-family:'DM Mono',monospace;color:#7c3aed;",mae),
            div(style="font-size:10px;color:#6b7280;text-transform:uppercase;","MAE µg/m³"))
        )
      )
    } else {
      div(style="background:#f8fafc;border:1px dashed #cbd5e1;border-radius:12px;padding:20px;text-align:center;color:#94a3b8;margin-bottom:16px;font-size:14px;",
        "Configurez les paramètres et cliquez sur 'Lancer la validation' pour comparer le modèle Lasso aux données réelles.")
    }
  })

  # Nuage de points réel vs prédit
  output$hc_val_scatter <- renderHighchart({
    df <- val_results(); req(!is.null(df) && nrow(df) > 0)
    min_v <- floor(min(c(df$pm25_real, df$pm25_pred), na.rm=TRUE)) - 1
    max_v <- ceiling(max(c(df$pm25_real, df$pm25_pred), na.rm=TRUE)) + 1

    # Ligne de régression parfaite
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(title=list(text="PM2.5 Réel (µg/m³)", style=list(color="#6b7280",fontSize="12px")),
      labels=list(style=list(color="#6b7280")), min=min_v, max=max_v) |>
    hc_yAxis(title=list(text="PM2.5 Prédit (µg/m³)", style=list(color="#6b7280",fontSize="12px")),
      labels=list(style=list(color="#6b7280")), min=min_v, max=max_v) |>
    hc_add_series(name="Prédictions",
      data=lapply(seq_len(nrow(df)), function(i)
        list(x=df$pm25_real[i], y=df$pm25_pred[i],
             name=paste0(df$city[i]," — ",df$date[i]))),
      type="scatter",
      color="rgba(37,99,235,0.6)",
      marker=list(radius=4, symbol="circle")) |>
    hc_add_series(name="Prédiction parfaite",
      data=list(list(x=min_v,y=min_v), list(x=max_v,y=max_v)),
      type="line", color="#dc2626", dashStyle="Dash",
      lineWidth=2, marker=list(enabled=FALSE),
      enableMouseTracking=FALSE) |>
    hc_tooltip(
      formatter=JS("function(){
        if(this.series.name==='Prédictions'){
          return '<b>'+this.point.name+'</b><br>Réel: <b>'+this.x.toFixed(2)+' µg/m³</b><br>Prédit: <b>'+this.y.toFixed(2)+' µg/m³</b><br>Erreur: <b>'+(this.x-this.y).toFixed(3)+'</b>';
        } return false;
      }"),
      backgroundColor="white", borderColor="#e5e7eb",
      style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # Métriques calculées
  output$val_metrics_ui <- renderUI({
    df <- val_results(); req(!is.null(df) && nrow(df) > 0)
    mae  <- round(mean(df$abs_error, na.rm=TRUE), 4)
    rmse <- round(sqrt(mean(df$error^2, na.rm=TRUE)), 4)
    mape <- round(mean(df$pct_error, na.rm=TRUE), 3)
    r2   <- round(1 - sum((df$pm25_real-df$pm25_pred)^2,na.rm=TRUE) /
                      sum((df$pm25_real-mean(df$pm25_real))^2,na.rm=TRUE), 5)
    bias <- round(mean(df$error, na.rm=TRUE), 4)
    tagList(
      div(class="info-row",span(class="info-key","R²"),
        span(class="info-val",style="color:#16a34a;font-size:14px;",r2)),
      div(class="info-row",span(class="info-key","MAE"),
        span(class="info-val",paste0(mae," µg/m³"))),
      div(class="info-row",span(class="info-key","RMSE"),
        span(class="info-val",paste0(rmse," µg/m³"))),
      div(class="info-row",span(class="info-key","MAPE"),
        span(class="info-val",paste0(mape,"%"))),
      div(class="info-row",span(class="info-key","Biais"),
        span(class="info-val",paste0(bias," µg/m³"))),
      div(class="info-row",span(class="info-key","Points validés"),
        span(class="info-val",nrow(df)))
    )
  })

  # Par ville
  output$val_by_city_ui <- renderUI({
    df <- val_results(); req(!is.null(df) && nrow(df) > 0)
    by_city <- aggregate(cbind(pm25_real,pm25_pred,abs_error) ~ city,
      data=df, FUN=function(x) round(mean(x,na.rm=TRUE),2))
    by_city <- by_city[order(by_city$abs_error),]
    tagList(lapply(seq_len(min(6,nrow(by_city))), function(i){
      cv <- by_city[i,]
      col <- if(cv$abs_error < 0.5) "#16a34a" else if(cv$abs_error < 1.5) "#d97706" else "#dc2626"
      div(class="info-row",
        span(style="color:#374151;font-weight:600;font-size:12px;",cv$city),
        span(style=paste0("color:",col,";font-family:'DM Mono',monospace;font-size:12px;font-weight:700;"),
          paste0("MAE=",cv$abs_error))
      )
    }))
  })

  # Série temporelle réel vs prédit
  output$hc_val_ts <- renderHighchart({
    df <- val_results(); req(!is.null(df) && nrow(df) > 0)
    df <- df[order(df$date),]
    ms <- as.numeric(as.POSIXct(as.Date(df$date)))*1000
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(type="datetime", labels=list(style=list(color="#6b7280",fontSize="11px"))) |>
    hc_yAxis(title=list(text="PM2.5 µg/m³",style=list(color="#6b7280",fontSize="11px")),
      labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="Réel",
      data=lapply(seq_along(ms),function(i)list(ms[i],df$pm25_real[i])),
      type="line", color="#374151", lineWidth=2,
      marker=list(radius=3, fillColor="#374151")) |>
    hc_add_series(name="Prédit (Lasso)",
      data=lapply(seq_along(ms),function(i)list(ms[i],df$pm25_pred[i])),
      type="line", color="#2563eb", lineWidth=2, dashStyle="ShortDash",
      marker=list(radius=3, fillColor="#2563eb")) |>
    hc_tooltip(shared=TRUE, backgroundColor="white", borderColor="#e5e7eb",
      style=list(color="#111827",fontFamily="DM Sans")) |>
    hc_add_theme(hc_theme_null())
  })

  # Distribution des erreurs
  output$hc_val_error <- renderHighchart({
    df <- val_results(); req(!is.null(df) && nrow(df) > 0)
    errs <- df$error[!is.na(df$error)]
    # Créer histogramme
    h <- hist(errs, breaks=15, plot=FALSE)
    cols <- sapply(h$mids, function(x) if(abs(x)<0.5)"#16a34a" else if(abs(x)<1.5)"#d97706" else "#dc2626")
    highchart() |>
    hc_chart(type="column", backgroundColor="white") |>
    hc_xAxis(title=list(text="Erreur (Réel − Prédit) µg/m³",style=list(color="#6b7280",fontSize="11px")),
      categories=round(h$mids,2),labels=list(style=list(color="#6b7280",fontSize="10px"))) |>
    hc_yAxis(title=list(text="Fréquence",style=list(color="#6b7280",fontSize="11px")),
      labels=list(style=list(color="#6b7280"))) |>
    hc_add_series(name="Fréquence", data=as.list(h$counts),
      colorByPoint=TRUE, colors=as.list(cols)) |>
    hc_xAxis(plotLines=list(list(value=which.min(abs(h$mids)),
      color="#dc2626",dashStyle="Dash",width=2,
      label=list(text="0",style=list(color="#dc2626",fontSize="11px"))))) |>
    hc_tooltip(pointFormat="<b>{point.y}</b> obs.",
      backgroundColor="white",borderColor="#e5e7eb",style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # Tableau des résultats
  output$val_table_ui <- renderUI({
    df <- val_results(); req(!is.null(df) && nrow(df) > 0)
    df_show <- head(df[order(df$abs_error),], 20)
    tags$table(style="width:100%;border-collapse:collapse;font-size:12px;",
      tags$thead(
        tags$tr(
          tags$th(style="padding:6px 8px;text-align:left;color:#6b7280;font-size:10px;text-transform:uppercase;border-bottom:2px solid #e5e7eb;","Ville"),
          tags$th(style="padding:6px 8px;text-align:center;color:#6b7280;font-size:10px;text-transform:uppercase;border-bottom:2px solid #e5e7eb;","Date"),
          tags$th(style="padding:6px 8px;text-align:right;color:#6b7280;font-size:10px;text-transform:uppercase;border-bottom:2px solid #e5e7eb;","Réel"),
          tags$th(style="padding:6px 8px;text-align:right;color:#6b7280;font-size:10px;text-transform:uppercase;border-bottom:2px solid #e5e7eb;","Prédit"),
          tags$th(style="padding:6px 8px;text-align:right;color:#6b7280;font-size:10px;text-transform:uppercase;border-bottom:2px solid #e5e7eb;","Erreur")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df_show)), function(i){
          row <- df_show[i,]
          err_col <- if(row$abs_error<0.5)"#16a34a" else if(row$abs_error<1.5)"#d97706" else "#dc2626"
          bg <- if(i%%2==0) "#f8fafc" else "white"
          tags$tr(style=paste0("background:",bg,";"),
            tags$td(style="padding:6px 8px;font-weight:600;color:#374151;",row$city),
            tags$td(style="padding:6px 8px;text-align:center;color:#6b7280;",row$date),
            tags$td(style="padding:6px 8px;text-align:right;font-family:'DM Mono',monospace;",paste0(row$pm25_real," µg")),
            tags$td(style="padding:6px 8px;text-align:right;font-family:'DM Mono',monospace;color:#2563eb;",paste0(row$pm25_pred," µg")),
            tags$td(style=paste0("padding:6px 8px;text-align:right;font-family:'DM Mono',monospace;font-weight:700;color:",err_col,";"),
              paste0(if(row$error>=0)"+","",round(row$error,3)))
          )
        })
      )
    )
  })


  # ============================================================================
  # SERVER — COMPARAISON DES MODELES
  # ============================================================================

  # Données des modèles (depuis METRICS chargé en haut du script)
  get_models_df <- function() {
    req(!is.null(METRICS))
    nms   <- names(METRICS$all_models)
    types <- c(Lasso="Linéaire", ElasticNet="Linéaire",
               MLP_Shallow="Réseau de neurones", MLP_Deep="Réseau de neurones",
               GBM="Ensemble", ExtraTrees="Ensemble", RandomForest="Ensemble")
    cols_type <- c("Linéaire"="#2563eb","Réseau de neurones"="#7c3aed","Ensemble"="#059669")

    do.call(rbind, lapply(nms, function(nm) {
      m <- METRICS$all_models[[nm]]
      data.frame(
        modele   = nm,
        type     = types[nm] %||% "Autre",
        r2       = as.numeric(m$r2   %||% 0),
        mae      = as.numeric(m$mae  %||% 0),
        rmse     = as.numeric(m$rmse %||% 0),
        mape     = as.numeric(m$mape_pct %||% 0),
        meilleur = (nm == (METRICS$best_model %||% "Lasso")),
        couleur  = ifelse(nm == (METRICS$best_model %||% "Lasso"),
                    "#2563eb",
                    cols_type[types[nm] %||% "Autre"]),
        stringsAsFactors = FALSE
      )
    })) |> arrange(desc(r2))
  }

  # KPIs meilleur modèle
  output$mod_kpi_best <- renderUI({
    req(!is.null(METRICS))
    best <- METRICS$best_model %||% "Lasso"
    div(class="kpi-box",
      div(style="font-size:18px;margin-bottom:8px;color:#2563eb;", icon("trophy")),
      div(class="kpi-value", style="color:#2563eb;font-size:1.5rem;", best),
      div(class="kpi-label", "Meilleur modèle"),
      div(class="kpi-sub", "Sélectionné et déployé")
    )
  })
  output$mod_kpi_r2 <- renderUI({
    df <- get_models_df()
    best_r2 <- df$r2[df$meilleur][1]
    div(class="kpi-box",
      div(style="font-size:18px;margin-bottom:8px;color:#16a34a;", icon("circle-check")),
      div(class="kpi-value", style="color:#16a34a;",
        paste0(round(best_r2 * 100, 2), "%")),
      div(class="kpi-label", "R² du meilleur modèle"),
      div(class="kpi-sub", "Variance expliquée")
    )
  })
  output$mod_kpi_mae <- renderUI({
    df <- get_models_df()
    best_mae <- df$mae[df$meilleur][1]
    div(class="kpi-box",
      div(style="font-size:18px;margin-bottom:8px;color:#0891b2;", icon("ruler")),
      div(class="kpi-value", style="color:#0891b2;", round(best_mae, 4)),
      div(class="kpi-label", "MAE — Erreur absolue"),
      div(class="kpi-sub", "µg/m³")
    )
  })
  output$mod_kpi_mape <- renderUI({
    df <- get_models_df()
    best_mape <- df$mape[df$meilleur][1]
    div(class="kpi-box",
      div(style="font-size:18px;margin-bottom:8px;color:#d97706;", icon("percent")),
      div(class="kpi-value", style="color:#d97706;", paste0(round(best_mape, 2), "%")),
      div(class="kpi-label", "MAPE — Erreur relative"),
      div(class="kpi-sub", "Moyenne sur toutes villes")
    )
  })

  # Graphique R²
  output$hc_mod_r2 <- renderHighchart({
    df <- get_models_df()
    cols <- ifelse(df$meilleur, "#2563eb", "#94a3b8")
    highchart() |>
    hc_chart(type="bar", backgroundColor="white") |>
    hc_xAxis(categories=df$modele,
      labels=list(style=list(color="#374151", fontSize="13px", fontWeight="600"))) |>
    hc_yAxis(
      title=list(text="R²", style=list(color="#9ca3af", fontSize="12px")),
      labels=list(style=list(color="#6b7280"), format="{value:.4f}"),
      min=floor(min(df$r2)*1000)/1000, max=1.001
    ) |>
    hc_add_series(
      name="R²", data=as.list(round(df$r2, 4)),
      colorByPoint=TRUE, colors=as.list(cols),
      dataLabels=list(enabled=TRUE, format="{point.y:.4f}",
        style=list(fontSize="11px", fontWeight="700", color="#374151"))
    ) |>
    hc_tooltip(
      pointFormat="<b>{point.category}</b><br>R² : <b>{point.y:.5f}</b>",
      backgroundColor="white", borderColor="#e5e7eb",
      style=list(color="#111827", fontFamily="DM Sans")
    ) |>
    hc_add_theme(hc_theme_null())
  })

  # Graphique MAE / RMSE
  output$hc_mod_err <- renderHighchart({
    df <- get_models_df()
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(categories=df$modele,
      labels=list(style=list(color="#6b7280", fontSize="11px"), rotation=-30)) |>
    hc_yAxis(
      title=list(text="Erreur µg/m³", style=list(color="#9ca3af", fontSize="11px")),
      labels=list(style=list(color="#6b7280"))
    ) |>
    hc_add_series(
      name="MAE", data=as.list(round(df$mae, 4)), type="column",
      color="#2563eb",
      dataLabels=list(enabled=TRUE, format="{point.y:.4f}",
        style=list(fontSize="10px", color="#374151"))
    ) |>
    hc_add_series(
      name="RMSE", data=as.list(round(df$rmse, 4)), type="column",
      color="#93c5fd"
    ) |>
    hc_plotOptions(column=list(grouping=TRUE, pointPadding=0.1)) |>
    hc_tooltip(shared=TRUE, backgroundColor="white", borderColor="#e5e7eb",
      style=list(color="#111827")) |>
    hc_add_theme(hc_theme_null())
  })

  # Graphique MAPE
  output$hc_mod_mape <- renderHighchart({
    df <- get_models_df()
    cols_mape <- ifelse(df$meilleur, "#16a34a",
                   ifelse(df$mape < 0.5, "#16a34a",
                     ifelse(df$mape < 1.0, "#d97706", "#dc2626")))
    highchart() |>
    hc_chart(backgroundColor="white") |>
    hc_xAxis(categories=df$modele,
      labels=list(style=list(color="#6b7280", fontSize="11px"), rotation=-30)) |>
    hc_yAxis(
      title=list(text="MAPE (%)", style=list(color="#9ca3af", fontSize="11px")),
      labels=list(style=list(color="#6b7280"), format="{value}%")
    ) |>
    hc_add_series(
      name="MAPE %", data=as.list(round(df$mape, 2)),
      type="column", colorByPoint=TRUE, colors=as.list(cols_mape),
      dataLabels=list(enabled=TRUE, format="{point.y:.2f}%",
        style=list(fontSize="11px", fontWeight="700", color="#374151"))
    ) |>
    hc_tooltip(
      pointFormat="<b>{point.category}</b><br>MAPE : <b>{point.y:.2f}%</b>",
      backgroundColor="white", borderColor="#e5e7eb", style=list(color="#111827")
    ) |>
    hc_add_theme(hc_theme_null())
  })

  # Importance des features
  output$hc_mod_feat <- renderHighchart({
    fi <- if (!is.null(FEAT_IMP)) {
      df_fi <- data.frame(
        feature    = names(FEAT_IMP),
        importance = as.numeric(unlist(FEAT_IMP)),
        stringsAsFactors = FALSE
      )
      df_fi |> arrange(desc(importance)) |> head(15)
    } else {
      # Valeurs intégrées si fichier manquant
      data.frame(
        feature=c("is_no_rain","precip_log","precipitation_sum","isa","rain_sum",
                  "isa_scaled","precipitation_hours","is_harmattan","is_dry_season",
                  "temperature_2m_max","temperature_2m_mean","et0_fao_evapotranspiration",
                  "precip_roll7","latitude","dayofyear_cos"),
        importance=c(0.2216,0.1299,0.1233,0.1095,0.1029,0.1024,0.0869,0.0468,
                     0.0407,0.0128,0.0038,0.0032,0.0031,0.0029,0.0024),
        stringsAsFactors=FALSE
      )
    }

    n   <- nrow(fi)
    pal <- colorRampPalette(c("#bfdbfe","#1d4ed8"))(n)

    highchart() |>
    hc_chart(type="bar", backgroundColor="white") |>
    hc_xAxis(
      categories=rev(fi$feature),
      labels=list(style=list(color="#374151", fontSize="12px", fontWeight="500"))
    ) |>
    hc_yAxis(
      title=list(text="Importance relative", style=list(color="#9ca3af", fontSize="11px")),
      labels=list(style=list(color="#6b7280"), format="{value:.3f}")
    ) |>
    hc_add_series(
      name="Importance", data=as.list(rev(round(fi$importance, 4))),
      colorByPoint=TRUE, colors=as.list(rev(pal)),
      dataLabels=list(enabled=TRUE, format="{point.y:.4f}",
        style=list(fontSize="10px", color="#374151"))
    ) |>
    hc_tooltip(
      pointFormat="<b>{point.category}</b><br>Importance : <b>{point.y:.4f}</b>",
      backgroundColor="white", borderColor="#e5e7eb", style=list(color="#111827")
    ) |>
    hc_add_theme(hc_theme_null())
  })

  # Résumé meilleur modèle
  output$mod_best_summary <- renderUI({
    req(!is.null(METRICS))
    best_nm <- METRICS$best_model %||% "Lasso"
    m  <- METRICS$all_models[[best_nm]]
    df <- get_models_df()

    tagList(
      div(style="text-align:center;background:#eff6ff;border:2px solid #bfdbfe;border-radius:12px;padding:16px;margin-bottom:14px;",
        div(style="font-size:11px;text-transform:uppercase;letter-spacing:.1em;color:#6b7280;","Modèle sélectionné"),
        div(style="font-size:1.8rem;font-weight:800;color:#1d4ed8;font-family:'DM Mono',monospace;", best_nm),
        div(style="font-size:12px;color:#6b7280;margin-top:4px;",
          df$type[df$modele == best_nm][1])
      ),
      div(class="info-row",
        span(class="info-key","R²"),
        span(class="info-val", style="color:#16a34a;font-size:15px;",
          paste0(round(as.numeric(m$r2 %||% 0) * 100, 3), "%"))
      ),
      div(class="info-row",
        span(class="info-key","MAE"),
        span(class="info-val", paste0(m$mae %||% "—", " µg/m³"))
      ),
      div(class="info-row",
        span(class="info-key","RMSE"),
        span(class="info-val", paste0(m$rmse %||% "—", " µg/m³"))
      ),
      div(class="info-row",
        span(class="info-key","MAPE"),
        span(class="info-val", paste0(m$mape_pct %||% "—", "%"))
      ),
      div(class="info-row",
        span(class="info-key","Features"),
        span(class="info-val", "49 variables météo")
      ),
      div(class="info-row",
        span(class="info-key","Observations"),
        span(class="info-val", "87 240")
      ),
      div(class="info-row",
        span(class="info-key","Validation"),
        span(class="info-val", "TimeSeriesSplit 5 folds")
      ),
      div(class="info-row",
        span(class="info-key","Déploiement"),
        span(class="info-val", "API REST — Render.com")
      )
    )
  })

  # Tableau complet
  output$mod_table_ui <- renderUI({
    df <- get_models_df()
    tags$table(
      style="width:100%;border-collapse:collapse;font-size:13px;",
      tags$thead(
        tags$tr(
          lapply(c("Modèle","Type","R²","MAE","RMSE","MAPE %","Statut"),
            function(h) tags$th(style="padding:10px 12px;text-align:left;color:#6b7280;font-size:11px;text-transform:uppercase;letter-spacing:.08em;border-bottom:2px solid #e5e7eb;", h))
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          row <- df[i,]
          bg  <- if (i %% 2 == 0) "#f8fafc" else "white"
          hl  <- if (row$meilleur) "background:#eff6ff;border-left:3px solid #2563eb;" else ""
          tags$tr(style=paste0("background:",bg,";",hl),
            tags$td(style="padding:10px 12px;font-weight:700;color:#111827;",
              if(row$meilleur)
                tagList(row$modele, tags$span(style="margin-left:8px;background:#dbeafe;color:#1d4ed8;font-size:10px;font-weight:700;padding:2px 8px;border-radius:100px;","BEST"))
              else row$modele),
            tags$td(style="padding:10px 12px;color:#6b7280;", row$type),
            tags$td(style=paste0("padding:10px 12px;font-family:'DM Mono',monospace;font-weight:700;color:",
              if(row$r2>=0.9999)"#16a34a" else if(row$r2>=0.999)"#d97706" else "#dc2626",";"),
              round(row$r2, 5)),
            tags$td(style="padding:10px 12px;font-family:'DM Mono',monospace;", round(row$mae, 4)),
            tags$td(style="padding:10px 12px;font-family:'DM Mono',monospace;", round(row$rmse, 4)),
            tags$td(style="padding:10px 12px;font-family:'DM Mono',monospace;", paste0(round(row$mape, 2),"%")),
            tags$td(style="padding:10px 12px;",
              if(row$meilleur)
                tags$span(style="background:#dcfce7;color:#16a34a;font-size:11px;font-weight:700;padding:3px 10px;border-radius:100px;",
                  tagList(icon("circle-check"), " Déployé"))
              else
                tags$span(style="background:#f1f5f9;color:#64748b;font-size:11px;padding:3px 10px;border-radius:100px;",
                  "Entraîné")
            )
          )
        })
      )
    )
  })


  # ============================================================================
  # SERVER — EQUIPE DE CONCEPTION
  # ============================================================================

  # Données des membres (à personnaliser)
  MEMBRES <- list(
    list(
      id          = 1,
      prenom      = "Manuellla Celleste",
      nom         = "MBOKOP NWAMEN",
      role        = "Chef de projet ",
      institution = "ISSEA",
      email       = "mbokopm@gmail.com",
      telephone   = "+237 698 323 426",
      photo       = "manuellla.png",
      description = "Étudiante en troisième année du cycle Analyste Statisticien (AS) option Statistiques Économiques à l’Institut Sous Régional d’Économie et de Statistique Appliquée (ISSEA).
Je suis passionnée par l’analyse de données, la gestion de l’information et les dynamiques économiques. Curieuse et dynamique, je suis toujours en quête de nouvelles connaissances pour développer mes compétences.",
      linkedin    = "",
      competences = c("Machine Learning","Python","R")
    ),
    list(
      id          = 2,
      prenom      = "Kodjo Maurice",
      nom         = "SEKOU",
      role        = "Analyste de Donnees / Visualisation",
      institution = "ISSEA",
      email       = "student.maurice.sekou@issea-cemac.org",
      telephone   = "+237 671 280 918",
      photo       = "maurice1.png",
      description = " Etudiant Analyste statistien en troisième année(Option Data Science) ,je suis passionné par les mathématiques, les statistiques, l’informatique ,l’Analyse des
données.",
      linkedin    = "SEKOU Kodjo Maurice",
      competences = c("Deep Learning","Machine learning","API REST","NLP")
    ),
    list(
      id          = 3,
      prenom      = "EDITH MONIQUE",
      nom         = "LONKOK DEKOU ",
      role        = "Analyste de Donnees / Visualisation",
      institution = "ISSEA",
      email       = "student.edith.dekou@issea-cemac.org",
      telephone   = "+237 697 337 711",
      photo       = "edith1.png",
      description = "Élève analyste statisticienne à l'issea en troisième année .Passionné par le nouveau et l'innovation, je suis toujours volontaire pour apprendre et développer de nouvelles compétences. J'ai un grand sens d'esprit d'équipe et l'amour du travail car seul lui façonne.",
      linkedin    = "",
      competences = c("R Shiny","ggplot2","Leaflet","SQL")
    ),
    list(
      id          = 4,
      prenom      = "Miguel",
      nom         = "MANGUI",
      role        = "Analyste de données",
      institution = "ISSEA",
      email       = " manguimiguel07@gmail.com",
      telephone   = "+237 686 564 431",
      photo       = "miguel.png",
      description = "Je suis élève Analyste statisticien à l'ISSEA de Yaoundé actuellement au niveau 2. Je suis un fan de mathématiques et un passionné de la data",
      linkedin    = "",
      competences = c("Base de données","Power BI","Latex")
    )
  )

  # Fonction de construction d'une carte membre
  build_membre_card <- function(m) {
    # Chemin photo dans www/
    photo_src <- m$photo
    photo_ok  <- file.exists(file.path("www", photo_src))

    div(style="background:white;border:1px solid #e5e7eb;border-radius:16px;overflow:hidden;box-shadow:0 2px 12px rgba(0,0,0,0.07);margin-bottom:20px;transition:box-shadow .2s;",

      # En-tete avec photo + nom
      div(style="display:flex;align-items:center;gap:20px;padding:22px 24px;border-bottom:1px solid #f1f5f9;background:linear-gradient(135deg,#f8fafc,white);",

        # Photo
        div(style="flex-shrink:0;",
          if (photo_ok) {
            tags$img(
              src    = photo_src,
              width  = "90px",
              height = "90px",
              style  = "border-radius:50%;object-fit:cover;border:3px solid #e5e7eb;box-shadow:0 2px 8px rgba(0,0,0,0.1);"
            )
          } else {
            div(style="width:90px;height:90px;border-radius:50%;background:linear-gradient(135deg,#1c2b3a,#2563eb);display:flex;align-items:center;justify-content:center;border:3px solid #e5e7eb;",
              div(style="font-size:28px;font-weight:800;color:white;",
                toupper(substr(m$prenom, 1, 1)))
            )
          }
        ),

        # Identité
        div(style="flex:1;min-width:0;",
          div(style="font-size:18px;font-weight:800;color:#111827;line-height:1.2;",
            paste(m$prenom, m$nom)),
          div(style="font-size:13px;font-weight:600;color:#2563eb;margin-top:4px;",
            m$role),
          div(style="font-size:12px;color:#6b7280;margin-top:3px;display:flex;align-items:center;gap:5px;",
            icon("building"), " ", m$institution)
        )
      ),

      # Corps — description
      div(style="padding:16px 24px;",
        div(style="font-size:13px;color:#374151;line-height:1.7;margin-bottom:14px;",
          m$description),

        # Compétences
        div(style="display:flex;flex-wrap:wrap;gap:6px;margin-bottom:16px;",
          lapply(m$competences, function(comp)
            div(style="background:#eff6ff;color:#1d4ed8;font-size:11px;font-weight:600;padding:3px 10px;border-radius:100px;border:1px solid #bfdbfe;",
              comp)
          )
        ),

        # Contacts
        div(style="display:flex;flex-direction:column;gap:8px;padding-top:14px;border-top:1px solid #f1f5f9;",
          div(style="display:flex;align-items:center;gap:10px;font-size:13px;color:#374151;",
            div(style="color:#2563eb;width:16px;text-align:center;", icon("envelope")),
            tags$a(href=paste0("mailto:", m$email),
              style="color:#2563eb;text-decoration:none;font-weight:500;", m$email)
          ),
          div(style="display:flex;align-items:center;gap:10px;font-size:13px;color:#374151;",
            div(style="color:#16a34a;width:16px;text-align:center;", icon("phone")),
            tags$span(style="font-weight:500;", m$telephone)
          )
        )
      )
    )
  }

  output$card_membre1 <- renderUI({ build_membre_card(MEMBRES[[1]]) })
  output$card_membre2 <- renderUI({ build_membre_card(MEMBRES[[2]]) })
  output$card_membre3 <- renderUI({ build_membre_card(MEMBRES[[3]]) })
  output$card_membre4 <- renderUI({ build_membre_card(MEMBRES[[4]]) })

}

shinyApp(ui=ui, server=server)
