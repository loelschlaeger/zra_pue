
# Geburten in Deutschland -------------------------------------------------

# besuche https://www-genesis.destatis.de/genesis//online?operation=table&code=12612-0002&bypass=true&levelindex=1&levelid=1680856431207#abreadcrumb
# klicke unten auf "Zeit auswählen" 
# wähle "Alle verfügbaren Zeitangaben"
# wähle ganz unten "Übernehmen"
# klicke auf "Werteabruf" und anschließend unter "Downloads" auf "CSV"
# es wird die Datei "12612-0002_$F.csv" heruntergeladen

# lese die Datei in R ein

data <- read.csv(
  file   = "12612-0002_$F.csv", # der Dateipfad
  skip   = 6,                   # ignoriere die ersten 6 Zeilen
  header = FALSE,               # interpretiere die erste Zeile nicht als Überschrift
  sep    = ";"                  # die Zellen sind durch Simikolons getrennt
)

# füge Spaltennamen hinzu

colnames(data) <- c("year", "month", "male", "female", "total")

# lösche die letzten Zeilen ohne Daten

data <- data[-(876:879), ]

# transformiere die Jahreszahlen zu numerischen Werten

data$year <- as.numeric(data$year)

# wähle den Zeitraum 2000 bis 2021 aus

data <- subset(data, year >= 2000 & year <= 2021)

# erzeuge ein Zeitreihenobjekt aus den Gesamtgeburtenzahlen

data_ts <- ts(data = data$total, start = c(2000, 1), end = c(2021, 12), frequency = 12)

# zeichne die Zeitreihe und speichere den Plot  

pdf("geburten.pdf")
plot(
  data_ts, 
  main = "Geburten in Deutschland (monatlich)", 
  sub = "Datenquelle: Statistisches Bundesamt",
  xlab = "Jahresbeginn", 
  ylab = "Anzahl",
  col = "darkblue"
)
dev.off()

# Touristenübernachtungen in Portugal -------------------------------------

# downloade die Daten von Eurostat

url <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tour_occ_nim.tsv.gz"
path <- tempfile(fileext = ".tsv.gz")
download.file(url, path)
data <- read.csv(file = path, sep = '\t', header = TRUE)

# filtere nach 
# - der Anzahl (NR) Übernachtungen 
# - in diversen Unterkünften (I551-I553) 
# - von In- und Ausländern (TOTAL)
# - in Portugal (PT)

labels <- data[, 1]
id <- grep("TOTAL,NR,I551-I553,PT", labels)
data <- data[id, ]

# entferne die ersten beiden Zellen ohne Daten

data <- data[, -(1:2)]

# ermittle die Zeitspannweite (von Januar 1990 bis Januar 2023)

range(colnames(data))

# transformiere die Übernachtungszahlen zu numerischen Werten

data <- as.numeric(data)

# kehre die Zeitreihe um, da sie zeitlich absteigend sortiert ist

data <- rev(data)

# erzeuge ein Zeitreihenobjekt aus den Übernachtungszahlen

data_ts <- ts(data, start = c(1990, 1), end = c(2023, 1), frequency = 12)

# zeichne die Zeitreihe und speichere den Plot  

pdf("uebernachtungen.pdf")
plot(
  data_ts, 
  main = "Übernachtungsanzahl von Touristen in Portugal (monatlich)", 
  sub = "Datenquelle: Statistisches Amt der Europäischen Union",
  xlab = "Jahresbeginn", 
  ylab = "Anzahl",
  col = "purple"
)
dev.off()

# Luftfeuchtigkeit in Indien ----------------------------------------------

# besuche https://www.kaggle.com/datasets/sumanthvrao/daily-climate-time-series-data
# klicke rechts oben auf "Download" (eventuell ist eine Anmeldung erforderlich)
# es wird die Datei "archive.zip" heruntergeladen
# entpacke die Datei "DailyDelhiClimateTrain.csv"

# lese die Datei in R ein

data <- read.csv(
  file   = "DailyDelhiClimateTrain.csv", 
  header = TRUE,              
  sep    = ","                
)

# reduziere die Daten auf maximal 1000 Beobachtungen

data <- data[1:1000, ]

# ermittle die Zeitspannweite (01.01.2013 bis 27.09.2015)

range(data$date)

# erzeuge ein Zeitreihenobjekt aus den Luftfeuchtigkeitsdaten

data_ts <- ts(
  data$humidity, 
  start = c(2013, 1), 
  frequency = 365
)

# zeichne die Zeitreihe und speichere den Plot  

pdf("luftfeuchtigkeit.pdf")
plot(
  data_ts, 
  main = "Luftfeuchtigkeit in Indian (täglich)", 
  sub = "Datenquelle: Kaggle",
  xlab = "Monatsbeginn", 
  ylab = "Prozentwert",
  col = "darkorange"
)
dev.off()
