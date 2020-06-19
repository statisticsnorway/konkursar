
options(encoding = "UTF-8")

##### DATA FOR ANTALL KONKURSER #####

konkurser <- ApiData("https://data.ssb.no/api/v0/no/table/12972/",
                     Region = FALSE,
                     Naring = FALSE,
                     Vekedag = TRUE,
                     ContentsCode = TRUE,
                     Tid = TRUE)[[1]] %>%
  spread(statistikkvariabel, value)

### Databehandling for plot med konkurser per dag:

konk2020 <- konkurser %>%
  filter(str_detect(uke, "2020*")) %>% # Henter ut verdier for 2020.
  filter(vekedag != "Heile veka") %>% # Tar ut verdi "Heile veka"
  mutate(vekedag = fct_relevel(vekedag, # Setter verdiene på vekedag slik at "Onsdag" kommer først, for 01.01.2020 var onsdag.
                               c("Onsdag", "Torsdag", "Fredag", "Laurdag", "Søndag", "Måndag", "Tysdag"))) %>%
  arrange(vekedag) %>%
  mutate(uke = str_extract(uke, "U[0-9]+"), # Henter ut uketallene
         uke = as.numeric(str_extract(uke, "[0-9]+"))) %>% # Og tar ut "U".
  arrange(uke) %>% # La uke 1 komme først.
  mutate(År = "2020") # Legg på årvariabel.

# Legger inn datovariabel for 2020
dato2020 <- vector() # Lager en tom vektor.

for (i in 1:nrow(konk2020)) {
  dato2020[i] <- lubridate::as_date(18261) # Dette er telling av dager fra 1970. 18261 tilsvarer "2019-12-31".
  dato2020[i] <- dato2020[i] + i # Legger på 1 for hver observasjon i df
  dato2020 <- lubridate::as_date(dato2020) # Gjør om tallene til datoer.
}

konk2020 <- konk2020 %>%
  mutate(dato = dato2020) %>% # Setter inn vektoren i datasettet.
  mutate(dato = as.Date(format(dato, format = "%Y-%m-%d"))) # Gjør om til datoformat.

# Gjør samme operasjon som over for 2019
konk2019 <- konkurser %>%
  filter(str_detect(uke, "2019*")) %>% 
  filter(vekedag != "Heile veka") %>%
  mutate(vekedag = fct_relevel(vekedag, # 01.01.2019 var tirsdag, så tirsdag må komme først.
                               c("Tysdag", "Onsdag", "Torsdag", "Fredag", "Laurdag", "Søndag", "Måndag"))) %>%
  arrange(vekedag) %>%
  mutate(uke = str_extract(uke, "U[0-9]+"),
         uke = as.numeric(str_extract(uke, "[0-9]+"))) %>%
  arrange(uke) %>%
  mutate(År = "2019") 

dato2019 <- vector()

for (i in 1:nrow(konk2019)) {
  dato2019[i] <- lubridate::as_date(17896) # Dette er telling av dager fra 1970. 17896 tilsvarer "2018-12-31".
  dato2019[i] <- dato2019[i] + i # Legger på 1 for hver observasjon i df
  dato2019 <- lubridate::as_date(dato2019) # Gjør om tallene til datoer.
}

konk2019 <- konk2019 %>%
  mutate(dato = dato2019) %>%
  mutate(dato = as.Date(format(dato, format = "%Y-%m-%d"))) 


### Databehandling for plot med konkurser akkumulert:

konk2020akk <- konk2020 %>%
  filter(uke <= 52) %>%
  select(Konkursar, dato) %>%
  mutate(datofilter = format(as.Date(dato, "%Y%m%d"), format = "%m-%d")) %>%
  filter(datofilter <= format(as.Date(Sys.Date(), "%Y%m%d"), format = "%m-%d")) %>% 
  mutate(datoplot = as.Date(datofilter, format = "%m-%d")) %>%
  group_by(datoplot) %>%
  summarise(freq_konkurs = sum(Konkursar, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Dato = format(as.Date(datoplot, format = "%m-%d"), "%d. %h")) %>%
  mutate(n = cumsum(freq_konkurs)) %>%
  mutate(År = "2020") 

konk2019akk <- konk2019 %>%
  filter(uke <= 52) %>%
  select(Konkursar, dato) %>%
  mutate(datofilter = format(as.Date(dato, "%Y%m%d"), format = "%m-%d")) %>%
  filter(datofilter <= format(as.Date(Sys.Date(), "%Y%m%d"), format = "%m-%d")) %>% 
  mutate(datoplot = as.Date(datofilter, format = "%m-%d")) %>%
  group_by(datoplot) %>%
  summarise(freq_konkurs = sum(Konkursar, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Dato = format(as.Date(datoplot, format = "%m-%d"), "%d. %h")) %>%
  mutate(n = cumsum(freq_konkurs)) %>%
  mutate(År = "2019")


##### DATA FOR NÆRINGSFORDELING TIL KONKURSER #####

konkurser_naring <- ApiData("https://data.ssb.no/api/v0/no/table/12972/",
                            Region = FALSE,
                            Naring = TRUE,
                            Vekedag = TRUE,
                            ContentsCode = TRUE,
                            Tid = TRUE)[[1]] %>%
  filter(næring != "Alle næringar") %>%
  filter(str_detect(uke, "2020*")) %>% 
  filter(vekedag != "Heile veka") %>% 
  filter(statistikkvariabel != "Konkursar hittil i år") %>%
  nest(statistikkvariabel, vekedag, uke, value) %>%
  mutate(data = map(data, #  Nøster data for å gjøre separate operasjoner på hver næring sin df
                    ~ .x %>% 
                      group_by(statistikkvariabel) %>%
                      summarise(value = sum(value, na.rm = TRUE)))) %>%
  unnest() %>%
  rename(var = statistikkvariabel,
         Verdi = value,
         Næring = næring)

konkurser_naring19 <- ApiData("https://data.ssb.no/api/v0/no/table/12972/",
                              Region = FALSE,
                              Naring = TRUE,
                              Vekedag = TRUE,
                              ContentsCode = TRUE,
                              Tid = TRUE)[[1]] %>%
  filter(næring != "Alle næringar") %>%
  filter(str_detect(uke, "2019*") | str_detect(uke, "2020*")) %>% 
  mutate(År = str_remove_all(uke, "U[0-9]+")) %>%
  mutate(uke = str_extract(uke, "U[0-9]+"), 
         uke = as.numeric(str_extract(uke, "[0-9]+"))) %>%
  filter(uke <= aggregate(uke ~ År, data = ., max)$uke[2]) %>%
  filter(vekedag != "Heile veka") %>% 
  filter(statistikkvariabel != "Konkursar hittil i år") %>%
  nest(statistikkvariabel, vekedag, uke, År, value) %>%
  mutate(data = map(data, #  Nøster data for å gjøre separate operasjoner på hver næring sin df
                    ~ .x %>% 
                      group_by(statistikkvariabel, År) %>%
                      summarise(value = sum(value, na.rm = TRUE)))) %>%
  unnest() %>%
  rename(var = statistikkvariabel,
         Verdi = value,
         Næring = næring)


##### DATA FOR GEOGRAFISK FORDELING TIL KONKURSER #####

konkurser_geo <- ApiData("https://data.ssb.no/api/v0/no/table/12972/",
                         Region = TRUE,
                         Naring = FALSE,
                         Vekedag = FALSE,
                         ContentsCode = TRUE,
                         Tid = TRUE)[[1]] %>%
  filter(str_detect(uke, "2020*")) %>%
  mutate(uke = str_extract(uke, "U[0-9]+"),
         uke = as.numeric(str_extract(uke, "[0-9]+"))) %>%
  spread(statistikkvariabel, value)

konkurser_geo_sum <- konkurser_geo %>%
  group_by(region) %>%
  summarise(Konkursar = sum(Konkursar, na.rm = TRUE),
            `Omsetning (1 000 kr)` = sum(`Omsetning (1 000 kr)`, na.rm = TRUE),
            Tilsette = sum(Tilsette, na.rm = TRUE),
            `Eksportverdi (1 000 kr)` = sum(`Eksportverdi (1 000 kr)`, na.rm = TRUE),
            `Importverdi (1 000 kr)` = sum(`Importverdi (1 000 kr)`, na.rm = TRUE))

shapefile <- st_read("./shapefiles/Okonreg_2018.shp") 

komoek1 <- read_csv2("./436.csv", locale = locale(encoding = 'latin1')) %>%
  rename(okonreg = sourceCode) %>%
  left_join(shapefile, by = "okonreg") %>% 
  rename(region = targetName) %>%
  left_join(konkurser_geo_sum) %>%
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  gather(Konkursar, `Omsetning (1 000 kr)`, Tilsette, `Eksportverdi (1 000 kr)`, `Importverdi (1 000 kr)`,
         key = "var", value = "Verdi") %>% 
  rename(Region = region)

komoek2 <- read_csv2("./436.csv", locale = locale(encoding = 'latin1')) %>%
  rename(okonreg = sourceCode) %>%
  left_join(shapefile, by = "okonreg") %>% 
  rename(region = targetName) %>%
  left_join(konkurser_geo) %>%
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  gather(Konkursar, `Omsetning (1 000 kr)`, Tilsette, `Eksportverdi (1 000 kr)`, `Importverdi (1 000 kr)`,
         key = "var", value = "Verdi") %>% 
  rename(Region = region)

