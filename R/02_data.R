
source("R/01_paquetes.R")

# File path
path0 <- "C:/Users/gerim/OneDrive - El Colegio de México A.C/Articulos/Primeraunion_Peru/endes/"

# 2010 ----
path1 <- "datos_2010/"
  
REC0111 <- read_sav(paste0(path0, path1, "REC0111.SAV"))
REC516171 <- read_sav(paste0(path0, path1, "REC516171.SAV"))
aux <- left_join(REC0111, REC516171, by = "CASEID")

dic_aux <- data.frame(
  variable = names(aux),
  stringsAsFactors = FALSE)

mujer <- aux %>% 
  select(CASEID, V007, V009, V010, V012, V021, V022, V102, V149,
         V131,AWFACTT,V502, V503, V507, V508, V509,V729, V730, 
         V605, V613, V621, V627, V628, V629,
         V743A, V743B, V743C, V743D, V743E, V743F, 
         V744A, V744B, V744C, V744D, V744E, V746,
         V133, V512, V715)

dic <- data.frame(
  variable = names(mujer),
  label = sapply(mujer, function(x) attr(x, "label")),
  class = sapply(mujer, function(x) class(x)[1]),
  stringsAsFactors = FALSE)

mujer_2010 <- mujer
diccionario_2010 <- dic


# 2015 ----
path1 <- "datos_2015/"

REC0111 <- read_sav(paste0(path0, path1, "REC0111.SAV"))
REC516171 <- read_sav(paste0(path0, path1, "RE516171.SAV"))
aux <- left_join(REC0111, REC516171, by = "CASEID")

dic_aux <- data.frame(
  variable = names(aux),
  label = sapply(aux, function(x) attr(x, "label")),
  stringsAsFactors = FALSE)

mujer <- aux %>% 
  select(CASEID, V007, V009, V010, V012, V021, V022, V102, V149,
         V131,AWFACTT,V502, V503, V507, V508, V509,V729, V730, 
         V605, V613, V621, V627, V628, V629,
         V743A, V743B, V743C, V743D, V743E, V743F, 
         V744A, V744B, V744C, V744D, V744E, V746,
         V133, V512, V715)

dic <- data.frame(
  variable = names(mujer),
  label = sapply(mujer, function(x) attr(x, "label")),
  class = sapply(mujer, function(x) class(x)[1]),
  stringsAsFactors = FALSE)

mujer_2015 <- mujer
diccionario_2015 <- dic


# 2020 ----
path1 <- "datos_2020/"

REC0111 <- read_sav(paste0(path0, path1, "REC0111.SAV"))
REC516171 <- read_sav(paste0(path0, path1, "RE516171.SAV"))
aux <- left_join(REC0111, REC516171, by = "CASEID")

dic_aux <- data.frame(
  variable = names(aux),
  label = sapply(aux, function(x) attr(x, "label")),
  stringsAsFactors = FALSE)

mujer <- aux %>% 
  select(CASEID, V007, V009, V010, V012, V021, V022, V102, V149,
         V131,AWFACTT,V502, V503, V507, V508, V509,V729, V730, 
         V605, V613, V621, V627, V628, V629,
         V743A, V743B, V743C, V743D, V743E, V743F, 
         V744A, V744B, V744C, V744D, V744E, V746,
         V133, V512, V715)

dic <- data.frame(
  variable = names(mujer),
  label = sapply(mujer, function(x) attr(x, "label")),
  class = sapply(mujer, function(x) class(x)[1]),
  stringsAsFactors = FALSE)

mujer_2020 <- mujer
diccionario_2020 <- dic


# 2025 ----
path1 <- "datos_2025/"

REC0111 <- read_sav(paste0(path0, path1, "REC0111_2025.SAV"))
REC516171 <- read_sav(paste0(path0, path1, "RE516171_2025.SAV"))
aux <- left_join(REC0111, REC516171, by = "CASEID")

dic_aux <- data.frame(
  variable = names(aux),
  label = sapply(aux, function(x) attr(x, "label")),
  stringsAsFactors = FALSE)

mujer <- aux %>% 
  select(CASEID, V007, V009, V010, V012, V021, V022, V102, V149,
         V131,AWFACTT,V502, V503, V507, V508, V509,V729, V730, 
         V605, V613, V621, V627, V628, V629,
         V743A, V743B, V743C, V743D, V743E, V743F, 
         V744A, V744B, V744C, V744D, V744E, V746,
         V133, V512, V715)

dic <- data.frame(
  variable = names(mujer),
  label = sapply(mujer, function(x) attr(x, "label")),
  class = sapply(mujer, function(x) class(x)[1]),
  stringsAsFactors = FALSE)

mujer_2025 <- mujer
diccionario_2025 <- dic


# Save ----
save(mujer_2010, mujer_2015, mujer_2020, mujer_2025, 
     diccionario_2010, diccionario_2015, diccionario_2020,
     diccionario_2025, file="data/rawdata.RData")
