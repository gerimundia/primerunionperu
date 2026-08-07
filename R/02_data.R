
source("R/01_paquetes.R")

# 2010 ----

REC0111 <- read_sav("endes/datos_2010/REC0111.SAV")
REC516171 <- read_sav("endes/datos_2010/REC516171.SAV")
aux_2010 <- left_join(REC0111, REC516171, by = "CASEID")

aux <- data.frame(
  variable = names(aux_2010),
  label = sapply(aux_2010, function(x) attr(x, "label")),
  stringsAsFactors = FALSE)

mujer_2010 <- aux_2010 %>% 
  select(CASEID, V007, V009, V010, V012, V021, V022, V102, V149,
         V131,AWFACTT,V502, V503, V507, V508, V509,V729, V730, 
         V605, V613, V621, V627, V628, V629,
         V743A, V743B, V743C, V743D, V743E, V743F, 
         V744A, V744B, V744C, V744D, V744E, V746)

diccionario <- data.frame(
  variable = names(mujer_2010),
  label = sapply(mujer_2010, function(x) attr(x, "label")),
  stringsAsFactors = FALSE)


