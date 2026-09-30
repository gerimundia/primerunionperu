rm(list=ls())
gc()

source("R/01_paquetes.R")

# Datos ----
load("data/rawdata.RData")

# 2010 -----
datos <- mujer_2010

## Id ----
length(unique(datos$CASEID)) == nrow(datos)
datos$id <- datos$CASEID


## Disenio muestral ----
datos$upm <- datos$V021
datos$strata <- datos$V022
datos$weight <- datos$AWFACTT


## Fecha de entrevista ----
summary(datos$V007); summary(datos$V006)
datos$anio_entrevista <- datos$V007

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_entrevista <- ifelse(is.na(datos$V006)==F, datos$V006, 6)

datos$aux <- paste(datos$anio_entrevista, datos$mes_entrevista, "15", sep="-")
datos$fecha_entrevista <- as.Date(datos$aux)
class(datos$fecha_entrevista); head(datos$fecha_entrevista)


## Fecha de nacimiento de Ego ----
summary(datos$V009); summary(datos$V010)
datos$anio_nacimiento <- datos$V010

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_edad <- ifelse(is.na(datos$V009)==F, datos$V009, 6)

datos$aux <- paste(datos$anio_nacimiento, datos$mes_edad, "15", sep="-")
datos$fecha_nacimiento <- as.Date(datos$aux)
class(datos$fecha_nacimiento); head(datos$fecha_nacimiento)


## Edad de Ego ----
datos$edad_anioscumplidos <- datos$V012
class(datos$edad_anioscumplidos); summary(datos$edad_anioscumplidos)


## Area de residencia ----
datos$area <- factor(datos$V102, levels=c(1,2), 
                     labels=c("Urbano", "Rural"))
table(datos$V102, datos$area, useNA = "ifany")


## Anios escolaridad ----
datos$escol <- datos$V133
datos$escol[datos$escol>=96] <- NA
summary(datos$escol)


## Nivel educativo ----
datos$aux <- as.integer(datos$V149)
table(datos$aux, datos$V149)

datos$niveleduc <- 99
datos$niveleduc[datos$aux<=2] <- 1
datos$niveleduc[datos$aux==3] <- 2
datos$niveleduc[datos$aux==4] <- 2
datos$niveleduc[datos$aux==5] <- 3
datos$niveleduc <- factor(datos$niveleduc, levels=c(1,2,3), 
                             labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc, datos$V149)


## Castellano como lengua materna ----
datos$castellano  <- ifelse(is.na(datos$V131)==T, NA, 0)
datos$castellano[datos$V131==1] <- 1
prop.table(table(datos$castellano, useNA = "ifany"))


## Alguna vez casada ----
datos$algunavezcasada <- datos$V502
head(datos$algunavezcasada)


## Numero de uniones ----
datos$uniones <- datos$V503
head(datos$uniones)


## Fecha de primera union -----
summary(datos$V507); summary(datos$V508)
datos$anio_matri <- datos$V508

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_matri <- ifelse((is.na(datos$V507)==F), datos$V507, 6)

datos$aux <- paste(datos$anio_matri, datos$mes_matri, "15", sep="-")
datos$aux[datos$aux=="NA-6-15"] <- NA

datos$fecha_matri <- as.Date(datos$aux) 
class(datos$fecha_matri); head(datos$fecha_matri); summary(datos$fecha_matri)


## Anios desde union 1 ----
datos$anios1union <- datos$V512
class(datos$anios1union); summary(datos$anios1union)


## Anios escolaridad de pareja ----
datos$escol_pareja <- datos$V715
datos$escol_pareja[datos$escol_pareja>=96] <- NA
summary(datos$escol_pareja)


## Nivel educativo de pareja ----
head(datos$V729)
datos$aux <- as.integer(datos$V729)
table(datos$aux, datos$V729)

datos$niveleduc_pareja <- 99
datos$niveleduc_pareja[datos$aux<=2] <- 1
datos$niveleduc_pareja[datos$aux==3] <- 2
datos$niveleduc_pareja[datos$aux==4] <- 2
datos$niveleduc_pareja[datos$aux==5] <- 3
datos$niveleduc_pareja <- factor(datos$niveleduc_pareja, levels=c(1,2,3), 
                                 labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc_pareja, datos$V729)


## Edad de pareja ----
datos$edad_pareja <- datos$V730
class(datos$edad_pareja); summary(datos$edad_pareja)


## Gana mas que pareja ----
datos$ingresorelativo <- datos$V746
head(datos$ingresorelativo)
table(datos$ingresorelativo, useNA = "ifany")
prop.table(table(datos$ingresorelativo, useNA = "ifany"))


## Deseo de hijos de Ego ----
datos$deseo <- datos$V605
head(datos$deseo)


## Ideal de hijos de Ego ----
datos$ideal <- as.integer(datos$V629)
datos$ideal[datos$ideal>=96] <- NA
summary(datos$ideal); head(datos$ideal); hist(datos$ideal)


## Ideal de hijos de Ego 2 ----
datos$ideal2 <- as.integer(datos$V613)
datos$ideal2[datos$ideal2>=96] <- NA
summary(datos$ideal2); head(datos$ideal2); hist(datos$ideal2)


## Deseo de hijos de pareja ----
datos$deseo_pareja <- datos$V621
head(datos$deseo_pareja)


## Escala de valores tradicionales de genero ----
head(datos$V744E)
datos[paste0("aux", 1:11)] <- 0

datos$aux1[datos$V743A==4 | datos$V743A==5 | datos$V743A==6] = 1
datos$aux2[datos$V743B==4 | datos$V743B==5 | datos$V743B==6] = 1
datos$aux3[datos$V743C==4 | datos$V743C==5 | datos$V743C==6] = 1
datos$aux4[datos$V743D==4 | datos$V743D==5 | datos$V743D==6] = 1
datos$aux5[datos$V743E==4 | datos$V743E==5 | datos$V743E==6] = 1
datos$aux6[datos$V743F==4 | datos$V743F==5 | datos$V743F==6] = 1

datos$aux7[datos$V744A==1] = 1
datos$aux8[datos$V744B==1] = 1
datos$aux9[datos$V744C==1] = 1
datos$aux10[datos$V744D==1] = 1
datos$aux11[datos$V744E==1] = 1

datos$aux <- rowSums(datos[paste0("aux", 1:11)], na.rm = TRUE)
summary(datos$aux)

datos$tradicional <- 10 * (datos$aux - min(datos$aux, na.rm = TRUE)) / 
  (max(datos$aux, na.rm = TRUE) - min(datos$aux, na.rm = TRUE))

summary(datos$tradicional)


## Percentil de escolaridad Ego ----
summary(datos$weight)
datos <- datos[is.na(datos$weight)==F, ]
disenioeduc <- svydesign(ids = ~upm, strata = ~strata, weights = ~AWFACTE,
                    data = datos)

cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
summary(datos$escol_perc); hist(datos$escol_perc)
summary(datos$escol[datos$escol_perc>=80])


## Cuartil de escolaridad Ego ----
cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.25, 1, by = 0.25),
                           na.rm = TRUE, ci = FALSE))

datos$escol_cuantil <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
table(datos$escol_cuantil)
summary(datos$escol[datos$escol_cuantil==4])


## Percentil de escolaridad pareja ----
cortes <- coef(svyquantile(~escol_pareja, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc_pareja <- findInterval(datos$escol_pareja, cortes, left.open = TRUE) + 1
summary(datos$escol_perc_pareja); hist(datos$escol_perc_pareja)
summary(datos$escol_pareja[datos$escol_perc_pareja>=80])


## Asistencia escolar ----
datos$asistencia <- as.integer(datos$S111)
datos$asistencia[datos$asistencia>=2] <- NA
table(datos$asistencia, useNA = "ifany")


# Variables ----
d2010 <- datos %>% 
  select(id, upm, strata, weight, 
         fecha_entrevista, fecha_nacimiento, anio_nacimiento, 
         edad_anioscumplidos, area, escol, niveleduc, asistencia, 
         castellano, algunavezcasada, 
         uniones, fecha_matri, anios1union, escol_pareja, niveleduc_pareja,
         edad_pareja, ingresorelativo, deseo, ideal, ideal2, deseo_pareja,
         tradicional, escol_perc, escol_cuantil, escol_perc_pareja)



# 2015 -----
gc()
datos <- mujer_2015

## Id ----
length(unique(datos$CASEID)) == nrow(datos)
datos$id <- datos$CASEID


## Disenio muestral ----
datos$upm <- datos$V021
datos$strata <- datos$V022
datos$weight <- datos$AWFACTT


## Fecha de entrevista ----
summary(datos$V007); summary(datos$V006)
datos$anio_entrevista <- datos$V007

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_entrevista <- ifelse(is.na(datos$V006)==F, datos$V006, 6)

datos$aux <- paste(datos$anio_entrevista, datos$mes_entrevista, "15", sep="-")
datos$fecha_entrevista <- as.Date(datos$aux)
class(datos$fecha_entrevista); head(datos$fecha_entrevista)


## Fecha de nacimiento de Ego ----
summary(datos$V009); summary(datos$V010)
datos$anio_nacimiento <- datos$V010

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_edad <- ifelse(is.na(datos$V009)==F, datos$V009, 6)

datos$aux <- paste(datos$anio_nacimiento, datos$mes_edad, "15", sep="-")
datos$fecha_nacimiento <- as.Date(datos$aux)
class(datos$fecha_nacimiento); head(datos$fecha_nacimiento)


## Edad de Ego ----
datos$edad_anioscumplidos <- datos$V012
class(datos$edad_anioscumplidos); summary(datos$edad_anioscumplidos)


## Area de residencia ----
datos$area <- factor(datos$V102, levels=c(1,2), 
                     labels=c("Urbano", "Rural"))
table(datos$V102, datos$area, useNA = "ifany")


## Anios escolaridad ----
datos$escol <- datos$V133
datos$escol[datos$escol>=96] <- NA
summary(datos$escol)


## Nivel educativo ----
datos$aux <- as.integer(datos$V149)
table(datos$aux, datos$V149)

datos$niveleduc <- 99
datos$niveleduc[datos$aux<=2] <- 1
datos$niveleduc[datos$aux==3] <- 2
datos$niveleduc[datos$aux==4] <- 2
datos$niveleduc[datos$aux==5] <- 3
datos$niveleduc <- factor(datos$niveleduc, levels=c(1,2,3), 
                          labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc, datos$V149)


## Castellano como lengua materna ----
datos$castellano  <- ifelse(is.na(datos$V131)==T, NA, 0)
datos$castellano[datos$V131==1] <- 1
prop.table(table(datos$castellano, useNA = "ifany"))


## Alguna vez casada ----
datos$algunavezcasada <- datos$V502
head(datos$algunavezcasada)


## Numero de uniones ----
datos$uniones <- datos$V503
head(datos$uniones)


## Fecha de primera union -----
summary(datos$V507); summary(datos$V508)
datos$anio_matri <- datos$V508

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_matri <- ifelse((is.na(datos$V507)==F), datos$V507, 6)

datos$aux <- paste(datos$anio_matri, datos$mes_matri, "15", sep="-")
datos$aux[datos$aux=="NA-6-15"] <- NA

datos$fecha_matri <- as.Date(datos$aux) 
class(datos$fecha_matri); head(datos$fecha_matri); summary(datos$fecha_matri)


## Anios desde union 1 ----
datos$anios1union <- datos$V512
class(datos$anios1union); summary(datos$anios1union)


## Anios escolaridad de pareja ----
datos$escol_pareja <- datos$V715
datos$escol_pareja[datos$escol_pareja>=96] <- NA
summary(datos$escol_pareja)


## Nivel educativo de pareja ----
head(datos$V729)
datos$aux <- as.integer(datos$V729)
table(datos$aux, datos$V729)

datos$niveleduc_pareja <- 99
datos$niveleduc_pareja[datos$aux<=2] <- 1
datos$niveleduc_pareja[datos$aux==3] <- 2
datos$niveleduc_pareja[datos$aux==4] <- 2
datos$niveleduc_pareja[datos$aux==5] <- 3
datos$niveleduc_pareja <- factor(datos$niveleduc_pareja, levels=c(1,2,3), 
                                 labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc_pareja, datos$V729)
table(datos$niveleduc_pareja, useNA = "ifany")


## Edad de pareja ----
datos$edad_pareja <- datos$V730
class(datos$edad_pareja); summary(datos$edad_pareja)


## Gana mas que pareja ----
datos$ingresorelativo <- datos$V746
head(datos$ingresorelativo)
table(datos$ingresorelativo, useNA = "ifany")
prop.table(table(datos$ingresorelativo, useNA = "ifany"))


## Deseo de hijos de Ego ----
datos$deseo <- datos$V605
head(datos$deseo)


## Ideal de hijos de Ego ----
datos$ideal <- as.integer(datos$V629)
datos$ideal[datos$ideal>=96] <- NA
summary(datos$ideal); head(datos$ideal); hist(datos$ideal)


## Ideal de hijos de Ego 2 ----
datos$ideal2 <- as.integer(datos$V613)
datos$ideal2[datos$ideal2>=96] <- NA
summary(datos$ideal2); head(datos$ideal2); hist(datos$ideal2)


## Deseo de hijos de pareja ----
datos$deseo_pareja <- datos$V621
head(datos$deseo_pareja)


## Escala de valores tradicionales de genero ----
head(datos$V744E)
datos[paste0("aux", 1:11)] <- 0

datos$aux1[datos$V743A==4 | datos$V743A==5 | datos$V743A==6] = 1
datos$aux2[datos$V743B==4 | datos$V743B==5 | datos$V743B==6] = 1
datos$aux3[datos$V743C==4 | datos$V743C==5 | datos$V743C==6] = 1
datos$aux4[datos$V743D==4 | datos$V743D==5 | datos$V743D==6] = 1
datos$aux5[datos$V743E==4 | datos$V743E==5 | datos$V743E==6] = 1
datos$aux6[datos$V743F==4 | datos$V743F==5 | datos$V743F==6] = 1

datos$aux7[datos$V744A==1] = 1
datos$aux8[datos$V744B==1] = 1
datos$aux9[datos$V744C==1] = 1
datos$aux10[datos$V744D==1] = 1
datos$aux11[datos$V744E==1] = 1

datos$aux <- rowSums(datos[paste0("aux", 1:11)], na.rm = TRUE)
summary(datos$aux)

datos$tradicional <- 10 * (datos$aux - min(datos$aux, na.rm = TRUE)) / 
  (max(datos$aux, na.rm = TRUE) - min(datos$aux, na.rm = TRUE))

summary(datos$tradicional); hist(datos$tradicional)


## Percentil de escolaridad Ego ----
summary(datos$weight)
datos <- datos[is.na(datos$weight)==F, ]
disenioeduc <- svydesign(ids = ~upm, strata = ~strata, weights = ~AWFACTE,
                         data = datos)

cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
summary(datos$escol_perc); hist(datos$escol_perc)
summary(datos$escol[datos$escol_perc>=80])


## Cuartil de escolaridad Ego ----
cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.25, 1, by = 0.25),
                           na.rm = TRUE, ci = FALSE))

datos$escol_cuantil <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
table(datos$escol_cuantil)
summary(datos$escol[datos$escol_cuantil==4])


## Percentil de escolaridad pareja ----
cortes <- coef(svyquantile(~escol_pareja, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc_pareja <- findInterval(datos$escol_pareja, cortes, left.open = TRUE) + 1
summary(datos$escol_perc_pareja); hist(datos$escol_perc_pareja)
summary(datos$escol_pareja[datos$escol_perc_pareja>=80])


## Asistencia escolar ----
datos$asistencia <- as.integer(datos$S111)
datos$asistencia[datos$asistencia>=2] <- NA
table(datos$asistencia, useNA = "ifany")


# Variables ----
d2015 <- datos %>% 
  select(id, upm, strata, weight, 
         fecha_entrevista, fecha_nacimiento, anio_nacimiento, 
         edad_anioscumplidos, area, escol, niveleduc, asistencia, 
         castellano, algunavezcasada, 
         uniones, fecha_matri, anios1union, escol_pareja, niveleduc_pareja,
         edad_pareja, ingresorelativo, deseo, ideal, ideal2, deseo_pareja,
         tradicional, escol_perc, escol_cuantil, escol_perc_pareja)



# 2020 -----
gc()
datos <- mujer_2020

## Id ----
length(unique(datos$CASEID)) == nrow(datos)
datos$id <- datos$CASEID


## Disenio muestral ----
datos$upm <- datos$V021
datos$strata <- datos$V022
datos$weight <- datos$AWFACTT


## Fecha de entrevista ----
datos$fecha_entrevista <- as.Date("1900-01-15") %m+%
  months(datos$V008 - 1)
head(datos[, c("fecha_entrevista", "V008", "V007")])


## Fecha de nacimiento de Ego ----
summary(datos$V009); summary(datos$V010)
datos$anio_nacimiento <- datos$V010

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_edad <- ifelse(is.na(datos$V009)==F, datos$V009, 6)

datos$aux <- paste(datos$anio_nacimiento, datos$mes_edad, "15", sep="-")
datos$fecha_nacimiento <- as.Date(datos$aux)
class(datos$fecha_nacimiento); head(datos$fecha_nacimiento)


## Edad de Ego ----
datos$edad_anioscumplidos <- datos$V012
class(datos$edad_anioscumplidos); summary(datos$edad_anioscumplidos)


## Area de residencia ----
datos$area <- factor(datos$V102, levels=c(1,2), 
                     labels=c("Urbano", "Rural"))
table(datos$V102, datos$area, useNA = "ifany")


## Anios escolaridad ----
datos$escol <- datos$V133
datos$escol[datos$escol>=96] <- NA
summary(datos$escol)


## Nivel educativo ----
datos$aux <- as.integer(datos$V149)
table(datos$aux, datos$V149)

datos$niveleduc <- 99
datos$niveleduc[datos$aux<=2] <- 1
datos$niveleduc[datos$aux==3] <- 2
datos$niveleduc[datos$aux==4] <- 2
datos$niveleduc[datos$aux==5] <- 3
datos$niveleduc <- factor(datos$niveleduc, levels=c(1,2,3), 
                          labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc, datos$V149)


## Castellano como lengua materna ----
head(datos$V131)
datos$castellano  <- ifelse(is.na(datos$V131)==T, NA, 0)
datos$castellano[datos$V131==10] <- 1
prop.table(table(datos$castellano, useNA = "ifany"))
# Alto nivel de NAs.


## Alguna vez casada ----
datos$algunavezcasada <- datos$V502
head(datos$algunavezcasada)


## Numero de uniones ----
datos$uniones <- datos$V503
head(datos$uniones)


## Fecha de primera union -----
summary(datos$V507); summary(datos$V508)
datos$anio_matri <- datos$V508

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_matri <- ifelse((is.na(datos$V507)==F), datos$V507, 6)

datos$aux <- paste(datos$anio_matri, datos$mes_matri, "15", sep="-")
datos$aux[datos$aux=="NA-6-15"] <- NA

datos$fecha_matri <- as.Date(datos$aux) 
class(datos$fecha_matri); head(datos$fecha_matri); summary(datos$fecha_matri)


## Anios desde union 1 ----
datos$anios1union <- datos$V512
class(datos$anios1union); summary(datos$anios1union)


## Anios escolaridad de pareja ----
datos$escol_pareja <- datos$V715
datos$escol_pareja[datos$escol_pareja>=96] <- NA
summary(datos$escol_pareja)


## Nivel educativo de pareja ----
head(datos$V729)
datos$aux <- as.integer(datos$V729)
table(datos$aux, datos$V729)

datos$niveleduc_pareja <- 99
datos$niveleduc_pareja[datos$aux<=2] <- 1
datos$niveleduc_pareja[datos$aux==3] <- 2
datos$niveleduc_pareja[datos$aux==4] <- 2
datos$niveleduc_pareja[datos$aux==5] <- 3
datos$niveleduc_pareja <- factor(datos$niveleduc_pareja, levels=c(1,2,3), 
                                 labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc_pareja, datos$V729)
table(datos$niveleduc_pareja, useNA = "ifany")


## Edad de pareja ----
datos$edad_pareja <- datos$V730
class(datos$edad_pareja); summary(datos$edad_pareja)


## Gana mas que pareja ----
datos$ingresorelativo <- datos$V746
head(datos$ingresorelativo)
table(datos$ingresorelativo, useNA = "ifany")
prop.table(table(datos$ingresorelativo, useNA = "ifany"))


## Deseo de hijos de Ego ----
datos$deseo <- datos$V605
head(datos$deseo)


## Ideal de hijos de Ego ----
datos$ideal <- as.integer(datos$V629)
datos$ideal[datos$ideal>=96] <- NA
summary(datos$ideal); head(datos$ideal); hist(datos$ideal)


## Ideal de hijos de Ego 2 ----
datos$ideal2 <- as.integer(datos$V613)
datos$ideal2[datos$ideal2>=96] <- NA
summary(datos$ideal2); head(datos$ideal2); hist(datos$ideal2)


## Deseo de hijos de pareja ----
datos$deseo_pareja <- datos$V621
head(datos$deseo_pareja)


## Escala de valores tradicionales de genero ----
head(datos$V744E)
datos[paste0("aux", 1:11)] <- 0

datos$aux1[datos$V743A==4 | datos$V743A==5 | datos$V743A==6] = 1
datos$aux2[datos$V743B==4 | datos$V743B==5 | datos$V743B==6] = 1
datos$aux3[datos$V743C==4 | datos$V743C==5 | datos$V743C==6] = 1
datos$aux4[datos$V743D==4 | datos$V743D==5 | datos$V743D==6] = 1
datos$aux5[datos$V743E==4 | datos$V743E==5 | datos$V743E==6] = 1
datos$aux6[datos$V743F==4 | datos$V743F==5 | datos$V743F==6] = 1

datos$aux7[datos$V744A==1] = 1
datos$aux8[datos$V744B==1] = 1
datos$aux9[datos$V744C==1] = 1
datos$aux10[datos$V744D==1] = 1
datos$aux11[datos$V744E==1] = 1

datos$aux <- rowSums(datos[paste0("aux", 1:11)], na.rm = TRUE)
summary(datos$aux)

datos$tradicional <- 10 * (datos$aux - min(datos$aux, na.rm = TRUE)) / 
  (max(datos$aux, na.rm = TRUE) - min(datos$aux, na.rm = TRUE))

summary(datos$tradicional); hist(datos$tradicional)


## Percentil de escolaridad Ego ----
summary(datos$weight)
datos <- datos[is.na(datos$weight)==F, ]
disenioeduc <- svydesign(ids = ~upm, strata = ~strata, weights = ~AWFACTE,
                         data = datos)

cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
summary(datos$escol_perc); hist(datos$escol_perc)
summary(datos$escol[datos$escol_perc>=80])


## Cuartil de escolaridad Ego ----
cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.25, 1, by = 0.25),
                           na.rm = TRUE, ci = FALSE))

datos$escol_cuantil <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
table(datos$escol_cuantil)
summary(datos$escol[datos$escol_cuantil==4])


## Percentil de escolaridad pareja ----
cortes <- coef(svyquantile(~escol_pareja, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc_pareja <- findInterval(datos$escol_pareja, cortes, left.open = TRUE) + 1
summary(datos$escol_perc_pareja); hist(datos$escol_perc_pareja)
summary(datos$escol_pareja[datos$escol_perc_pareja>=80])


## Asistencia escolar ----
datos$asistencia <- as.integer(datos$S111)
datos$asistencia[datos$asistencia>=2] <- NA


# Variables ----
d2020 <- datos %>% 
  select(id, upm, strata, weight, 
         fecha_entrevista, fecha_nacimiento, anio_nacimiento, 
         edad_anioscumplidos, area, escol, niveleduc, asistencia, 
         castellano, algunavezcasada, 
         uniones, fecha_matri, anios1union, escol_pareja, niveleduc_pareja,
         edad_pareja, ingresorelativo, deseo, ideal, ideal2, deseo_pareja,
         tradicional, escol_perc, escol_cuantil, escol_perc_pareja)



# 2025 -----
gc()
datos <- mujer_2025

## Id ----
length(unique(datos$CASEID)) == nrow(datos)
datos$id <- datos$CASEID


## Disenio muestral ----
datos$upm <- datos$V021
datos$strata <- datos$V022
datos$weight <- datos$AWFACTT


## Fecha de entrevista ----
datos$fecha_entrevista <- as.Date("1900-01-15") %m+%
  months(datos$V008 - 1)
head(datos[, c("fecha_entrevista", "V008", "V007")])


## Fecha de nacimiento de Ego ----
summary(datos$V009); summary(datos$V010)
datos$anio_nacimiento <- datos$V010

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_edad <- ifelse(is.na(datos$V009)==F, datos$V009, 6)

datos$aux <- paste(datos$anio_nacimiento, datos$mes_edad, "15", sep="-")
datos$fecha_nacimiento <- as.Date(datos$aux)
class(datos$fecha_nacimiento); head(datos$fecha_nacimiento)


## Edad de Ego ----
datos$edad_anioscumplidos <- datos$V012
class(datos$edad_anioscumplidos); summary(datos$edad_anioscumplidos)


## Area de residencia ----
datos$area <- factor(datos$V102, levels=c(1,2), 
                     labels=c("Urbano", "Rural"))
table(datos$V102, datos$area, useNA = "ifany")


## Anios escolaridad ----
datos$escol <- datos$V133
datos$escol[datos$escol>=96] <- NA
summary(datos$escol)


## Nivel educativo ----
datos$aux <- as.integer(datos$V149)
table(datos$aux, datos$V149)

datos$niveleduc <- 99
datos$niveleduc[datos$aux<=2] <- 1
datos$niveleduc[datos$aux==3] <- 2
datos$niveleduc[datos$aux==4] <- 2
datos$niveleduc[datos$aux==5] <- 3
datos$niveleduc <- factor(datos$niveleduc, levels=c(1,2,3), 
                          labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc, datos$V149)


## Castellano como lengua materna ----
head(datos$V131)
datos$castellano  <- ifelse(is.na(datos$V131)==T, NA, 0)
datos$castellano[datos$V131==10] <- 1
prop.table(table(datos$castellano, useNA = "ifany"))


## Alguna vez casada ----
datos$algunavezcasada <- datos$V502
head(datos$algunavezcasada)


## Numero de uniones ----
datos$uniones <- datos$V503
head(datos$uniones)


## Fecha de primera union -----
summary(datos$V507); summary(datos$V508)
datos$anio_matri <- datos$V508

# En caso de no tener mes, se asume a mitad de anio.
datos$mes_matri <- ifelse((is.na(datos$V507)==F), datos$V507, 6)

datos$aux <- paste(datos$anio_matri, datos$mes_matri, "15", sep="-")
datos$aux[datos$aux=="NA-6-15"] <- NA

datos$fecha_matri <- as.Date(datos$aux) 
class(datos$fecha_matri); head(datos$fecha_matri); summary(datos$fecha_matri)


## Anios desde union 1 ----
datos$anios1union <- datos$V512
class(datos$anios1union); summary(datos$anios1union)


## Anios escolaridad de pareja ----
datos$escol_pareja <- datos$V715
datos$escol_pareja[datos$escol_pareja>=96] <- NA
summary(datos$escol_pareja)


## Nivel educativo de pareja ----
head(datos$V729)
datos$aux <- as.integer(datos$V729)
table(datos$aux, datos$V729)

datos$niveleduc_pareja <- 99
datos$niveleduc_pareja[datos$aux<=2] <- 1
datos$niveleduc_pareja[datos$aux==3] <- 2
datos$niveleduc_pareja[datos$aux==4] <- 2
datos$niveleduc_pareja[datos$aux==5] <- 3
datos$niveleduc_pareja <- factor(datos$niveleduc_pareja, levels=c(1,2,3), 
                                 labels=c("Primaria", "Secundaria", "Superior"))
table(datos$niveleduc_pareja, datos$V729)
table(datos$niveleduc_pareja, useNA = "ifany")


## Edad de pareja ----
datos$edad_pareja <- datos$V730
class(datos$edad_pareja); summary(datos$edad_pareja)


## Gana mas que pareja ----
datos$ingresorelativo <- datos$V746
head(datos$ingresorelativo)
table(datos$ingresorelativo, useNA = "ifany")
prop.table(table(datos$ingresorelativo, useNA = "ifany"))


## Deseo de hijos de Ego ----
datos$deseo <- datos$V605
head(datos$deseo)


## Ideal de hijos de Ego ----
datos$ideal <- as.integer(datos$V629)
datos$ideal[datos$ideal>=96] <- NA
summary(datos$ideal); head(datos$ideal); hist(datos$ideal)


## Ideal de hijos de Ego 2 ----
datos$ideal2 <- as.integer(datos$V613)
datos$ideal2[datos$ideal2>=96] <- NA
summary(datos$ideal2); head(datos$ideal2); hist(datos$ideal2)


## Deseo de hijos de pareja ----
datos$deseo_pareja <- datos$V621
head(datos$deseo_pareja)


## Escala de valores tradicionales de genero ----
head(datos$V744E)
datos[paste0("aux", 1:11)] <- 0

datos$aux1[datos$V743A==4 | datos$V743A==5 | datos$V743A==6] = 1
datos$aux2[datos$V743B==4 | datos$V743B==5 | datos$V743B==6] = 1
datos$aux3[datos$V743C==4 | datos$V743C==5 | datos$V743C==6] = 1
datos$aux4[datos$V743D==4 | datos$V743D==5 | datos$V743D==6] = 1
datos$aux5[datos$V743E==4 | datos$V743E==5 | datos$V743E==6] = 1
datos$aux6[datos$V743F==4 | datos$V743F==5 | datos$V743F==6] = 1

datos$aux7[datos$V744A==1] = 1
datos$aux8[datos$V744B==1] = 1
datos$aux9[datos$V744C==1] = 1
datos$aux10[datos$V744D==1] = 1
datos$aux11[datos$V744E==1] = 1

datos$aux <- rowSums(datos[paste0("aux", 1:11)], na.rm = TRUE)
summary(datos$aux)

datos$tradicional <- 10 * (datos$aux - min(datos$aux, na.rm = TRUE)) / 
  (max(datos$aux, na.rm = TRUE) - min(datos$aux, na.rm = TRUE))

summary(datos$tradicional); hist(datos$tradicional)


## Percentil de escolaridad Ego ----
summary(datos$weight)
datos <- datos[is.na(datos$weight)==F, ]
disenioeduc <- svydesign(ids = ~upm, strata = ~strata, weights = ~AWFACTE,
                         data = datos)

cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
summary(datos$escol_perc); hist(datos$escol_perc)
summary(datos$escol[datos$escol_perc>=80])


## Cuartil de escolaridad Ego ----
cortes <- coef(svyquantile(~escol, disenioeduc, 
                           quantiles = seq(0.25, 1, by = 0.25),
                           na.rm = TRUE, ci = FALSE))

datos$escol_cuantil <- findInterval(datos$escol, cortes, left.open = TRUE) + 1
table(datos$escol_cuantil)
summary(datos$escol[datos$escol_cuantil==4])


## Percentil de escolaridad pareja ----
cortes <- coef(svyquantile(~escol_pareja, disenioeduc, 
                           quantiles = seq(0.01, 1, by = 0.01),
                           na.rm = TRUE, ci = FALSE))
summary(cortes)

datos$escol_perc_pareja <- findInterval(datos$escol_pareja, cortes, left.open = TRUE) + 1
summary(datos$escol_perc_pareja); hist(datos$escol_perc_pareja)
summary(datos$escol_pareja[datos$escol_perc_pareja>=80])


## Asistencia escolar ----
datos$asistencia <- as.integer(datos$S111)
datos$asistencia[datos$asistencia>=2] <- NA



# Variables ----
d2025 <- datos %>% 
  select(id, upm, strata, weight, 
         fecha_entrevista, fecha_nacimiento, anio_nacimiento, 
         edad_anioscumplidos, area, escol, niveleduc, asistencia, 
         castellano, algunavezcasada, 
         uniones, fecha_matri, anios1union, escol_pareja, niveleduc_pareja,
         edad_pareja, ingresorelativo, deseo, ideal, ideal2, deseo_pareja,
         tradicional, escol_perc, escol_cuantil, escol_perc_pareja)


# Base consolidada ----
datos <- rbind(d2010, d2015, d2020, d2025)


## Cohorte de nacimiento ----
summary(datos$anio_nacimiento)
datos <- datos[datos$anio_nacimiento<=2009, ]
datos$cohorte <- factor(floor(datos$anio_nacimiento / 10) * 10)
table(datos$cohorte, useNA = "ifany")
prop.table(table(datos$cohorte, useNA = "ifany"))
table(datos$cohorte, datos$anio_nacimiento)


## Unos ----
names(datos)
datos$unos <- 1


## Edad a la primera union ----
datos$edad_1union <- as.numeric((datos$fecha_matri - datos$fecha_nacimiento)/365)
head(datos[datos$uniones==1, c("uniones", "fecha_matri", "fecha_nacimiento", "edad_1union")])
summary(datos$edad_1union)


## Anios entrevista ----
datos$anio_entrevista <- year(datos$fecha_entrevista)
table(datos$anio_entrevista)


# Save ----
names(datos)

save(datos, 
     diccionario_2010, diccionario_2015, 
     diccionario_2020, diccionario_2025, 
     file="data/datos.RData")
