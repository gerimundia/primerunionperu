rm(list=ls())
gc()

source("R/01_paquetes.R")


# Datos ----
load("data/datos.RData")
datos <- data.table(datos)


# Edad exacta ----
datos$edad <- (datos$fecha_entrevista - datos$fecha_nacimiento)/365


# Evento ----
table(datos$algunavezcasada, useNA = "ifany")
head(datos$algunavezcasada)

datos[algunavezcasada==1 | algunavezcasada==2, evento:=1]
datos[algunavezcasada==0, evento:=0]

prop.table(table(datos$evento, useNA = "ifany"))
prop.table(table(datos$evento, datos$anio_entrevista, useNA="ifany"), margin=2)


# Tiempo ----
datos[evento==1, tiempo:=edad_1union]
datos[evento==0, tiempo:=edad]
head(datos[, c("tiempo","evento","fecha_nacimiento", "fecha_entrevista", "fecha_matri")])


# Muestra analitica ----
summary(datos)
datos$completo <- complete.cases(datos[, c("id","upm","strata","weight",
                                           "tiempo","evento","edad","cohorte",
                                           "area",
                                           "niveleduc", 
                                           "escol","escol_perc", "escol_cuantil")])

table(datos$completo)
prop.table(table(datos$completo, useNA = "ifany"))
table(datos$completo, datos$anio_entrevista, useNA="ifany")
prop.table(table(datos$completo, datos$anio_entrevista, useNA="ifany"), margin=2)
# View(datos[completo==F, c("id","upm","strata","weight",
#            "tiempo","evento","algunavezcasada","edad","cohorte",
#            "area", "niveleduc", "escol","escol_perc", "escol_cuantil")])


# Ventana de observacion ----
datos <- datos[tiempo>=15, ]


# Ajuste de variables ----
prop.table(table(datos$cohorte))
datos$cohorte2 <- relevel(datos$cohorte, ref = "1980")

class(datos$area)
prop.table(table(datos$area))

class(datos$castellano)
prop.table(table(datos$castellano))


# Disenio muestral ----
disenio <- svydesign(ids = ~upm, strata = ~strata, weights = ~weight,
                         data = datos, nest=TRUE)

sub_disenio <- subset(disenio, completo==T)
dim(sub_disenio$variables)
114796

# Descriptivo ----

# KM por cohorte
km <- svykm(Surv(tiempo, evento) ~ cohorte2, design = sub_disenio)
km

datos_km <- do.call(rbind, lapply(names(km), function(g) {
 x <- km[[g]]
    data.frame(
    tiempo = x$time,
    supervivencia = x$surv,
    cohorte = g)
}))

datos_km$propacum <- 1 - datos_km$supervivencia

datos_km %>% 
  filter(tiempo>=15) %>% 
ggplot(aes(x = tiempo, y = propacum, color = cohorte)) +
  scale_x_continuous(breaks = seq(15, 50, 10)) +
  geom_step(linewidth = 0.8) +
  labs(x = "Edad", 
       y = "Proporción acumulada", 
       color = "Cohorte",
    fill = "Cohorte") +
  theme_minimal() +
  theme(axis.text = element_text(size=11), 
        axis.title = element_text(size=13), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)), 
        legend.text = element_text(size=12))

ggsave(last_plot(), file="output/km_cohorte.jpeg", 
       units = "cm", width = 12, height = 10, dpi = 400)


# KM por cuantil
km <- svykm(Surv(tiempo, evento) ~ escol_cuantil, design = sub_disenio)
km

datos_km <- do.call(rbind, lapply(names(km), function(g) {
  x <- km[[g]]
  data.frame(
    tiempo = x$time,
    supervivencia = x$surv,
    escol_cuantil = g)
}))

datos_km$propacum <- 1 - datos_km$supervivencia

datos_km %>% 
  filter(tiempo>=15) %>% 
  ggplot(aes(x = tiempo, y = propacum, color = escol_cuantil)) +
  scale_x_continuous(breaks = seq(15, 50, 10)) +
  geom_step(linewidth = 0.8) +
  labs(x = "Edad", 
       y = "Proporción acumulada", 
       color = "Cuantil",
       fill = "Cuantil") +
  theme_minimal() +
  theme(axis.text = element_text(size=11), 
        axis.title = element_text(size=13), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)), 
        legend.text = element_text(size=12))

ggsave(last_plot(), file="output/km_cuantil.jpeg", 
       units = "cm", width = 12, height = 10, dpi = 400)



# KM por area
km <- svykm(Surv(tiempo, evento) ~ area, design = sub_disenio)
km

datos_km <- do.call(rbind, lapply(names(km), function(g) {
  x <- km[[g]]
  data.frame(
    tiempo = x$time,
    supervivencia = x$surv,
    area = g)
}))

datos_km$propacum <- 1 - datos_km$supervivencia

datos_km %>% 
  filter(tiempo>=15) %>% 
  ggplot(aes(x = tiempo, y = propacum, color = area)) +
  scale_x_continuous(breaks = seq(15, 50, 10)) +
  geom_step(linewidth = 0.8) +
  labs(x = "Edad", 
       y = "Proporción acumulada", 
       color = "Área",
       fill = "Área") +
  theme_minimal() +
  theme(axis.text = element_text(size=11), 
        axis.title = element_text(size=13), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)), 
        legend.text = element_text(size=12))

ggsave(last_plot(), file="output/km_area.jpeg", 
       units = "cm", width = 12, height = 10, dpi = 400)


# Modelo ----
modelo1 <- svycoxph(Surv(tiempo, evento) ~ cohorte2 
                    + edad, 
                    design = sub_disenio)

modelo2 <- svycoxph(Surv(tiempo, evento) ~ cohorte2 + escol_perc + edad, 
                    design = sub_disenio)

modelo3 <- svycoxph(Surv(tiempo, evento) ~ cohorte2 + escol_perc + area + edad, 
                    design = sub_disenio)

summary(modelo3)
# Modelo 3: escol_perc  -0.0148230  0.9852863
# Por cada incremento de un percentil en la distribucion de los anios de escolaridad, 
# el riesgo de ingresar en union en cada momento del tiempo se reduce en 1.5%.
exp(-0.0097023)
(1 - 0.9903446)*100

modelo_tab <- data.frame(summary(modelo3)$conf.int) 
modelo_tab$term <- row.names(modelo_tab)
modelo_tab$etiqueta <- c("Cohorte 1960-1969", "Cohorte 1970-1979", 
                         "Cohorte 1990-1999", "Cohorte 2000-2009", 
                         "Escolaridad (percentil)", "Castellano",  
                         "Rural", "Edad")

modelo_tab$etiqueta <- factor(modelo_tab$etiqueta,
                              levels = rev(unique(modelo_tab$etiqueta)))

ggplot(modelo_tab, 
       aes(y=etiqueta, x=exp.coef., xmin=lower..95, xmax=upper..95, color=etiqueta)) + 
 geom_pointrange(color=c("gray10", "gray10", "gray10" , "gray10",
                         "gray10", "gray10", "gray50" , "gray10"), 
                  linewidth=1) +
  geom_vline(xintercept=1, color="steelblue", linewidth=0.9) +
  labs(x="Razón de riesgos (IC 95%)", y=NULL) +
  theme_classic() +
    theme(axis.text = element_text(size=11, color="gray20"), 
        axis.title = element_text(size=12), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggsave(last_plot(), file="output/modelo4.jpeg", 
       units = "cm", width = 12, height = 10, dpi = 400)

temp <- cox.zph(modelo4) 
print(temp)             
