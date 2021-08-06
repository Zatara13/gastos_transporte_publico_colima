## Transporte urbano en Colima.
## Código para saber quiénes utilizan el transporte público en Colima
## Por Zatara

######### Librerías de trabajo ############

## Función para descargar paquetes en automático
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


####### Cargamos librerías de trabajo#######
foo(c("readr",
      "tidyverse",
      "ggsci",
      "ggthemes",
      "sf",
      "srvyr",
      "kableExtra",
      "doBy")
    )


########### Bases de datos de trabajo #################
## ENIGH 2020
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="concentradohogar.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "concentradohogar.csv")
unlink(td)

#Leer el archivo
concentrado_hogar <-read.csv(fpath)

## Limpiar área de trabajo
rm(foo,
   fpath,
   td,
   tf,
   url)

######## Procesamiento de datos #############

## Seleccionamos datos para Colima
datos_colima <-  concentrado_hogar %>% 
  ## Filtramos para Colima
  filter(ubica_geo %in% c(6001,
                          6002,
                          6003,
                          6004,
                          6005,
                          6006,
                          6007,
                          6008,
                          6009,
                          6010)) 
rm(concentrado_hogar)

### Deciles de Ingreso
## Esta parte del código se extrajo de las recomendaciones de INEGI para estimar los indicadores principales de la ENIGH.
## El código se adaptó de la página 7 del documento de descripción de cálculo de indicadores de la ENIGH
## Disponible en https://inegi.org.mx/contenidos/programas/enigh/nc/2020/doc/enigh2020_ns_descripcion_calculo_r.pdf

# Deja activa la tabla de datos para Colima
attach(datos_colima)
# Ordena Conc de acuerdo a ing_cor,folioviv,foliohog.
## NOTA: Por algún motivo extraño, mi computadora guarda la variable folioviv como ï..folioviv.
## Si el código te da problemas en la siguiente línea, cambia la variable ï..folioviv por folioviv
datos_colima<- orderBy(~+ing_cor+ï..folioviv+foliohog, data=datos_colima)
# Suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor)
# Se divide la suma de factores entre diez para sacar el tamaño del decil 
#(se debe de truncar el resultado quitando los decimales).
tam_dec<-trunc(tot_hogares/10)
# Muestra la suma del factor en variable hog.
datos_colima$tam_dec=tam_dec
# Se renombra la tabla concentrado a BD1.
BD1 <- datos_colima
# Dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor
# Se ordena de menor a mayor segun la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]
# Se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)
### Entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9){
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])
                                                                  [1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9){
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10

datos_colima <- BD1

## Limpiamos archivos residuales
rm(BD1,
   a1,
   b1,
   i,
   tam_dec,
   tot_hogares)

## Creamos indicador para identificaar gasto de transporte público en el hogar y renombramos la variable sexo_jefe acorde a la clave
datos_colima <- datos_colima %>% 
  mutate(uso_tp = case_when(publico > 0 ~ 1,
                             TRUE ~ 0),
         ## La clave está disponible en la página 189 del documento de descripción de bases de datos de la ENIGH 2020
         ## Disponible en https://inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463901242.pdf
         sexo_jefe = case_when(sexo_jefe == 1 ~ "Hombre",
                               sexo_jefe == 2 ~ "Mujer")
         )

############# Realizamos estimaciones ############
## Diseño muestral para Colima
my_design <- datos_colima %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

## Estimaciones del ingreso destinado a transporte público por deciles de ingreso
## Gasto medio por decil de ingresos en transporte público
mean_hogs_gtp <- my_design %>% 
  group_by(DECIL) %>% 
  summarise(publico=survey_mean(publico,
    vartype = c("cv",
                "ci"),
    level=0.95))%>%
  mutate(publico_cv=
           publico_cv*100,
  )

## Ingreso medio por decil de ingresos
mean_ingcor_hogs <- my_design %>% 
  group_by(DECIL) %>% 
  summarise(ing_cor=survey_mean(ing_cor,
    vartype = c("cv",
                "ci"),
    level=0.95))%>%
  mutate(ing_cor_cv=
           ing_cor_cv*100,
  )

## Porcentaje de ingresos destinados a transporte urbano por decil de ingresos
## Dado que el nivel de precisión de los estimadores de ingreso medio es alto (0-15), para simplificar el análisis utilizaré los estimados puntuales para calcular el porcentaje
# #La precisión del porcentaje del ingreso destinado a transporte público se reportará sobre el coeficiente de variación del gasto en transporte público, porque es un cambio de escala
pct_gasto_tp_dec <- merge(mean_hogs_gtp,
                          mean_ingcor_hogs,
                          by = "DECIL") %>% 
  mutate(pct_gasto_tp = publico / ing_cor * 100,
         pct_gasto_tp_low = publico_low / ing_cor * 100,
         pct_gasto_tp_upp = publico_upp / ing_cor *100) %>% 
  rename(pct_gasto_tp_cv = publico_cv) %>% 
  select(DECIL,
         pct_gasto_tp,
         pct_gasto_tp_cv,
         pct_gasto_tp_low,
         pct_gasto_tp_upp) %>% 
  arrange(DECIL)

## Análisis con perspectiva de género
## Para esta sección, identificaremos las diferencias entre familias encabezadas por mujeres respecto a las familias encabezadas por hombres en cada decil de ingreso.
## Repetiremos el cálculo de las estimaciones previas, pero desagregaremos de acuerdo al género de la persona que encabeza el hogar.

## Ingreso medio por decil de ingresos desagregando por género de jefa del hogar
mean_ingcor_hogs_pg <- my_design %>% 
  group_by(DECIL,
           sexo_jefe) %>% 
  summarise(ing_cor=survey_mean(ing_cor,
                                vartype = c("cv",
                                            "ci"),
                                level=0.95
                                )
            )%>%
  mutate(ing_cor_cv=
           ing_cor_cv*100,
  )

## Gasto medio por decil de ingresos en transporte público desagregando por género de jefa del hogar
mean_hogs_gtp_pg <- my_design %>% 
  group_by(DECIL,
           sexo_jefe
           ) %>% 
  summarise(publico=survey_mean(publico,
                                vartype = c("cv",
                                            "ci"),
                                level=0.95
                                )
            )%>%
  mutate(publico_cv=
           publico_cv*100,
  )


## Porcentaje de ingresos destinados a transporte urbano por decil de ingresos considerando género de jefa del hogar
## Dado que el nivel de precisión de los estimadores de ingreso medio es alto (0-15), para simplificar el análisis utilizaré los estimados puntuales para calcular el porcentaje
# #La precisión del porcentaje del ingreso destinado a transporte público se reportará sobre el coeficiente de variación del gasto en transporte público, porque es un cambio de escala
pct_gasto_tp_dec_pg <- merge(mean_hogs_gtp_pg,
                             mean_ingcor_hogs_pg,
                             by = c("DECIL",
                                    "sexo_jefe")) %>% 
  mutate(pct_gasto_tp_pg = publico / ing_cor * 100,
         pct_gasto_tp_pg_low = publico_low / ing_cor * 100,
         pct_gasto_tp_pg_upp = publico_upp / ing_cor *100) %>% 
  rename(pct_gasto_tp_pg_cv = publico_cv) %>% 
  select(DECIL,
         sexo_jefe,
         pct_gasto_tp_pg,
         pct_gasto_tp_pg_cv,
         pct_gasto_tp_pg_low,
         pct_gasto_tp_pg_upp) %>% 
  arrange(DECIL)

rm(
  mean_hogs_gtp,
  mean_hogs_gtp_pg,
  mean_ingcor_hogs,
  mean_ingcor_hogs_pg,
  datos_colima,
  my_design
)

############ Visualización de datos ###################

## Porcentaje de ingreso medio que destinan los hogares a transporte público
pct_gasto_tp_dec %>% 
  ggplot(
    aes(
      x = DECIL,
      y = pct_gasto_tp
    )
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        axis.title.x =  element_blank()) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue") +
  geom_errorbar(aes(ymin=pct_gasto_tp_low, 
                    ymax=pct_gasto_tp_upp),
    width=.2,
    position=position_dodge(.9)
    ) +
  scale_x_continuous(breaks = c(1:10),
                     labels = c("I",
                                "II",
                                "III",
                                "IV",
                                "V",
                                "VI",
                                "VII",
                                "VIII",
                                "IX",
                                "X"
                                )
                     )+
  labs(title = "Porcentaje de ingreso medio destinado a transporte público",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Porcentaje de ingreso destinado a transporte público",
       caption = "Fuente: ENIGH 2018, INEGI,
       Nota 1: Las precisión de las estimaciones para los deciles I, II, III, V, VI Y VII es alta. Para los deciles IV, VIII, IX y X es moderada.
       Nota 2: Para revisar el nivel de confianza de las estimaciones, se recomienda ir a la tabla pct_gasto_tp_dec_pg y evaluar el coeficiente de variación reportado.
       Elaborado por @jkvisfocri"
       )

## Porcentaje de ingreso medio que destinan los hogares a transporte público considerando género de la jefa del hogar
pct_gasto_tp_dec_pg %>% 
  ggplot(
    aes(
      x = DECIL,
      y = pct_gasto_tp_pg,
      fill = sexo_jefe
    )
  ) +
  theme_bw()+
  theme(
    text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
    plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
    axis.title.x =  element_blank()
        ) + 
  geom_bar(stat = "identity",
           position=position_dodge()
           ) +
  geom_errorbar(aes(ymin=pct_gasto_tp_pg_low, 
                    ymax=pct_gasto_tp_pg_upp),
                width=.2,
                position=position_dodge(.9)
  ) +
  scale_x_continuous(breaks = c(1:10),
                     labels = c("I",
                                "II",
                                "III",
                                "IV",
                                "V",
                                "VI",
                                "VII",
                                "VIII",
                                "IX",
                                "X"
                                )
                     )+
  scale_fill_lancet()+
  labs(title = "Porcentaje de ingreso medio destinado a transporte público, considerando género de la jefa del hogar",
       subtitle = "Colima, 2020",
       x = "Decil de ingresos",
       y = "Porcentaje de ingreso destinado a transporte público",
       fill = "Jefa del hogar",
       caption = "Fuente: ENIGH 2018, INEGI,
       Nota 1: El nivel de confianza de las estimaciones es moderada para todas las particiones de la gráfica excepto para el decil X mujeres. El nivel de precisión de esa estimación es baja.
       Nota 2: Para revisar el nivel de confianza de las estimaciones, se recomienda ir a la tabla pct_gasto_tp_dec_pg y evaluar el coeficiente de variación reportado.
       Elaborado por @jkvisfocri"
       )