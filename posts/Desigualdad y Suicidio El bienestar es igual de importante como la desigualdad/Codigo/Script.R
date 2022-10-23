# Librerias ####

library(tidyverse)
library(latticeExtra)
library(ggtext)
library(extrafont)

# Theme ####

theme_elegante <- function(base_size = 10,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = NA)) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}

# Data ####

# https://www.oecd.org/centrodemexico/estadisticas/diferencia-ingresos.htm
Pobreza <- read_csv("Codes/Post 1/Gini.csv") %>% 
  filter(SUBJECT == "GINI") %>% 
  select(LOCATION, TIME, Value) %>% 
  mutate(Value = Value*100) # Values to percent

# https://data.oecd.org/healthstat/suicide-rates.htm
Suicidios <- read_csv("Codes/Post 1/Suicidios.csv") %>%
  select(LOCATION, TIME, Value)

Data_0 = merge(x = Pobreza, y = Suicidios, by = c("LOCATION", "TIME")) %>% 
  rename("Gini" = "Value.x", "Suicidios" = "Value.y" )

Data_0_2017 = Data_0 %>% 
  filter(TIME == 2017) %>% 
  select(-TIME)

Data_1 = Data_0 %>%
  pivot_longer(cols = Gini:Suicidios, names_to = "Estado", values_to = "count")

Data_1_2017 = Data_1 %>% 
  filter(TIME == 2017) %>% 
  select(-TIME)

# Calculos ####

Stats = Data_1_2017 %>%
  group_by(Estado) %>%
  summarise(mean = mean(count),
            SD = sd(count)) %>%
  mutate(meanpos = mean + 1 *SD,
         meanneg = mean - 1 *SD)

Stats_Gini <- Stats %>%
  filter(Estado == "Gini")

Stats_Suicidio <- Stats %>%
  filter(Estado == "Suicidios")

# Visualization #### EX: https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html ####

# Codigos a español

{
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "AUT"] = "AUSTRIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "BGR"] = "BULGARIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "CAN"] = "CANADÁ"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "CHE"] = "SUIZA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "CHL"] = "CHILE"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "CRI"] = "COSTA RICA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "CZE"] = "CHEQUIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "DEU"] = "ALEMANIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "DNK"] = "DINAMARCA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "ESP"] = "ESPAÑA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "EST"] = "ESTONIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "FIN"] = "FINLANDIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "GBR"] = "REINO UNIDO"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "GRC"] = "GRECIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "HUN"] = "HUNGRÍA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "ISL"] = "ISLANDIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "ISR"] = "ISRAEL"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "ITA"] = "ITALIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "KOR"] = "KOREA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "LTU"] = "LITUANIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "LUX"] = "LUXEMBURGO"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "LVA"] = "LETONIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "NLD"] = "PAISES BAJOS"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "POL"] = "POLONIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "PRT"] = "PORTUGAL"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "ROU"] = "RUMANIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "SVK"] = "ESLOVAQUIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "SVN"] = "ESLOVENIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "SWE"] = "SUECIA"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "USA"] = "ESTADOS UNIDOS"
  Data_0_2017$LOCATION[Data_0_2017$LOCATION == "ZAF"] = "SUDÁFRICA"

}


# Visualizacion general

Data_0_2017 %>% 
  ggplot(country = Codigo) +
  geom_segment(aes(x = LOCATION, xend = LOCATION, y = Suicidios, yend = Gini), alpha = 0.6,  color = "gray85", size = 7) +
  geom_point(aes(x = LOCATION, y = Suicidios), color = "skyblue3", size = 8) +
  geom_point(aes(x = LOCATION, y = Gini), color = "sienna2", size = 8) +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(x = LOCATION, y = Gini, label = format(Gini, digits = 1, big.mark = ',')),
            position = position_dodge(width = 0), size = 3.2, color = "white", family = "Segoe UI Semibold") +
  geom_text(aes(x = LOCATION, y = Suicidios, label = format(Suicidios, digits = 1, big.mark = ',')),
            position = position_dodge(width = 0), size = 3.2, color = "white", family = "Segoe UI Semibold") +
  geom_rect(xmin = 100, xmax = 0, ymin = Stats_Gini$meanneg, ymax = Stats_Gini$meanpos, fill = "sienna2", alpha = 0.01) +
  geom_hline(yintercept = Stats_Gini$mean, linetype = "solid", size = 1, alpha = 0.4, color = "sienna2") +
  geom_rect(xmin = 100, xmax = 0, ymin = Stats_Suicidio$meanneg, ymax = Stats_Suicidio$meanpos, fill = "skyblue3", alpha = 0.01) +
  geom_hline(yintercept = Stats_Suicidio$mean, linetype = "solid", size = 1, alpha = 0.4, color = "skyblue3") +
  geom_text(y = Stats_Gini$mean - 0.8, x = "SUDÁFRICA", label = "PROMEDIO (32.99)", angle = 90, size = 2.5, color = "sienna2", family = "Segoe UI Semibold") +
  geom_text(y = Stats_Gini$meanpos - 0.8, x = "SUDÁFRICA", label = "DESVIACIÓN (7.96)", angle = 90, size = 2.5, color = "sienna2", family = "Segoe UI Semibold") +
  geom_text(y = Stats_Suicidio$mean - 0.8, x = "ITALIA", label = "PROMEDIO (11.20)", angle = 90, size = 2.5, color = "skyblue3", family = "Segoe UI Semibold") +
  geom_text(y = Stats_Suicidio$meanpos - 0.8, x = "ITALIA", label = "DESVIACIÓN (5.28)", angle = 90, size = 2.5, color = "skyblue3", family = "Segoe UI Semibold") +
  ggtitle("Tendencia Datos de la OCDE") +
  labs(subtitle = "Relación entre <span style = 'color: skyblue3;'>**Tasa de Suicidios (%)**</span> y <span style = 'color: sienna2;'>**Desigualdad de Ingresos (%)**</span> | Año 2017<br>",
       caption = "Visualización por Ignacio Zamora | **Your Health in Data** <br> Datos de oecd.org | **OCDE** <br>") +
  theme(plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
        plot.subtitle = element_markdown(size = 12),
        plot.title = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Segoe UI", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI "),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm"))

# Observaciones ####

# Aclarar lo que es el Gini.
# Aclarar el formato de cálculo de la tasa de suicidios.
# Aclarar parámetros de medición.
# Buscar una forma de poner banderas.
# Ordenar de menor a mayor o mayor a menor y buscar que se vea bien.


