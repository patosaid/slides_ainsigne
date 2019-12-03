# grafica de antecedentes
library(highcharter)
datos <- readxl::read_excel("datos/superficies_plantaciones_1.xlsx") 
library(tidyverse)
datos <-  datos %>% gather(especie,Superficie,2:5) %>%  
  mutate(Superficie = Superficie /1000000) 


x <- data.frame("Año" = 1900 , "especie" = "Pino radiata", "Superficie" = 0.1)

datos <- datos %>%  bind_rows(x) 
datos$especie <- factor(datos$especie , levels = c("Pino radiata","Eucalipto" ,"Otras especies", "Total" ), ordered = T)

grafica <- datos %>% arrange(Año) %>% 
  filter(especie %in% c("Pino radiata", "Eucalipto")) %>% 
  ggplot( aes(x = Año, y = Superficie, color = especie)) +
  geom_line(size= 2)+
  theme_light()+ 
  scale_x_continuous(breaks =seq(1900, 2016, by = 10))+
  theme(       
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    axis.title.x = element_text(size = rel(1.5)),  
    axis.text.y = element_text(size = rel(1.5)) ,
    axis.text.x = element_text(size = rel(1.5)) ,
    legend.title = element_text(face="bold"),
    legend.text = element_text(size = rel(1.1)) ,
    #plot.caption=element_text(size=rel(1), 
    #                         face="bold.italic", hjust = 0),
    legend.position = "bottom")+     
  geom_point(size = 2)+    
  labs(x = "Año", y = "Superficie (millones de ha)",colour = "Especie" ,caption = "Datos de INFOR")+
  scale_colour_manual(values=c( "#61B087", "#FF834C"))
#install.packages("svglite")
ggsave("graf.svg", width = 10, height = 6)
