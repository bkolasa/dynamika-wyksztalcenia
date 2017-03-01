
library(ggplot2)
library(dplyr)
library(ggthemes)
library(RColorBrewer)

edu<-read.csv("education_data.csv")

edu %>% group_by(rok,wojewodztwo) %>% summarise(norm = sum(waga)) -> edu2
edu %>% group_by(rok,wojewodztwo,edukacja) %>% summarise(waga=sum(waga)) ->edu3 
edu2 %>% inner_join(edu3) %>% mutate(waga=waga/norm)%>% ungroup %>% mutate(rok=as.Date(paste0(rok,"-01-01"))) -> edu

edu  %>%  filter(!is.na(wojewodztwo))  %>%
  ggplot(aes(x=rok,y=waga,color=edukacja)) + 
  scale_color_manual(name="",values = wsj_pal()(4)) +
  geom_path(stat = "identity", size=1.5) + 
  theme_wsj() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5),
         plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text())+
  facet_wrap(~wojewodztwo) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_date(date_labels = "'%y") +
  ggtitle("Dynamika zmian struktury wykształcenia", 
          subtitle = "Linie opisują jak zmieniał się odsetek ludzi \no określonym wykształceniu w danym województwie")
  ggsave(filename = "dynamika-wykształcenia.png", width = 25, height = 20, units = "cm")