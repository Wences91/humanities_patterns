library(dplyr)
library(ggplot2)
library(scales)
library(ggsci)

df <- read.delim('data/authors_metrics_dialnet.tsv',
                 fileEncoding = 'UTF-8',
                 check.names = FALSE)

df <- df %>%
  filter(!is.na(Age)) %>%
  filter(First_pub > 1949) %>%
  filter(Total_pubs>1) %>%
  filter(Total_pubs != max(Total_pubs))

disciplina_map <- data.frame(
  Disciplina = c(
    "Antropología Social",
    "Arqueología",
    "Expresión Gráfica Arquitectónica",
    "Expresión Gráfica en la Ingeniería",
    "Historia del Arte",
    "Pintura",
    "Estudios Árabes e Islámicos",
    "Estudios de Asía Oriental",
    "Estudios Hebreos y Arameos",
    "Filología Alemana",
    "Filología Catalana",
    "Filología Eslava",
    "Filología Francesa",
    "Filología Gallega y Portuguesa",
    "Filología Griega",
    "Filología Inglesa",
    "Filología Italiana",
    "Filología Latina",
    "Filología Románica",
    "Filología Vasca",
    "Filosofía",
    "Filosofía Moral",
    "Lógica y Filosofía de la Ciencia",
    "Geografía Humana",
    "Ciencias y Técnicas Historiográficas",
    "Historia Antigua",
    "Historia Contemporánea",
    "Historia de América",
    "Historia de la Ciencia",
    "Historia del Pensamiento y de los Movimientos ...",
    "Historia Medieval",
    "Historia Moderna",
    "Prehistoria",
    "Lengua Española",
    "Lingüística General",
    "Lingüística Indoeuropea",
    "Literatura Española",
    "Música",
    "Paleontología",
    "Traducción e Interpretación"
  ),
  field = c(
    "Anthropology",
    "Archaeology",
    "Arts",
    "Arts",
    "Arts",
    "Arts",
    "Cultural Studies",
    "Cultural Studies",
    "Cultural Studies",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philology",
    "Philosophy",
    "Philosophy",
    "Philosophy",
    "Geography",
    "History",
    "History",
    "History",
    "History",
    "History",
    "History",
    "History",
    "History",
    "History",
    "Language & Linguistics",
    "Language & Linguistics",
    "Language & Linguistics",
    "Literature",
    "Music",
    "Paleontology",
    "Translation Studies"
  )
)

df$field <- disciplina_map$field[match(df$Disciplina, disciplina_map$Disciplina)]

df_doc <- read.delim('data/authors_publications_full_lang.tsv',
                     fileEncoding = 'UTF-8',
                     check.names = FALSE)

df_doc <- inner_join(df_doc, df[,c('CODIGO AUTOR DIALNET', 'field')], by='CODIGO AUTOR DIALNET')

table(df_doc$Type)

all(df$`CODIGO AUTOR DIALNET` %in% df_doc$`CODIGO AUTOR DIALNET`)

df_doc <- df_doc[which(df_doc$`CODIGO AUTOR DIALNET` %in% df$`CODIGO AUTOR DIALNET`),]


# Matrix plot 1
## 1. Authors count

df_aut_count <- df %>%
  group_by(field) %>%
  summarise(`N. scholars`=n()) %>%
  arrange(desc(`N. scholars`))

ggplot(df_aut_count, aes(y=reorder(field, `N. scholars`), x=`N. scholars`)) +
  geom_col(fill='#e06236') +
  labs(y='') +
  theme_classic() +
  theme(axis.title.x = element_text(color='black', size=18, face='bold'),
        axis.text.x = element_text(color='black', size=14),
        axis.text.y = element_text(color='black', size=13,  face='bold'),
        panel.grid.major.x = element_line(color='grey90', linewidth=0.9),
        panel.grid.minor.x = element_line(color='grey95', linewidth=0.5))

## 2. Publications count

df_pub_count <- df_doc %>%
  group_by(field) %>%
  summarise(`N. publications`=n()) %>%
  arrange(desc(`N. publications`)) %>%
  inner_join(df_aut_count, by='field')

ggplot(df_pub_count, aes(y=reorder(field, `N. scholars`), x=`N. publications`)) +
  geom_col(fill='#304c7a') +
  scale_x_continuous(labels = comma, limits = (x=c(0,310000)))+
  labs(y='') +
  theme_classic() +
  theme(axis.title.x = element_text(color='black', size=18, face='bold'),
        axis.text.x = element_text(color='black', size=14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color='grey90', linewidth=0.9),
        panel.grid.minor.x = element_line(color='grey95', linewidth=0.5))


## 3. Age boxplot
df_aut_age <- df %>%
  select(field, Age)

df_aut_age$field <- factor(df_aut_age$field, levels=rev(df_aut_count$field), ordered=TRUE)

ggplot(df_aut_age, aes(x=Age, y=field)) +
  stat_boxplot(geom = 'errorbar', width = 0.75) + 
  geom_boxplot(fill='#e06236') +
  labs(x='Academic age', y='')+
  theme_classic() +
  theme(axis.title.x = element_text(color='black', size=18, face='bold'),
        axis.text.x = element_text(color='black', size=14),
        axis.text.y = element_text(color='black', size=13,  face='bold'),
        panel.grid.major.x = element_line(color='grey90', linewidth=0.9),
        panel.grid.minor.x = element_line(color='grey95', linewidth=0.5))

df_aut_age_stat <- df_aut_age %>%
  group_by(field) %>%
  summarise(age_mean=mean(Age), age_median=median(Age))

## 4. Gen boxplot
df_aut_gen <- df %>%
  select(field, First_pub)

df_aut_gen$field <- factor(df_aut_gen$field, levels=rev(df_aut_count$field), ordered=TRUE)

ggplot(df_aut_gen, aes(x=First_pub, y=field)) +
  stat_boxplot(geom = 'errorbar', width = 0.75) + 
  geom_boxplot(fill='#e06236') +
  labs(x='First publication', y='')+
  theme_classic() +
  theme(axis.title.x = element_text(color='black', size=18, face='bold'),
        axis.text.x = element_text(color='black', size=14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color='grey90', linewidth=0.9),
        panel.grid.minor.x = element_line(color='grey95', linewidth=0.5))

df_aut_gen_stat <- df_aut_gen %>%
  group_by(field) %>%
  summarise(age_mean=mean(First_pub), age_median=median(First_pub))

## 3. First pub.
length(unique(df_first_count$`CODIGO AUTOR DIALNET`))

bin <- 2

ggplot(df, aes(x=First_pub)) +
  geom_histogram(binwidth=bin, color='white', fill='grey60') + 
  geom_density(aes(y = ..count..*bin), linewidth=1) +
  labs(x='First publication year', y='Scholars') +
  theme_classic() +
  theme(axis.title = element_text(color='black', size=14, face='bold'),
        axis.text.x = element_text(color='black', size=12),
        axis.text.y = element_text(color='black', size=12),
        panel.grid.major.y = element_line(color='grey90', linewidth=0.9),
        panel.grid.minor.y = element_line(color='grey95', linewidth=0.5))

## 4. Types

df_doc_count <- df_doc %>%
  inner_join(df[,c('CODIGO AUTOR DIALNET', 'Área')], by='CODIGO AUTOR DIALNET') %>%
  select(field, `CODIGO OBRA DIALNET`, Type) %>%
  distinct() %>%
  group_by(field, Type) %>%
  summarise(Publications=n()) %>%
  mutate(`Publications %`=100*Publications/sum(Publications)) %>%
  as.data.frame()

df_doc_count$Type[which(df_doc_count$Type == 'Proceeding paper')] <- 'Proceedings'

df_doc_count$Type <- factor(df_doc_count$Type,
                            levels=rev(unique(df_doc_count$Type)),
                            ordered=TRUE)

df_doc_count$field <- factor(df_doc_count$field, levels=rev(df_aut_count$field), ordered=TRUE)

ggplot(df_doc_count, aes(x=`Publications %`, y=field, fill=Type))+
  geom_bar(stat='identity') +
  labs(y='', fill='')+
  scale_fill_brewer(palette = 'Spectral',
                    breaks=unique(df_doc_count$Type)) +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.text = element_text(color='black', size=10),
        axis.title.x = element_text(color='black', size=18, face='bold'),
        axis.text.x = element_text(color='black', size=14),
        axis.text.y = element_text(color='black', size=13,  face='bold'))

df_doc$Lang[which(!(df_doc$Lang %in% c('gl', 'it',  'pt', 'fr', 'ca', 'en', 'es')))] <- 'Other'
df_doc$Lang[which(df_doc$Lang == 'gl')] <- 'Galician'
df_doc$Lang[which(df_doc$Lang == 'it')] <- 'Italian'
df_doc$Lang[which(df_doc$Lang == 'pt')] <- 'Portuguese'
df_doc$Lang[which(df_doc$Lang == 'fr')] <- 'French'
df_doc$Lang[which(df_doc$Lang == 'ca')] <- 'Catalan'
df_doc$Lang[which(df_doc$Lang == 'en')] <- 'English'
df_doc$Lang[which(df_doc$Lang == 'es')] <- 'Spanish'

df_lang_count <- df_doc %>%
  inner_join(df[,c('CODIGO AUTOR DIALNET', 'Área')], by='CODIGO AUTOR DIALNET') %>%
  select(field, `CODIGO OBRA DIALNET`, Lang) %>%
  distinct() %>%
  group_by(field, Lang) %>%
  summarise(Publications=n()) %>%
  mutate(`Publications %`=100*Publications/sum(Publications)) %>%
  as.data.frame()

df_lang_count$Lang <- factor(df_lang_count$Lang,
                            levels=rev(unique(df_lang_count$Lang)),
                            ordered=TRUE)

df_lang_count$field <- factor(df_lang_count$field, levels=rev(df_aut_count$field), ordered=TRUE)

ggplot(df_lang_count, aes(x=`Publications %`, y=field, fill=Lang))+
  geom_bar(stat='identity') +
  labs(y='', fill='')+
  scale_fill_manual(values = rev(pal_npg('nrc')(9)),
                    breaks=unique(df_lang_count$Lang)) +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.text = element_text(color='black', size=11),
        axis.title.x = element_text(color='black', size=18, face='bold'),
        axis.text.x = element_text(color='black', size=14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Profiles

df <- data.frame()
for(i in c('Anthropology', 'Archaeology', 'Arts', 'Cultural Studies', 'Geography', 'History',
           'Language & Linguistics', 'Literature', 'Music', 'Paleontology', 'Philology', 'Philosophy', 'Translation Studies')){
  df_aux <- read.csv2(paste0('data/profiles/',i,'.csv'))
  df_aux$Área <- i
  df <- rbind.data.frame(df, df_aux)
}
df$Label[which(df$Label=='Bicultural Scholar')] <- 'The Bridger'
df$Label[which(df$Label=='Global scholar')] <- 'The Cosmopolitan'
df$Label[which(df$Label=='Traditionalist')] <- 'The Local Chronicler'
df$Label[which(df$Label=='Monograph specialist')] <- 'The Sage'
df$Label[which(df$Label=='Omni-publication scholar')] <- 'The Polymath'
df$Label[which(df$Label=='Versatile chapter author')] <- 'The Collaborator'


df_plot <- df %>%
  group_by(Área, Label) %>%
  summarise(Total_pubs = mean(Total_pubs))

df_plot$Label <- factor(df_plot$Label, levels = sort(unique(df_plot$Label), decreasing = FALSE), ordered = TRUE)

ggplot(df_plot, aes(x=Total_pubs, y=Label, fill=Label), color='white') +
  geom_col()+
  scale_fill_manual(values=c('The Bridger'='#e64b35', 'The Cosmopolitan'='#00a087', 'The Local Chronicler'='#3c5488', 'The Sage'='#7e6148', 'The Polymath'='#4DBBD5', 'The Collaborator'='#F39B7F'))+
  scale_color_manual(values=c('The Bridger'='#e64b35', 'The Cosmopolitan'='#00a087', 'The Local Chronicler'='#3c5488', 'The Sage'='#7e6148', 'The Polymath'='#4DBBD5', 'The Collaborator'='#F39B7F'))+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))+
  labs(x='Scholarly outputs', y='')+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme_light() +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_line(colour = 'grey80'),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1), 
        strip.background = element_rect(color = 'black', fill='black'),
        strip.text = element_text(size=10, face='bold'),
        axis.title = element_text(color='black', size=14, face='bold'),
        axis.text.x = element_text(color='black', size=11),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap(.~Área, scales = 'free_y', ncol = 3)
