library(pheatmap)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(geomtextpath)

df <- read.delim2("data/archetypes_summary.csv")
rownames(df) <- paste(df$Area, df$Archetype)

scaled_data <- scale(df[,c(4:11)]) 
rownames(scaled_data) <- paste(scaled_data$Area, scaled_data$Archetype)

# Cluster the rows 
clustered_data <- pheatmap(df[,c(4:11)],border_color = 'white',
                           color=hcl.colors(10, palette = "Zissou 1"),
                           #color=rev(hcl.colors(10, palette = "Heat 2")),
                           cutree_rows = 6, display_numbers = round(df[,c(4:11)],2), number_color = 'grey95',
                           cluster_cols = FALSE, legend.position = 'bottom')+
  theme(legend.position = 'bottom')

clustered_data <- pheatmap(scaled_data,
                           cutree_rows = 6, display_numbers = round(scaled_data,2), number_color = 'black',
                           main = "Heatmap")


df_radar <- read.csv2("data/archetypes.csv",
                      check.names = FALSE)

df_radar_plot <- df_radar[which(df_radar$Grouping==6),c(4:11)] %>%
  summarise_all(mean) %>%
  pivot_longer(cols = names(df_radar[1,c(4:11)]), names_to = c("name"))

ggplot(df_radar_plot ,
       aes(x=name, y=value))+
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0,0.25,0.5,0.75,1)), color = 'lightgrey') + 
  geom_segment(aes(x = name, y = 0,
                   xend = name, yend = 1),
               linetype = 'dashed', linewidth = 0.4, color = 'gray90') + 
  geom_col(data=df_radar_plot ,
           aes(x=name, y=value), alpha=0.7, fill='#F39B7F', position = 'identity') +
  annotate(x = 0, y = 0.29, label = '25%', geom = 'text', color = 'gray12', size = 4) +
  annotate(x = 0, y = 0.54, label = '50%', geom = 'text', color = 'gray12', size = 4) +
  annotate(x = 0, y = 0.79, label = '75%', geom = 'text', color = 'gray12', size = 4) +
  geom_col(alpha=0.05, position = 'identity') +
  labs(x='', y='', color='Archetype', fill='Archetype') +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) + 
  coord_polar(clip = 'off') +
  coord_curvedpolar() +
  theme_light() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12, face='bold'),
    legend.position = "none"
  )


df_numeric <- df[, 4:11]
dist_matrix <- dist(df_numeric, method = "euclidean")

mds_result <- cmdscale(dist_matrix, k = 2)  # k=2 para 2 dimensiones

df$MDS1 <- mds_result[,1]
df$MDS2 <- mds_result[,2]

df$Archetype <- as.factor(df$Archetype)

ggplot(df, aes(x = MDS1, y = MDS2)) +
  geom_mark_ellipse(aes(group = Label, fill = Label), expand = 0.01, alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_point(aes(color = Label, shape = Archetype), size = 3) +
  geom_text_repel(aes(label = Area), size = 3) +
  scale_fill_manual(values=c('The Bridger'='#e64b35', 'The Cosmopolitan'='#00a087', 'The Local Chronicler'='#3c5488', 'The Sage'='#7e6148', 'The Polymath'='#4DBBD5', 'The Participant'='#F39B7F'))+
  scale_color_manual(values=c('The Bridger'='#e64b35', 'The Cosmopolitan'='#00a087', 'The Local Chronicler'='#3c5488', 'The Sage'='#7e6148', 'The Polymath'='#4DBBD5', 'The Participant'='#F39B7F'))+
  theme_light() +
  theme(legend.position = 'bottom',
        panel.grid = element_blank(),
        panel.border = element_rect(color = 'black'),
        axis.ticks = element_line(color = 'black'),
        axis.text = element_text(color = 'black')
        )+
  labs(color = "Archetype")
