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
