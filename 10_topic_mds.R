library(data.table)
library(dplyr)
library(vegan)
library(proxy)
library(ggplot2)

df_docs_authors <- fread('data/authors_publications.tsv', data.table = FALSE)

df_terms <- read.delim('data/terms_all.tsv', check.names = FALSE)
df_doc_terms <- read.delim('data/doc_terms_all.tsv', check.names = FALSE)

df_doc_terms_ids <- inner_join(df_doc_terms, df_docs_authors, by=c('document id'='CODIGO OBRA DIALNET'))

df_topics <- read.delim('data/vertex.txt')

df_topics <- inner_join(df_topics, df_terms[,c('id', 'term')], by=c('name'='term'))

df_doc_terms_ids <- inner_join(df_doc_terms_ids, df_topics[,c('cluster', 'id', 'name')], by=c('term id'='id'))

mds_total <- data.frame()

for(area in c('Anthropology', 'Archaeology', 'Arts', 'Cultural Studies', 'Geography', 'History',
           'Language & Linguistics', 'Literature', 'Music', 'Paleontology', 'Philology', 'Philosophy', 'Translation Studies')){
  
  df_archetype <- read.csv2(paste0('profiles/',area,'.csv'))
  
  df_archetype$Label[which(df_archetype$Label=='Bicultural Scholar')] <- 'The Bridger'
  df_archetype$Label[which(df_archetype$Label=='Global scholar')] <- 'The Cosmopolitan'
  df_archetype$Label[which(df_archetype$Label=='Traditionalist')] <- 'The Local Chronicler'
  df_archetype$Label[which(df_archetype$Label=='Monograph specialist')] <- 'The Sage'
  df_archetype$Label[which(df_archetype$Label=='Omni-publication scholar')] <- 'The Polymath'
  df_archetype$Label[which(df_archetype$Label=='Versatile chapter author')] <- 'The Collaborator'
  
  df_arcs <- data.frame(matrix(NA, nrow = 1, ncol = length(unique(df_topics$cluster))))
  names(df_arcs) <- unique(df_topics$cluster)
  
  for(i in unique(df_archetype$archetype)){
    df_arc_aux <- as.data.frame(table(unique(df_doc_terms_ids[which(df_doc_terms_ids$`CODIGO AUTOR DIALNET` %in% df_archetype$dialnet_id[which(df_archetype$archetype==i)]), c('CODIGO AUTOR DIALNET', 'document id', 'cluster')])$cluster), stringsAsFactors = FALSE)
    df_arc_aux <- as.data.frame(t(df_arc_aux))
    names(df_arc_aux) <- df_arc_aux[1,]
    df_arc_aux <- df_arc_aux[2,]
    row.names(df_arc_aux) <- unique(df_archetype[which(df_archetype$archetype==i), 'Label'])
    
    df_arcs <- bind_rows(df_arcs, df_arc_aux)
  }
  
  df_arcs <- df_arcs[2:dim(df_arcs)[1],]
  
  df_arcs <- df_arcs %>%
    mutate_all(as.integer) %>%
    mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
  
  
  similitude <- vegdist(df_arcs[,2:dim(df_arcs)[2]], method = "jaccard")
  
  write.csv2(as.matrix(similitude), paste0('data/jaccard/',area,'.csv'))
  
  mds <- cmdscale(similitude, k = 1)
  
  mds <- as.data.frame(mds)
  mds$profile <- rownames(mds)
  
  mds_size <- as.data.frame(table(df_archetype$Label)/dim(df_archetype)[1], stringsAsFactors = FALSE)
  names(mds_size) <- c('profile', 'size')
  
  mds <- inner_join(mds, mds_size)
  
  mds$area <- area
  
  mds_total <- rbind.data.frame(mds_total, mds)
}

mds_total$V1 <- (mds_total$V1 - min(mds_total$V1)) / (max(mds_total$V1) - min(mds_total$V1))
mds_total$V1 <- mds_total$V1 * 2 - 1

ggplot(mds_total, aes(y = area, x = V1, size = size, color=profile)) +
  geom_point(alpha=0.4) +
  theme_classic() +
  scale_radius(range=c(4,12), labels = scales::percent_format(accuracy=1))+
  labs(x='Dimension 1 (Thematic similarity)', y='Field', size='Size', color='Profile') +
  scale_color_manual(values=c('The Bridger'='#e64b35', 'The Cosmopolitan'='#00a087', 'The Local Chronicler'='#3c5488', 'The Sage'='#7e6148', 'The Polymath'='#4DBBD5', 'The Collaborator'='#F39B7F'))+
  guides(color=guide_legend(override.aes = list(size=6)))+
  theme(legend.position = 'bottom',
        legend.box = 'vertical',
        axis.title = element_text(size=14, face = 'bold'),
        axis.text.x = element_text(size=12, color = 'black'),
        axis.text.y = element_text(size=13, color = 'black'),
        panel.grid.major.y = element_line())


