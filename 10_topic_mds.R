library(data.table)
library(dplyr)
library(vegan)
library(proxy)
library(ggplot2)
library(ggrepel)
library(tidyr)

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


df_docs_authors <- fread('data/authors_publications.tsv', data.table = FALSE)
df_docs <- fread('data/titles.tsv', data.table = FALSE)
df_terms <- read.delim('data/terms_2_all.txt', check.names = FALSE)
df_doc_terms <- read.delim('data/doc_terms_2_all.txt', check.names = FALSE)
df_topics <- read.delim('data/vertex.txt')
df_topics <- inner_join(df_topics, df_terms[,c('id', 'term')], by=c('name'='term'))

df_docs$`document id` <- 2:(dim(df_docs)[1]+1)

df_doc_terms_ids <- df_doc_terms %>%
  inner_join(df_docs[,c('CODIGO OBRA DIALNET', 'document id')], by='document id') %>%
  inner_join(df_docs_authors, by='CODIGO OBRA DIALNET') %>%
  inner_join(df_topics[,c('cluster', 'id', 'name')], by=c('term id'='id'))

areas <- c('Anthropology', 'Archaeology', 'Arts', 'Cultural Studies', 'Geography', 'History',
           'Language & Linguistics', 'Literature', 'Music', 'Paleontology', 'Philology', 'Philosophy', 'Translation Studies')

all_profiles <- list()

for(area in areas){
  
  df_archetype <- read.csv2(paste0('profiles/',area,'.csv'))
  
  df_archetype$Label[which(df_archetype$Label=='Bicultural Scholar')] <- 'The Bridger'
  df_archetype$Label[which(df_archetype$Label=='Global scholar')] <- 'The Cosmopolitan'
  df_archetype$Label[which(df_archetype$Label=='Traditionalist')] <- 'The Local Chronicler'
  df_archetype$Label[which(df_archetype$Label=='Monograph specialist')] <- 'The Sage'
  df_archetype$Label[which(df_archetype$Label=='Omni-publication scholar')] <- 'The Polymath'
  df_archetype$Label[which(df_archetype$Label=='Versatile chapter author')] <- 'The Participant'
  
  for(i in unique(df_archetype$archetype)){
    temp <- df_doc_terms_ids %>%
      filter(`CODIGO AUTOR DIALNET` %in% df_archetype$dialnet_id[df_archetype$archetype==i]) %>%
      select(`CODIGO AUTOR DIALNET`, `document id`, cluster) %>%
      distinct()
    
    profile_data <- as.data.frame(table(temp$cluster), stringsAsFactors = FALSE)
    profile_data$profile <- unique(df_archetype$Label[df_archetype$archetype == i])
    profile_data$area <- area
    
    all_profiles[[length(all_profiles)+1]] <- profile_data
  }
}

df_all_profiles <- bind_rows(all_profiles) %>%
  pivot_wider(names_from = Var1, values_from = Freq, values_fill = 0)

df_all_profiles[ , 3:ncol(df_all_profiles)] <- t(apply(df_all_profiles[ , 3:ncol(df_all_profiles)], 1, function(x) x / sum(x) * 100))

mds_input <- df_all_profiles %>% select(-profile, -area)
similitude <- vegdist(mds_input, method = 'jaccard')
mds_coords <- cmdscale(similitude, k = 2)


mds_df <- df_all_profiles %>%
  select(profile, area) %>%
  bind_cols(as.data.frame(mds_coords)) %>%
  rename(Dim1 = V1, Dim2 = V2)

df_sizes <- df_all_profiles %>%
  group_by(profile, area) %>%
  summarise(size = n(), .groups = 'drop')

mds_df <- inner_join(mds_df, df_sizes, by = c('profile', 'area'))

mds_df <- mds_df %>%
  mutate(across(starts_with("Dim"), ~(. - min(.)) / (max(.) - min(.)) * 2 - 1))

ggplot(mds_df, aes(x = Dim1, y = Dim2, color = profile)) +
  geom_point(alpha = 0.6, size = 6) +
  geom_text_repel(aes(label = area),
                  color = 'black',
                  point.padding = 5,
                  size = 4) +
  #facet_wrap(~ area, ncol = 3) +
  #scale_x_continuous(limits = c(-1.25, 1.25))+
  #scale_y_continuous(limits = c(-1.25, 1.25))+
  scale_radius(range = c(3, 10)) +
  scale_color_manual(values = c(
    'The Bridger' = '#e64b35',
    'The Cosmopolitan' = '#00a087',
    'The Local Chronicler' = '#3c5488',
    'The Sage' = '#7e6148',
    'The Polymath' = '#4DBBD5',
    'The Participant' = '#F39B7F'
  )) +
  theme_light(base_size = 13) +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_rect(color = 'black'),
        axis.ticks = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        strip.background = element_rect(color = 'black', fill='black'),
        strip.text = element_text(size=9, face='bold'),
        axis.title = element_text(color='black', size=14, face='bold'),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

mds_df_a <- mds_df
mds_df_a$area[which(mds_df_a$area=='Language & Linguistics')] <- 'Lang. & Linguistics'
                                                       
ggplot(mds_df_a, aes(x = Dim1, y = Dim2, color = profile)) +
  geom_point(alpha = 0.6, size = 6) +
  #geom_text_repel(aes(label = area),
  #                color = 'black',
  #                point.padding = 5,
  #                size = 4) +
  facet_wrap(~ area, ncol = 3) +
  scale_x_continuous(limits = c(-1.25, 1.25))+
  scale_y_continuous(limits = c(-1.25, 1.25))+
  scale_radius(range = c(3, 10)) +
  scale_color_manual(values = c(
    'The Bridger' = '#e64b35',
    'The Cosmopolitan' = '#00a087',
    'The Local Chronicler' = '#3c5488',
    'The Sage' = '#7e6148',
    'The Polymath' = '#4DBBD5',
    'The Participant' = '#F39B7F'
  )) +
  labs(x='', y='')+
  guides(color=guide_legend(ncol=2))+
  theme_light(base_size = 13) +
  theme(legend.position = 'bottom',
        panel.grid = element_blank(),
        panel.border = element_rect(color = 'black'),
        axis.ticks = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        strip.background = element_rect(color = 'black', fill='black'),
        strip.text = element_text(size=9, face='bold'),
        axis.title = element_text(color='black', size=14, face='bold'),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

mds_df_background <- data.frame()
for (i in unique(mds_df_a$area)) {
  mds_aux <- mds_df[which(mds_df$area!=i),]
  mds_aux$area <- i
  mds_df_background <- rbind.data.frame(mds_df_background,
                                        mds_aux)
}

ggplot() +
  geom_point(data = mds_df_background,
             aes(x = Dim1, y = Dim2),
             color = "grey90", size = 3, alpha = 0.5) +
    geom_point(data = mds_df_a,
             aes(x = Dim1, y = Dim2, color = profile),
             size = 3, alpha = 0.8) +
  facet_wrap(~ area, ncol = 3) +
  scale_x_continuous(limits = c(-1.25, 1.25)) +
  scale_y_continuous(limits = c(-1.25, 1.25)) +
  scale_color_manual(values = c(
    'The Bridger' = '#e64b35',
    'The Cosmopolitan' = '#00a087',
    'The Local Chronicler' = '#3c5488',
    'The Sage' = '#7e6148',
    'The Polymath' = '#4DBBD5',
    'The Participant' = '#F39B7F'
  )) +
  labs(x = '', y = '') +
  guides(color = guide_legend(ncol = 2)) +
  theme_light(base_size = 13) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    panel.border = element_rect(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    axis.text = element_text(color = 'black'),
    strip.background = element_rect(color = 'black', fill = 'black'),
    strip.text = element_text(size = 9, face = 'bold'),
    axis.title = element_text(color = 'black', size = 14, face = 'bold'),
    axis.line = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )
