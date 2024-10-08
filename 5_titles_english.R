
df <- read.delim("data/titles_eng.tsv", header=TRUE, encoding = 'UTF-8', check.names = FALSE)

for (x in names(df)[4:29]) {
  print(paste(x, length(which(!is.na(df[,x]) & df[,x]!=''))))
  df[which(!is.na(df[,x]) & df[,x]!=''),'TITULO'] <- df[which(!is.na(df[,x]) & df[,x]!=''),x]
}

df <- df[which(!is.na(df$TITULO)),]
df <- df[which(df$TITULO!=''),]
df <- df[which(!is.na(df$`CODIGO OBRA DIALNET`)),]
df <- df[which(df$`CODIGO OBRA DIALNET`!=''),]

df_all <- read.delim("data/authors_publications_full_lang.tsv", header=TRUE, encoding = 'UTF-8', check.names = FALSE)
df_all <- df_all[which(df_all$DB=='DIALNET'),]

df <- df[which(df$`CODIGO OBRA DIALNET` %in% df_all$`CODIGO OBRA DIALNET`),]

df <-rbind.data.frame(df[,c('CODIGO OBRA DIALNET', 'TITULO')], unique(df_all[which(df_all$Lang=='en'),c('CODIGO OBRA DIALNET', 'TITULO')]),
                      stringsAsFactors = FALSE)

df_exp <- unique(df[,c('CODIGO OBRA DIALNET', 'TITULO')])
write.table(df_exp, "data/titles.tsv", sep='\t', quote = FALSE, fileEncoding = 'UTF-8', row.names = FALSE)