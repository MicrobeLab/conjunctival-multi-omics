library(tidyverse)
library(RColorBrewer)
library(ggridges)


df_ridge <- read.csv('source_data_fig4-1.csv')
ggplot(df_ridge, aes(x=predicted_expr, y=gene_name, fill=grp)) +
  geom_density_ridges(alpha=0.9) +
  ylab(NULL) + xlab('Predicted expression') +
  theme_classic() +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text.y = element_text(size = 12, colour = 'black', face = 'italic')) +
  theme(axis.text.x = element_text(size = 12, colour = 'black')) +
  theme(legend.text = element_text(size = 12, color = 'black'),
        legend.title = element_blank()) +
  scale_fill_manual(values = brewer.pal(n=8, 'Paired')[c(1,5)]) +
  theme(legend.position = 'top')



df_inter <- read.csv('source_data_fig4-2.csv')
taxa <- unique(df_inter$taxa_feature)
snp <- unique(df_inter$variant_id)
total_mtx <- matrix(nrow = length(taxa), ncol = length(snp))
rownames(total_mtx) <- taxa
colnames(total_mtx) <- snp

for(i in 1:nrow(df_inter)){
  total_mtx[df_inter[i,'taxa_feature'], df_inter[i,'variant_id']] <- df_inter[i,'coefficient_EN']
}

snp2gene <- unique(df_inter[,c('variant_id', 'gene_name')])
snp2gene <- data.frame(row.names = snp2gene$variant_id, gene = snp2gene$gene_name)

ann_colors <- list(gene = c(ACP2 = pal_ann[1], CD276 = pal_ann[2], CFLAR = pal_ann[3],
                            CXCR2 = pal_ann[4], LAMC2 = pal_ann[5], 
                            PGM2L1 = pal_ann[6], PTTG1IP = pal_ann[7], SCIN = pal_ann[8],
                            TAMM41 = pal_ann[10], TMEM18 = pal_ann[11], TRIOBP = pal_ann[12]))

total_mtx <- abs(total_mtx)
total_mtx[is.na(total_mtx)] <- -0.01

bk <- seq(-0.01,0.04,by=0.001)

my_palette <- colorRampPalette(c('white', 'blue3'))(n = length(bk)-1)


pheatmap(total_mtx, cluster_rows = F, cluster_cols = F,
         annotation_col = snp2gene, annotation_colors = ann_colors,
         colorNA = 'white', angle_col = '90', fontsize = 10,
         border_color = 'gray90', color = my_palette, breaks = bk, scale = "none",
         legend_breaks = c(0,0.01,0.02,0.03,0.04), annotation_legend = T)




