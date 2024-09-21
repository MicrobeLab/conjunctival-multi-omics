library(ggplot2)
library(RColorBrewer)


pal <- brewer.pal(n=11, 'Spectral')
pal <- brewer.pal(n=9, 'Blues')

# A
load('source_data_fig2-1.RData')

fc_avg <- data.frame(Description = fc$Description, avg_fc = rowMeans(fc[,-c(1:2)]))
fc_med <- data.frame(Description = fc$Description, med_fc = apply(fc[,-c(1:2)], 1, median))

fc_avg$grp <- 'mid'
fc_avg$grp[fc_avg$avg_fc > 1.5] <- '> 1.5'
fc_avg$grp[fc_avg$avg_fc < 1] <- '< 1'
fc_avg$grp[fc_avg$avg_fc > 2] <- '> 2'

ggplot(fc_avg, aes(x=avg_fc, fill=grp)) +
  geom_histogram(binwidth = 0.25, boundary = 0, color = 'black', size = 0.4) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,6000)) +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text = element_text(size = 12, colour = 'black')) +
  xlab('log2(FC)') +
  ylab('Number of genes') +
  guides(fill = F) +
  scale_fill_manual(values = pal[c(7,5,3,1)]) +
  labs(title='Distribution of mean within-twins FC') +
  theme(plot.title = element_text(hjust = 0.5, size = 12, colour = 'black'))

# B
fc_plot <- read.csv('source_data_fig2-2.csv')
ggplot(fc_plot, aes(x=Description, y=avg_fc, ymin=avg_fc-sd_fc, ymax=avg_fc+sd_fc, color=grp)) +
  geom_point(size = 1.2) +
  geom_linerange() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = 'black', angle = 90, hjust = 1, vjust = 0.5, face = 'italic')) +
  theme(axis.text.y = element_text(size = 11, colour = 'black')) +
  theme(axis.title = element_text(size = 12, colour = 'black')) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = 'black')) +
  xlab(NULL) + ylab('log2(FC)') +
  scale_y_continuous(limits = c(2.5, 4.5)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.border = element_rect(color = 'black', size = 0.9)) +
  scale_color_manual(values = brewer.pal(12, 'Set3')[c(5,6,7,4,9)]) +
  labs(title='Top 50 genes with the highest differences in monozygotic twin pairs') +
  theme(plot.title = element_text(hjust = 0.5, size = 12, colour = 'black')) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(legend.position = 'bottom')


# C-D
get_spearman <- function(gene_vec, bray_vec){
  slist <- cor.test(gene_vec, bray_vec, method = "spearman")
  pv <- slist$p.value
  rho <- slist$estimate
  return(c(rho, pv))
}


bray <- read.delim('source_data_fig2-3.txt')
fc_diff <- fc[fc_avg$avg_fc > 2 & fc_med$med_fc > 2,]
res_spearman <- as.data.frame(t(apply(fc_diff[,3:ncol(fc_diff)], 1, get_spearman, bray_vec = bray$bray_dist)))
colnames(res_spearman) <- c('rho', 'pval')
res_spearman <- cbind(fc_diff[,1:2], res_spearman)
res_spearman$fdr <- p.adjust(res_spearman$pval, method = 'fdr')


# E
rho <- read.csv('source_data_fig2-4-rho.csv', row.names = 1)
sig <- read.csv('source_data_fig2-4-sig.csv', row.names = 1)

library(pheatmap)
bk <- seq(-0.8,0.8,by=0.01)
my_palette <- colorRampPalette(colors = rev(brewer.pal(11, 'RdBu'))[2:10])(n = length(bk)-1)

pheatmap(rho, color = my_palette, breaks = bk, legend_breaks = seq(-0.8,0.8,by=0.2), border_color = "gray90")


# F
load('source_data_fig2-5.RData')
df_plot <- df[,c("Description", "CHR", "BP", "lmer_pvalue")]
df_plot$CHR <- as.integer(gsub('chr', '', df_plot$CHR, fixed = T))
df_plot <- df_plot[order(df_plot$lmer_pvalue),]

color_vec <- rep(c("#4197d8", "#f8c120", "#413496", "#495226",
                   "#d60b6f", "#e66519", "#d581b7", "#83d3ad", "#7c162c", "#26755d"),3)
library(CMplot)
CMplot(df_plot, plot.type = 'm', 
       highlight.col = color_vec[c(1,3,4,5,7,8,9,11,12,14,19,22)], width = 12, height = 4,
       highlight.text.font=1, main.font = 1, cex=0.5,file='pdf')




