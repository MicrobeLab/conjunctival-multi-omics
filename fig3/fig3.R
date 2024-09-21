library(tidyverse)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(ggrepel)

load('source_data_fig3-1.RData')

ggplot(df_r2) +
  geom_histogram(aes(x=improve_r2, y= ..density..), color='black', fill='white', binwidth = 0.01, boundary=0) +
  theme_classic() +
  xlab('R2 improvement with interactions') + ylab('Density') +
  scale_x_continuous(breaks = c(0,0.1,0.2)) +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text.x = element_text(size = 12, color = 'black')) +
  theme(axis.text.y = element_text(size = 12, color = 'black')) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) 

ggplot(df_r2) +
  geom_point(aes(x=r2_non_interact, y=r2_interact), size = 1, color='gray', alpha=1) +
  geom_abline(slope = 1, intercept = 0, color = 'brown3') +
  theme_classic() +
  xlab('R2 of models without interactions') + ylab('R2 of models with interactions') +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text = element_text(size = 12, colour = 'black')) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8)) + 
  scale_x_continuous(expand = c(0,0), limits = c(0,0.8)) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) 


load('source_data_fig3-2.RData')

ggplot(df_overlap, aes(x=go_name, y=-log10(go_pval))) +
  geom_point(aes(size = num, color=perc)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'right') +
  xlab(NULL) + ylab('-log10(P-value)') +
  theme(axis.text = element_text(size = 12, colour = 'black')) +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(legend.text = element_text(size = 10, color = 'black')) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(3,5)) +
  scale_color_gradientn(colors=c("gray", "red2", "red3"))


load('source_data_fig3-3.RData')

ggplot(antxr1_inter, aes(x=predicted_expr, y=true_expr)) +
  geom_point(aes(color=Fold), size=0.8) +
  geom_smooth(method = 'lm', color='gray60') +
  scale_color_manual(values = brewer.pal(n=9, name = 'Oranges')[3:9]) +
  xlab('Predicted expression') + ylab('Observed expression') +
  theme_classic() +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text = element_text(size = 12, colour = 'black')) +
  theme(legend.title = element_text(size = 12, color = 'black')) +
  theme(legend.text = element_text(size = 12, colour = 'black')) +
  theme(legend.position = 'none') +
  labs(title='ANTXR1') +
  theme(plot.title = element_text(hjust = 0.5, size = 12, colour = 'black', face = 'italic')) +
  scale_y_continuous(limits = c(-1.5, 4.5), breaks = c(-1,0,1,2,3,4)) +
  scale_x_continuous(limits = c(-0.8,1))


load('source_data_fig3-4.RData')

ggplot(antxr1_non, aes(x=predicted_expr, y=true_expr)) +
  geom_point(aes(color=Fold), size=0.8) +
  geom_smooth(method = 'lm', color='gray60') +
  scale_color_manual(values = brewer.pal(n=9, name = 'Greens')[3:9]) +
  xlab('Predicted expression') + ylab('Observed expression') +
  theme_classic() +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text = element_text(size = 12, colour = 'black')) +
  theme(legend.title = element_text(size = 12, color = 'black')) +
  theme(legend.text = element_text(size = 12, colour = 'black')) +
  theme(legend.position = 'none') +
  labs(title='ANTXR1') +
  theme(plot.title = element_text(hjust = 0.5, size = 12, colour = 'black', face = 'italic')) +
  scale_y_continuous(limits = c(-1.5, 4.5), breaks = c(-1,0,1,2,3,4)) +
  scale_x_continuous(limits = c(-0.8,1))


load('source_data_fig3-5.RData')

pal <- brewer.pal(n=12, 'Paired')
node_colors <- c("Microbial species" = pal[3], "Cell adhesion molecules" = pal[8])

ggraph(g, layout = 'fr') +
  geom_edge_link(alpha = 0.4) +
  geom_node_point(aes(color = node_types), size = 5) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 size = 2.5,
                 fontface = "italic",
                 family = "sans",
                 color = 'black') +
  scale_color_manual(values = node_colors) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 


load('source_data_fig3-6.RData')

taxa_num <- df %>%
  group_by(taxa_feature) %>%
  summarise(num_inter_gene = n())

taxa_num <- taxa_num[order(taxa_num$num_inter_gene, decreasing = T),]
taxa_num$taxa_feature <- gsub('-',' ',taxa_num$taxa_feature, fixed = T)
taxa_num$taxa_feature <- factor(taxa_num$taxa_feature, levels = taxa_num$taxa_feature)

ggplot(taxa_num, aes(x=taxa_feature, y=num_inter_gene)) +
  geom_segment(aes(x = taxa_feature, xend = taxa_feature, y = 0, yend = num_inter_gene), size=1) +
  geom_point(size=3.5) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12, colour = 'black', angle = 45, hjust = 1)) +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text.y = element_text(size = 12, colour = 'black')) +
  xlab(NULL) + ylab('Number of genes') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 70)) +
  theme(axis.ticks.x = element_blank()) +
  theme(plot.margin = unit(c(1, 1, 1, 5), "lines")) +
  theme(legend.text = element_text(size = 12, color = 'black', face = 'italic')) +
  theme(legend.title = element_blank()) +
  theme(legend.position = 'right') 



