library(tidyverse)
library(RColorBrewer)

load('source_data_fig1.RData')


info_stat <- df %>%
  group_by(gene_id) %>%
  summarise(num = n())


info_stat$grp <- '1'
info_stat$grp[info_stat$num == 2] <- '2'
info_stat$grp[info_stat$num == 3] <- '3'
info_stat$grp[info_stat$num == 4] <- '4'
info_stat$grp[info_stat$num == 5] <- '5'
info_stat$grp[info_stat$num >= 6 & info_stat$num <=10] <- '6-10'
info_stat$grp[info_stat$num >= 11 & info_stat$num <=50] <- '11-50'
info_stat$grp[info_stat$num >= 51 & info_stat$num <=100] <- '51-100'
info_stat$grp[info_stat$num >= 101 & info_stat$num <=500] <- '101-500'
info_stat$grp[info_stat$num >= 501] <- '> 500'


adv <- info_stat %>%
  group_by(grp) %>%
  summarise(num_genes = n())

adv$grp <- factor(adv$grp, levels = c('1','2','3','4','5','6-10','11-50','51-100','101-500','> 500'))

ggplot(adv, aes(x=grp, y=num_genes,fill=grp)) +
  geom_bar(stat = 'identity', width = 0.7) +
  xlab('Number of eQTLs') + ylab('Number of genes') +
  theme_classic() +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text.y = element_text(size = 11, colour = 'black')) +
  theme(axis.text.x = element_text(size = 11, colour = 'black', angle = 315, hjust = 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1800)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(n=9,name = 'Blues')[3:8])(10)) +
  theme(legend.position = 'none') 



gtex_share <- read.csv('gtex_shared.csv')
options(scipen = 999)

ggplot(df_tidy, aes(x=formal_name, y=num_overlap, color = metric)) +
  geom_point() +
  geom_line(group =1) +
  facet_wrap(~metric,scales="free_y", ncol = 1, strip.position = 'top') +
  theme_bw() +
  theme(axis.title = element_text(size = 12, color = 'black')) +
  theme(axis.text.x = element_text(size = 10, colour = 'black', angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 10, colour = 'black')) +
  theme(strip.text = element_text(size = 10, colour = 'black')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect(color = "black", size = 0.9),
        strip.background = element_rect(color = "black", size = 0.9)) +
  xlab(NULL) + ylab(NULL) +
  labs(title='Number of shared eQTLs with GTEx tissues') +
  theme(plot.title = element_text(hjust = 0.5, size = 12, colour = 'black')) +
  guides(fill=F, color = F) +
  scale_color_manual(values = pal[c(1,2)]) 







