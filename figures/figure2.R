
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(patchwork)

setwd('~/Documents/NeuroDev/trio_pilot/figures')

# Read data ------
nd_regions = read.delim("~/Documents/NeuroDev/wave1/sample_info/geno/full_w1_seqinfo_curated.tsv")
hgdp_tgp_meta = read.delim("~/Documents/NeuroDev/wave1/data/geno/hgtp_tgp_pops.tsv")
hgdp_tgp = read.delim("/Users/heesu/Documents/NeuroDev/trio_pilot/data/pca/hgdp_tgp_project_pca_scores.txt", sep ="\t")
neurodev = read.delim("/Users/heesu/Documents/NeuroDev/trio_pilot/data/pca/neurodev_trio_pilot_pca_project_scores.tsv", sep = '\t')
normal_pca = read.delim("/Users/heesu/Documents/NeuroDev/trio_pilot/data/pca/neurodev_trio_pilot_pca_scores_normal.tsv", sep='\t')
langs = read.delim('~/Documents/NeuroDev/trio_pilot/data/trio_lang_history.csv', sep = ",")


# Pre-plotting processing ------

#merge the two files (projection pca)
joined_file <- rbind(hgdp_tgp, neurodev) #"neurodev" includes all individuals from families

#combine regions and hgdp_tgp meta
nd_regions <- nd_regions[,c("sample_id", "region")]
colnames(nd_regions)[1] <- "s"

hgdp_tgp_meta = hgdp_tgp_meta[,1:2]
colnames(hgdp_tgp_meta) <- c("s", "region")

regions = rbind(hgdp_tgp_meta, nd_regions)

#join to PCA data
joined_pca = left_join(joined_file, regions)

#rename regions in ND
joined_pca$region[joined_pca$region == "Kenya"] <- "ND-Kenya"
joined_pca$region[joined_pca$region == "South Africa"] <- "ND-South Africa"

#add project names
joined_pca$project <- NA
joined_pca$project[joined_pca$s %in% nd_regions$s] <- "NeuroDev"
joined_pca$project[joined_pca$s %in% hgdp_tgp_meta$s] <- "HGDP-TGP"

#refactor
joined_pca$region <- factor(joined_pca$region, levels = c("ND-Kenya", "ND-South Africa",
                                                          "AFR", "AMR","CSA",  
                                                          "EAS","EUR", "MID", "OCE"))

#AGVP Pre-processing-----

agvp_pca = read.delim('/Users/heesu/Documents/NeuroDev/trio_pilot/data/pca/agvp_data.txt', sep = '\t')
nd_agvp_project = read.delim('/Users/heesu/Documents/NeuroDev/trio_pilot/data/pca/trio_pilot_project_agvp.tsv', sep = '\t')
agvp_annot = read.delim('~/Documents/NeuroDev/wave1/agvp_populations.tsv', sep = '\t')

#first, remove individuals who are not of African descent - since we are projecting onto an African reference
#identify this with p1 data - likely those with PC1 < 0 | PC2 >.15 or <-.1
ids_keep = neurodev %>% filter(PC2 < (5*PC1 + .4))  #y < 5x + .4
ids_keep = ids_keep$s

#now subset these ids in nd data
nd_agvp_project = nd_agvp_project %>% filter(s %in% ids_keep)

agvp = rbind(agvp_pca, nd_agvp_project)
colnames(agvp_annot) <- c('region', 's')
agvp_regions = rbind(agvp_annot, nd_regions)


#join to PCA data
agvp_project_pca = left_join(agvp, agvp_regions)

#add project names
agvp_project_pca$project <- NA
agvp_project_pca$project[agvp_project_pca$s %in% nd_regions$s] <- "NeuroDev"
agvp_project_pca$project[agvp_project_pca$s %in% agvp_annot$s] <- "AGVP"

agvp_project_pca$region[agvp_project_pca$region == 'Kenya'] <- 'ND-Kenya'

agvp_project_pca$region[agvp_project_pca$region == 'South Africa'] <- 'ND-South Africa'

#reorder labels
agvp_project_pca$region <- factor(agvp_project_pca$region, 
                                  levels = c("ND-Kenya" , "ND-South Africa",
                                             "Jola", "Mandinka","Wolof", "Fula", #green
                                             "Ga-Adangbe","Igbo",  #brown
                                             "Sotho", "Zulu", #red
                                             "Baganda", "Barundi", "Banyarwanda", #orange,
                                             "Kalenjin","Kikuyu" ,"Ethiopia")) #purple

agvp_data = agvp_project_pca %>% filter(!grepl('ND-', as.character(agvp_project_pca$region)))
nd_data = agvp_project_pca %>% filter(grepl('ND-', as.character(agvp_project_pca$region)))

# Plot AGVP/AFR reference map ---------------------------------
library(maptools)
library(cowplot)

#first, specify continental Africa's colors and parameters

nd_colors = c('ND-Kenya' ='#E41A1C',
              'ND-South Africa' ="#377EB8",
              
              'Jola'="#dcffea",
              'Mandinka'="#01bea8",
              'Wolof'="#5bffb4",
              'Fula'="#016e69",
              
              'Ga-Adangbe'="#7f7f00",
              'Igbo'="#7c542c",
              
              'Sotho'="#dc306c",
              'Zulu'="#ff7be9",
              
              'Baganda'="#ff838e",
              'Barundi'="#ff6452",
              'Banyarwanda'="#ffd496",
              
              'Kalenjin'="#a846f2",
              'Kikuyu'="#006be8",
              'Ethiopia'="#c0c9ff")


#create outline colors vector
nd_outline = nd_colors
nd_outline[1:length(nd_outline)] = rep('gray50')
nd_outline['ND-Kenya'] <- 'black'
nd_outline['ND-South Africa'] <- 'black'

#create alpha vector
afr_alpha = nd_colors
afr_alpha[1:2] <- 1
afr_alpha[3:16] <- .3

#create shape vector
afr_shapes <- rep_len(c(21:25), length.out = length(nd_colors))
names(afr_shapes) <- names(nd_colors)

# Read in data for world map plotting
data(wrld_simpl)
world <- fortify(wrld_simpl)
# Read the latitude/longitdue/plotting data for reference populations
world_data <- read.csv('/Users/heesu/Documents/NeuroDev/trio_pilot/figures/pop_plot_info.csv', header=T) %>%
  filter(Population !='ASW')

# Keep only relevant populations for AFR
pops_to_keep = c(unique(as.character(agvp_data$region)), "ND-Kenya", "ND-South Africa")
pop_pos_plot = world_data %>% filter(Population %in% pops_to_keep)


# Generating the continental Africa plot

marys_world <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group=group), 
               fill='white', color='lightgrey') +
  geom_point(data = pop_pos_plot, 
             aes(Longitude, Latitude, 
                 color=Population, fill=Population, 
                 shape=Population), 
             size=2,
             show.legend = FALSE) +
  coord_fixed(xlim = c(-20,50), ylim = c(-35,35)) +
  labs(x='Longitude', y='Latitude') +
  theme_classic() +
  scale_fill_manual(values=nd_colors, name = 'Population', 
                    breaks=names(nd_colors)) +
  scale_shape_manual(values = afr_shapes, name = 'Population', 
                     breaks = names(afr_shapes))+
  scale_color_manual(values=nd_outline, name = 'Population', 
                     breaks=names(nd_outline)) +
  guides(fill=guide_legend(nrow = 2, 
                           override.aes = list(size = 2, alpha = 1)))+
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size=10))

ggsave('afr_ref_map.pdf', marys_world, width=6, height=5)


# AGVP PCA Plots-----
#PC1 and 2
agvp_p1 <- ggplot() + 
  geom_point(data = agvp_data, 
             aes(x = PC1, y= PC2, 
                 shape = region,  
                 fill= region, 
                 color=region), 
             size = 1.3, alpha=.65)+
  geom_point(data = nd_data,
             aes(x = PC1, y= PC2, 
                 shape = region, 
                 fill=region,
                 color=region), 
             size=1.3, alpha=1)+
  scale_fill_manual(values=nd_colors, name = 'Population', 
                    breaks=names(nd_colors)) +
  scale_shape_manual(values = afr_shapes, name = 'Population', 
                     breaks = names(afr_shapes))+
  scale_color_manual(values=nd_outline, name = 'Population', 
                     breaks=names(nd_outline)) +
  theme_classic() +
  guides(fill=guide_legend(nrow = 4, 
                           override.aes = list(size = 2, alpha = 1)))+
  theme(axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        legend.position = 'top',
        legend.key.height = unit(12,'pt'))

#PC3 and 4
agvp_p2 <- ggplot() + 
  geom_point(data = agvp_data, 
             aes(x = PC3, y= PC4, shape = region, fill= region, color=region), 
             size = 1.3, alpha=.65)+
  geom_point(data = nd_data,
             aes(x = PC3, y= PC4, shape = region, fill=region, color=region), 
             size=1.3)+
  scale_fill_manual(values=nd_colors, name = 'Population', 
                    breaks=names(nd_colors)) +
  scale_shape_manual(values = afr_shapes, name = 'Population', 
                     breaks=names(afr_shapes))+
  scale_color_manual(values=nd_outline, name = 'Population', 
                     breaks=names(nd_outline)) +
  theme_classic() +
  guides(fill=guide_legend(nrow = 4, 
                           override.aes = list(size = 2, alpha = 1)))+
  theme(axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        legend.position = 'top',
        legend.key.height = unit(12,'pt'))

ggsave('agvp_pc1+2.pdf', agvp_p1, width=5, height=5)
ggsave('agvp_pc3+4.pdf', agvp_p2, width=9, height=5)


#agvp_legend <- as_ggplot(get_legend(agvp_p2))
#ggsave('legend.pdf', legend, height = 5)




# Plot HGDP-TGP/world ref map ---------------------------------
#read in metadata
hgdp_tgp_meta_v2 = read.delim("/Users/heesu/Documents/NeuroDev/resources/gnomad_meta_hgdp_tgp_v1.txt")

#subset relevant groups and add info for ND
meta_sub = unique(hgdp_tgp_meta_v2 %>% select(hgdp_tgp_meta.Continent.colors, 
                                       hgdp_tgp_meta.Genetic.region,
                                       hgdp_tgp_meta.Latitude,
                                       hgdp_tgp_meta.Longitude,
                                       hgdp_tgp_meta.Population))

meta_sub[nrow(meta_sub)+1, ] <- c(NA, 'ND-Kenya', -3.51, 39.90, 'ND-Kenya')
meta_sub[nrow(meta_sub)+1, ] <- c(NA, 'ND-South Africa', -33.90, 18.48, 'ND-South Africa')

#rename columns
colnames(meta_sub) <- c('Colors', 'Continent', 'Latitude', 'Longitude', 'Population')

#add project names
meta_sub$project <- rep('HGDP-TGP')
meta_sub$project[meta_sub$Continent %in% c('ND-Kenya', 'ND-South Africa')] <- 'NeuroDev'

meta_sub$Latitude <- as.numeric(meta_sub$Latitude)
meta_sub$Longitude <- as.numeric(meta_sub$Longitude)

#add global plotting parameters
global_shapes = c('NeuroDev' = 21, 'HGDP-TGP' = 24)
global_color = c('ND-Kenya' = 'black', 
                 "ND-South Africa" = 'black',
                 "AFR" = 'gray50', 
                 "AMR" = 'gray50',
                 "CSA" = 'gray50',  
                 "EAS" = 'gray50',
                 "EUR" = 'gray50', 
                 "MID" = 'gray50', 
                 "OCE" = 'gray50')


#set colors
cont_colors = brewer.pal(n=9, 'Set1')
names(cont_colors) = c('ND-Kenya', 'ND-South Africa', 'AFR', 'AMR', 'CSA', 'EAS', 'EUR', 'MID', 'OCE')

perlas_world <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group=group), 
               fill='white', color='lightgrey') +
  geom_point(data = meta_sub, aes(Longitude, Latitude, 
                                    fill = Continent, 
                                    color = Continent, 
                                    shape = project), 
             size=1.3, 
             show.legend = FALSE) +
  theme_classic() +
  coord_fixed(xlim = c(-150,150), ylim = c(-90,90)) +
  labs(x='Longitude', y='Latitude') +
  scale_fill_manual(values = cont_colors, 
                    name = 'Population', 
                    breaks = names(cont_colors))+
  scale_color_manual(values = global_color, 
                     name = 'Population', 
                     breaks = names(global_color))+
  scale_shape_manual(values = global_shapes, 
                     name = 'Source', 
                     breaks = names(global_shapes))+
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size=10))
  


ggsave('world_ref_map.pdf', perlas_world, width=6, height=5)


# Global PCA Plots ------

global_p1 <- ggplot(joined_pca, aes(x = PC1, y= PC2, 
                                    fill = region, 
                                    color = region, 
                                    shape = project)) + 
  geom_point(data = joined_pca %>% 
               filter(!(region %in% c("ND-Kenya", "ND-South Africa"))),
             size = 1.3, alpha = .6)+
  geom_point(data = joined_pca %>% 
               filter(region %in% c("ND-Kenya", "ND-South Africa")),
            size = 1.3)+
  theme_classic()+
  scale_fill_manual(values = cont_colors, name = 'Population', 
                    breaks = names(cont_colors))+
  scale_color_manual(values = global_color, name = 'Population', 
                     breaks = names(global_color))+
  scale_shape_manual(values = global_shapes, name = 'Source', 
                     breaks = names(global_shapes))+
  guides(fill=guide_legend(nrow = 3, 
                           override.aes = list(size = 2, shape = 21)),
         shape = guide_legend(nrow=2, 
                              override.aes = list(size = 2))) +
  theme(axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        legend.position = 'top',
        legend.key.height = unit(12,'pt'))


global_p2 <- ggplot(joined_pca, aes(x = PC3, y= PC4, 
                                    fill = region, 
                                    color = region, 
                                    shape = project)) + 
  geom_point(data = joined_pca %>% 
               filter(!(region %in% c("ND-Kenya", "ND-South Africa"))),
             size = 1.3, alpha = .6)+
  geom_point(data = joined_pca %>% 
               filter(region %in% c("ND-Kenya", "ND-South Africa")),
             size = 1.3)+
  theme_classic()+
  scale_fill_manual(values = cont_colors, name = 'Population', 
                    breaks = names(cont_colors))+
  scale_color_manual(values = global_color, name = 'Population', 
                     breaks = names(global_color))+
  scale_shape_manual(values = global_shapes, name = 'Source', 
                     breaks = names(global_shapes))+
  guides(fill=guide_legend(nrow = 3, 
                           override.aes = list(size = 2, shape = 21)),
         shape = guide_legend(nrow=2, 
                              override.aes = list(size = 2))) +
  theme(axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        legend.position = 'top',
        legend.key.height = unit(12,'pt'))

ggsave('global_pc1+2.pdf', global_p1, width=5, height=5)
ggsave('global_pc3+4.pdf', global_p2, width=8, height=5)


# Normal PCA ---------

annot = read.delim('/Users/heesu/Documents/NeuroDev/wave1/sample_info/k_sa_all_anno.tsv', sep = '\t')
annot = annot %>% select(entity.sample_id, collaborator_participant_id)

#reformat IDs in langs
langs <- left_join(langs,annot, by = c('subject_id' = 'collaborator_participant_id') )

#if desired, we can parse by eval(parse(text= langs$__colname__))

normal_pca = left_join(normal_pca, regions)
normal_pca = inner_join(normal_pca, langs, by = c('s'='entity.sample_id'))
normal_pca$project <- "NeuroDev"

#let's clean up the language variables
normal_pca[normal_pca == "c(\"Afrikaans\", \"English\")"] <- "Afrikaans, English"
normal_pca[normal_pca == "c(\"Kiswahili\", \"Kigiriama\")"] <- "Kiswahili, Kigiriama"
normal_pca[normal_pca == "c(\"Kiswahili\", \"Kichonyi\")"]<- "Kiswahili, Kichonyi"
normal_pca[normal_pca == "c(\"Kiswahili\", \"Kiwaatha\", \"Kigiriama\")"]<- "Kiswahili, Kiwaatha, Kigiriama"
normal_pca[normal_pca == "NULL"]<- "Unknown"

#finalized language variables
normal_pca$mgm_langs_final <-NA
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Afrikaans'] <- 'Afrikaans'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Afrikaans, English'] <- 'Afrikaans & English'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'English'] <- 'English'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Kichonyi'] <- 'Kichonyi'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Kiswahili'] <- 'Kiswahili'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Kigiriama'] <- 'Kigiriama'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Kiswahili, Kichonyi'] <- 'Multiple Mijikenda \nlanguages'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Kiswahili, Kigiriama'] <- 'Multiple Mijikenda \nlanguages'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Kiswahili, Kiwaatha, Kigiriama'] <- 'Multiple Mijikenda \nlanguages'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Shona'] <- 'Shona'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Swahili'] <- 'Kiswahili'
normal_pca$mgm_langs_final[normal_pca$mgm_langs == 'Xhosa or isiXhosa'] <- 'Xhosa or isiXhosa'
normal_pca$mgm_langs_final[is.na(normal_pca$mgm_langs_final)] <- 'Other'


#colors
colors = c(
  'Afrikaans' ='green',
  'Afrikaans & English' = 'blue',
  'English' = 'cyan',
  'Shona' = 'yellow',
  'Kichonyi' = 'pink',
  'Kiswahili' = 'tan4',
  'Kigiriama' = 'red2',
  'Xhosa or isiXhosa' = 'orange',
  'Multiple Mijikenda \nlanguages' = 'purple',
  'Other' = '#cccccc')


normal_p1 <- ggplot(normal_pca, aes(x = PC1, y= PC2, 
                                    shape = region, 
                                    fill = mgm_langs_final)) + 
  geom_point(size = 1.3, alpha = .8)+
  scale_shape_manual(values = c(21,22), name = 'Region') +
  scale_fill_manual(values = colors, name = 'Language')+
  theme_classic()+
  guides(shape = guide_legend(nrow=2, override.aes = list(size = 2)),
         fill=guide_legend(nrow = 4, byrow=TRUE,
                           override.aes = list(size = 2, 
                                               shape = 21, 
                                               alpha = 1)))+
  theme(axis.title = element_text(size=10),
        legend.text = element_text(size=10, lineheight = .8),
        legend.title = element_text(size=11),
        legend.position = 'top',
        legend.spacing.y=unit(0, 'mm'),
        legend.key.height = unit(12,'pt'))


normal_p2 <- ggplot(normal_pca, aes(x = PC3, y= PC4, 
                                    shape = region, 
                                    fill = mgm_langs_final)) + 
  geom_point(size = 1.3, alpha = .8)+
  scale_shape_manual(values = c(21,22), name = 'Region') +
  scale_fill_manual(values = colors, name = 'Language')+
  theme_classic()+
  guides(shape = guide_legend(nrow=2, override.aes = list(size = 2)),
         fill=guide_legend(nrow = 4, byrow=TRUE,
                           override.aes = list(size = 2, 
                                               shape = 21, 
                                               alpha = 1)))+
  theme(axis.title = element_text(size=10),
        legend.text = element_text(size=10, lineheight = .8),
        legend.title = element_text(size=11),
        legend.position = 'top',
        legend.spacing.y=unit(0, 'mm'),
        legend.key.height = unit(12,'pt'))

ggsave('normal_pc1+2.pdf', normal_p1, width=5, height=5)
ggsave('normal_pc3+4.pdf', normal_p2, width=9.3, height=5)


# Using patchwork and cowplot to create publication-ready panel figure

global = perlas_world + global_p1 + global_p2 + 
  plot_layout(guides = "collect", widths = c(1,1,1)) & 
  theme(legend.position = 'top')
agvp = marys_world + agvp_p1 + agvp_p2 + 
  plot_layout(guides = "collect", widths = c(1,1,1)) & 
  theme(legend.position = 'top')
normal = plot_spacer() + normal_p1 + normal_p2 + 
  plot_layout(guides = "collect", widths = c(1,1,1)) & 
  theme(legend.position = 'top')

#final plot
final_fig = global/agvp/normal +
  plot_layout(widths=c(1,1,1))+
  plot_annotation(theme = theme(plot.margin = margin(.5,1,.5,1, unit='cm')))

ggsave('final_fig_2.pdf', final_fig, width=7.5, height=8.5)
ggsave('final_fig_2.svg', final_fig, width=7.5, height=8.5) #to make further edits manually
