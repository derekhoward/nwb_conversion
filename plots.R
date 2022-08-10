library(here)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(readr)
library(patchwork)
library(cowplot)
library(ggsignif)
library(ggpubr)

df <- read_csv(here('./data/processed/features/tidyfeatures.csv'))
df <- rename(df, species = `Data Type`)
df$resection_location %<>% 
  str_replace('Right-', '') %>% 
  str_replace('Left-', '')

df$resection_location %<>% recode('Unknown' = 'Not annotated', 'ATL' = 'Temporal', 'FL' = 'Frontal', 'Parietal lobe' = 'Parietal')
table(df$resection_location)

df$sex %<>% 
  recode('F' = 'Female', 'M' = 'Male')
table(df$sex)

subset <- df %>% 
  filter(species == 'Human') %>% 
  filter(layer_name %in% c('L23', 'L3C', 'L5')) #%>% 

subset %>% select(-feature, -value) %>% distinct() %>% select(layer_name, putative_interneuron, external_soln) %>% table()

df %>% 
  filter(species == 'Mouse') %>% 
  select(-feature, -value) %>% 
  distinct() %>% 
  select(layer_name, putative_interneuron, external_soln) %>%
  table()

df %>% 
  filter(species == 'Human') %>% 
  filter(layer_name %in% c('L23', 'L3C', 'L5')) %>% 
  select(subject_id, age) %>% 
  distinct() %>% 
  #select(age) %>% 
  summarise(meanage = mean(age), sd=sd(age))

df %>% 
  filter(species == 'Human') %>% 
  filter(layer_name %in% c('L23', 'L3C', 'L5')) %>% 
  select(resection_location, subject_id) %>% 
  #distinct() %>% 
  select(resection_location) %>% 
  table()


dir.create(path = './results')

########################################################################################################################

test <- read_csv('./data/processed/features/all_features_wide.csv')
test %>%
  filter(`Data Type` == 'Human') %>% 
  select(age) %>% 
  summarise(M = mean(age), SD=sd(age), R=range(age))



########################################################################################################################
# F3: show 3 datasets together
# - difference in Rin by external solution
# - difference in same solution in a single layer (layer5)
# - show Rin and sag, difference across species

# TODO add figure showing difference in max_rate of fi_fit_slope
########################################################################################################################
theme_set(theme_cowplot(font_size = 22))

p3a <- df %>% 
  filter(species == 'Human' & layer_name == 'L5' & feature == 'input_resistance') %>% 
  ggplot(aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  geom_signif(
    comparisons = list(c("aCSF", "Synaptic Blockers")),
    map_signif_level = TRUE, textsize = 6, test = 'wilcox.test'
  ) +
  labs(x='', y='Input resistance (MΩ)') +
  #theme(text = element_text(size = 16)) +  
  ggtitle('A') 

p3b <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'Synaptic Blockers') %>% 
  filter(feature == 'input_resistance') %>% 
  ggplot(aes(x=species, y=value)) + 
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='Input resistance (MΩ)') +
  #theme(text = element_text(size = 16)) +
  ggtitle('B')

p3c <- df %>% 
  filter(species == 'Human' & layer_name == 'L5' & feature == 'width') %>% 
  mutate(value = value*1000) %>% 
  ggplot(aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  labs(x='', y='AP width (ms)') +
  #theme(text = element_text(size = 16)) +  
  ggtitle('C') 

p3d <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'Synaptic Blockers') %>% 
  filter(feature == 'sag') %>% 
  ggplot(aes(x=species, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='Sag ratio') +
  #theme(text = element_text(size = 16)) +
  ggtitle('D') 

p3d2 <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'Synaptic Blockers') %>% 
  filter(feature == 'width') %>% 
  mutate(value = value*1000) %>% 
  ggplot(aes(x=species, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='AP width (ms)') +
  #theme(text = element_text(size = 16)) +
  ggtitle('D') 

p3e <- df %>% 
  filter(species == 'Human' & layer_name == 'L5' & feature == 'fi_fit_slope') %>% 
  ggplot(aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='F/I Fit Slope') +
  #theme(text = element_text(size = 16)) +
  ggtitle('E') 

p3f <- df %>% 
  filter(external_soln == 'Synaptic Blockers' & layer_name == 'L5' & feature == 'fi_fit_slope') %>% 
  ggplot(aes(x = species, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='F/I Fit Slope') +
  #theme(text = element_text(size = 16)) +
  ggtitle('F') 

p3e2 <- df %>% 
  filter(species == 'Human' & layer_name == 'L5' & feature == 'avg_rate') %>% 
  ggplot(aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='Avg. Rate (Hz)') +
  #theme(text = element_text(size = 16)) +
  ggtitle('E') 

p3f2 <- df %>% 
  filter(external_soln == 'Synaptic Blockers' & layer_name == 'L5' & feature == 'avg_rate') %>% 
  ggplot(aes(x = species, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='Avg. Rate (Hz)') +
  #theme(text = element_text(size = 16)) +
  ggtitle('F') 

#fig3 <- (rin_plot | width_plot) / (p1c | p1d)
#fig3alt <- (rin_plot | sagratio_plot) / (p1c | p1d)
#fig3alt2 <- (rin_plot | peakv_plot) / (p1c | p1d)
fig3 <- (p3a | p3b) / (p3c | p3d)
fig3 <- (p3a | p3b) /  (p3c | p3d2)  / (p3e2 | p3f2)
#fig3alt <- (rin_plot | sagratio_plot) / (p1c | p1d)
#fig3alt2 <- (rin_plot | peakv_plot) / (p1c | p1d)
ggsave(filename = './results/fig3v2-v3.pdf', plot=fig3, dpi = 300, height = 19, width = 12)
ggsave(filename = './results/fig3v2-v3.png', plot=fig3, dpi = 300, height = 19, width = 12, bg = 'white')

########################################################################################################################
# F3: statistical analyses
########################################################################################################################
test_3a <- df %>% 
  filter(species == 'Human' & layer_name == 'L5' & feature == 'input_resistance') %>% 
  select(external_soln, value)

test_3a %>% group_by(external_soln) %>% summarise(M=mean(value), SD=sd(value), n=n())

t.test(test_3a %>% filter(external_soln == 'Synaptic Blockers') %>% .$value, 
       test_3a %>% filter(external_soln == 'aCSF') %>% .$value)

test_3b <- df %>%
  filter(layer_name == 'L5' & external_soln == 'Synaptic Blockers' & feature == 'input_resistance') %>% 
  select(species, value)

t.test(test_3b %>% filter(species == 'Human') %>% .$value, 
       test_3b %>% filter(species == 'Mouse') %>% .$value)

test_3c <- df %>%
  filter(species == 'Human' & layer_name == 'L5' & feature == 'width') %>% 
  select(external_soln, value)

t.test(test_3c %>% filter(external_soln == 'Synaptic Blockers') %>% .$value, 
       test_3c %>% filter(external_soln == 'aCSF') %>% .$value)

test_3d2 <- df %>%
  filter(layer_name == 'L5' & external_soln == 'Synaptic Blockers' & feature == 'width') %>% 
  select(species, value)

t.test(test_3d2 %>% filter(species == 'Human') %>% .$value, 
       test_3d2 %>% filter(species == 'Mouse') %>% .$value)

test_3e2 <- df %>%
  filter(species == 'Human' & layer_name == 'L5' & feature == 'avg_rate') %>% 
  select(external_soln, value)

t.test(test_3e2 %>% filter(external_soln == 'Synaptic Blockers') %>% .$value, 
       test_3e2 %>% filter(external_soln == 'aCSF') %>% .$value)

test_3f2 <- df %>%
  filter(layer_name == 'L5' & external_soln == 'Synaptic Blockers' & feature == 'avg_rate') %>% 
  select(species, value)

t.test(test_3f2 %>% filter(species == 'Human') %>% .$value, 
       test_3f2 %>% filter(species == 'Mouse') %>% .$value)

########################################################################################################################
# F4: 
########################################################################################################################
cutting_sol_plot <- df %>% 
  filter(species == 'Human' & feature == 'input_resistance') %>%
  filter(external_soln == 'Synaptic Blockers') %>%
  filter(layer_name %in% c('L23', 'L5')) %>% 
  ggplot(aes(x=cutting_solution, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  geom_signif(
    comparisons = list(c("NMDG", "Sucrose")),
    map_signif_level = TRUE, textsize = 6, test = 'wilcox.test'
  ) +
  facet_wrap(~ layer_name) +
  labs(x='', y='Input resistance (MΩ)', ) +
  #theme(axis.title.y = element_text(size = 8)) +
  ggtitle('A')

cutting_sol_plot2 <- df %>% 
  filter(species == 'Human' & feature == 'sag') %>% 
  filter(external_soln == 'Synaptic Blockers') %>%
  filter(layer_name %in% c('L23', 'L5')) %>% 
  ggplot(aes(x=cutting_solution, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  #  geom_signif(
  #    comparisons = list(c("NMDG", "Sucrose")),
  #    map_signif_level = TRUE, textsize = 6, test = 'wilcox.test'
  #  )  +
  facet_wrap(~ layer_name) +
  labs(x='', y='Sag ratio') +
  #theme(axis.title.y = element_text(size = 8)) +
  ggtitle('B')

fig4 <- (cutting_sol_plot / cutting_sol_plot2)
ggsave(fig4, filename = './results/fig4-layer_comparison-cutting_soln-input_R-sagR2.png', dpi = 300, height = 14, width = 10, bg = 'white')
ggsave(fig4, filename = './results/fig4-layer_comparison-cutting_soln-input_R-sagR2.pdf', dpi = 300, height = 14, width = 10)



df %>% 
  filter(species == 'Human' & feature == 'input_resistance') %>% 
  filter(external_soln == 'Synaptic Blockers') %>%
  #filter(layer_name %in% c('L23', 'L5')) %>% 
  group_by(cutting_solution) %>% #, layer_name
  summarise(avg = mean(value), sdev = sd(value), n = n())

df %>% 
  filter(species == 'Human' & feature == 'sag') %>% 
  filter(external_soln == 'Synaptic Blockers') %>%
  #filter(layer_name %in% c('L23', 'L5')) %>% 
  group_by(cutting_solution) %>%  #, layer_name
  summarise(avg = mean(value), sdev = sd(value), n = n())

# ALT F4
df %>% 
  mutate(size = case_when(file_id %in% c('18320021', '18o22020') ~ 5, 
                          TRUE ~ 1)) %>% 
  select(size)

alt_F4A <- df %>% 
  mutate(size = case_when(file_id %in% c('18320021', '18o22020') ~ 2, 
                          TRUE ~ 1)) %>% 
  filter(species == 'Human' & feature == 'input_resistance') %>%
  filter(external_soln == 'Synaptic Blockers') %>%
  filter(layer_name %in% c('L23', 'L3C', 'L5')) %>% 
  ggplot(aes(x=cutting_solution, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.85, position = position_jitter(width = 0.15), aes(colour=layer_name, size=size)) +
  geom_signif(
    comparisons = list(c("NMDG", "Sucrose")),
    map_signif_level = TRUE, textsize = 6, test = 't.test'
  ) +
  theme(legend.position="none") +
  labs(x='', y='Input resistance (MΩ)', color="") +
  #theme(axis.title.y = element_text(size = 8)) +
  ggtitle('A')

alt_F4B <- df %>% 
  mutate(size = case_when(file_id %in% c('18320021', '18o22020') ~ 2, 
                          TRUE ~ 1)) %>% 
  filter(species == 'Human' & feature == 'sag') %>% 
  filter(external_soln == 'Synaptic Blockers') %>%
  filter(layer_name %in% c('L23', 'L3C', 'L5')) %>% 
  ggplot(aes(x=cutting_solution, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.85, position = position_jitter(width = 0.15), aes(colour=layer_name, size=size)) +
  geom_signif(
    comparisons = list(c("NMDG", "Sucrose")),
    map_signif_level = TRUE, textsize = 6, test = 't.test'
  ) +
  labs(x='', y='Sag ratio', color="Layer") +
  #theme(axis.title.y = element_text(size = 8)) +
  ggtitle('B')

alt_fig4 <- (alt_F4A | alt_F4B)
ggsave(alt_fig4, filename = './results/fig4-alt.png', dpi = 300, height = 7, width = 10, bg = 'white')
ggsave(alt_fig4, filename = './results/fig4-alt.pdf', dpi = 300, height = 7, width = 10)


test_4a <- df %>% 
  filter(species == 'Human' & feature == 'input_resistance' & external_soln == 'Synaptic Blockers') %>% 
  select(cutting_solution, value)

test_4a %>% group_by(cutting_solution) %>% summarise(M=mean(value), SD=sd(value), n=n())

wilcox.test(test_4a %>% filter(cutting_solution == 'NMDG') %>% .$value, 
            test_4a %>% filter(cutting_solution == 'Sucrose') %>% .$value)
t.test(test_4a %>% filter(cutting_solution == 'NMDG') %>% .$value, 
       test_4a %>% filter(cutting_solution == 'Sucrose') %>% .$value)

test_4b <- df %>% 
  filter(species == 'Human' & feature == 'sag' & external_soln == 'Synaptic Blockers') %>% 
  select(cutting_solution, value)

test_4b %>% group_by(cutting_solution) %>% summarise(M=mean(value), SD=sd(value), n=n())

t.test(test_4b %>% filter(cutting_solution == 'NMDG') %>% .$value,
       test_4b %>% filter(cutting_solution == 'Sucrose') %>% .$value)

# ID potential traces to highlight
# F4A
df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L5') %>% 
  filter(cutting_solution == 'NMDG') %>% 
  filter(feature == 'input_resistance' & (value > 100) & (value < 400)) %>% 
  select(file_id, feature, value)
filter(file_id == '18201011')

df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L5') %>% 
  filter(cutting_solution == 'Sucrose') %>% 
  filter(feature == 'input_resistance' & (value > 100) & (value < 200)) %>% 
  select(file_id, feature, value)
  filter(file_id == '19228058') # this example seems most comparable to the other with NMDG

# F4B
df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L5') %>% 
  filter(cutting_solution == 'NMDG') %>% 
  filter(feature == 'sag' & (value > 0.05) & (value < 0.1)) %>% 
  select(file_id, feature, value)
  #filter(file_id == '18201004')

df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L5') %>% 
  filter(cutting_solution == 'Sucrose') %>% 
  filter(feature == 'sag' & (value > 0.075) & (value < 0.1)) %>% 
  select(file_id, feature, value)
  #filter(file_id == '19228058') # this example seems most comparable to the other with NMDG


#### Attempt #2 at finding good examples, this time looking at L2/3
df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L23') %>% 
  filter(cutting_solution == 'NMDG') %>% 
  filter(feature == 'input_resistance' & (value > 200) & (value < 400)) %>% 
  select(file_id, feature, value)
  filter(file_id == '18426010')

df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L23') %>% 
  filter(cutting_solution == 'Sucrose') %>% 
  filter(feature == 'input_resistance' & (value > 80) & (value < 250)) %>% 
  select(file_id, value)
filter(file_id == '18o22001')


df %>% 
  filter(species == 'Human' & external_soln == 'Synaptic Blockers') %>% 
  filter(layer_name == 'L23') %>% 
  filter(cutting_solution == 'Sucrose') %>% 
  filter(feature == 'sag' & (value > 0.07) ) %>% 
  select(file_id, value)

df %>% 
  filter(file_id %in% c('18426010', '18o22010'))

test <- read_csv('/Users/derek_howard/triplab/nwb_conversion/data/processed/meta/h2018_meta.csv') %>% select(file_id, stim_start_time, stim_end_time)
########################################################################################################################
# F5: illustrating different demographics variables present for human data (sex, age, diagnosis, resection area)
# - make sure to lump together left/right, so comparing frontal/temporal/parietal/Not annotated (replace unknown with Not annotated)
# - pick a feature (sag, Rin) to showcase across the demographics
# - keep solutions data separate to highlight specifics (focus on whichever blockers/acsf has more data)
# - try to plot raw traces that show differences between blockers/no blockers?
########################################################################################################################

# Data subsets
data_subset = 'acsf' #'acsf' #'blockers' 

if (data_subset == 'blockers') {
  human_subset <- df %>% 
    filter(species == 'Human' & layer_name != 'Int' & external_soln == 'Synaptic Blockers') %>% 
    filter(is_epileptogenic == FALSE)
} else {
  human_subset <- df %>% 
    filter(species == 'Human' & layer_name != 'Int' & external_soln == 'aCSF') %>% 
    filter(is_epileptogenic == FALSE)
}

human_subset_rin <- human_subset %>% 
  #filter(layer_name == 'L5') %>% 
  filter(feature == 'input_resistance')

human_subset_sag <- human_subset %>% 
  #filter(layer_name == 'L5') %>% 
  filter(feature == 'sag')
#########################################################################################
# boxplots: sag/rin vs sex
################################################
#p2_text_size = 14
p5a <- ggplot(human_subset_rin, aes(x=resection_location, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.85, position = position_jitter(width = 0.2), aes(colour=layer_name)) +
  labs(x='', y='Input resistance (MΩ)') +
  theme(legend.position="none") +
  ggtitle('A')

anova <- aov(value~resection_location, data = human_subset_rin)
summary(anova)
TukeyHSD(anova)
kruskal.test(value~resection_location, data = human_subset_rin)

# 2way anova is not appropriate given the lack of of measurements from multiple layers in frontal or parietal cortex samples
anova_2way <- aov(value~resection_location + layer_name, data = human_subset_rin)
summary(anova_2way)

p5b <- ggplot(human_subset_rin, aes(x=sex, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.85, position = position_jitter(width = 0.2), aes(colour=layer_name)) +
  labs(x='', y='') +
  #theme(legend.position="none") +
  ggtitle('B') +
  labs(color = "Layer")

t.test(human_subset_rin %>% filter(sex == 'Male')  %>% .$value, human_subset_rin %>% filter(sex == 'Female')  %>% .$value)
wilcox.test(human_subset_rin %>% filter(sex == 'Male')  %>% .$value, human_subset_rin %>% filter(sex == 'Female')  %>% .$value)

#p5c <- ggplot(human_subset_rin, aes(x=diagnosis, y=value)) +
#  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
#  geom_point(alpha=0.85, position = position_jitter(width = 0.2), aes(colour=layer_name)) +
#  labs(x='', y='') +
#  #theme(legend.position="none") +
#  ggtitle('C')

#######################################################
# 2nd row of figures, looking at sag ratio across demographic features
#######################################################
p5d <- ggplot(human_subset_sag, aes(x=resection_location, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.85, position = position_jitter(width = 0.2), aes(colour=layer_name)) +
  labs(x='', y='Sag ratio') +
  theme(legend.position="none") +
  ggtitle('C')

anova_c <- aov(value~resection_location, data = human_subset_sag)
summary(anova_c)
TukeyHSD(anova_c)
kruskal.test(value~resection_location, data = human_subset_sag)


p5e <- ggplot(human_subset_sag, aes(x=sex, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.85, position = position_jitter(width = 0.2), aes(colour=layer_name)) +
  labs(x='', y='') +
  theme(legend.position="none") +
  ggtitle('D')

t.test(human_subset_sag %>% filter(sex == 'Male')  %>% .$value, human_subset_sag %>% filter(sex == 'Female')  %>% .$value)
wilcox.test(human_subset_sag %>% filter(sex == 'Male')  %>% .$value, human_subset_sag %>% filter(sex == 'Female')  %>% .$value)

#p5f <- ggplot(human_subset_sag, aes(x=diagnosis, y=value)) +
#  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
#  geom_point(alpha=0.85, position = position_jitter(width = 0.2), aes(colour=layer_name)) +
#  labs(x='', y='') +
#  theme(legend.position="none") +
#  ggtitle('F')

#fig5v2 <- (p5a | p5b | p5c) / (p5d | p5e | p5f)
#fig5_name <- str_glue('./results/fig5-{data_subset}-colour_version')
#ggsave(fig5v2, filename = paste0(fig5_name, '.pdf'), dpi = 300, height=8, width = 14)
#ggsave(fig5v2, filename = paste0(fig5_name, '.png'), dpi = 300, height=8, width = 14, bg = 'white')

# remove the epilepsy vs tumour panels
fig5v3 <- (p5a | p5b) / (p5d | p5e)
fig5_name <- str_glue('./results/fig5-{data_subset}-colour_version-removed_epilepsy_vs_tumor')
ggsave(fig5v3, filename = paste0(fig5_name, '.pdf'), dpi = 300, height=8, width = 14)
ggsave(fig5v3, filename = paste0(fig5_name, '.png'), dpi = 300, height=8, width = 14, bg = 'white')



########################################################################################################################
# F6: 
########################################################################################################################

p6a <- ggplot(human_subset_rin, aes(x=age, y=value)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm') +
  #scale_y_continuous(trans='log10', breaks = c(25,100,200)) + 
  scale_y_log10() +
  stat_cor(method = "pearson", size=5) + 
  labs(x='Age (years)', y='Input resistance (MΩ)') +
  ggtitle('A') 
#scale_x_continuous(limits=c(20,60), breaks=seq(0,65,5)) + 

# seizure duration
p6b <- ggplot(human_subset_rin, aes(x=seizure_duration_years, y=value)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm') +
  #scale_y_continuous(trans='log10', breaks = c(25,100,200)) + 
  scale_y_log10() + 
  stat_cor(method = "pearson", size=5) + 
  labs(x='Seizure duration (years)', y='') +
  ggtitle('B')

p6c <- ggplot(human_subset_sag, aes(x=age, y=value)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm') +
  stat_cor(method = "pearson", size=5) + 
  labs(x='Age (years)', y='Sag ratio') +
  ggtitle('C')


p6d <- ggplot(human_subset_sag, aes(x=seizure_duration_years, y=value)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm') +
  stat_cor(method = "pearson", size=5) + 
  labs(x='Seizure duration (years)', y='') +
  ggtitle('D')


fig6 <- (p6a | p6b) / (p6c | p6d)
fig6_name <- str_glue('./results/fig6-{data_subset}')


ggsave(fig6, filename = paste0(fig6_name, '.pdf'), dpi = 300, height=12, width = 12)
ggsave(fig6, filename = paste0(fig6_name, '.png'), dpi = 300, height=12, width = 12, bg = 'white')































rin_l5_human_subset <- df %>% 
  filter(species == 'Human' & feature == 'input_resistance' & layer_name == 'L5')

peakv_l5_human_subset <- df %>% 
  filter(species == 'Human' & feature == 'peak_v' & layer_name == 'L5')

acsf_peakv <- peakv_l5_human_subset %>% filter(external_soln == 'aCSF') %>% select(value) %>% .$value
blockers_peakv <- peakv_l5_human_subset %>% filter(external_soln == 'Synaptic Blockers') %>% select(value) %>% .$value
t.test(acsf_peakv, blockers_peakv)

width_l5_human_subset <- df %>% 
  filter(species == 'Human' & feature == 'width' & layer_name == 'L5')

acsf_width <- width_l5_human_subset %>% filter(external_soln == 'aCSF') %>% select(value) %>% .$value
blockers_width <- width_l5_human_subset %>% filter(external_soln == 'Synaptic Blockers') %>% select(value) %>% .$value
t.test(acsf_width, blockers_width)

sagratio_l5_human_subset <- df %>% 
  filter(species == 'Human' & feature == 'sag' & layer_name == 'L5')

acsf_sagratio <- sagratio_l5_human_subset %>% filter(external_soln == 'aCSF') %>% select(value) %>% .$value
blockers_sagratio <- sagratio_l5_human_subset %>% filter(external_soln == 'Synaptic Blockers') %>% select(value) %>% .$value
t.test(acsf_sagratio, blockers_sagratio)

rin_plot <- ggplot(rin_l5_human_subset, aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  labs(x='', y='Input resistance (MΩ)') +
  #theme(text = element_text(size = 16)) +  
  ggtitle('A') #\nInput resistance of human L5 neurons in different external solutions')

rin_plot
ggsave(filename = './results/v2-Rin_by_solution.pdf', dpi = 300)

peakv_plot <- ggplot(peakv_l5_human_subset, aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  labs(x='', y='Peak voltage (mV)') +
  #theme(text = element_text(size = 16)) +  
  ggtitle('B') #\nInput resistance of human L5 neurons in different external solutions')


width_plot <- width_l5_human_subset %>% 
  mutate(value = value*1000) %>% 
  ggplot(aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  labs(x='', y='AP width (ms)') +
  #theme(text = element_text(size = 16)) +  
  ggtitle('B') #\nInput resistance of human L5 neurons in different external solutions')

sagratio_plot <- ggplot(sagratio_l5_human_subset, aes(x = external_soln, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.15)) +
  labs(x='', y='Sag ratio') +
  #theme(text = element_text(size = 16)) +  
  ggtitle('B') #\nInput resistance of human L5 neurons in different external solutions')


acsf_l5_sag <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'aCSF') %>% 
  filter(feature == 'sag')

blockers_l5_sag <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'Synaptic Blockers') %>% 
  filter(feature == 'sag')

acsf_l5_rin <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'aCSF') %>% 
  filter(feature == 'input_resistance')

blockers_l5_rin <- df %>%
  filter(layer_name == 'L5') %>% 
  filter(external_soln == 'Synaptic Blockers') %>% 
  filter(feature == 'input_resistance')


p1c <- ggplot(blockers_l5_rin, aes(x=species, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='Input resistance (MΩ)') +
  #theme(text = element_text(size = 16)) +
  ggtitle('C')#\nInput resistance in human vs mouse L5\nmeasured with blockers in aCSF')


p1d <- ggplot(blockers_l5_sag, aes(x=species, y=value)) +
  geom_boxplot(outlier.shape = NA) + #this does not remove the outliers, it only hides them, so the range calculated for the y-axis will be the same with outliers shown and outliers hidden
  geom_point(alpha=0.5, position = position_jitter(width = 0.2)) +
  labs(x='', y='Sag ratio') +
  #theme(text = element_text(size = 16)) +
  ggtitle('D') #\nSag ratio in human vs mouse L5\nmeasured with blockers in aCSF')

#scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10), expand = c(0, 0))

fig1 <- (rin_plot | width_plot) / (p1c | p1d)
fig1alt <- (rin_plot | sagratio_plot) / (p1c | p1d)
fig1alt2 <- (rin_plot | peakv_plot) / (p1c | p1d)
ggsave(filename = './results/fig1.pdf', plot=fig1, dpi = 300, height = 8, width = 12)
ggsave(filename = './results/fig1.png', plot=fig1, dpi = 300, height = 8, width = 12, bg = 'white')
