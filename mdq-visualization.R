library(readxl)
library(reshape2)
library(ggplot2)
library(tidyverse)

load(r'(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\visualization\vis.rds)')

#--------------------------------- MAIN RESULTS ---------------------------

# 9 years: model 3 to model 6 -----
mod <- read_excel('/Users/ymou/Library/CloudStorage/OneDrive-ErasmusMC/maternal_diet_results/visualization/mdq-MainResultsVisualization.xlsx', sheet = 'mdq-10')

mod$cimin <- as.numeric(mod$cimin)
mod$cimax <- as.numeric(mod$cimax)
mod$dietarypatterns <- as.factor(mod$dietarypatterns)
mod$outcomes <- as.factor(mod$outcomes)

mod <- mod %>% 
  mutate(outcomes = fct_relevel(outcomes,
                                "Total brain", 'Cerebral white matter', 'Cerebral gray matter',
                                'Subcortical gray matter'))

mod$mod <- mod$mod %>% 
  as.factor() %>% 
  fct_collapse(
    "Basic Model" = "1",
    "+ Breastfeeding" = "2",
    "+ Child diet quality" = "3",
    "+ Both" = "4"
  ) 

mod_g <- mod %>% filter(group == "global") # select global measures 

mod_g$outcomes <- factor(mod_g$outcomes, levels=c("Total brain", 'Cerebral white matter', 'Cerebral gray matter',
                                                  'Subcortical gray matter'))

pglobal = ggplot(data=mod_g,
           aes(x = outcomes,y = estimates, ymin = cimin, ymax = cimax,
               group = mod))+
  geom_pointrange(aes(col= mod), position = position_dodge(width = 1), size = 2)+
  geom_hline(aes(fill=mod),yintercept =0, linetype=2)+
  xlab("") +
  ylab("Estimated differences (cm3) (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=cimin, ymax=cimax,col=mod),width=0.1,cex=1, position=position_dodge(width=1))+ 
  labs(color = "Models") +
  #theme(legend.position = "none") +
  theme(plot.title=element_blank(),
        axis.text.x=element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 24), 
        axis.ticks.x = element_blank(),
        axis.title=element_text(size= 24,face="bold"),
        strip.text.x = element_text(size = 24, face = "bold"),
        legend.title = element_text(size =24),
        legend.text = element_text(size = 24))

pglobal+scale_color_viridis(discrete = T) # color blind needed here


ggsave('/Users/ymou/Library/CloudStorage/OneDrive-ErasmusMC/maternal_diet_results/visualization/mdq_9y_colorblind.jpg',
       dpi = 600,
       width = 20,
       height = 12)

# 13 years: model 3 to model 6 -----
mod <- read_excel('/Users/ymou/Library/CloudStorage/OneDrive-ErasmusMC/maternal_diet_results/visualization/mdq-MainResultsVisualization.xlsx', sheet = 'mdq-13')

mod$cimin <- as.numeric(mod$cimin)
mod$cimax <- as.numeric(mod$cimax)
mod$dietarypatterns <- as.factor(mod$dietarypatterns)
mod$outcomes <- as.factor(mod$outcomes)

mod <- mod %>% 
  mutate(outcomes = fct_relevel(outcomes,
                                "Total brain", 'Cerebral white matter', 'Cerebral gray matter',
                                'Subcortical gray matter'))

mod$mod <- mod$mod %>% 
  as.factor() %>% 
  fct_collapse(
    "Basic Model" = "1",
    "+ Breastfeeding" = "2",
    "+ Child diet quality" = "3",
    "+ Both" = "4"
  ) 

mod_g <- mod %>% filter(group == "global") 

mod_g$outcomes <- factor(mod_g$outcomes, levels=c("Total brain", 'Cerebral white matter', 'Cerebral gray matter',
                                                  'Subcortical gray matter'))

pglobal = ggplot(data=mod_g,
                 aes(x = outcomes,y = estimates, ymin = cimin, ymax = cimax,
                     group = mod))+
  geom_pointrange(aes(col= mod), position = position_dodge(width = 1), size = 2)+
  geom_hline(aes(fill=mod),yintercept =0, linetype=2)+
  xlab("") +
  ylab("Estimated differences (cm3) (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=cimin, ymax=cimax,col=mod),width=0.1,cex=1, position=position_dodge(width=1))+ 
  labs(color = "Models") +
  #theme(legend.position = "none") +
  theme(plot.title=element_blank(),
        axis.text.x=element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 24), 
        axis.ticks.x = element_blank(),
        axis.title=element_text(size= 24,face="bold"),
        strip.text.x = element_text(size = 24, face = "bold"),
        legend.title = element_text(size =24),
        legend.text = element_text(size = 24))

pglobal+scale_color_viridis(discrete = T) # color blind needed here

ggsave('/Users/ymou/Library/CloudStorage/OneDrive-ErasmusMC/maternal_diet_results/visualization/mdq_13y_colorblind.jpg',
       dpi = 300,
       width = 20,
       height = 12)

save.image(r'(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\visualization\vis.rds)')
