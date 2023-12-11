library(ggdag)
library(ggplot2)

theme_set(theme_dag())

# scenario 1: breastfeeding and diet quality are not mediators
coord_dag <- list(
  x = c(dietquality = 0, breastfeeding = 2, cdq = 3.5, U = 0, C = 2.5,
        brainmorphology = 4),
  y = c(dietquality = 4, breastfeeding = 5.5, cdq = 7, U = 5.5, C = 8,  
        brainmorphology = 4)
)

mini_dag <- dagify(brainmorphology ~ dietquality,
                   U ~ dietquality,
                   breastfeeding ~ U,
                   cdq ~ U,
                   brainmorphology ~ U,
                   brainmorphology ~ C,
                   dietquality ~ C, 
                   breastfeeding ~ C,
                   cdq ~ C,
                   brainmorphology ~ breastfeeding,
                   brainmorphology ~ cdq,
         labels = c(
           "brainmorphology" = "Brain\n Morphology",
           "dietquality" = "Diet Quality",
           "U" = "Unobserved",
           "C" = "Adjusted\n (e.g., SES)",
           "breastfeeding" = "Breastfeeding",
           "cdq" = "Child Diet Quality"
         ),
         exposure = "dietquality",
         outcome = "brainmorphology",
         coords = coord_dag) %>% 
  tidy_dagitty()


ggdag(mini_dag, text = FALSE, use_labels = "label")



ggdag_adjustment_set(mini_dag, text = FALSE, use_labels = "label", shadow = TRUE) 

ggsave("/Users/ymou/Library/CloudStorage/OneDrive-ErasmusMC/maternal_diet_results/visualization/dag_notmediator.tiff",
       units="in", width=10, height=10, dpi=1000, compression = 'lzw')

dev.off()

# scenario 2: breastfeeding and diet quality ARE mediators
coord_dag <- list(
  x = c(dietquality = 0, breastfeeding = 2, cdq = 3.5, U = 0, C = 2.5,
        brainmorphology = 4),
  y = c(dietquality = 4, breastfeeding = 5.5, cdq = 7, U = 5.5, C = 8,  
        brainmorphology = 4)
)

mini_dag <- dagify(brainmorphology ~ dietquality,
                   U ~ dietquality,
                   breastfeeding ~ U,
                   cdq ~ U,
                   brainmorphology ~ U,
                   brainmorphology ~ C,
                   dietquality ~ C, 
                   breastfeeding ~ C,
                   cdq ~ C,
                   brainmorphology ~ breastfeeding,
                   brainmorphology ~ cdq,
                   breastfeeding ~ dietquality,
                   cdq ~ dietquality,
                   labels = c(
                     "brainmorphology" = "Brain\n Morphology",
                     "dietquality" = "Diet Quality",
                     "U" = "Unobserved",
                     "C" = "Adjusted\n (e.g., SES)",
                     "breastfeeding" = "Breastfeeding",
                     "cdq" = "Child Diet Quality"
                   ),
                   exposure = "dietquality",
                   outcome = "brainmorphology",
                   coords = coord_dag) %>% 
  tidy_dagitty()


ggdag(mini_dag, text = FALSE, use_labels = "label")



ggdag_adjustment_set(mini_dag, text = FALSE, use_labels = "label", shadow = TRUE) 

ggsave("/Users/ymou/Library/CloudStorage/OneDrive-ErasmusMC/maternal_diet_results/visualization/dag_mediator.tiff",
       units="in", width=10, height=10, dpi=1000, compression = 'lzw')

dev.off()