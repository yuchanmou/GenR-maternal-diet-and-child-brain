library(tableone)

##User input
analysis_date = Sys.Date()

tempdir()
dir.create(tempdir())

load(file = r'(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\data\analysis_MDP_brain.RData)') 


#-----------------------------------------------------------------------------
#9 years ------------------------
#Your data 
analysis = "MDP_brain"
afterimp_rdsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\data\imp_mdq_brain_2022-11-30.RData)"
modsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\MaternalDiet_Brain\model loop\dp_brain_9y.mods)"
detsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\MaternalDiet_Brain\model loop\dietarypatterns_mothers.dets)"


#out
outsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\MaternalDiet_Brain\model loop\volume_9y.outs)"


#Load libraries
packages <- c('tidyverse', 'mice', 'data.table', 'Hmisc', 'xlsx', "openxlsx")
invisible(lapply(packages, library, character.only = T))

#load data
fdata <- load(afterimp_rdsfile)
models = as.character(as.list(read.table(modsfile,header=F, sep = '\t'))[[1]])
#model 3 is the main model, model 4 + breastfeeding, model 5 + diet quality@8y, model 6 + both
fmodels = c(models[1:2], paste(models[3:6], '+ eTIV_f09'))

dets=as.character(as.list(read.table(detsfile,header=F, sep = '\t'))[[1]])
outs=as.character(as.list(read.table(outsfile,header=F, sep = '\t'))[[1]])

dti = c("glb_mean_FA_f09", "cigulum_mean_FA_f09", "uncinate_mean_FA_f09", "glb_mean_MD_f09", "cigulum_mean_MD_f09", "uncinate_mean_MD_f09")
subcortical = c('hippocampus_vol_f09', 'amygdala_vol_f09')


#----------------------------------------
#Data Manipulation ------------------------

# dti data
imp_9y_dti <- mice::filter(imp_9y, dti_has_nii_f09 == "yes" & dti_man_qc_f09 != "unusable" )

# maternal diet quality tertiles
imp_9y_quartile <- imp_9y %>% 
  mice::complete(action = "long", include = T) 

# 1st quartile: 6.796, median 7.833, 3rd quartile: 8.875

imp_9y_quartile <- mutate(imp_9y_quartile, mdq_quartile = ntile(DietScore_pregnancy, 4))

imp_9y_quartile$mdq_quartile <- factor(imp_9y_quartile$mdq_quartile,levels = c(1,2,3, 4), 
                                      labels = c("Q1", "Q2", "Q3", "Q4"),
                                      ordered = F)  
imp_9y_quartile$mdq_quartile <- relevel(imp_9y_quartile$mdq_quartile, ref = "Q1")

table(imp_9y_quartile$mdq_quartile, useNA = "always")

# child diet quality tertile
# 1st qu 3.7, 3rd qu 5.4n

imp_9y_quartile <- mutate(imp_9y_quartile, cdq_tertile = ntile(DietScore_8y, 3))

imp_9y_quartile$cdq_tertile <- factor(imp_9y_quartile$cdq_tertile,levels = c(1,2,3), 
                                      labels = c("low", "mid", "high"),
                                      ordered = F)

table(imp_9y_quartile$cdq_tertile, useNA = "always")


#back to mids
imp_9y <- as.mids(imp_9y_quartile, .id = ".id")


## dti dataset


# maternal diet quality tertiles
imp_9y_dti_quartile <- imp_9y_dti %>% 
  mice::complete(action = "long", include = T) 

# 1st quartile: 6.7, 3rd quartile: 8.9

imp_9y_dti_quartile <- mutate(imp_9y_dti_quartile, mdq_quartile = ntile(DietScore_pregnancy, 4))

imp_9y_dti_quartile$mdq_quartile <- factor(imp_9y_dti_quartile$mdq_quartile,levels = c(1,2,3,4), 
                                     labels = c("Q1", "Q2", "Q3", "Q4"),
                                     ordered = F)  
imp_9y_dti_quartile$mdq_quartile <- relevel(imp_9y_dti_quartile$mdq_quartile, ref = "Q1")

table(imp_9y_dti_quartile$mdq_quartile, useNA = "always")

# child diet quality tertile
# 1st qu 3.7, 3rd qu 5.4

imp_9y_dti_quartile <- mutate(imp_9y_dti_quartile, cdq_tertile = ntile(DietScore_8y, 3))

imp_9y_dti_quartile$cdq_tertile <- factor(imp_9y_dti_quartile$cdq_tertile,levels = c(1,2,3), 
                                     labels = c("low", "mid", "high"),
                                     ordered = F)

table(imp_9y_dti_quartile$cdq_tertile, useNA = "always")


#back to mids
imp_9y_dti <- as.mids(imp_9y_dti_quartile, .id = ".id")


#----------------------------------------
#TABLE 1
dat_afimp <- complete(imp_9y, action = 'long', include = F)
dat_afimp$GENDER <- as.factor(dat_afimp$GENDER)

# var list
varlist <- c('gsi', 'DietScore_pregnancy', 'age_child_mri_f09', 'DietScore_8y',
                'edu', 'income_r', 'SMOKE_ALL', 'GENDER', 'ethm', 'breastfeeding', "parity", 'mdq_quartile', "cdq_tertile")
catvarlist <- c('edu', 'income_r',  'SMOKE_ALL', 'GENDER', 'ethm', 'breastfeeding', "parity", 
                'mdq_quartile', "cdq_tertile")


tblone <- CreateTableOne(data = dat_afimp, vars = varlist, factorVars = catvarlist, testNonNormal = kruskal.test)

#check if missing
summary(tblone)

#IQR
varlist_iqr <- c("gsi", "age_child_mri_f09")

tbl1 <- print(tblone, nonnormal = varlist_iqr, contDigits = 1, catDigits = 1, noSpaces = T, format = "fp")
xlsx::write.xlsx(tbl1, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\tableone_9y_)", analysis, analysis_date, '.xlsx', sep = ''))


#----------------------------------------
#REGRESSION MODELS - diet quality as continuous score ------------------------
#allocate output
dets1 = dets[1]

coefs <- as.data.frame(matrix(NA,nrow=1,ncol=7))
colnames(coefs) <- c("DateTime","Structure","Dietary patterns","Model","Beta","CI","P")
count <- 0

for (o in outs) {
  if (o %nin% subcortical & o %nin% dti) {
    for (d in dets1) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_9y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)

        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
      }
    }
  } else if (o %in% dti){
      for (d in dets1) {
        for (m in 1:length(models)) {
          count <- count + 1
          # Run regression
          fit <- imp_9y_dti %>%
            mice::complete("all") %>%
            lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
          res <- summary(pool(fit), conf.int = T)
          # Round values
          res <- res %>%
            mutate_at(c("p.value"), round, 4)

          # Save results
          coefs[count, 1] <- gsub(" ","_",Sys.time())
          coefs[count, 2] <- o
          coefs[count, 3] <- d
          coefs[count, 4] <- m
          coefs[count, 5] <- res$estimate[2]
          coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
          coefs[count, 7] <- res$p.value[2]
        }
      }
    } else {
    for (d in dets1) {
      for (m in 1:length(fmodels)) {
        count <- count + 1
        # Run regression
        fit <- imp_9y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', fmodels[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
      }
    }
   }
}

## The following syntax concerns converting a results file with beta, confidence interval(combined ci.lo and ci.hi)
## and p value from a LONG format to a WIDE format.

# Format table
# Empty template
mod_tbl <- as.data.frame(matrix(NA,nrow=1,ncol=20))
colnames(mod_tbl) <- c('Structures', "Dietary patterns",
                       'beta_mod1', '95ci_mod1', 'pvalue_mod1',
                       'beta_mod2', '95ci_mod2', 'pvalue_mod2',
                       'beta_mod3', '95ci_mod3', 'pvalue_mod3',
                       'beta_mod4', '95ci_mod4', 'pvalue_mod4',
                       'beta_mod5', '95ci_mod5', 'pvalue_mod5',
                       'beta_mod6', '95ci_mod6', 'pvalue_mod6')
count <- 0


for (o in outs) {
  for (d in dets1) {
    sep_coefs <- coefs %>%
      filter(Structure == o) %>%
      filter(`Dietary patterns` == d)
    count <- count + 1
    # Loop through each model.In this case, four models
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta[1]
    mod_tbl[count, 4] <- sep_coefs$CI[1]
    mod_tbl[count, 5] <- sep_coefs$P[1]
    mod_tbl[count, 6] <- sep_coefs$Beta[2]
    mod_tbl[count, 7] <- sep_coefs$CI[2]
    mod_tbl[count, 8] <- sep_coefs$P[2]
    mod_tbl[count, 9] <- sep_coefs$Beta[3]
    mod_tbl[count, 10] <- sep_coefs$CI[3]
    mod_tbl[count, 11] <- sep_coefs$P[3]
    mod_tbl[count, 12] <- sep_coefs$Beta[4]
    mod_tbl[count, 13] <- sep_coefs$CI[4]
    mod_tbl[count, 14] <- sep_coefs$P[4]
    mod_tbl[count, 15] <- sep_coefs$Beta[5]
    mod_tbl[count, 16] <- sep_coefs$CI[5]
    mod_tbl[count, 17] <- sep_coefs$P[5]
    mod_tbl[count, 18] <- sep_coefs$Beta[6]
    mod_tbl[count, 19] <- sep_coefs$CI[6]
    mod_tbl[count, 20] <- sep_coefs$P[6]
  }
}


write.xlsx(mod_tbl, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\mdq_9y_)", analysis, analysis_date, '.xlsx', sep = ''), row.names = FALSE, col.names = TRUE)


#----------------------------------------
#REGRESSION MODELS - diet quality as categories ------------------------
#allocate output
dets2 = dets[2]

coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("DateTime","Structure","Dietary patterns","Model","Beta_q2","CI_q2","P_q2", 
                     "Beta_q3","CI_q3","P_q3", "Beta_q4","CI_q4","P_q4")
count <- 0

for (o in outs) {
  if (o %nin% subcortical & o %nin% dti) {
    for (d in dets2) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_9y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
        coefs[count, 8] <- res$estimate[3]
        coefs[count, 9] <- paste(res$`2.5 %`[3], ',', ' ', res$`97.5 %`[3], sep = '')
        coefs[count, 10] <- res$p.value[3]
        coefs[count, 11] <- res$estimate[4]
        coefs[count, 12] <- paste(res$`2.5 %`[4], ',', ' ', res$`97.5 %`[4], sep = '')
        coefs[count, 13] <- res$p.value[4]
      }
    }
  } else if (o %in% dti){
    for (d in dets2) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_9y_dti %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("p.value"), round, 4)
        
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
        coefs[count, 8] <- res$estimate[3]
        coefs[count, 9] <- paste(res$`2.5 %`[3], ',', ' ', res$`97.5 %`[3], sep = '')
        coefs[count, 10] <- res$p.value[3]
        coefs[count, 11] <- res$estimate[4]
        coefs[count, 12] <- paste(res$`2.5 %`[4], ',', ' ', res$`97.5 %`[4], sep = '')
        coefs[count, 13] <- res$p.value[4]
      }
    }
  } else {
    for (d in dets2) {
      for (m in 1:length(fmodels)) {
        count <- count + 1
        # Run regression
        fit <- imp_9y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', fmodels[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
        coefs[count, 8] <- res$estimate[3]
        coefs[count, 9] <- paste(res$`2.5 %`[3], ',', ' ', res$`97.5 %`[3], sep = '')
        coefs[count, 10] <- res$p.value[3]
        coefs[count, 11] <- res$estimate[4]
        coefs[count, 12] <- paste(res$`2.5 %`[4], ',', ' ', res$`97.5 %`[4], sep = '')
        coefs[count, 13] <- res$p.value[4]
      }
    }
  }
}

## The following syntax concerns converting a results file with beta, confidence interval(combined ci.lo and ci.hi)
## and p value from a LONG format to a WIDE format.

# Format table
# Empty template
mod_tbl <- as.data.frame(matrix(NA,nrow=1,ncol=20))
colnames(mod_tbl) <- c('Structures', "Dietary patterns",
                       'beta_mod1', '95ci_mod1', 'pvalue_mod1',
                       'beta_mod2', '95ci_mod2', 'pvalue_mod2',
                       'beta_mod3', '95ci_mod3', 'pvalue_mod3',
                       'beta_mod4', '95ci_mod4', 'pvalue_mod4',
                       'beta_mod5', '95ci_mod5', 'pvalue_mod5',
                       'beta_mod6', '95ci_mod6', 'pvalue_mod6')
count <- 0


for (o in outs) {
  for (d in dets2) {
    sep_coefs <- coefs %>%
      filter(Structure == o)
    count <- count + 1
    # Loop through each model.In this case, four models
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta_q2[1]
    mod_tbl[count, 4] <- sep_coefs$CI_q2[1]
    mod_tbl[count, 5] <- sep_coefs$P_q2[1]
    mod_tbl[count, 6] <- sep_coefs$Beta_q2[2]
    mod_tbl[count, 7] <- sep_coefs$CI_q2[2]
    mod_tbl[count, 8] <- sep_coefs$P_q2[2]
    mod_tbl[count, 9] <- sep_coefs$Beta_q2[3]
    mod_tbl[count, 10] <- sep_coefs$CI_q2[3]
    mod_tbl[count, 11] <- sep_coefs$P_q2[3]
    mod_tbl[count, 12] <- sep_coefs$Beta_q2[4]
    mod_tbl[count, 13] <- sep_coefs$CI_q2[4]
    mod_tbl[count, 14] <- sep_coefs$P_q2[4]
    mod_tbl[count, 15] <- sep_coefs$Beta_q2[5]
    mod_tbl[count, 16] <- sep_coefs$CI_q2[5]
    mod_tbl[count, 17] <- sep_coefs$P_q2[5]
    mod_tbl[count, 18] <- sep_coefs$Beta_q2[6]
    mod_tbl[count, 19] <- sep_coefs$CI_q2[6]
    mod_tbl[count, 20] <- sep_coefs$P_q2[6]
    
    count <- count + 1
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta_q3[1]
    mod_tbl[count, 4] <- sep_coefs$CI_q3[1]
    mod_tbl[count, 5] <- sep_coefs$P_q3[1]
    mod_tbl[count, 6] <- sep_coefs$Beta_q3[2]
    mod_tbl[count, 7] <- sep_coefs$CI_q3[2]
    mod_tbl[count, 8] <- sep_coefs$P_q3[2]
    mod_tbl[count, 9] <- sep_coefs$Beta_q3[3]
    mod_tbl[count, 10] <- sep_coefs$CI_q3[3]
    mod_tbl[count, 11] <- sep_coefs$P_q3[3]
    mod_tbl[count, 12] <- sep_coefs$Beta_q3[4]
    mod_tbl[count, 13] <- sep_coefs$CI_q3[4]
    mod_tbl[count, 14] <- sep_coefs$P_q3[4]
    mod_tbl[count, 15] <- sep_coefs$Beta_q3[5]
    mod_tbl[count, 16] <- sep_coefs$CI_q3[5]
    mod_tbl[count, 17] <- sep_coefs$P_q3[5]
    mod_tbl[count, 18] <- sep_coefs$Beta_q3[6]
    mod_tbl[count, 19] <- sep_coefs$CI_q3[6]
    mod_tbl[count, 20] <- sep_coefs$P_q3[6]
    
    count <- count + 1
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta_q4[1]
    mod_tbl[count, 4] <- sep_coefs$CI_q4[1]
    mod_tbl[count, 5] <- sep_coefs$P_q4[1]
    mod_tbl[count, 6] <- sep_coefs$Beta_q4[2]
    mod_tbl[count, 7] <- sep_coefs$CI_q4[2]
    mod_tbl[count, 8] <- sep_coefs$P_q4[2]
    mod_tbl[count, 9] <- sep_coefs$Beta_q4[3]
    mod_tbl[count, 10] <- sep_coefs$CI_q4[3]
    mod_tbl[count, 11] <- sep_coefs$P_q4[3]
    mod_tbl[count, 12] <- sep_coefs$Beta_q4[4]
    mod_tbl[count, 13] <- sep_coefs$CI_q4[4]
    mod_tbl[count, 14] <- sep_coefs$P_q4[4]
    mod_tbl[count, 15] <- sep_coefs$Beta_q4[5]
    mod_tbl[count, 16] <- sep_coefs$CI_q4[5]
    mod_tbl[count, 17] <- sep_coefs$P_q4[5]
    mod_tbl[count, 18] <- sep_coefs$Beta_q4[6]
    mod_tbl[count, 19] <- sep_coefs$CI_q4[6]
    mod_tbl[count, 20] <- sep_coefs$P_q4[6]
  }
}


write.xlsx(mod_tbl, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\mdq_quartiles_9y_)", analysis, analysis_date, '.xlsx', sep = ''), row.names = FALSE, col.names = TRUE)


#-----------------------------------------------------------------------------
#13 years ------------------------
#Your data 
analysis = "MDP_brain"
afterimp_rdsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\data\imp_mdq_brain_2022-11-30.RData)"
modsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\MaternalDiet_Brain\model loop\dp_brain_13y.mods)"
detsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\MaternalDiet_Brain\model loop\dietarypatterns_mothers.dets)"


#out
outsfile = r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\syntax\MaternalDiet_Brain\model loop\volume_13y.outs)"


#Load libraries
packages <- c('tidyverse', 'mice', 'data.table', 'Hmisc', 'xlsx', "openxlsx")
invisible(lapply(packages, library, character.only = T))

#load data
fdata <- load(afterimp_rdsfile)
models = as.character(as.list(read.table(modsfile,header=F, sep = '\t'))[[1]])
#model 3 is the main model, model 4 + breastfeeding, model 5 + diet quality@8y, model 6 + both
fmodels = c(models[1:2], paste(models[3:6], '+ eTIV_f13'))

dets=as.character(as.list(read.table(detsfile,header=F, sep = '\t'))[[1]])
outs=as.character(as.list(read.table(outsfile,header=F, sep = '\t'))[[1]])

dti = c("glb_mean_FA_f13", "cigulum_mean_FA_f13", "uncinate_mean_FA_f13", "glb_mean_MD_f13", "cigulum_mean_MD_f13", "uncinate_mean_MD_f13")
subcortical = c('hippocampus_vol_f13', 'amygdala_vol_f13')


#----------------------------------------
#Data Manipulation ------------------------

# dti data
imp_13y_dti <- mice::filter(imp_13y, dti_has_nii_f13 == "yes" & dti_man_qc_f13 != "unusable" )

# maternal diet quality tertiles
imp_13y_quartile <- imp_13y %>% 
  mice::complete(action = "long", include = T) 

# 1st quartile: 6.796, median 7.833, 3rd quartile: 8.875

imp_13y_quartile <- mutate(imp_13y_quartile, mdq_quartile = ntile(DietScore_pregnancy, 4))

imp_13y_quartile$mdq_quartile <- factor(imp_13y_quartile$mdq_quartile,levels = c(1,2,3, 4), 
                                        labels = c("Q1", "Q2", "Q3", "Q4"),
                                        ordered = F)  
imp_13y_quartile$mdq_quartile <- relevel(imp_13y_quartile$mdq_quartile, ref = "Q1")

table(imp_13y_quartile$mdq_quartile, useNA = "always")

# child diet quality tertile
# 1st qu 3.7, 3rd qu 5.4n

imp_13y_quartile <- mutate(imp_13y_quartile, cdq_tertile = ntile(DietScore_8y, 3))

imp_13y_quartile$cdq_tertile <- factor(imp_13y_quartile$cdq_tertile,levels = c(1,2,3), 
                                       labels = c("low", "mid", "high"),
                                       ordered = F)

table(imp_13y_quartile$cdq_tertile, useNA = "always")


#back to mids
imp_13y <- as.mids(imp_13y_quartile, .id = ".id")


## dti dataset


# maternal diet quality tertiles
imp_13y_dti_quartile <- imp_13y_dti %>% 
  mice::complete(action = "long", include = T) 

# 1st quartile: 6.7, 3rd quartile: 8.9

imp_13y_dti_quartile <- mutate(imp_13y_dti_quartile, mdq_quartile = ntile(DietScore_pregnancy, 4))

imp_13y_dti_quartile$mdq_quartile <- factor(imp_13y_dti_quartile$mdq_quartile,levels = c(1,2,3,4), 
                                            labels = c("Q1", "Q2", "Q3", "Q4"),
                                            ordered = F)  
imp_13y_dti_quartile$mdq_quartile <- relevel(imp_13y_dti_quartile$mdq_quartile, ref = "Q1")

table(imp_13y_dti_quartile$mdq_quartile, useNA = "always")

# child diet quality tertile
# 1st qu 3.7, 3rd qu 5.4

imp_13y_dti_quartile <- mutate(imp_13y_dti_quartile, cdq_tertile = ntile(DietScore_8y, 3))

imp_13y_dti_quartile$cdq_tertile <- factor(imp_13y_dti_quartile$cdq_tertile,levels = c(1,2,3), 
                                           labels = c("low", "mid", "high"),
                                           ordered = F)

table(imp_13y_dti_quartile$cdq_tertile, useNA = "always")


#back to mids
imp_13y_dti <- as.mids(imp_13y_dti_quartile, .id = ".id")


#----------------------------------------
#TABLE 1
dat_afimp <- complete(imp_13y, action = 'long', include = F)
dat_afimp$GENDER <- as.factor(dat_afimp$GENDER)

# var list
varlist <- c('gsi', 'DietScore_pregnancy', 'age_child_mri_f13', 'DietScore_8y',
             'edu', 'income_r', 'SMOKE_ALL', 'GENDER', 'ethm', 'breastfeeding', "parity", 'mdq_quartile', "cdq_tertile")
catvarlist <- c('edu', 'income_r',  'SMOKE_ALL', 'GENDER', 'ethm', 'breastfeeding', "parity", 
                'mdq_quartile', "cdq_tertile")


tblone <- CreateTableOne(data = dat_afimp, vars = varlist, factorVars = catvarlist, testNonNormal = kruskal.test)

#check if missing
summary(tblone)

#IQR
varlist_iqr <- c("gsi", "age_child_mri_f13")

tbl1 <- print(tblone, nonnormal = varlist_iqr, contDigits = 1, catDigits = 1, noSpaces = T, format = "fp")
xlsx::write.xlsx(tbl1, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\tableone_13y_)", analysis, analysis_date, '.xlsx', sep = ''))


#----------------------------------------
#REGRESSION MODELS - diet quality as continuous score ------------------------
#allocate output
dets1 = dets[1]

coefs <- as.data.frame(matrix(NA,nrow=1,ncol=7))
colnames(coefs) <- c("DateTime","Structure","Dietary patterns","Model","Beta","CI","P")
count <- 0

for (o in outs) {
  if (o %nin% subcortical & o %nin% dti) {
    for (d in dets1) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_13y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
      }
    }
  } else if (o %in% dti){
    for (d in dets1) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_13y_dti %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("p.value"), round, 4)
        
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
      }
    }
  } else {
    for (d in dets1) {
      for (m in 1:length(fmodels)) {
        count <- count + 1
        # Run regression
        fit <- imp_13y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', fmodels[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
      }
    }
  }
}

## The following syntax concerns converting a results file with beta, confidence interval(combined ci.lo and ci.hi)
## and p value from a LONG format to a WIDE format.

# Format table
# Empty template
mod_tbl <- as.data.frame(matrix(NA,nrow=1,ncol=20))
colnames(mod_tbl) <- c('Structures', "Dietary patterns",
                       'beta_mod1', '95ci_mod1', 'pvalue_mod1',
                       'beta_mod2', '95ci_mod2', 'pvalue_mod2',
                       'beta_mod3', '95ci_mod3', 'pvalue_mod3',
                       'beta_mod4', '95ci_mod4', 'pvalue_mod4',
                       'beta_mod5', '95ci_mod5', 'pvalue_mod5',
                       'beta_mod6', '95ci_mod6', 'pvalue_mod6')
count <- 0


for (o in outs) {
  for (d in dets1) {
    sep_coefs <- coefs %>%
      filter(Structure == o) %>%
      filter(`Dietary patterns` == d)
    count <- count + 1
    # Loop through each model.In this case, four models
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta[1]
    mod_tbl[count, 4] <- sep_coefs$CI[1]
    mod_tbl[count, 5] <- sep_coefs$P[1]
    mod_tbl[count, 6] <- sep_coefs$Beta[2]
    mod_tbl[count, 7] <- sep_coefs$CI[2]
    mod_tbl[count, 8] <- sep_coefs$P[2]
    mod_tbl[count, 9] <- sep_coefs$Beta[3]
    mod_tbl[count, 10] <- sep_coefs$CI[3]
    mod_tbl[count, 11] <- sep_coefs$P[3]
    mod_tbl[count, 12] <- sep_coefs$Beta[4]
    mod_tbl[count, 13] <- sep_coefs$CI[4]
    mod_tbl[count, 14] <- sep_coefs$P[4]
    mod_tbl[count, 15] <- sep_coefs$Beta[5]
    mod_tbl[count, 16] <- sep_coefs$CI[5]
    mod_tbl[count, 17] <- sep_coefs$P[5]
    mod_tbl[count, 18] <- sep_coefs$Beta[6]
    mod_tbl[count, 19] <- sep_coefs$CI[6]
    mod_tbl[count, 20] <- sep_coefs$P[6]
  }
}


write.xlsx(mod_tbl, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\mdq_13y_)", analysis, analysis_date, '.xlsx', sep = ''), row.names = FALSE, col.names = TRUE)


#----------------------------------------
#REGRESSION MODELS - diet quality as categories ------------------------
#allocate output
dets2 = dets[2]

coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("DateTime","Structure","Dietary patterns","Model","Beta_q2","CI_q2","P_q2", 
                     "Beta_q3","CI_q3","P_q3", "Beta_q4","CI_q4","P_q4")
count <- 0

for (o in outs) {
  if (o %nin% subcortical & o %nin% dti) {
    for (d in dets2) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_13y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
        coefs[count, 8] <- res$estimate[3]
        coefs[count, 9] <- paste(res$`2.5 %`[3], ',', ' ', res$`97.5 %`[3], sep = '')
        coefs[count, 10] <- res$p.value[3]
        coefs[count, 11] <- res$estimate[4]
        coefs[count, 12] <- paste(res$`2.5 %`[4], ',', ' ', res$`97.5 %`[4], sep = '')
        coefs[count, 13] <- res$p.value[4]
      }
    }
  } else if (o %in% dti){
    for (d in dets2) {
      for (m in 1:length(models)) {
        count <- count + 1
        # Run regression
        fit <- imp_13y_dti %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', models[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("p.value"), round, 4)
        
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
        coefs[count, 8] <- res$estimate[3]
        coefs[count, 9] <- paste(res$`2.5 %`[3], ',', ' ', res$`97.5 %`[3], sep = '')
        coefs[count, 10] <- res$p.value[3]
        coefs[count, 11] <- res$estimate[4]
        coefs[count, 12] <- paste(res$`2.5 %`[4], ',', ' ', res$`97.5 %`[4], sep = '')
        coefs[count, 13] <- res$p.value[4]
      }
    }
  } else {
    for (d in dets2) {
      for (m in 1:length(fmodels)) {
        count <- count + 1
        # Run regression
        fit <- imp_13y %>%
          mice::complete("all") %>%
          lapply(lm, formula = paste(o, '~', d, '+', fmodels[m], sep = ''))
        res <- summary(pool(fit), conf.int = T)
        # Round values
        res <- res %>%
          mutate_at(c("estimate", "2.5 %", "97.5 %"), round, 2) %>%
          mutate_at(c("p.value"), round, 4)
        # Save results
        coefs[count, 1] <- gsub(" ","_",Sys.time())
        coefs[count, 2] <- o
        coefs[count, 3] <- d
        coefs[count, 4] <- m
        coefs[count, 5] <- res$estimate[2]
        coefs[count, 6] <- paste(res$`2.5 %`[2], ',', ' ', res$`97.5 %`[2], sep = '')
        coefs[count, 7] <- res$p.value[2]
        coefs[count, 8] <- res$estimate[3]
        coefs[count, 9] <- paste(res$`2.5 %`[3], ',', ' ', res$`97.5 %`[3], sep = '')
        coefs[count, 10] <- res$p.value[3]
        coefs[count, 11] <- res$estimate[4]
        coefs[count, 12] <- paste(res$`2.5 %`[4], ',', ' ', res$`97.5 %`[4], sep = '')
        coefs[count, 13] <- res$p.value[4]
      }
    }
  }
}

## The following syntax concerns converting a results file with beta, confidence interval(combined ci.lo and ci.hi)
## and p value from a LONG format to a WIDE format.

# Format table
# Empty template
mod_tbl <- as.data.frame(matrix(NA,nrow=1,ncol=20))
colnames(mod_tbl) <- c('Structures', "Dietary patterns",
                       'beta_mod1', '95ci_mod1', 'pvalue_mod1',
                       'beta_mod2', '95ci_mod2', 'pvalue_mod2',
                       'beta_mod3', '95ci_mod3', 'pvalue_mod3',
                       'beta_mod4', '95ci_mod4', 'pvalue_mod4',
                       'beta_mod5', '95ci_mod5', 'pvalue_mod5',
                       'beta_mod6', '95ci_mod6', 'pvalue_mod6')
count <- 0


for (o in outs) {
  for (d in dets2) {
    sep_coefs <- coefs %>%
      filter(Structure == o)
    count <- count + 1
    # Loop through each model.In this case, four models
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta_q2[1]
    mod_tbl[count, 4] <- sep_coefs$CI_q2[1]
    mod_tbl[count, 5] <- sep_coefs$P_q2[1]
    mod_tbl[count, 6] <- sep_coefs$Beta_q2[2]
    mod_tbl[count, 7] <- sep_coefs$CI_q2[2]
    mod_tbl[count, 8] <- sep_coefs$P_q2[2]
    mod_tbl[count, 9] <- sep_coefs$Beta_q2[3]
    mod_tbl[count, 10] <- sep_coefs$CI_q2[3]
    mod_tbl[count, 11] <- sep_coefs$P_q2[3]
    mod_tbl[count, 12] <- sep_coefs$Beta_q2[4]
    mod_tbl[count, 13] <- sep_coefs$CI_q2[4]
    mod_tbl[count, 14] <- sep_coefs$P_q2[4]
    mod_tbl[count, 15] <- sep_coefs$Beta_q2[5]
    mod_tbl[count, 16] <- sep_coefs$CI_q2[5]
    mod_tbl[count, 17] <- sep_coefs$P_q2[5]
    mod_tbl[count, 18] <- sep_coefs$Beta_q2[6]
    mod_tbl[count, 19] <- sep_coefs$CI_q2[6]
    mod_tbl[count, 20] <- sep_coefs$P_q2[6]
    
    count <- count + 1
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta_q3[1]
    mod_tbl[count, 4] <- sep_coefs$CI_q3[1]
    mod_tbl[count, 5] <- sep_coefs$P_q3[1]
    mod_tbl[count, 6] <- sep_coefs$Beta_q3[2]
    mod_tbl[count, 7] <- sep_coefs$CI_q3[2]
    mod_tbl[count, 8] <- sep_coefs$P_q3[2]
    mod_tbl[count, 9] <- sep_coefs$Beta_q3[3]
    mod_tbl[count, 10] <- sep_coefs$CI_q3[3]
    mod_tbl[count, 11] <- sep_coefs$P_q3[3]
    mod_tbl[count, 12] <- sep_coefs$Beta_q3[4]
    mod_tbl[count, 13] <- sep_coefs$CI_q3[4]
    mod_tbl[count, 14] <- sep_coefs$P_q3[4]
    mod_tbl[count, 15] <- sep_coefs$Beta_q3[5]
    mod_tbl[count, 16] <- sep_coefs$CI_q3[5]
    mod_tbl[count, 17] <- sep_coefs$P_q3[5]
    mod_tbl[count, 18] <- sep_coefs$Beta_q3[6]
    mod_tbl[count, 19] <- sep_coefs$CI_q3[6]
    mod_tbl[count, 20] <- sep_coefs$P_q3[6]
    
    count <- count + 1
    mod_tbl[count, 1] <- o
    mod_tbl[count, 2] <- d
    mod_tbl[count, 3] <- sep_coefs$Beta_q4[1]
    mod_tbl[count, 4] <- sep_coefs$CI_q4[1]
    mod_tbl[count, 5] <- sep_coefs$P_q4[1]
    mod_tbl[count, 6] <- sep_coefs$Beta_q4[2]
    mod_tbl[count, 7] <- sep_coefs$CI_q4[2]
    mod_tbl[count, 8] <- sep_coefs$P_q4[2]
    mod_tbl[count, 9] <- sep_coefs$Beta_q4[3]
    mod_tbl[count, 10] <- sep_coefs$CI_q4[3]
    mod_tbl[count, 11] <- sep_coefs$P_q4[3]
    mod_tbl[count, 12] <- sep_coefs$Beta_q4[4]
    mod_tbl[count, 13] <- sep_coefs$CI_q4[4]
    mod_tbl[count, 14] <- sep_coefs$P_q4[4]
    mod_tbl[count, 15] <- sep_coefs$Beta_q4[5]
    mod_tbl[count, 16] <- sep_coefs$CI_q4[5]
    mod_tbl[count, 17] <- sep_coefs$P_q4[5]
    mod_tbl[count, 18] <- sep_coefs$Beta_q4[6]
    mod_tbl[count, 19] <- sep_coefs$CI_q4[6]
    mod_tbl[count, 20] <- sep_coefs$P_q4[6]
  }
}


write.xlsx(mod_tbl, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\mdq_quartiles_13y_)", analysis, analysis_date, '.xlsx', sep = ''), row.names = FALSE, col.names = TRUE)


# test for fun ------------------------

a = complete(imp_9y, n = 10)
fit <- lm(formula = SupraTentorialVol_f09 ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, data = a)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(fit)

# Linear mixed model ------------------------
library(nlme)
library(reshape2)

# n = 1057
lme_dt = imp_13y %>% mice::complete(action = "long", include = T) %>% 
  filter(!is.na(genr_tbv_f09)) %>% 
  as.mids()
a = complete(lme_dt, n = 1)
a$IDC = factor(a$IDC)
dat_long <- melt(a,
                 id.vars = c("IDC", 
                              "DietScore_pregnancy", "mdq_quartile", "Energy_kcal_pregnancy", 
                              "AGE_M_v2", "DietScore_8y", "gsi", 
                              "age_child_mri_f13", 
                              "GENDER", "edu", 
                              "income_r",
                              "ethm", "SMOKE_ALL","breastfeeding", 
                               "WISC13_FSIQ"),
                 measure.vars = c("genr_tbv_f09", "genr_tbv_f13"),
                 variable.name = "time",
                 value.name = "tbv")

xyplot(tbv ~ time | IDC,
       panel = function (x, y, ...) {
         panel.xyplot(x, y, type = "p", col = 1, ...)
         panel.loess(x, y, col = 2, lwd = 2, span = 0.8)
       }, data = dat_long, layout = c(4, 4), as.table = TRUE, 
       xlab = "time", ylab = "total brain volume")

lme_mod <- lme(tbv ~ time + DietScore_pregnancy + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, dat_long, random = ~ 1 | IDC )
summary(lme_mod)


dat_long <- melt(a,
                 id.vars = c("IDC", 
                             "DietScore_pregnancy", "mdq_quartile", "Energy_kcal_pregnancy", 
                             "AGE_M_v2", "DietScore_8y", "gsi", 
                             "age_child_mri_f13", 
                             "GENDER", "edu", 
                             "income_r",
                             "ethm", "SMOKE_ALL","breastfeeding", 
                             "WISC13_FSIQ"),
                 measure.vars = c("CortexVol_f13", "CortexVol_f09"),
                 variable.name = "time",
                 value.name = "gm")

xyplot(tbv ~ time | IDC,
       panel = function (x, y, ...) {
         panel.xyplot(x, y, type = "p", col = 1, ...)
         panel.loess(x, y, col = 2, lwd = 2, span = 0.8)
       }, data = dat_long, layout = c(4, 4), as.table = TRUE, 
       xlab = "time", ylab = "total brain volume")

lme_mod <- lme(gm ~ time + mdq_quartile + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, dat_long, random = ~ 1 | IDC )
summary(lme_mod)

# Mediation analysis ------------------------
# maternal diet quality and white matter volume
library(mediation)

# mdq - wm - iq -------
analysis = 'mDQ_wm_IQ'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(CerebralWhiteMatterVol_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ CerebralWhiteMatterVol_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "CerebralWhiteMatterVol_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)


# mdq - gm - iq -------
analysis = 'mDQ_gm_IQ'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(CortexVol_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ CortexVol_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "CortexVol_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\mediation_)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)

# mdq - subcortgray - iq -------
analysis = 'mDQ_tbv_IQ'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(SubCortGrayVol_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ SubCortGrayVol_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "SubCortGrayVol_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)



# mdq - supratentorial - iq -------
analysis = 'mDQ_sup_IQ'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(SupraTentorialVol_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ SupraTentorialVol_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "SupraTentorialVol_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)

# mdq - tbv - iq -------
analysis = 'mediation_mDQ_tbv_IQ'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(genr_tbv_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ genr_tbv_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "genr_tbv_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)


# mdq - tbv - iq only adjusted for breastfeeding -------

analysis = 'mediation_mDQ_tbv_IQ_adjbreastfeeding'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(genr_tbv_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ genr_tbv_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "genr_tbv_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)


# mdq - tbv - iq only adjusted for child diet quality -------

analysis = 'mediation_mDQ_tbv_IQ_adjcdq8'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(genr_tbv_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ genr_tbv_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "genr_tbv_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)


# mdq - tbv - iq adjusted for child diet quality and breastfeeding -------

analysis = 'mediation_mDQ_tbv_IQ_adjbfcdq8'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(genr_tbv_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding + DietScore_8y, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ genr_tbv_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding + DietScore_8y,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "genr_tbv_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)


# mdq - wm - iq adjusted for child diet quality and breastfeeding -------

analysis = 'mediation_mDQ_wm_IQ_adjbfcdq8'
# d.avg is ACME, z.avg is ADE, n.avg is prop.mediated, tau.coef is total effect
coefs <- as.data.frame(matrix(NA,nrow=1,ncol=13))
colnames(coefs) <- c("n", "ACME", "ACME.se", "ACME.p", "ADE", "ADE.se", "ADE.p", "TE", "TE.se", "TE.p",
                     "prop", "prop.se", "prop.p")
count <- 0

for (i in 1:10) {
  count <- count + 1
  dt <- complete(imp_9y, n = i)
  medmodel <- glm(CerebralWhiteMatterVol_f09  ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding + DietScore_8y, 
                  family = "gaussian", data = dt)
  outmodel <- glm(WISC13_FSIQ ~ CerebralWhiteMatterVol_f09  + DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding + DietScore_8y,
                  family = "gaussian", data = dt)
  med <- mediate(model.m = medmodel, model.y = outmodel, 
                 treat = "DietScore_pregnancy", mediator = "CerebralWhiteMatterVol_f09", data = dt)
  
  # Save results
  coefs[count, 1] <- i
  coefs[count, 2] <- med$d.avg
  coefs[count, 3] <- abs((med$d.avg.ci[1] - med$d.avg.ci[2])/3.92)
  coefs[count, 4] <- med$d.avg.p
  coefs[count, 5] <- med$z.avg
  coefs[count, 6] <- abs((med$z.avg.ci[1] - med$z.avg.ci[2])/3.92)
  coefs[count, 7] <- med$z.avg.p
  coefs[count, 8] <- med$tau.coef
  coefs[count, 9] <- abs((med$tau.ci[1] - med$tau.ci[2])/3.92)
  coefs[count, 10] <- med$tau.p
  coefs[count, 11] <- med$n.avg
  coefs[count, 12] <- abs((med$n.avg.ci[1] - med$n.avg.ci[2])/3.92)
  coefs[count, 13] <- med$n.avg.p
}

# pooled results
pool_res <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(pool_res) <- c("effect", "estimate", "ci.lo", "ci.hi", "p")

a <- mitml::testEstimates(qhat=coefs$ACME, uhat=coefs$ACME.se^2)$estimates
pool_res[1,1] <- c("ACME")
pool_res[1,2] <- c(a[1])
pool_res[1,3] <- c(a[1]-1.96*a[2])
pool_res[1,4] <- c(a[1]+1.96*a[2])
pool_res[1,5] <- c(a[5])

b <- mitml::testEstimates(qhat=coefs$ADE, uhat=coefs$ADE.se^2)$estimates
pool_res[2,1] <- c("ADE")
pool_res[2,2] <- c(b[1])
pool_res[2,3] <- c(b[1]-1.96*b[2])
pool_res[2,4] <- c(b[1]+1.96*b[2])
pool_res[2,5] <- c(b[5])

c <- mitml::testEstimates(qhat=coefs$TE, uhat=coefs$TE.se^2)$estimates
pool_res[3,1] <- c("TE")
pool_res[3,2] <- c(c[1])
pool_res[3,3] <- c(c[1]-1.96*c[2])
pool_res[3,4] <- c(c[1]+1.96*c[2])
pool_res[3,5] <- c(c[5])

write.xlsx(pool_res, file = paste(r"(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\results\)", analysis, '.xlsx', sep = ''), rowNames = FALSE, colNames = TRUE)


# multiple mechanisms
dt <- complete(imp_9y, n = 1)
Xnames <- c("breastfeeding","age_child_mri_f09", "GENDER", "AGE_M_v2",  "edu",  "income_r",  "ethm",  "SMOKE_ALL",  "gsi", "Energy_kcal_pregnancy")
m.med <- multimed(outcome = "WISC13_FSIQ", med.main = "genr_tbv_f09", 
                  med.alt = "breastfeeding",
                  treat = "DietScore_pregnancy", covariates = Xnames, data = dt)
summary(m.med)

m.med <- multimed(outcome = "WISC13_FSIQ", med.main = "genr_tbv_f09", 
                  med.alt = "DietScore_8y",
                  treat = "DietScore_pregnancy", covariates = Xnames, data = dt)

summary(m.med)
plot(m.med, type = "point")

medmodel <- glm(genr_tbv_f09  ~ breastfeeding + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding + DietScore_8y, 
                family = "gaussian", data = dt)
outmodel <- glm(WISC13_FSIQ ~ genr_tbv_f09  + breastfeeding + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + breastfeeding + DietScore_8y,
                family = "gaussian", data = dt)
med <- mediate(model.m = medmodel, model.y = outmodel, 
               treat = "breastfeeding", mediator = "genr_tbv_f09", data = dt)
summary(med)
plot(med, type = "point")

# save image -------
save.image(file = paste(r'(V:\HomeDir\044073(J. Mou)\Projects\4 Maternal diet and brain morphology\data\analysis_)', analysis, '.RData', sep = ''))
