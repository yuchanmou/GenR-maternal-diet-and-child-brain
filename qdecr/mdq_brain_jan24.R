#Libraries
library(QDECR)
library(mice)

dsImp <- load("/home/r044073/maternal_diet/data/imp_mdq_brain_2024-02-01.RData")



# MATERNAL DIET QUALITY----------------------------------------------------------------

#Specify output directory
output_dir <- "/home/r044073/maternal_diet/year9"


#THICKNESS
hemis <- c("lh", "rh")
for(hemi in hemis){
  
#Model 1 main model

  dq1_thickness_model1 <- qdecr_fastlm(qdecr_thickness ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                  data = imp_9y, 
                  id = "folders_f09", 
                  hemi = hemi, 
                  dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr/",
                  clobber = T, 
                  n_cores = 8, 
                  chunk_size = 500,
                  dir_out = output_dir,
                  project = "dq1_model1")

#Model 2 main model + breastfeeding + diet quality@8y
  dq1_thickness_model1 <- qdecr_fastlm(qdecr_thickness ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                  data = imp_9y, 
                  id = "folders_f09", 
                  hemi = hemi, 
                  dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                  clobber = T, 
                  n_cores = 8, 
                  chunk_size = 500,
                  dir_out = output_dir,
                  project = "dq1_model2")
}


# SURFACE AREA
for(hemi in hemis){
  #Model 1
  dq1_area_model1 <- qdecr_fastlm(qdecr_area.pial ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                    data = imp_9y, 
                    id = "folders_f09", 
                    hemi = hemi, 
                    dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                    clobber = T, 
                    n_cores = 8, 
                    chunk_size = 500,
                    dir_out = output_dir,
                    project = "dq1_model1")
  
  #Model 2
  dq1_area_model1 <- qdecr_fastlm(qdecr_area.pial ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                    data = imp_9y, 
                    id = "folders_f09", 
                    hemi = hemi, 
                    dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                    clobber = T, 
                    n_cores = 8, 
                    chunk_size = 500,
                    dir_out = output_dir,
                    project = "dq1_model2")
}

# GYRIFICATION
for(hemi in hemis){
  
  #Model 1
  dq1_gyri_model1 <- qdecr_fastlm(qdecr_pial_lgi ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                    data = imp_9y, 
                    id = "folders_f09", 
                    hemi = hemi, 
                    dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                    clobber = T, 
                    n_cores = 8, 
                    chunk_size = 500,
                    dir_out = output_dir,
                    project = "dq1_model1")
  
  #Model 2
  dq1_gyri_model2 <- qdecr_fastlm(qdecr_pial_lgi ~ DietScore_pregnancy + age_child_mri_f09 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                    data = imp_9y, 
                    id = "folders_f09", 
                    hemi = hemi, 
                    dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                    clobber = T, 
                    n_cores = 8, 
                    chunk_size = 500,
                    dir_out = output_dir,
                    project = "dq1_model2")
}


#Specify output directory
output_dir <- "/home/r044073/maternal_diet/year13"


#THICKNESS
hemis <- c("lh", "rh")
for(hemi in hemis){
  
  #Model 1 main model
  
  dq1_thickness_model1 <- qdecr_fastlm(qdecr_thickness ~ DietScore_pregnancy + age_child_mri_f13 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                                       data = imp_13y, 
                                       id = "folders_f13", 
                                       hemi = hemi, 
                                       dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                                       clobber = T, 
                                       n_cores = 8, 
                                       chunk_size = 500,
                                       dir_out = output_dir,
                                       project = "dq1_model1")
  
  #Model 2 main model + breastfeeding + diet quality@8y
  dq1_thickness_model1 <- qdecr_fastlm(qdecr_thickness ~ DietScore_pregnancy + age_child_mri_f13 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                                       data = imp_13y, 
                                       id = "folders_f13", 
                                       hemi = hemi, 
                                       dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                                       clobber = T, 
                                       n_cores = 8, 
                                       chunk_size = 500,
                                       dir_out = output_dir,
                                       project = "dq1_model2")
}


# SURFACE AREA
for(hemi in hemis){
  #Model 1
  dq1_area_model1 <- qdecr_fastlm(qdecr_area.pial ~ DietScore_pregnancy + age_child_mri_f13 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                                  data = imp_13y, 
                                  id = "folders_f13", 
                                  hemi = hemi, 
                                  dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                                  clobber = T, 
                                  n_cores = 8, 
                                  chunk_size = 500,
                                  dir_out = output_dir,
                                  project = "dq1_model1")
  
  #Model 2
  dq1_area_model1 <- qdecr_fastlm(qdecr_area.pial ~ DietScore_pregnancy + age_child_mri_f13 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                                  data = imp_13y, 
                                  id = "folders_f13", 
                                  hemi = hemi, 
                                  dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                                  clobber = T, 
                                  n_cores = 8, 
                                  chunk_size = 500,
                                  dir_out = output_dir,
                                  project = "dq1_model2")
}

# GYRIFICATION
for(hemi in hemis){
  
  #Model 1
  dq1_gyri_model1 <- qdecr_fastlm(qdecr_pial_lgi ~ DietScore_pregnancy + age_child_mri_f13 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy,
                                  data = imp_13y, 
                                  id = "folders_f13", 
                                  hemi = hemi, 
                                  dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                                  clobber = T, 
                                  n_cores = 8, 
                                  chunk_size = 500,
                                  dir_out = output_dir,
                                  project = "dq1_model1")
  
  #Model 2
  dq1_gyri_model2 <- qdecr_fastlm(qdecr_pial_lgi ~ DietScore_pregnancy + age_child_mri_f13 + GENDER + AGE_M_v2 + edu + income_r + ethm + SMOKE_ALL + gsi + Energy_kcal_pregnancy + DietScore_8y,
                                  data = imp_13y, 
                                  id = "folders_f13", 
                                  hemi = hemi, 
                                  dir_subj = "/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/qdecr", 
                                  clobber = T, 
                                  n_cores = 8, 
                                  chunk_size = 500,
                                  dir_out = output_dir,
                                  project = "dq1_model2")
}