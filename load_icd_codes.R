
spine_codes_df <- fread("icd10_codes_2022_updated_dec_21.csv")

# spine_sections <- c("msk", "tumor", "congenital", "trauma")

spine_sections <- c(unique(spine_codes_df$section))

spine_category_labels <- c("Degen/Inflammatory" = "msk",
                           "Deformity" = "deformity",
                           "Trauma" = "trauma", 
                           "Tumor" = "tumor", 
                           "Infection" = "infection",
                           "Congenital" = "congenital", 
                           "Other Neurological Diseases" = "other_neuro_conditions")

spine_region_labels <- c("Sacral" =  'sacral',
                         "Lumbosacral" ='lumbosacral',
                         "Lumbar" = 'lumbar', 
                         "Thoracolumbar" = 'thoracolumbar',
                         "Thoracic" = 'thoracic',
                         "Cervicothoraic" = 'cervicothoracic', 
                         "Cervical" ='cervical',
                         "O-C2" = 'occipito-atlanto-axial'
)

jh_filter_icd_codes_generate_vector_function <- function(section_input, spine_region_input, age = 999){
  
  if(age != 999){
    if(age >25){
      filtered_df <- spine_codes_df %>%
        filter(section %in% section_input) %>%
        filter(spine_region %in% spine_region_input | spine_region == "unspecified") %>%
        filter(str_detect(str_to_lower(diagnosis), "juvenile|infantile|adolescent", negate = TRUE))
    }else{
      filtered_df <- spine_codes_df %>%
        filter(section %in% section_input) %>%
        filter(spine_region %in% spine_region_input | spine_region == "unspecified")  
    }
  }else{
    filtered_df <- spine_codes_df %>%
      filter(section %in% section_input) %>%
      filter(spine_region %in% spine_region_input | spine_region == "unspecified") 
  }
  
  diagnosis_list <- purrr::discard(list("Degen/Inflammatory" = (filtered_df %>% filter(section == "msk"))$diagnosis,
                                        "Deformity" = (filtered_df %>% filter(section == "deformity"))$diagnosis, 
                                        "Trauma" = (filtered_df %>% filter(section == "trauma"))$diagnosis, 
                                        "Tumor" = (filtered_df %>% filter(section == "tumor"))$diagnosis, 
                                        "Infection" = (filtered_df %>% filter(section == "infection"))$diagnosis,
                                        "Congenital" = (filtered_df %>% filter(section == "congenital"))$diagnosis, 
                                        "Other Neurological Diseases" = (filtered_df %>% filter(section == "other_neuro_conditions"))$diagnosis),
                                   .p = ~ length(.x) <1)
  
  return(diagnosis_list)
}


jh_determine_if_section_dx_function <- function(diagnosis_vector, section_to_determine){
  if(length(diagnosis_vector)>0){
    any(diagnosis_vector %in% (spine_codes_df %>% filter(section == section_to_determine))$diagnosis)
  }else{
    FALSE
  }
}
jh_add_codes_to_diagnosis_function <- function(diagnosis_vector){
  codes_df <- tibble(diagnosis = diagnosis_vector) %>%
    left_join(spine_codes_df) %>%
    mutate(diagnosis_codes = paste(diagnosis, " (", icd_10_code, ")", sep = ""))
  
  return(codes_df$diagnosis_codes)
}