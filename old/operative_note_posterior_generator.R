identify_cranial_caudal_interspace_body_list_function <- function(level){
  # segment_or_interspace_start = "segment"){
  cranial_interspace_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = if_else(str_detect(level, "-"), vertebral_number - 1, vertebral_number - 0.5)) %>%
    # mutate(vertebral_number = vertebral_number - 0.5) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  cranial_level_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = if_else(str_detect(level, "-"), vertebral_number - 0.5, vertebral_number - 1)) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  caudal_interspace_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = if_else(str_detect(level, "-"), vertebral_number + 1, vertebral_number + 0.5)) %>%
    # mutate(vertebral_number = vertebral_number - 0.5) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  caudal_level_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = if_else(str_detect(level, "-"), vertebral_number + 0.5, vertebral_number + 1)) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  


  
  return(list(cranial_interspace = cranial_interspace_df$level[[1]],
              cranial_level = cranial_level_df$level[[1]],
              caudal_interspace = caudal_interspace_df$level[[1]],
              caudal_level = caudal_level_df$level[[1]]
  ))

}


op_note_procedure_category_function <- function(object){
  procedure_category <- case_when(
    object == "cement_augmentation" ~ "vertebral cement augmentation",
    object == "laminar_downgoing_hook" ~ "posterior spinal instrumentation",
    object == "laminar_upgoing_hook" ~ "posterior spinal instrumentation",
    object == "lateral_mass_screw" ~ "posterior spinal instrumentation",
    object == "occipital_screw" ~ "occiput instrumentation",
    object == "pars_screw" ~ "posterior spinal instrumentation",
    object == "pedicle_hook" ~ "posterior spinal instrumentation",
    object == "pedicle_screw" ~ "posterior spinal instrumentation",
    object == "laminar_downgoing_hook" ~ "posterior spinal instrumentation",
    object == "sublaminar_wire" ~ "posterior spinal instrumentation",
    object == "tp_hook" ~ "posterior spinal instrumentation",
    object == "translaminar_screw" ~ "posterior spinal instrumentation",
    object == "transarticular_screw" ~ "posterior spinal instrumentation",
    object == "pelvic_screw" ~ "pelvic instrumentation",
    object == "tether" ~ "spinous process posterior tethering/wiring",
    object == "costovertebral_approach" ~ "decompression using a costovertebral approach",
    object == "revision_costovertebral_approach" ~ "reexploration with revision decompression using a costovertebral approach",
    object == "transpedicular_approach" ~ "decompression using a transpedicular approach",
    object == "revision_transpedicular_approach" ~ "reexploration with revision decompression using a transpedicular approach",
    object == "diskectomy" ~ "decompression",
    object == "sublaminar_decompression" ~ "decompression",
    object == "laminectomy" ~ "decompression",
    object == "laminotomy" ~ "decompression",
    object == "revision_diskectomy" ~ "reexploration with revision decompression",
    object == "revision_sublaminar_decompression" ~ "reexploration with revision decompression",
    object == "revision_laminectomy" ~ "reexploration with revision decompression",
    object == "revision_laminotomy" ~ "reexploration with revision decompression",
    object == "laminoplasty" ~ "laminoplasty",
    object == "grade_1" ~ "inferior facetectomies",
    object == "complete_facetectomy" ~ "complete facetectomies",
    object == "grade_2" ~ "posterior column osteotomies",
    object == "grade_3" ~ "pedicle subtraction osteotomy",
    object == "grade_4" ~ "extended pedicle subtraction osteotomy",
    object == "grade_5" ~ "vertebral column resection",
    object == "costotransversectomy" ~ "costovertebral approach with costotransversectomy",
    object == "no_implant_interbody_fusion" ~ "interbody fusion (without interbody implant)",
    object == "llif" ~ "lateral lumbar interbody fusion and insertion of interbody device",
    object == "plif" ~ "posterior lumbar interbody fusion and insertion of interbody device",
    object == "tlif" ~ "transforaminal lumbar interbody fusion and insertion of interbody device",
    object == "intervertebral_cage" ~ "insertion of intervertebral biomechanical implant",
    #anterior#
    object == "anterior_disc_arthroplasty" ~ "disk arthroplasty",
    object == "decompression_diskectomy_fusion" ~ "anterior diskectomy and fusion with decompression of the central canal and nerve roots",
    object == "diskectomy_fusion" ~ "anterior diskectomy and fusion",
    object == "diskectomy_fusion_no_interbody_device" ~ "anterior diskectomy and fusion",
    object == "anterior_interbody_implant" ~ "insertion of interbody biomechanical implant",
    object == "corpectomy" ~ "anterior vertebral corpectomy",
    object == "corpectomy_cage" ~ "insertion of intervertebral biomechanical implant",
    object == "anterior_plate" ~ "anterior spinal instrumentation",
    object == "anterior_buttress_plate" ~ "anterior spinal instrumentation",
    object == "screw_washer" ~ "anterior spinal instrumentation"
  )
  procedure_category
}


op_note_number_of_paragraphs_for_procedure_category <- function(procedure_cat){
  paragraph_type <- case_when(
    procedure_cat == 'vertebral cement augmentation' ~ "combine",
    procedure_cat == 'posterior spinal instrumentation' ~ "combine",
    procedure_cat == 'occiput instrumentation' ~ "combine",
    procedure_cat == 'pelvic instrumentation' ~ "combine",
    procedure_cat == 'spinous process posterior tethering/wiring' ~ "combine",
    procedure_cat == 'complete facetectomies' ~ "combine",
    procedure_cat == 'inferior facetectomies' ~ "combine",
    procedure_cat == 'posterior column osteotomies' ~ "combine",
    procedure_cat == 'pedicle subtraction osteotomy' ~ "distinct",
    procedure_cat == 'extended pedicle subtraction osteotomy' ~ "distinct",
    procedure_cat == 'vertebral column resection' ~ "distinct",
    procedure_cat == 'costovertebral approach with costotransversectomy' ~ "combine",
    procedure_cat == 'decompression' ~ "combine",
    procedure_cat == 'decompression using a costovertebral approach' ~ "distinct",
    procedure_cat == 'decompression using a transpedicular approach' ~ "distinct",
    procedure_cat == 'reexploration with revision decompression' ~ "combine",
    procedure_cat == 'reexploration with revision decompression using a costovertebral approach' ~ "distinct",
    procedure_cat == 'reexploration with revision decompression using a transpedicular approach' ~ "distinct",
    procedure_cat == 'laminoplasty' ~ "combine",
    procedure_cat == 'lateral lumbar interbody fusion and insertion of interbody device' ~ "distinct",
    procedure_cat == 'transforaminal lumbar interbody fusion and insertion of interbody device' ~ "distinct",
    procedure_cat == 'interbody fusion (without interbody implant)' ~ "distinct",
    procedure_cat == 'posterior lumbar interbody fusion and insertion of interbody device' ~ "distinct",
    #anterior#
    procedure_cat == "disk arthroplasty" ~ "distinct",
    procedure_cat == "anterior diskectomy and fusion with decompression of the central canal and nerve roots" ~ "distinct",
    procedure_cat == "anterior diskectomy and fusion" ~ "distinct",
    procedure_cat == "insertion of interbody biomechanical implant" ~ "distinct",
    procedure_cat == "anterior vertebral corpectomy" ~ "distinct",
    procedure_cat == "insertion of intervertebral biomechanical implant" ~ "distinct",
    procedure_cat == "insertion of interbody biomechanical implant" ~ "distinct",
    procedure_cat == "anterior spinal instrumentation" ~ "combine")
  paragraph_type
}


op_note_technique_statement <- function(object, levels_side_df, revision_modifier = "xx"){
  
  revision_statement <- if_else(revision_modifier == "xx", " ", " reexploration and revision decompression with a ")
  
  if(object == "pelvic_screw"){
    
    pelvic_screws_wide <- levels_side_df %>%
      select(level, side) %>%
      mutate(count = row_number()) %>%
      pivot_wider(names_from = side, values_from = count) %>%
      unnest()
    
    pelvic_screws_statement_list <- list()
    
    if(sum(str_count(string = pelvic_screws_wide$level, "Iliac")) == 1){
      pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk using a rongeur. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured.")
    }
    if(sum(str_count(string = pelvic_screws_wide$level, "Iliac")) == 2){
      pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk using a rongeur. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured. To fit two iliac screws, the start point was moved slightly proximal and distal to a traditional start point.")
    }
    
    if(sum(str_count(string = levels_side_df$level, "Iliac")) > 0){
      iliac_screws_df <- levels_side_df %>%
        filter(level == "Iliac") %>%
        mutate(statement = glue("a {str_to_lower(screw_size_type)} iliac screw was placed on the {side}")) 
      
      pelvic_screws_statement_list$iliac_sizes_statement <- glue("Using this technique, {glue_collapse(x = iliac_screws_df$statement, sep = ', ', last = ' and ')}.")
      }
    
    if(sum(str_count(string = pelvic_screws_wide$level, "S2AI")) == 1){
      pelvic_screws_statement_list$s2ai_technique <- glue("For S2AI screw insertion, the starting point was identified at the inferior and lateral border of the S1 foramen. A probe was used to bore the path of the screw between the two cortices, across the sacroiliac joint, and aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured.")
    }
    if(sum(str_count(string = pelvic_screws_wide$level, "S2AI")) == 2){
      pelvic_screws_statement_list$s2ai_technique <- glue("For S2AI screw insertion, the starting point was identified at the inferior and lateral border of the S1 foramen. A probe was used to bore the path of the screw between the two cortices, across the sacroiliac joint, and aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured. To fit two S2AI screws, the start point was moved slightly proximal and distal to a traditional start point.")
    }
    
    if(sum(str_count(string = levels_side_df$level, "S2AI")) > 0){
      s2ai_screws_df <- levels_side_df %>%
        filter(level == "S2AI") %>%
        mutate(statement = glue("a {str_to_lower(screw_size_type)} S2AI screw was placed on the {side}")) 
      
      pelvic_screws_statement_list$s2ai_sizes_statement <- glue("Using this technique, {glue_collapse(x = s2ai_screws_df$statement, sep = ', ', last = ' and ')}.")
    }
    
      pelvic_screw_statement <- glue_collapse(pelvic_screws_statement_list, sep = " ")
      
    
  }else{
    pelvic_screw_statement <- ""
    
  }
  
  screw_statements_wide_df <- levels_side_df %>%
    mutate(unilateral_screw_statement = str_to_lower(glue("a {screw_size_type} screw was inserted on the {side}"))) %>%
    pivot_wider(names_from = side, values_from = unilateral_screw_statement) %>%
    unnest()

  screw_statements_df <- tibble(level = character(), left = character(), right = character()) %>%
    union_all(screw_statements_wide_df) %>%
    replace_na(replace = list(left = "xx", right = "xx")) %>%
    mutate(screw_statement = case_when(
      left != "xx" & right != "xx" ~ glue("At {level}, {left} and {right}."), 
      left == "xx" & right != "xx" ~ glue("At {level}, {right}."), 
      left != "xx" & right == "xx" ~ glue("At {level}, {left}."), 
      left == "xx" & right == "xx" ~ glue(""))) %>%
    left_join(levels_numbered_df) %>%
    arrange(vertebral_number)
  
  levels_side_df <- levels_side_df %>%
    group_by(level) %>%
    add_tally(name = "number_per_level") %>%
    mutate(level_side = as.character(if_else(number_per_level == 1, glue("on the {side} at {level}"), glue("on the left and on the right at {level}")))) %>%
    ungroup() %>%
    select(-side) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    arrange(vertebral_number)   
  
  
  technique_statement <- case_when(
    object == "cement_augmentation" ~ glue("For vertebral body cement augmentation, a cannula was inserted into the vertebral body through the pedicle. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Cement augmentation was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "laminar_downgoing_hook" ~ glue("For downgoing laminar hooks, the ligamentum flavum was removed at the site of the hook insertion. A hook was then inserted securely under the lamina {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "laminar_upgoing_hook" ~ glue("For upgoing laminar hooks, the inferior edge of the cranial lamina was identified and the plane between the lamina and ligamentum was developed and the upgoing hook is then placed within this plane, secured to the lamina aimed cranially {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "lateral_mass_screw" ~ glue("For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a high speed burr and the screw path drilled incrementally to the far cortex. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "occipital_screw" ~ glue("An appropriately sized occipital plate was selected and was placed centered in the midline and just caudal to the external occipital protuberance, but cranial to the foramen magnum. The plate was held in place to identify the appropriate start points for the occipital screws. The occipital screws were drilled incrementally until the anterior cortex was penetrated. The length of the path was measured to acheive bicortical fixation. The screw paths were then tapped and the screws placed, securing the plate to the occiput."), 
    object == "pars_screw" ~ glue("For pars screws, the start point was identified just 3-4mm cranial to the inferior facet joint and in the midpoint of the pars from medial to lateral. Once the start point was identified, the superficial cortex was opened at the entry point using the high speed burr. A drill was used to cannulate a screw path, aiming as dorsal as possible while not perforating the dorsal cortex of the pars. The path was then tapped and the length measured and pars screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pedicle_screw" ~ glue("For pedicle screws, the transverse process, pars, and superior facet were used as landmarks to identify the appropriate starting point. After identifying the start point, the superficial cortex was opened at each entry point using a high speed burr. A pedicle probe was then used to navigate down the pedicle, followed by palpating a medial, lateral, superior and inferior pedicle wall, measuring, and tapping if appropriate. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "translaminar_screw" ~ glue("For translaminar screw fixation, the starting point was identified using the spinolaminar junction and in the central plane of the contralateral lamina. A burr hole was made to open the superficial cortex, the path was then cannulated and intralaminar screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),
    object == "pedicle_hook" ~ glue("For pedicle hooks, a pedicle finder was carefully placed just under the inferior facet and above the superior facet, until the pedicle was found. Once the pedicle was identified, a pedicle hook was inserted directly toward the inferior pedicle, secured within the residual facet joint {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pelvic_screw" ~ pelvic_screw_statement,
    object == "sublaminar_wire" ~ glue("For sublaminar wiring, the doubled-up sublaminar was was bent into a hook shape and and passed under the inferior lamina from a caudal to cranial direction. Once the wire was safely passed, the tip of the wire was cut and the two strands were directed in opposite directions of the midline to be attached to the rods on the left and right side. Wires were passed under {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tether" ~ glue("For posterior vertebral tethering, a hole was drilled in the transverse process over the lamina. A band was then thread through the hole and tied in a figure-8 fashion, tethering {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tp_hook" ~ glue("For transverse process hooks, the cranial edge of the transverse process was identified. A transverse process finder was used to develop the plane and a transverse process hook was placed securely into position along the superior edge of the transverse process."),  
    object == 'complete_facetectomy' ~ glue("For a complete facetectomy, the inferior, superior, medial and lateral borders of the inferior and superior facet were identified. Both the superior and inferior facet were completely excised and the underlying nerve root decompression. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} and the bone was morselized to be used as morselized autograft."),
    object == 'grade_1' ~ glue("The inferior, superior, medial and lateral borders of the inferior facet were identified. The inferior facets {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} were excised and the bone was morselized to be used as morselized autograft."),
    object == 'grade_2' ~ glue("For the posterior column osteotomies, a small rent was made in the interlaminar space to develop the plane between the ligamentum and the dura. A Kerrison rongeur was then used to excise the ligamentum. this was carried out laterally in both directions until the facets were encountered. The superior and inferior facet were both adequately resected to fully release the posterior column. I performed posterior column (Smith-Peterson) osteotomies at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}"),
    
    object == 'diskectomy' ~ glue("For a diskectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a diskectomy with partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Using a high-speed burr and Kerrison rongeurs, I first performed a foraminotomy and excised the ligamentum flavum. The dural sac was identified and traversing root was protected. The annulus was then incised and the diseased disk materal was removed. Following the decompression, the canal and foramen and lateral recess were palpated to confirm an appropriate decompression had been completed."),
    
    object == 'laminotomy' ~ glue("For the laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. Once I had determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    
    object == 'sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a partial laminectomy bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I resected the dorsal lamina and then excised the ligamentum flavum, exposing the dura. To fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess, the medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. Following the decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_diskectomy' ~ glue("For the revision diskectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a revision and reexploration with diskectomy and partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. Using a high-speed burr and Kerrison rongeurs, I performed a foraminotomy and excised the scar and ligamentum. The dural sac was identified and traversing root was protected. The annulus and disk space were identified and the diseased disk material was excised. Following the revision decompression, the canal and foramen and lateral recess were palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_laminotomy' ~ glue("For the revision laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. I carefully exposed the dorsal elements until I had identified the edges of the laminar defect. Once I determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a revision laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a reexploration and revision laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. Using a combination of a high-speed burr, currettes and Kerrison rongeurs, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I removed any dorsal lamina, ligamentum flavum and scar thought to still be causing compression. The central canal was palpated to confirm full decompression of the thecal sac. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a reexploration and revision decompression bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I proceeded with partial laminectomies and excised the scar and underlying ligamentum flavum. The medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace, to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess. Following the revision decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    
    object == 'costotransversectomy' ~ glue("For the costotransversectomy, the rib was identified and a subperiosteal dissection was carried out laterally 6-7cm. Once the medial rib was skeletonized and a safe, subperiorsteal plane was confirmed, 5-6cm of the medial rib was transected. This process was completed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}, allowing a safe costovertebral approach to the anterior vertebral body."),
    
    object == 'laminoplasty' ~ glue("For laminoplasty, an opening and hinge trough was created with a high-speed burr angled perpendicular to the lamina at the lateral mass-lamina junction. This was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. After all troughs were completed, greenstick fractures were created to open the lamina at the laminoplasty levels, the ligamentum flavum at the proximal and distal levels was excised, and laminoplasty plates were sequentially secured into place. The spinous processes were trimmed and the canal was palpated to confirm an adequate decompression had been completed.")
    )
  
  technique_statement
}



op_note_distinct_technique_statement <- function(object, level, side, revision_modifier = "xx", interbody_statement = NULL){
  
  revision_statement <- if_else(revision_modifier == "xx", "", "reexploration and revision decompression with a")
  
  cranial_level <- identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level
  caudal_level <- identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level
  
  cranial_interspace <- identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace
  caudal_interspace <- identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace
  
  thoracic_nerve_ligation_statement <- {if_else(str_detect(level, 'T'), glue(" Clamps were then placed on the exiting {cranial_level} and {level} nerve roots medial to the dorsal root ganglion and the spinal cord was monitored for any neuromonitoring changes. Once we were satisfied with the neuromonitoring data, the nerve roots were ligated with sutures. "), glue(" "))} 
  
  technique_statement <- case_when(
    object == 'transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for decompression from the {side} at {level}. Using a combination of a high-speed burr and Kerrison rongeurs, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and 5-6cm of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr and Kerrison rongeurs, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for a reexploration and revision decompression from the {side} at {level}. Using a combination of a high-speed burr, currettes, and Kerrison rongeurs, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for a reexploration and revision decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and 5-6cm of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr, currettes, and Kerrison rongeurs, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'grade_3' ~ glue("I then proceeded with the pedicle subtraction osteotomy at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a high speed burr was used to develop a defect in the vertebral body at the level of the pedicle. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, taking care to leave adequate anterior body and cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once an adequate defect had been generated in the vertebral body, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate alignment."),
    object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a high speed burr was used to develop a defect in the vertebral body. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, extending up into the cranial disk space, taking care not to perforate the anterior cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once roughly 50% of the vertebral body had been resected, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate alignment."),
    # object == 'grade_5' ~ glue("I then proceeded with a vertebral column resection at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the entire anterior portion of the {level} body and retractors were held in place to protect the anterior structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a high speed burr was used to develop a defect in the vertebral body. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, extending up into the cranial and caudal disk space. Any remaining pedicle walls were then resected with an osteotome. We then placed a temporary rod and repeated the same process from the contralateral pedicle. Once 80-90% of the vertebral body had been resected, the nerve roots were checked again and the dorsal wall of the vertebral body was impacted. The nerve roots and the ventral thecal sac were checked again to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the remaining vertebral body was resected and the other rod was placed into position."),
    object == 'grade_5' ~ glue("I then proceeded with a vertebral column resection at the level of {level}. A complete central decompression was performed and confirmed extending from the {cranial_level} pedicle to the {caudal_level} pedicle in order to prevent neural compression after osteotomy closure. A complete facetectomy was performed superiorly and any dorsal bone was removed, allowing the {cranial_level} nerve root to be visualized and tracked laterally. Similarly, a complete facetectomy was performed inferiorly, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected and the {level} pedicles were completely skeletonized. The cranial and caudal nerve roots were then free and a full decompression was confirmed from the {cranial_level} pedicle to the {caudal_level} pedicle.{thoracic_nerve_ligation_statement}Once we were satisfied with the neuromonitoring data, the nerve roots were ligated with sutures. ',' ')} In preparation for the vertebral column resection, a unilateral stabilizing rod was placed on the convex side and secured with set screws multiple levels above and below the {level} level. Starting on the concavity, a combination of blunt and electrocautery dissection was used to develop a subperiosteal plane around the entire anterior portion of the {level} body and retractors were held in place to protect the anterior and neural structures. A lateral pedicle body window was developed for accessing the cancellous bone of the vertebral body and using currettes and rongeurs, the accessible cancellous vertebral body was removed, extending up toward the cranial and caudal disk space. The remaining pedicle of {level} was resected down to the dorsal vertebral body, allowing the spinal cord to drift medially. The same process was then carried out on the convexity to access the cancellous vertebral body and remove the convex pedicle. The entire vertebral body was removed except for a thin anterior section, preserving the anterior longitudinal ligament. Discectomies were performed at the {cranial_interspace} and {caudal_interspace}, taking care to preserve the endplates. Once the entire body had been removed, a plane was developed between the ventral dural sac and the posterior body wall to be sure the dural sac was completely free from the body wall. An impactor was then used to impact the posterior body wall, completing the vertebral column resection. The dural sac was again inspected to confirm no potential sites of impingement. We then turned toward closure of the resection site. Starting with compression on the convexity, the site was closed through alternating compression on the convexity and slight distraction on the concave side. Once I was satisfied with the closure, rods were secured into place bilaterally with set screws."),
    object == 'no_implant_interbody_fusion' ~ glue("I then proceeded with the interbody fusion from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, the size was confirmed and the interbody implant was placed into position along with allograft. The final position was confirmed using intraoperative xray."),
    object == 'plif' ~ glue("I then proceeded with the posterior lumbar interbody fusion (PLIF) procedure and placement of interbody implant at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were partially resected and the exiting and traversing nerve roots were appropriately protected. The dura was retracted in order to access the disc space. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. {interbody_statement} Once I was satisfied with the size and fit of the trial, the interbody implant was placed into the interbody space along with allograft bone. The final position of the implant was confirmed using intraoperative xray."),
    object == "tlif" ~ glue("I then proceeded with the transforaminal interbody fusion (TLIF) procedure and placement of interbody implant from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. {interbody_statement} Once I was satisfied with the size and fit of the trial, the interbody implant was placed into the interbody space along with allograft bone. The final position of the implant was confirmed using intraoperative xray."),
    object == "intervertebral_cage" ~ glue("I then proceed with placement of the intervertebral cage. After ensuring the neural structures were completely free and the bony surfaces cranially and caudally were appropriately prepared, the intervertebral distance was measured. {interbody_statement} The implant was then inserted into the defect, abutting bone superiorly and inferiorly. {if_else(str_detect(interbody_statement, 'expandable'), 'The implant was then expanded until a solid bony fit was sensed.', '')} Intraoperative xray was used to confirm appropriate size, fit, and alignment of the intervertebral implant spanning {level}.")
    
    )
  
  technique_statement
}

create_full_paragraph_statement_function <- function(procedure_paragraph_intro, df_with_levels_object_nested, paragraphs_combined_or_distinct){
  if(paragraphs_combined_or_distinct == "combine"){
    
    df_with_statement <- df_with_levels_object_nested %>%
      group_by(object) %>%
      nest() %>%
      mutate(object_levels_side_df = data) %>%
      select(-data) %>%
      mutate(tech_statement = map2(.x = object, .y = object_levels_side_df, .f = ~ op_note_technique_statement(object = .x, levels_side_df = .y))) %>%
      select(object, tech_statement) %>%
      unnest() %>%
      distinct()

    df_levels <- df_with_levels_object_nested %>%
      select(level, vertebral_number) %>%
      distinct() %>%
      arrange(vertebral_number)
    
    
    if(procedure_paragraph_intro == "pelvic instrumentation" | procedure_paragraph_intro == "posterior spinal instrumentation"){
      if(procedure_paragraph_intro == "pelvic instrumentation"){
        statement <- glue("I then proceeded with instrumentation of the pelvis. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This completed the instrumentation of the pelvis.")
      }
      if(procedure_paragraph_intro == "posterior spinal instrumentation"){
        statement <- glue("I then proceeded with the posterior spinal instrumentation. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This partially completed the {procedure_paragraph_intro} of {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
      }
      
    }else{
      statement <- glue("I then proceeded with the {procedure_paragraph_intro}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This completed the {procedure_paragraph_intro} of {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
    }
    
  }
  
  if(paragraphs_combined_or_distinct == "distinct"){
    df_with_statement <- df_with_levels_object_nested %>%
      mutate(tech_statement_detail = pmap(.l = list(..1 = object, 
                                                    ..2 = level, 
                                                    ..3 = side, 
                                                    ..4 = implant_statement),
                                          .f = ~op_note_distinct_technique_statement(object = ..1, level = ..2, side = ..3, interbody_statement = ..4))) %>% 
      select(object, level, object, tech_statement_detail) %>%
      unnest() %>%
      mutate(procedure_category = op_note_procedure_category_function(object = object)) %>%
      mutate(tech_statement = paste(tech_statement_detail, glue("This completed the {procedure_category} at the {level} {if_else(str_detect(level, '-'), 'interspace', 'level')}.")))
    
    statement <- glue_collapse(df_with_statement$tech_statement, sep = "\n\n")
  }
  
  return(statement)
  
}

op_note_procedure_paragraphs_function <- function(objects_added_df, revision_decompression_vector){
  
  if(length(revision_decompression_vector) > 0){
    df_for_paragraphs <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector, implant_statement, screw_size_type) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      mutate(procedure_category = op_note_procedure_category_function(object = object)) %>%
      select(level, vertebral_number, procedure_category, object, side, implant_statement) %>%
      mutate(procedures_combine = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) %>%
      group_by(procedure_category, procedures_combine, object) %>%
      nest() %>%
      ungroup() %>%
      group_by(procedure_category, procedures_combine) %>%
      nest() %>%
      mutate(nested_data = map(.x = data, .f = ~ unnest(.x))) %>%
      select(-data)
  }else{
    df_for_paragraphs <- objects_added_df %>%
      mutate(procedure_category = op_note_procedure_category_function(object = object)) %>%
      select(level, vertebral_number, procedure_category, object, side, implant_statement, screw_size_type) %>%
      mutate(procedures_combine = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) %>%
      group_by(procedure_category, procedures_combine, object) %>%
      nest() %>%
      ungroup() %>%
      group_by(procedure_category, procedures_combine) %>%
      nest() %>%
      mutate(nested_data = map(.x = data, .f = ~ unnest(.x))) %>%
      select(-data)  
  }
  
  procedures_op_full_df <- df_for_paragraphs %>%
    mutate(procedure_statement = pmap(list(..1 = procedure_category,
                                           ..2 = nested_data, 
                                           ..3 = procedures_combine),
                                      .f = ~create_full_paragraph_statement_function(procedure_paragraph_intro = ..1,
                                                                                     df_with_levels_object_nested = ..2, 
                                                                                     paragraphs_combined_or_distinct = ..3))) %>%
    select(procedure_category, procedures_combine, procedure_statement) %>%
    unnest()
  
  glue_collapse(x = procedures_op_full_df$procedure_statement, sep = "\n\n")
  
}


















# op_note_procedure_performed_summary_classifier_function <- function(object){
#   procedure_category <- case_when(
#     object == "cement_augmentation" ~ "Vertebral cement augmentation",
#     object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
#     object == "laminar_upgoing_hook" ~ "Posterior spinal instrumentation",
#     object == "lateral_mass_screw" ~ "Posterior spinal instrumentation",
#     object == "occipital_screw" ~ "Occiput instrumentation",
#     object == "pars_screw" ~ "Posterior spinal instrumentation",
#     object == "pedicle_hook" ~ "Posterior spinal instrumentation",
#     object == "pedicle_screw" ~ "Posterior spinal instrumentation",
#     object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
#     object == "sublaminar_wire" ~ "Posterior spinal instrumentation",
#     object == "tp_hook" ~ "Posterior spinal instrumentation",
#     object == "translaminar_screw" ~ "Posterior spinal instrumentation",
#     object == "pelvic_screw" ~ "Pelvic instrumentation",
#     object == "tether" ~ "Spinous process posterior tethering/wiring",
#     object == "costovertebral_approach" ~ "Decompression using a costovertebral approach",
#     object == "revision_costovertebral_approach" ~ "Reexploration and revision decompression using a costovertebral approach",
#     object == "transpedicular_approach" ~ "Decompression using a transpedicular approach",
#     object == "revision_transpedicular_approach" ~ "Reexploration and revision decompression using a transpedicular approach",
#     object == "diskectomy" ~ "Decompression with diskectomy and laminotomy",
#     object == "sublaminar_decompression" ~ "Decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies",
#     object == "laminectomy" ~ "Decompression with central laminectomy",
#     object == "laminotomy" ~  "Decompression with laminotomy and medial facetectomy",
#     object == "revision_diskectomy" ~ "Reexploration and revision decompression with diskectomy and laminotomy",
#     object == "revision_sublaminar_decompression" ~ "Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies",
#     object == "revision_laminectomy" ~ "Reexploration and revision decompression with central laminectomy",
#     object == "revision_laminotomy" ~ "Reexploration and revision decompression with laminotomy and medial facetectomy",
#     object == "laminoplasty" ~ "Laminoplasty",
#     object == "grade_1" ~ "Inferior facetectomies",
#     object == "complete_facetectomy" ~ "Complete facetectomy",
#     object == "grade_2" ~ "Posterior column osteotomy",
#     object == "grade_3" ~ "Pedicle subtraction osteotomy",
#     object == "grade_4" ~ "Extended three column osteotomy (vertebral body partial corpectomy)",
#     object == "grade_5" ~ "Vertebral column resection",
#     object == "no_implant_interbody_fusion" ~ "Interbody fusion (without interbody implant)",
#     object == "llif" ~ "Lateral lumbar interbody fusion and insertion of interbody device",
#     object == "plif" ~ "Posterior lumbar interbody fusion and insertion of interbody device",
#     object == "tlif" ~ "Transforaminal lumbar interbody fusion and insertion of interbody device",
#     object == "intervertebral_cage" ~ "Insertion of intervertebral biomechanical device"
#   )
#   procedure_category
# }
# 
# op_note_procedure_performed_summary_per_line_function <- function(procedure_type){
#   procedures_per_line <- case_when(
#     procedure_type == 'Vertebral cement augmentation' ~ 'multiple',
#     procedure_type == 'Posterior spinal instrumentation' ~ 'multiple',
#     procedure_type == 'Occiput instrumentation' ~ 'multiple',
#     procedure_type == 'Pelvic instrumentation' ~ 'multiple',
#     procedure_type == 'Spinous process posterior tethering/wiring' ~ 'multiple',
#     procedure_type == 'Complete facetectomy' ~ 'multiple',
#     procedure_type == 'Inferior facetectomies' ~ 'multiple',
#     procedure_type == 'Posterior column osteotomy' ~ 'multiple',
#     procedure_type == 'Pedicle subtraction osteotomy' ~ 'one',
#     procedure_type == 'Extended three column osteotomy (vertebral body partial corpectomy)' ~ 'one',
#     procedure_type == 'Vertebral column resection' ~ 'one',
#     procedure_type == 'Decompression with diskectomy and laminotomy' ~ 'multiple',
#     procedure_type == 'Decompression with laminotomy and medial facetectomy' ~ 'multiple',
#     procedure_type == 'Decompression with central laminectomy' ~ 'multiple',
#     procedure_type == 'Decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies' ~ 'multiple',
#     procedure_type == "Reexploration and revision decompression with diskectomy and laminotomy" ~ 'multiple',
#     procedure_type == "Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies" ~ 'multiple',
#     procedure_type == "Reexploration and revision decompression with central laminectomy" ~ 'multiple',
#     procedure_type == "Reexploration and revision decompression with laminotomy and medial facetectomy"~ 'multiple',
#     procedure_type == 'Laminoplasty' ~ 'multiple',
#     procedure_type == 'Decompression using a transpedicular approach' ~ 'one',
#     procedure_type == 'Decompression using a costovertebral approach' ~ 'one',
#     procedure_type == 'Reexploration and revision decompression using a transpedicular approach' ~ 'one',
#     procedure_type == 'Reexploration and revision decompression using a costovertebral approach' ~ 'one',
#     procedure_type == 'Interbody fusion (without interbody implant)' ~ 'one',
#     procedure_type == 'Lateral lumbar interbody fusion and insertion of interbody device' ~ 'one',
#     procedure_type == 'Transforaminal lumbar interbody fusion and insertion of interbody device' ~ 'one',
#     procedure_type == 'Interbody fusion (without interbody implant)' ~ 'one',
#     procedure_type == 'Posterior lumbar interbody fusion and insertion of interbody device' ~ 'one',
#     procedure_type == "Insertion of intervertebral biomechanical device" ~ 'multiple'
#   )
#   procedures_per_line
# }

extract_levels_function <- function(input_df){
  levels_df <- input_df %>%
    arrange(vertebral_number) %>%
    select(level) %>%
    distinct()
  
  glue_collapse(x = levels_df$level, sep = ", ", last = " and ")
  
}



op_note_procedures_performed_numbered_function <- function(objects_added_df,
                                                           revision_decompression_vector = NULL,
                                                           fusion_levels_vector = NULL,
                                                           additional_procedures_performed_vector = NULL){
  if(length(revision_decompression_vector) > 0){
    summary_nested_df <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_procedure_performed_summary_per_line_function(procedure_type = procedure_class)) 
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_procedure_performed_summary_per_line_function(procedure_type = procedure_class)) 
  }
  
  
  summary_single_statements <- summary_nested_df %>%
    filter(procedures_per_line == "one") %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {level}"))%>%
    select(procedure_class, procedure_performed_statement)
  
  summary_multiple_nested <- summary_nested_df %>%
    filter(procedures_per_line == "multiple") %>%
    group_by(procedure_class) %>%
    nest() %>%
    ungroup() %>%
    mutate(count = row_number()) %>%
    mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
    select(-data) %>%
    unnest() %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {levels}")) %>%
    mutate(procedure_performed_statement = if_else(procedure_class == "Pelvic instrumentation", paste("Instrumentation of the Pelvis with", levels, "fixation"), as.character(procedure_performed_statement))) %>%
    select(procedure_class, procedure_performed_statement)
  
  added_procedures_df <- tibble(procedure_performed_statement = additional_procedures_performed_vector) 
  
  procedures_numbered_df <- summary_nested_df %>%
    select(procedure_class) %>%
    distinct() %>%
    left_join(summary_single_statements %>%
                union_all(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="xx") %>%
    mutate(count = row_number()) %>%
    mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    select(procedures_performed_numbered)
  
  glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n")
}

op_note_procedures_present_listed_function <- function(objects_added_df,
                                                           revision_decompression_vector = NULL,
                                                           fusion_levels_vector = NULL,
                                                           additional_procedures_performed_vector = NULL){
  
  additional_procedures_performed_vector <- keep(.x = additional_procedures_performed_vector, .p = ~ str_detect(.x, "Biopsy") | 
         str_detect(.x, "Repair of dural/CSF")  | 
         str_detect(.x, "Dural")  | 
         str_detect(.x, "Removal of spinal") | 
         str_detect(.x, "Exploration of") | 
         str_detect(.x, "Open treatment"))
  
  if(length(revision_decompression_vector) > 0){
    summary_nested_df <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_procedure_performed_summary_per_line_function(procedure_type = procedure_class)) 
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_procedure_performed_summary_per_line_function(procedure_type = procedure_class)) 
  }
  
  
  summary_single_statements <- summary_nested_df %>%
    filter(procedures_per_line == "one") %>%
    mutate(procedure_performed_statement = glue("{str_to_lower(procedure_class)} at {level}"))%>%
    select(procedure_class, procedure_performed_statement)
  
  summary_multiple_nested <- summary_nested_df %>%
    filter(procedures_per_line == "multiple") %>%
    group_by(procedure_class) %>%
    nest() %>%
    ungroup() %>%
    mutate(count = row_number()) %>%
    mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
    select(-data) %>%
    unnest() %>%
    mutate(procedure_performed_statement = glue("{str_to_lower(procedure_class)} at {levels}")) %>%
    mutate(procedure_performed_statement = if_else(str_detect(procedure_class, "pelvic instr"), 
                                                   paste("instrumentation of the pelvis with", levels, "fixation"), 
                                                   as.character(procedure_performed_statement))) %>%
    select(procedure_class, procedure_performed_statement)
  
  added_procedures_df <- tibble(procedure_performed_statement = str_to_lower(additional_procedures_performed_vector)) 
  
  procedures_listed_df <- summary_nested_df %>%
    select(procedure_class) %>%
    distinct() %>%
    left_join(summary_single_statements %>%
                union_all(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("posterior spinal fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="xx")
  
  as.character(glue_collapse(procedures_listed_df$procedure_performed_statement, sep = ", ", last = ", and the "))
}


################### FULL OPERATIVE NOTE BODY GENERATOR #######################

op_note_posterior_function <- function(all_objects_to_add_df,
                                       fusion_levels_df,
                                       revision_decompression_vector,
                                       left_main_rod_size = "5.5",
                                       left_main_rod_material = "Titanium",
                                       right_main_rod_size = "5.5",
                                       right_main_rod_material = "Titanium",
                                       additional_rods_statement = NULL,
                                       antibiotics = vector(),
                                       prior_fusion_levels_vector = vector(),
                                       instrumentation_removal_vector = vector(),
                                       additional_procedures_vector = NULL,
                                       bmp = 0,
                                       bone_graft_vector = NULL,
                                       morselized_allograft = 0,
                                       morselized_autograft_separate = 0,
                                       structural_allograft_location = NULL,
                                       structural_autograft_harvest = NULL,
                                       structural_autograft_location = NULL,
                                       deep_drains = 0,
                                       superficial_drains = 0,
                                       end_procedure_details = NULL,
                                       closure = NULL,
                                       dressing = NULL){
  
  procedure_details_list <- list()
  
  procedures_numbered_list <- list()
  
  additional_procedures_for_numbered_list <- c(as.character(glue("Application of {glue_collapse(bone_graft_vector, sep = ', ', last = ', and ')}")))
  
  additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, additional_procedures_vector)
  
  procedures_numbered_list$primary_procedures <- op_note_procedures_performed_numbered_function(objects_added_df = all_objects_to_add_df, 
                                                                                                revision_decompression_vector = revision_decompression_vector, 
                                                                                                fusion_levels_vector = fusion_levels_df$level, 
                                                                                                additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  first_paragraph_list <- list()
  
  first_paragraph_list$transport_anesthesia <- paste("The patient was brought to the operating room and after obtaining appropriate IV access, the patient underwent general anesthesia.")
  
  if(length(antibiotics) == 0){
    first_paragraph_list$antibiotic_statement <- "Preoperative Antibiotics were administered."
  }else{
    if(antibiotics == "None (Antibiotics were held)"){
      first_paragraph_list$antibiotic_statement <- "Preoperative antibiotics were held until tissue cultures could be obtained."
    }else{
      first_paragraph_list$antibiotic_statement <- paste("Preoperative Antibiotics were administered including ", glue_collapse(antibiotics, sep = ", ", last = " and "), ".")
    }
  }
  
  if(any(str_detect(additional_procedures_vector, "Halo"))){
    first_paragraph_list$head_statement <- "A Halo was applied to the patient's skull for positioning and the pins were sequentially tightened to the appropriate torque."
  }else{
    if(any(str_detect(additional_procedures_vector, "Tongs"))){
      first_paragraph_list$head_statement <- "Cranial tongs were applied to the patient's skull for positioning and an appropriate weight was selected for cranial traction."
    }else{
      first_paragraph_list$head_statement <- "The proneview faceplate was used to pad and secure the patient's head and face during surgery. "
    }
    
  } 
  
  if(any(str_detect(additional_procedures_vector, "Spinal Cord Monitoring"))){
    first_paragraph_list$spinal_cord_monitoring <- "Spinal Cord Monitoring needles were inserted by the neurophysiology technologist."
  }
  
  first_paragraph_list$positioning <- paste("The patient was then positioned prone on the OR table and all bony prominences were appropriately padded.",
                                            "After prepping and draping in the standard fashion, a surgical timeout was performed.")
  
  proximal_exposure_level <- all_objects_to_add_df %>%
    filter(vertebral_number == min(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    select(level) %>%
    distinct()
  
  distal_exposure_level <- all_objects_to_add_df %>%
    filter(vertebral_number == max(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number + 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    select(level) %>%
    distinct() %>%
    mutate(level = if_else(level == "S2AI", "S1", 
                           if_else(level == "Iliac", "S1", 
                                   level)))
  
  first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, exposing proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################### Revision Procedures PARAGRAPHS ##################
  
  if(any(additional_procedures_vector == "Removal of spinal instrumentation")  | any(additional_procedures_vector == "Exploration of spinal prior fusion")){
    revision_statements_list <- list()
    if(length(instrumentation_removal_vector)){
      revision_statements_list$removal_of_implants_statement <- glue("I then proceeded with removal of the prior spinal instrumentation. The prior instrumentation was identified at {glue_collapse(instrumentation_removal_vector, sep = ', ', last = ' and ')}, and removed without major difficulty.")
    }else{
      revision_statements_list$removal_of_implants_statement <- glue("I then proceeded with removal of the prior spinal instrumentation. The prior instrumentation was identified and removed without major difficulty.")
    }
    
    if(length(prior_fusion_levels_vector) > 0){
      revision_statements_list$exploration_fusion_statement <- glue("I then proceeded with exploration of the prior fusion. Overlying scar was excised from the posterior elements and the prior fusion at {glue_collapse(prior_fusion_levels_vector, sep = ', ', last = ' and ')} was inspected and examined for any motion.")
    }else{
      revision_statements_list$exploration_fusion_statement <- glue("I then proceeded with exploration of the prior fusion. Overlying scar was excised from the posterior elements and the fusion was inspected and examined for any motion at the previously fused levels.")
    }
    
    procedure_details_list$exploration <- paste("Once the exposure was completed, ",
                                                glue_collapse(revision_statements_list, sep = " "))
  }
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  procedure_details_list$procedures <- op_note_procedure_paragraphs_function(objects_added_df = all_objects_to_add_df,
                                                                             revision_decompression_vector = revision_decompression_vector)
  
  
  ############# COMPLETING INSTRUMENTATION ###########
  ############# COMPLETING INSTRUMENTATION ###########
  posterior_implants_all_df <- all_objects_to_add_df %>%
    mutate(procedure_category = op_note_procedure_category_function(object = object)) %>%
    filter(str_detect(string = procedure_category, pattern = "instrumentation")) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number, procedure_category, object, side) %>%
    arrange(vertebral_number) %>%
    remove_missing() %>%
    group_by(level, object) %>%
    add_tally(name = "total_per_level") %>%
    mutate(side = if_else(total_per_level == 2, "bilateral", side)) %>%
    ungroup() %>%
    distinct() 
  
  posterior_implant_df <- posterior_implants_all_df %>%
    filter(procedure_category != "pelvic instrumentation") %>%
    mutate(level = if_else(level == "Ileum", "Pelvis", level)) %>%
    mutate(level = if_else(level == "S2AI", "Pelvis", level))
  
  if(nrow(posterior_implant_df) > 0){
    
    if(any(str_detect(posterior_implant_df$side, "bilateral"))){
      procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                     glue("{left_main_rod_size} {left_main_rod_material} rod was contoured for the left and a {right_main_rod_size} {right_main_rod_material}"),
                                                                     "rod was contoured for the right and the rods placed into position and secured with set screws.",
                                                                     additional_rods_statement,
                                                                     "The set screws were then tightened with a final tightener to the appropriate torque. Intraoperative Xray was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                     glue_collapse(x = posterior_implants_all_df$level, sep = ', ', last = ', and '), 
                                                                     ".") 
      
    }else{
      if(any(str_detect(posterior_implant_df$side, "left"))){
        procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                       glue("{left_main_rod_size} {left_main_rod_material}"),
                                                                       " rod was contoured for the left and placed into position and secured with set screws.",
                                                                       additional_rods_statement,
                                                                       "The set screws were then tightened with a final tightener to the appropriate torque. Intraoperative Xray was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                       glue_collapse(x = posterior_implants_all_df$level, sep = ', ', last = ', and '), 
                                                                       ".") 
      }else{
        procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                       glue("{right_main_rod_size} {right_main_rod_material}"),
                                                                       " rod was contoured for the right and placed into position and secured with set screws.",
                                                                       additional_rods_statement,
                                                                       "The set screws were then tightened with a final tightener to the appropriate torque. Intraoperative Xray was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                       glue_collapse(x = posterior_implants_all_df$level, sep = ', ', last = ', and '), 
                                                                       ".") 
      }
    }
    
  }
  
  
  #############   #############   #############  FUSIONS FUSIONS ###########  #############   ############# 
  #############   #############   #############  FUSIONS FUSIONS ###########  #############   ############# 
  
  fusion_statement_list <- list()
  
  if(nrow(fusion_levels_df) > 0){
    fusion_segments_df <- fusion_levels_df %>%
      separate(col = level, into = c("proximal", "distal")) %>%
      pivot_longer(cols = c(proximal, distal), names_to = "name", values_to = "level") %>%
      select(level) %>%
      left_join(levels_numbered_df) %>%
      arrange(vertebral_number) %>%
      distinct() 
    
    fusion_statement_list$fusion_begin_statement <- glue("I then proceeded with the spinal fusion portion of the procedure. The posterior elements of {glue_collapse(fusion_segments_df$level, sep = ', ', last = ', and ')} were decorticated.")
    
    if(bmp > 0){
      fusion_statement_list$bmp_statement <- glue("To improve the odds of a successful fusion, {bmp}mg of BMP was placed into the posterior fusion bed. ")
    }
    
    if(any(str_detect(string = bone_graft_vector, pattern = "Morselized"))){
      
      morselized_graft_df <- tibble(graft_type = discard(.x = bone_graft_vector, .p = ~ str_detect(string = .x, pattern =  "Structural"))) %>%
        mutate(morselized_allo_amount = morselized_allograft) %>%
        mutate(graft_type = if_else(morselized_allo_amount > 0 & graft_type == "Morselized Allograft", 
                                    paste0("A total of ", morselized_allo_amount, "(cc) of Morselized allograft"), 
                                    graft_type))
      
      fusion_statement_list$morselized_allograft_statement <- glue("{glue_collapse(morselized_graft_df$graft_type, sep = ', ', last = ' and ')} was then impacted into the lateral gutters and into the facet joints.")
    }else{
      fusion_statement_list$morselized_allograft_statement <- glue("Allograft chips and and local autograft was then impacted into the lateral gutters and into the facet joints of {glue_collapse(fusion_levels_df$level, sep = ', ', last = ', and ')}.")
    }
    
    fusion_statement_list$complete <- glue("This completed the posterior spinal fusion procedure of {glue_collapse(fusion_levels_df$level, sep = ', ', last = ', and ')}.")
    
    procedure_details_list$posterior_fusion_details <- glue_collapse(fusion_statement_list, sep = " ")
  }
  
  ############################# CLOSURE #########################
  
  
  closure_statements_list <- list()
  
  closure_statements_list$start <- "I then proceeded with closure of the wound."
  
  
  
  if(length(end_procedure_details) > 0){
    closure_statements_list$added_to_wound <- glue("{glue_collapse(end_procedure_details, sep = ', ', last = ' and ')} was then placed into the surgical bed.")
  }
  
  if(deep_drains >0){
    closure_statements_list$deep_drains_statement <- case_when(deep_drains == 1 ~ 'One drain was placed deep to the fascial layer.',
                                                               deep_drains >1 ~ paste(glue("A total of {deep_drains} drains were placed deep to the fascial layer.")))
  }
  
  closure_statements_list$layered_closure <- "The wound was then closed in a layered fashion. The fascial layer was closed with a watertight closure."
  
  if(superficial_drains > 0){
    closure_statements_list$superficial_drains_statement <- case_when(superficial_drains == 1 ~ 'One drain was placed superficial to the fascial layer.',
                                                                      superficial_drains >1 ~ paste(glue("A total of {superficial_drains} drains were placed superficial to the fascial layer.")))
  }
  
  if(length(closure > 0)){
    closure_statements_list$superficial_closure <- paste("The subdermal layer was closed and",
                                                         str_to_lower(glue_collapse(closure, sep = ', ', last = ' and ')),
                                                         if_else(length(closure) == 1, "was", "were"),
                                                         "used to close the skin layer."
    )
  }else{
    closure_statements_list$superficial_closure <- "The subdermal layers and skin layers were then closed."
    
  }
  
  closure_statements_list$dressing <- glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")
  
  procedure_details_list$closure <- glue_collapse(closure_statements_list, sep = " ")
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "a 8", replacement = "an 8"))
  
  ### FINAL PARAGRAPH ####
  
  procedures_listed <- op_note_procedures_present_listed_function(objects_added_df = all_objects_to_add_df, 
                                                                                                revision_decompression_vector = revision_decompression_vector, 
                                                                                                fusion_levels_vector = fusion_levels_df$level, 
                                                                                                additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. The drapes were removed and the patient was turned onto the hospital bed, and awoke uneventfully. I was personally present for the entirety of the case, including {procedures_listed}.")
  
  procedure_paragraphs <- glue_collapse(x = procedure_details_list, sep = "\n\n")
  
  return(list(procedure_details_paragraph = procedure_paragraphs, 
              procedures_numbered_paragraph = procedures_numbered_list[[1]]))
  
}





