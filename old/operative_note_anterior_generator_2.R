# operative_note_anterior_generator_2

identify_cranial_caudal_interspace_body_list_function <- function(level, segment_or_interspace_start = "segment"){
  cranial_interspace_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = vertebral_number + 0.5) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  cranial_level_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = vertebral_number + 1) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  caudal_interspace_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = vertebral_number - 0.5) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  caudal_level_df <- tibble(level = level) %>%
    left_join(levels_numbered_df) %>%
    mutate(vertebral_number = vertebral_number - 1) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number)
  
  if(segment_or_interspace_start == "segment"){
    return(list(cranial_interspace = cranial_interspace_df$level[[1]],
                cranial_level = cranial_level_df$level[[1]],
                caudal_interspace = caudal_interspace_df$level[[1]],
                caudal_level = cranial_level_df$level[[1]]
    )) 
  }else{
    return(list(cranial_interspace = cranial_level_df$level[[1]],
                cranial_level = cranial_interspace_df$level[[1]],
                caudal_interspace = caudal_level_df$level[[1]],
                caudal_level = caudal_interspace_df$level[[1]]
    ))
  }
}

# "Disk Arthroplasty" = "disk_arthroplasty",
# "Diskectomy & Fusion + Interbody object" = "diskectomy_fusion",
# "Diskectomy & Fusion (No object)" = "diskectomy_fusion_no_interbody_device",
# "Corpectomy" = "corpectomy",
# "Corpectomy Cage" = "corpectomy_cage",
# "Anterior Plate" = "anterior_plate",
# "Anterior Buttress Plate" = "anterior_plate",
# "Screw +/Washer" = "screw_washer"


op_note_procedure_category_function <- function(object){
  procedure_category <- case_when(
    object == "disk_arthroplasty" ~ "disk arthroplasty",
    object == "decompression_diskectomy_fusion" ~ "anterior diskectomy and fusion with decompression of the spinal cord and nerve roots",
    object == "diskectomy_fusion" ~ "anterior diskectomy and fusion",
    object == "diskectomy_fusion_no_interbody_device" ~ "posterior spinal instrumentation",
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
    procedure_cat == 'decompression' ~ "combine",
    procedure_cat == 'decompression using a costovertebral approach' ~ "distinct",
    procedure_cat == 'decompression using a transpedicular approach' ~ "distinct",
    procedure_cat == 'revision and reexploration with decompression' ~ "combine",
    procedure_cat == 'revision and reexploration with decompression using a costovertebral approach' ~ "distinct",
    procedure_cat == 'revision and reexploration with decompression using a transpedicular approach' ~ "distinct",
    procedure_cat == 'laminoplasty' ~ "combine",
    procedure_cat == 'lateral lumbar interbody fusion and insertion of interbody device' ~ "distinct",
    procedure_cat == 'transforaminal lumbar interbody fusion and insertion of interbody device' ~ "distinct",
    procedure_cat == 'interbody fusion (without interbody implant)' ~ "distinct",
    procedure_cat == 'posterior lumbar interbody fusion and insertion of interbody device' ~ "distinct"
  )
  paragraph_type
}


op_note_technique_statement <- function(object, levels_side_df, revision_modifier = "xx"){
  
  revision_statement <- if_else(revision_modifier == "xx", " ", " reexploration and revision decompression with a ")
  

  levels_side_df <- levels_side_df %>%
    group_by(level) %>%
    add_tally(name = "number_per_level") %>%
    mutate(level_side = as.character(if_else(number_per_level == 1, glue("on the {side} at {level}"), glue("on the left and on the right at {level}")))) %>%
    ungroup() %>%
    select(-side) %>%
    distinct()
  
  technique_statement <- case_when(
    object == "cement_augmentation" ~ glue("For vertebral body cement augmentation, a cannula was inserted into the vertebral body through the pedicle. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Cement augmentation was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "laminar_downgoing_hook" ~ glue("For downgoing laminar hooks, the ligamentum flavum was removed at the site of the hook insertion. A hook was then inserted securely under the lamina {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "laminar_upgoing_hook" ~ glue("For upgoing laminar hooks, the inferior edge of the cranial lamina was identified and the plane between the lamina and ligamentum was developed and the upgoing hook is then placed within this plane, secured to the lamina aimed cranially {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "lateral_mass_screw" ~ glue("For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a high speed burr and the screw path drilled incrementally to the far cortex. Lateral mass screws were placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "occipital_screw" ~ glue("An appropriately sized occipital plate was selected and was placed centered in the midline and just caudal to the external occipital protuberance, but cranial to the foramen magnum. The plate was held in place to identify the appropriate start points for the occipital screws. The occipital screws were drilled incrementally until the anterior cortex was penetrated. The length of the path was measured to acheive bicortical fixation. The screw paths were then tapped and the screws placed, securing the plate to the occiput."), 
    object == "pars_screw" ~ glue("For pars screws, the start point was identified just 3-4mm cranial to the inferior facet joint and in the midpoint of the pars from medial to lateral. Once the start point was identified, the superficial cortex was opened at the entry point using the high speed burr. A drill was used to cannulate a screw path, aiming as dorsal as possible while not perforating the dorsal cortex of the pars. The path was then tapped and the length measured and pars screw placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pedicle_screw" ~ glue("For pedicle screws, the transverse process, pars, and superior facet were used as landmarks to identify the appropriate starting point. After identifying the start point, the superficial cortex was opened at each entry point using a high speed burr. A pedicle probe was then used to navigate down the pedicle, followed by palpating a medial, lateral, superior and inferior pedicle wall, measuring, and tapping if appropriate. Pedicle screws were placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pedicle_hook" ~ glue("For pedicle hooks, a pedicle finder was carefully placed just under the inferior facet and above the superior facet, until the pedicle was found. Once the pedicle was identified, a pedicle hook was inserted directly toward the inferior pedicle, secured within the residual facet joint {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pelvic_screw" ~ pelvic_screw_statement,
    object == "sublaminar_wire" ~ glue("For sublaminar wiring, the doubled-up sublaminar was was bent into a hook shape and and passed under the inferior lamina from a caudal to cranial direction. Once the wire was safely passed, the tip of the wire was cut and the two strands were directed in opposite directions of the midline to be attached to the rods on the left and right side. Wires were passed under {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tether" ~ glue("For posterior vertebral tethering, a hole was drilled in the transverse process over the lamina. A band was then thread through the hole and tied in a figure-8 fashion, tethering {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tp_hook" ~ glue("For transverse process hooks, the cranial edge of the transverse process was identified. A transverse process finder was used to develop the plane and a transverse process hook was placed securely into position along the superior edge of the transverse process."),  
    object == "translaminar_screw" ~ glue("For translaminar screw fixation, the starting point was identified using the spinolaminar junction and in the central plane of the contralateral lamina. A burr hole was made to open the superficial cortex, the path was then cannulated and laminar screws placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."
    ),
    
    object == 'complete_facetectomy' ~ glue("For a complete facetectomy, the inferior, superior, medial and lateral borders of the inferior and superior facet were identified. Both the superior and inferior facet were completely excised and the underlying nerve root decompression. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} and the bone was morselized to be used as morselized autograft."),
    object == 'grade_1' ~ glue("The inferior, superior, medial and lateral borders of the inferior facet were identified. The inferior facets {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} were excised and the bone was morselized to be used as morselized autograft."),
    
    object == 'grade_2' ~ glue("For the posterior column osteotomies, a small rent was made in the interlaminar space to develop the plane between the ligamentum and the dura. A Kerrison rongeur was then used to excise the ligamentum. this was carried out laterally in both directions until the facets were encountered. The superior and inferior facet were both adequately resected to fully release the posterior column. I performed posterior column (Smith-Peterson) osteotomies at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}"),
    
    object == 'diskectomy' ~ glue("For a diskectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a diskectomy with partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Using a high-speed burr and Kerrison rongeurs, I first performed a foraminotomy and excised the ligamentum flavum. The dural sac was identified and traversing root was protected. The annulus was then incised and the diseased disk materal was removed. Following the decompression, the canal and foramen and lateral recess were palpated to confirm an adequate decompression had been completed."),
    
    object == 'laminotomy' ~ glue("For the laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. Once I had determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    
    object == 'sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a sublaminar decompression bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I performed partial laminectomies and excised the ligamentum flavum, exposing the dura. The medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace, to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess. Following the decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_diskectomy' ~ glue("For the revision diskectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a revision and reexploration with diskectomy and partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. Using a high-speed burr and Kerrison rongeurs, I performed a foraminotomy and excised the scar and ligamentum. The dural sac was identified and traversing root was protected. The annulus and disk space were identified and the diseased disk material was excised. Following the revision decompression, the canal and foramen and lateral recess were palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_laminotomy' ~ glue("For the revision laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. I carefully exposed the dorsal elements until I had identified the edges of the laminar defect. Once I determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a revision laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a reexploration and revision laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. Using a combination of a high-speed burr, currettes and Kerrison rongeurs, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I removed any dorsal lamina, ligamentum flavum and scar thought to still be causing compression. The central canal was palpated to confirm full decompression of the thecal sac. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    
    object == 'revision_sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a reexploration and revision sublaminar decompression bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I proceeded with partial laminectomies and excised the scar and underlyingligamentum flavum. The medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace, to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess. Following the revision decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    
    object == 'laminoplasty' ~ glue("For laminoplasty, an opening and hinge trough was created with a high-speed burr angled perpendicular to the lamina at the lateral mass-lamina junction. This was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. After all troughs were completed, greenstick fractures were created to open the lamina at the laminoplasty levels, the ligamentum flavum at the proximal and distal levels was excised, and laminoplasty plates were sequentially secured into place. The spinous processes were trimmed and the canal was palpated to confirm an adequate decompression had been completed.")
  )
  technique_statement
}



op_note_distinct_technique_statement <- function(object, level, side, revision_modifier = "xx"){
  
  revision_statement <- if_else(revision_modifier == "xx", "", "reexploration and revision decompression with a")
  
  
  
  technique_statement <- case_when(
    object == 'transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for decompression from the {side} at {level}. Using a combination of a high-speed burr and Kerrison rongeurs, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed. This completed the decompression through a transpedicular approach at {level}."),
    object == 'costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and an adequate amount of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr and Kerrison rongeurs, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed. This completed the decompression through a costovertebral approach at {level}."),
    object == 'revision_transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for a reexploration and revision decompression from the {side} at {level}. Using a combination of a high-speed burr, currettes, and Kerrison rongeurs, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed. This completed the reexploration and revision decompression through a transpedicular approach at {level}."),
    object == 'revision_costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for a reexploration and revision decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and an adequate amount of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr, currettes, and Kerrison rongeurs, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed. This completed the reexploration and revision decompression through a costovertebral approach at {level}."),
    object == 'grade_3' ~ glue("I then proceeded with the pedicle subtraction osteotomy at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed at the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. Both pedicles were skeletonized at {level} and the cranial and caudal nerve roots were identified and fully decompressed from the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle to the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle. Once the dorsal decompression was complete, we began the osteotomy. A subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperiteal structures. The dura, cranial and caudal nerve roots were protected. A high speed burr was used to bore out the pedicle and develop a defect in the vertebral body at the level of the pedicle. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, taking care to not perforate the anterior cortex and the pedicle wall was excised. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once an adequate defect had been generated in the vertebral body, the dorsal wall of the vertebral body was excised, the osteotomy was closed, and the other rod was placed into position and set screws final tightened. Intraoperative xray was performed to confirm adequate lordosis. This completed the pedicle subtraction osteotomy at the {level} level."),
    object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed at the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. Both pedicles were skeletonized at {level} and the cranial and caudal nerve roots were identified and fully decompressed from the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle to the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle. Once the dorsal decompression was complete, we began the osteotomy. A subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperiteal structures. The dura, cranial and caudal nerve roots were protected. A high speed burr was used to bore out the pedicle and develop a defect in the vertebral body at the level of the pedicle and extending into the cranial disk space. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, taking care to not perforate the anterior cortex and the pedicle wall was excised. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once an adequate defect (over 50%) had been generated in the vertebral body, the dorsal wall of the vertebral body was excised, the osteotomy was closed, and the other rod was placed into position and set screws final tightened. Intraoperative xray was performed to confirm adequate lordosis. This completed the extended three column osteotomy/partial corpectomy at the {level} level."),
    object == 'grade_5' ~ glue("I then proceeded with a vertebral column resection at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed at the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. Both pedicles were skeletonized at {level} and the cranial and caudal nerve roots were identified and fully decompressed from the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle to the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle. Once the dorsal decompression was complete, we began the vertebral resection. A subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperiteal structures. The dura, cranial and caudal nerve roots were protected. A high speed burr was used to bore out the pedicle and develop a defect in the vertebral body at the level of the pedicle and extending into the cranial disk space. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, using both pedicles to access the body. We then placed a temporary rod. Retractors were held in place and the remaining portion of the entire vertebral body was removed. The other rod was placed into position and the deformity was corrected. Intraoperative xray was performed to confirm adequate alignment. This completed the vertebral column resection at the {level} level."),
    object == 'no_implant_interbody_fusion' ~ glue("I then proceeded with the interbody fusion from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, the size was confirmed and the interbody implant was placed into position along with allograft. The final position was confirmed using intraoperative xray."),
    object == 'plif' ~ glue("I then proceeded with the posterior lumbar interbody fusion (PLIF) procedure and placement of interbody implant at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were partially resected and the exiting and traversing nerve roots were appropriately protected. The dura was retracted in order to access the disc space. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. Once I was satisfied with the size and fit of the trial, the interbody implant was placed into the interbody space along with allograft bone. The final position of the implant was confirmed using intraoperative xray."),
    object == "tlif" ~ glue("I then proceeded with the transforaminal interbody fusion (TLIF) procedure and placement of interbody implant from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. Once I was satisfied with the size and fit of the trial, the interbody implant was placed into the interbody space along with allograft bone. The final position of the implant was confirmed using intraoperative xray.")
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
                                                    ..3 = side),
                                          .f = ~op_note_distinct_technique_statement(object = ..1, level = ..2, side = ..3))) %>% 
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
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      mutate(procedure_category = op_note_procedure_category_function(object = object)) %>%
      select(level, vertebral_number, procedure_category, object, side) %>%
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
      select(level, vertebral_number, procedure_category, object, side) %>%
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


















op_note_procedure_performed_summary_classifier_function <- function(object){
  procedure_category <- case_when(
    object == "cement_augmentation" ~ "Vertebral cement augmentation",
    object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
    object == "laminar_upgoing_hook" ~ "Posterior spinal instrumentation",
    object == "lateral_mass_screw" ~ "Posterior spinal instrumentation",
    object == "occipital_screw" ~ "Occiput instrumentation",
    object == "pars_screw" ~ "Posterior spinal instrumentation",
    object == "pedicle_hook" ~ "Posterior spinal instrumentation",
    object == "pedicle_screw" ~ "Posterior spinal instrumentation",
    object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
    object == "sublaminar_wire" ~ "Posterior spinal instrumentation",
    object == "tp_hook" ~ "Posterior spinal instrumentation",
    object == "translaminar_screw" ~ "Posterior spinal instrumentation",
    object == "pelvic_screw" ~ "Pelvic instrumentation",
    object == "tether" ~ "Spinous process posterior tethering/wiring",
    object == "costovertebral_approach" ~ "Decompression using a costovertebral approach",
    object == "revision_costovertebral_approach" ~ "Reexploration and revision decompression using a costovertebral approach",
    object == "transpedicular_approach" ~ "Decompression using a transpedicular approach",
    object == "revision_transpedicular_approach" ~ "Reexploration and revision decompression using a transpedicular approach",
    object == "diskectomy" ~ "Decompression with diskectomy and laminotomy",
    object == "sublaminar_decompression" ~ "Decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies",
    object == "laminectomy" ~ "Decompression with central laminectomy",
    object == "laminotomy" ~  "Decompression with laminotomy and medial facetectomy",
    
    object == "revision_diskectomy" ~ "Reexploration and revision decompression with diskectomy and laminotomy",
    object == "revision_sublaminar_decompression" ~ "Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies",
    object == "revision_laminectomy" ~ "Reexploration and revision decompression with central laminectomy",
    object == "revision_laminotomy" ~ "Reexploration and revision decompression with laminotomy and medial facetectomy",
    object == "laminoplasty" ~ "Laminoplasty",
    object == "grade_1" ~ "Inferior facetectomies",
    object == "complete_facetectomy" ~ "Complete facetectomy",
    object == "grade_2" ~ "Posterior column osteotomy",
    object == "grade_3" ~ "Pedicle subtraction osteotomy",
    object == "grade_4" ~ "Extended three column osteotomy (vertebral body partial corpectomy)",
    object == "grade_5" ~ "Vertebral column resection",
    object == "no_implant_interbody_fusion" ~ "Interbody fusion (without interbody implant)",
    object == "llif" ~ "Lateral lumbar interbody fusion and insertion of interbody device",
    object == "plif" ~ "Posterior lumbar interbody fusion and insertion of interbody device",
    object == "tlif" ~ "Transforaminal lumbar interbody fusion and insertion of interbody device"
  )
  procedure_category
}

op_note_procedure_performed_summary_per_line_function <- function(procedure_type){
  procedures_per_line <- case_when(
    procedure_type == 'Vertebral cement augmentation' ~ 'multiple',
    procedure_type == 'Posterior spinal instrumentation' ~ 'multiple',
    procedure_type == 'Occiput instrumentation' ~ 'multiple',
    procedure_type == 'Pelvic instrumentation' ~ 'multiple',
    procedure_type == 'Spinous process posterior tethering/wiring' ~ 'multiple',
    procedure_type == 'Complete facetectomy' ~ 'multiple',
    procedure_type == 'Inferior facetectomies' ~ 'multiple',
    procedure_type == 'Posterior column osteotomy' ~ 'multiple',
    procedure_type == 'Pedicle subtraction osteotomy' ~ 'one',
    procedure_type == 'Extended three column osteotomy (vertebral body partial corpectomy)' ~ 'one',
    procedure_type == 'Vertebral column resection' ~ 'one',
    procedure_type == 'Decompression with diskectomy and laminotomy' ~ 'multiple',
    procedure_type == 'Decompression with laminotomy and medial facetectomy' ~ 'multiple',
    procedure_type == 'Decompression with central laminectomy' ~ 'multiple',
    procedure_type == 'Decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies' ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with diskectomy and laminotomy" ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies" ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with central laminectomy" ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with laminotomy and medial facetectomy"~ 'multiple',
    procedure_type == 'Laminoplasty' ~ 'multiple',
    procedure_type == 'Decompression using a transpedicular approach' ~ 'one',
    procedure_type == 'Decompression using a costovertebral approach' ~ 'one',
    procedure_type == 'Reexploration and revision decompression using a transpedicular approach' ~ 'one',
    procedure_type == 'Reexploration and revision decompression using a costovertebral approach' ~ 'one',
    procedure_type == 'Interbody fusion (without interbody implant)' ~ 'one',
    procedure_type == 'Lateral lumbar interbody fusion and insertion of interbody device' ~ 'one',
    procedure_type == 'Transforaminal lumbar interbody fusion and insertion of interbody device' ~ 'one',
    procedure_type == 'Interbody fusion (without interbody implant)' ~ 'one',
    procedure_type == 'Posterior lumbar interbody fusion and insertion of interbody device' ~ 'one',
  )
  procedures_per_line
}

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
    add_row(procedure_performed_statement = if_else(is.null(fusion_levels_vector), "", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="") %>%
    mutate(count = row_number()) %>%
    mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    select(procedures_performed_numbered)
  
  glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n")
}

op_note_procedures_present_listed_function <- function(objects_added_df,
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
    add_row(procedure_performed_statement = if_else(is.null(fusion_levels_vector), "", paste("posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="")
  
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
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  procedure_details_list$procedures <- op_note_procedure_paragraphs_function(objects_added_df = all_objects_to_add_df,
                                                                             revision_decompression_vector = revision_decompression_vector)
  
  
  ############# COMPLETING INSTRUMENTATION ###########
  ############# COMPLETING INSTRUMENTATION ###########
  posterior_implant_df <- all_objects_to_add_df %>%
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
    distinct() %>%
    filter(procedure_category != "pelvic instrumentation")
  
  if(nrow(posterior_implant_df) > 0){
    
    if(any(str_detect(posterior_implant_df$side, "bilateral"))){
      procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                     glue("{left_main_rod_size} {left_main_rod_material} rod was contoured for the left and a {right_main_rod_size} {right_main_rod_material}"),
                                                                     "rod was contoured for the right and the rods placed into position and secured with set screws.",
                                                                     additional_rods_statement,
                                                                     "The set screws were then tightened with a final tightener to the appropriate torque. Intraoperative Xray was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                     glue_collapse(x = posterior_implant_df$level, sep = ', ', last = ', and '), 
                                                                     ".") 
    }else{
      if(any(str_detect(posterior_implant_df$side, "left"))){
        procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                       glue("{left_main_rod_size} {left_main_rod_material}"),
                                                                       " rod was contoured for the left and placed into position and secured with set screws.",
                                                                       additional_rods_statement,
                                                                       "The set screws were then tightened with a final tightener to the appropriate torque. Intraoperative Xray was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                       glue_collapse(x = posterior_implant_df$level, sep = ', ', last = ', and '), 
                                                                       ".") 
      }else{
        procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                       glue("{right_main_rod_size} {right_main_rod_material}"),
                                                                       " rod was contoured for the right and placed into position and secured with set screws.",
                                                                       additional_rods_statement,
                                                                       "The set screws were then tightened with a final tightener to the appropriate torque. Intraoperative Xray was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                       glue_collapse(x = posterior_implant_df$level, sep = ', ', last = ', and '), 
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
      
      fusion_statement_list$morselized_allograft_statement <- glue("{glue_collapse(morselized_graft_df$graft_type, sep = ', ', last = ' and ')} was then impacted into the lateral gutters and into the facet joints of {glue_collapse(fusion_levels_df$level, sep = ', ', last = ', and ')}.")
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





