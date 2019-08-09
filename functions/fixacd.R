function(acd){
   acd$sides <- paste0(acd$side_a, ', ', acd$side_b)
   acd_grp <- acd %>%
      group_by(conflict_id) %>%
      summarize(sides = glue_collapse(sides, sep = ', ') )
   acd_grp$conflict_name <- sapply(acd_grp$sides, function(actorstring){
      actors <- str_split(actorstring, ', ') %>%
         unlist() %>%
         unique() %>%
         glue_collapse(sep = ' - ')
   })
   acd_grp
}
