
function(files,name){
   relevantfiles <- files[str_detect(files,name)] 

   if(length(relevantfiles) == 0){
      stop(glue('No files matched {name}'))
   }

   filenumbers <- relevantfiles %>%
      sapply(str_replace_all,
             pattern = '[^0-9]',
             replacement = '') %>%
      as.numeric()

   names(filenumbers) <- relevantfiles
   sorted <- sort(filenumbers, decreasing = TRUE)

   if(length(sorted) == 0){
      stop(glue('No files contained numbers')) 
   } else {
      latest <- sorted[1]
   }
   names(latest)
}
