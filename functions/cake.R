
function(ceasefires,colors){
   lab_sum <- ceasefires %>%
      group_by(category) %>%
      summarize(cnt = n())
   labs <- paste0(lab_sum$category,' - ',lab_sum$cnt)

   nncolors <- colors
#   names(nncolors) <- NULL
   ggplot(ceasefires,aes(x = "", y = 1, fill = category))+
      geom_col() +
      coord_polar(theta = "y",start = 0) +
      scale_fill_manual(values = rep(nncolors,5), labels = labs) +
      theme_void() +
      theme(text = element_text(size = 18),
            legend.position = "left",
            legend.key.size = unit(1,"cm")) + 
      labs(fill = "")
}
