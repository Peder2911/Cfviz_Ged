
function(ged,cfs,acd,usenames = TRUE,range = c(1989,2019)){

   # ================================

   datebreaks <- 'years'

   range_start <- ymd(glue('{range[1]}-01-01'))
   range_end <- ymd(glue('{range[2]}-12-31'))

   # Plotting ==========================================

   daysum <- ged %>% group_by(date) %>% summarize(totcnt = sum(cnt))

   upperlim_1 <- max(daysum$totcnt,na.rm = T) * 1.5 
   upperlim_2 <- upperlim_1 - (upperlim_1*0.15)
   ylab <- 'Battle-related deaths'

   scaleargs <- list(expand = c(0,0), limits = c(0,upperlim_1))
   labargs <- list(x = 'Month',
                   y = 'Deaths (monthly)')

   gedAesArgs <- list(x = as.symbol('date'), 
                      y = as.symbol('cnt'))
   cfSegmentArgs <- list(x = as.symbol('start'),
                         xend = as.symbol('start'),
                         y = 0,
                         yend = as.symbol('upperlim_2'))
   cfPointArgs <- list(x = as.symbol('start'), y = as.symbol('upperlim_2'))
   cfTextArgs <- cfPointArgs
   cfTextArgs$label <- as.symbol('start')

   if(usenames){
      ged <- merge(ged,acd, by ='conflict_id')
      gedAesArgs$fill <- as.symbol('conflict_name')

      cfs <- merge(cfs,acd, by ='conflict_id')
      cfPointArgs$color <- as.symbol('conflict_name')
      cfSegmentArgs$color <- as.symbol('conflict_name')

      pcolors <- dget('functions/priocolors.R') %>% unlist() %>% rep(10)

      allCids <- union(cfs$conflict_name, ged$conflict_name) 

      colorscale <- list(scale_discrete_manual(aesthetics = c('color','fill'), values = pcolors[1:length(allCids)]))
      #colorscale <- lapply(list(scale_color_manual, scale_fill_manual),function(f){
      #   f(values = unique(union(cfs$conflict_name, ged$conflict_name)))
      #   }) 
   } else {
      colorscale <- list()
   }

   if(nrow(cfs) > 0){
      ceasefire_aestetics <- list(
        geom_point(data = cfs,do.call(aes, cfPointArgs), color = 'green'), 
        geom_segment(data = cfs,do.call(aes, cfSegmentArgs), color = 'red'),
        geom_text(data = cfs,do.call(aes, cfTextArgs), 
                  angle = -45, size = 3, hjust = 1.1, check_overlap = TRUE), color = 'blue')
   } else {
      ceasefire_aestetics <- list()
   }
   if(nrow(ged) > 0){
      ged_aestetics <- list(geom_col(do.call(aes,gedAesArgs)))
   } else {
      ged_aestetics <- list()
   }

   .e <- environment()

   gedtl <- ggplot(ged, enviroment = .e)+
      ged_aestetics + 
      ceasefire_aestetics + 
      theme_classic()+
      theme(axis.line.y = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1),
            panel.grid.major.x = element_line(color = 'gray'),
            #panel.grid.minor.x = element_line(color = 'light gray'),
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.key.size = unit(0.2,units = 'cm'),
            legend.spacing.x = unit(0.1,units = 'cm'),
            legend.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1, unit = 'cm'),
            plot.margin = margin(t = 0.3,l = 0.1,r = 0.3,unit = 'cm')) + 
      scale_x_date(expand = c(0,0), date_breaks = datebreaks, 
                   date_labels = '%m-%Y',limits = c(range_start,range_end)) +
      colorscale + 
      do.call(scale_y_continuous,scaleargs) +
      do.call(labs,labargs)
   gedtl
}
