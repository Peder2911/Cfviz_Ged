
function(sum_w_missing){

   ged <- sum_w_missing

   # ================================

   datebreaks <- 'years'

   # Plotting ==========================================

   daysum <- ged %>% group_by(date) %>% summarize(totcnt = sum(cnt))

   upperlim_1 <- max(daysum$totcnt,na.rm = T) * 1.5 
   upperlim_2 <- upperlim_1 - (upperlim_1*0.15)
   ylab <- 'Battle-related deaths'

   scaleargs <- list(expand = c(0,0), limits = c(0,upperlim_1))
   labargs <- list(x = 'Month',
                   y = 'Deaths (monthly)')

   gedtl <- ggplot(ged, enviroment = .e)+
     geom_col(aes(x = date, y = cnt))+
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
                  date_labels = '%m-%Y') +
     do.call(scale_y_continuous,scaleargs) +
     #scale_y_continuous(expand = c(0,0), limits = c(0,upperlim_1)) +
     do.call(labs,labargs)

  saveRDS(gedtl,'tee.rds')
  gedtl
}
