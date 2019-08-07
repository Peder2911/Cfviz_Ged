
function(ged,cfs,range = c(1989,2019)){

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

   gedtl <- ggplot(ged, enviroment = .e)+
     geom_col(aes(x = date, y = cnt))+
     geom_segment(data = cfs,aes(x = start, xend = start, y = 0, yend = upperlim_2))+
     geom_point(data = cfs,aes(x = start, y = upperlim_2)) + 
     geom_text(data = cfs,aes(x = start, y = upperlim_2, label = start), 
               angle = -45, size = 3, hjust = 1.1, check_overlap = TRUE) +
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
     do.call(scale_y_continuous,scaleargs) +
     #scale_y_continuous(expand = c(0,0), limits = c(0,upperlim_1)) +
     do.call(labs,labargs)

  gedtl
}
