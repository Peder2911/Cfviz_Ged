
function(ged, cfs, gedtype, range = c(1989,2019),
         categoryName, colors, dategrouping){

   if(class(ged$date) == "numeric"){
      class(ged$date) = "Date"
   }


   supplement <- function(call,args){
      # Basically c(call,args), but retaining the
      # call nature of `call`
      for(n in names(args)){
         call[[n]] <- args[[n]]
      }
      call 
   }

   # ================================================
   # Scales =========================================
   # ================================================

   # Y ==============================================

   datesum <- ged %>% group_by(date) %>% summarize(totcnt = sum(cnt))
   upperlim_1 <- max(max(datesum$totcnt,na.rm = T) * 1.5, 1)
   upperlim_2 <- upperlim_1 - (upperlim_1*0.15)

   scale_args_y <- list(expand = c(0,0), limits = c(0,upperlim_1))

   yscale <- call('scale_y_continuous',
                  expand = c(0,0), limits = c(0, upperlim_1)) %>%
      eval()

   # X ==============================================

   range_start <- as.Date(glue('{range[1]}-01-01'))
   range_end <- as.Date(glue('{range[2]}-01-01'))

   rangeSize <- range[2] - range[1]
   dateBreaks <- local({
      if(rangeSize > 9){
         "years"
      } else if(rangeSize > 2){
         "6 months"
      } else if(rangeSize > 1){
         "3 months"
      } else {
         "months"
      }
   })
   dateLabels <- ifelse(dateBreaks == "years",
                        "%Y", "%m-%Y")

   xscale <- call('scale_x_date', date_breaks = dateBreaks, expand = c(0,0),
                  date_labels = dateLabels, limits = c(range_start,range_end)) %>%
      eval()

   # ================================================
   # GED aes ========================================
   # ================================================
   # Building the geoms by constructing `call`s =====

   ged_col_aes <- call('aes', x = quote(date), y = quote(cnt))
   
   ged_geom <- local({
      #TODO implement this
      #if(TRUE){
         #geomcall[[1]] <- 'geom_col'
      #} else if(gedtype == 'line'){
         #geomcall[[1]] <- 'geom_line'
         #geomcall$position <- 'stack'
      #}
      geomcall <- call('geom_path', mapping = eval(ged_col_aes),
                       size = 1.5, color = "#333333")
   })

   ged_geoms <- list(ged_geom)

   if(nrow(ged) > 0){
      ged_geoms <- lapply(ged_geoms, eval, envir = environment()) 
   } else {
      ged_geoms <- list()
   }

   # ================================================
   # CF aes==========================================
   # ================================================
   # More involved than the GED geoms ===============

   # First set up base ==============================
   cf_base_aes <- call("aes", x = quote(start), y = quote(upperlim_2))
   cf_base <- call(' ', data = quote(cfs))

   if(all(is.na(cfs$category))){
      cf_base$color <- sample(colors, size = 1) 
      colorscale <- list()
   } else {
      cf_base_aes$color <- quote(category) 
      cf_base_aes$fill <- quote(category)

      nncolors <- colors
      #names(nncolors) <- NULL
      colorscale <- scale_discrete_manual(aesthetics = c('color','fill'),
                                          values = rep(nncolors, 5))
   }
   cf_base$mapping <- eval(cf_base_aes)

   # Points, simple =================================
   cf_point <- cf_base
   cf_point[[1]] <- geom_point
   cf_point <- cf_point %>%
      supplement(list(size = 3, shape = 18))
 
   # Segments, a bit more involved ==================
   cf_segment <- cf_base
   cf_segment[[1]] <- geom_segment 
   cf_segment$mapping <- cf_base_aes %>%
      supplement(list(xend = quote(start), y = 0, yend = quote(upperlim_2))) %>%
      eval()
   cf_segment$size <- 1.2 

   # Text, also some options ========================
   cf_text <- cf_base
   cf_text[[1]] <- geom_text 

   cf_text <- cf_text %>%
      supplement(list(angle = -45, hjust = 1.2, size = 3,
                     check_overlap = TRUE))

   cf_text$mapping <- cf_base_aes %>%
      supplement(list(label = quote(start))) %>%
      eval()

   # Assembly =======================================
   cf_geoms <- list(cf_point, cf_segment, cf_text)
   #if(nrow(cfs) > 0){
      cf_geoms <- lapply(cf_geoms, eval, envir = environment())
   #} else {
   #   cf_geoms <- list()
   #}

   # ================================================
   # Labels and theming =============================
   # ================================================

   casLabel <- switch(dategrouping,
                      `3 months`="Three months",
                      months = "Monthly",
                      weeks = "Weekly")

   plotlabels <- labs(x = '', y = glue('Casualties ({casLabel})'),
                      color = categoryName, fill = categoryName)
   plottheme <- theme_classic() %>%
      supplement(list(
         axis.line.y = element_blank(),
         axis.title.y = element_text(size = 15, angle = 90,
                                     vjust = 1,
                                     margin = margin(-5,0,5,0, unit = "mm")),
         axis.text.y = element_text(size = 12,
                                    hjust = 1,
                                    margin = margin(-2,2,-2,0, unit = "mm")),

         axis.title.x = element_blank(),
         axis.text.x = element_text(angle = 45,
                                    hjust = 0.8,
                                    size = 15),
         panel.grid.major.x = element_line(color = 'gray'),

         legend.position = 'none',
         plot.margin = margin(t = 0,l = 0,r = 0.5, b = 1,unit = 'cm')))

   # ================================================
   # ================================================
   # ================================================

   .e <- environment()
   ggplot(ged, enviroment = .e)+
      cf_geoms + 
      ged_geoms + 
      xscale + 
      yscale + 
      colorscale +
      plottheme + 
      plotlabels 
}
