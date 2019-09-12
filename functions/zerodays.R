
function(data,datecolumn,ncolumn,dateres = "days"){
   datecol <- as.character(substitute(datecolumn))
   ncol <- as.character(substitute(ncolumn))

   dates <- data[[datecol]]
   range <- seq(min(dates),max(dates),by=dateres)

   zerodates <- range[!range %in% dates]
   useSchema <- any(!names(data) %in% c(datecol,ncol))
   if(useSchema) schema <- unique(data[!names(data) %in% c(datecol,ncol)])
   zerodays <- lapply(zerodates,function(date){
      if(useSchema){
         zerodaySchema <- schema
         zerodaySchema[[datecol]] <- date 
         zerodaySchema[[ncol]] <- 0
      } else {
         zerodaySchema <- data.frame(date, ncolumn = 0)
         names(zerodaySchema) <- c(datecol,ncol)
      } 
      zerodaySchema
   }) %>%
      do.call(rbind, .)
   comb <- rbind(data,zerodays)
   comb[order(comb[[datecol]]),]
}
