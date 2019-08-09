
function(ged){
   if(nrow(ged) > 0){
      ged$date_start <- as.Date(ged$date_start)
      ged$date_end <- as.Date(ged$date_end)

      rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
         # Yields a vector of length N and sum M
         # "randomly distributes" M outcomes on N cases

         vec <- rnorm(N, M/N, sd)
         if (abs(sum(vec)) < 0.01) vec <- vec + 1
            vec <- round(vec / sum(vec) * M)
            deviation <- M - sum(vec)
            for (. in seq_len(abs(deviation))) { 
            vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
         }
         if (pos.only) while (any(vec < 0)) {
            negs <- vec < 0
            pos  <- vec > 0
            vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
            vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
         }
         vec
      }

      explodeGed <- function(ged){
        # Distributes deaths from ged periods among period days
        # distribution is uniformly random
        
        ged$rowname <- row.names(ged)
        ged$diff <- ged$date_end - ged$date_start
        ged <- ged[rep(1:nrow(ged), ged$diff + 1),]
        
        ged$consec <- sequence(rle(ged$rowname)$lengths) -1
        ged$date <- ged$date_start + ged$consec
        
        ged$deathsdistrib <- apply(ged[ged$consec == 0,], 1, function(row){
          
          rdiff <- str_extract(row['diff'],'[0-9]+') %>%
            as.numeric() + 1
          vec <- rand_vect(rdiff , as.numeric(row['best']))
        }) %>% unlist()
        ged
      }

      gexpl <- explodeGed(ged)

      # Summarization =====================================

      gexpl <- rename(gexpl, dyad_id = dyad_new_id,
                             conflict_id = conflict_new_id)

      # This is important stuff!
      # Groups all events by MONTH 
      ged_sum <- gexpl %>%
        group_by(year(date),month(date),dyad_name, dyad_id, conflict_id) %>%
        summarize(cnt = sum(deathsdistrib)) %>% # oops!
        mutate(date = as.Date(paste0(`year(date)`,'-',`month(date)`,'-01'))) # Monthly!

      dyads <- unique(ged_sum$dyad_name)
      ids <- unique(ged_sum$dyad_id)
      #cids <- unique(ged_sum$conflict_id)
      span <- range(ged_sum$date)

      span <- seq.Date(span[1],span[2],by = 'month')

      missing <- lapply(span ,function(d){
        data.frame(date = d,dyad_name = dyads, dyad_id = ids)
      }) %>%
        do.call(rbind, .)

      sum_w_missing <- merge(missing ,ged_sum ,by = c('date','dyad_name'), all.x = TRUE) %>%
         select(date,
                cnt,
                dyad = dyad_name,
                dyad_id = dyad_id.x,
                conflict_id)

      sum_w_missing$cnt <- ifelse(is.na(sum_w_missing$cnt),
                                  0,
                                  sum_w_missing$cnt)

      sum_w_missing
   } else {
      tibble(date = integer(),
             cnt = integer(),
             dyad = character(),
             dyad_id = integer())
   }
}
