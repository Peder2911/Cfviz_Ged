
function(x,y){
   sapply(as.character(x), function(val){
      y[val]
   }, USE.NAMES = FALSE)
}

