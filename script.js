
/* 
 * Allow plot updating to be triggered by both pressing enter, 
 * and hitting the "refresh" button.
 */

let enterpress = 1;

$(document).on('shiny:inputchanged', function(event){
   if(event.name == 'refresh'){
      enterpress = enterpress + 1;
      Shiny.setInputValue("enterpress",enterpress);
   }
});

$(document).on("keypress", function(e){
   if(e.keyCode == 13){
      enterpress = enterpress + 1;
      Shiny.setInputValue("enterpress",enterpress);
   }
});
