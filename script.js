
/* 
 * Allow plot updating to be triggered by both pressing enter, 
 * and hitting the "refresh" button.
 */

let enterpress = 1;
let plotchangers = ['refresh','usenames'];

$(document).on('shiny:inputchanged', function(event){
   // If string in list...
   if(plotchangers.indexOf(event.name) > -1){
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
