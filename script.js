
/* 
 * Allow plot updating to be triggered by both pressing enter, 
 * and hitting the "refresh" button.
 */

$(document).ready(function(){
   let enterpress = 1;
   $("#controlpanel").on('change',$.debounce(250,function(e){
      enterpress = enterpress + 1;
      Shiny.setInputValue("enterpress",enterpress);
   }))
   $(function () {
        $('[data-toggle="popover"]').popover()
   })
});
