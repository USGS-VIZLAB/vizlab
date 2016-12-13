$(document).ready(function(){
  var navigation = $('#navigation');
  var menu = $('#menu');
  
  $(document).on('click', function(){
    $(navigation).hide();
  });
  
  $(menu).on('click', function(e){
    e.stopPropagation();
    $(navigation).toggle();
  });
});
 

