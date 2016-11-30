document.addEventListener('DOMContentLoaded', function() {
  var menu = document.getElementById('navigation');
  document.getElementById('mobileBurger').onclick=function(){
    menu.style.display = (menu.style.display == 'block') ? 'none' : 'block';
  }
});
