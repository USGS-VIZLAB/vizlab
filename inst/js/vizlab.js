(function() {
  var vizlab = {};

  if (typeof $ === "undefined") {
    console.err("jQuery required");
    return;
  }

  vizlab.analytics = {};

  vizlab.analytics.init = function() {
    var addClickHandler = $('.vizClick');
    $.each(addClickHandler, function(index, value) {
      value.on("click", vizlab.analytics.click);
    });

    vizlab.analytics.chapters = $('.vizScroll').map(function(){return "#" + this.id});
    $(window).scroll(vizlab.analytics.scrollwatch);
  };

  vizlab.analytics.inview = function (el) {
    var rect = el.getBoundingClientRect();

    return rect.bottom > 0 &&
      rect.right > 0 &&
      rect.left < (window.innerWidth || document.documentElement.clientWidth) &&
      rect.top < (window.innerHeight || document.documentElement.clientHeight);
  };

  var triggers = {}; // closure scope triggers
  vizlab.analytics.chapterScroll = function(ids) {
    $.each(vizlab.analytics.chapters, function(index, value) {
      if (!triggers[value] && vizlab.analytics.inview($(value)[0])) {
        triggers[value] = true; // trigger
        ga('send', 'event', 'chapter', 'scrolled to ' + value);
      }
    });
  };

  var scrollTimer = null; // closure scope timer
  var SCROLL_DELAY = 250; // ms
  vizlab.analytics.scrollwatch = function(){
    if (scrollTimer) {
      clearTimeout(scrollTimer);
    }
    scrollTimer = setTimeout(vizlab.analytics.chapterScroll, SCROLL_DELAY);
  };

  vizlab.clicklink = function(url) {
    ga('send', 'event', 'outbound', 'click', url, {
       'transport': 'beacon',
       'hitCallback': function(){document.location = url;}
     });
  };

  window.vizlab = vizlab;
})();
