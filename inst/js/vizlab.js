if (typeof $ === "undefined") {
  console.error("jQuery required");
}

(function() {
  var vizlab = {};
  vizlab.analytics = {};
  vizlab.load = {};

  /* Call this on page ready to initialize the viz */
  vizlab.init = function() {
    vizlab.analytics.init();
    vizlab.load.init();
  };

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

  vizlab.load.init = function() {
    vizlab.load.inject();
    // TODO: here we can hide the loading overlay when it exists
  };

  vizlab.load.inject = function() {
    SVGInjector($("img.vizlab-inject"), {
      evalScripts: "once",
      pngFallback: "images/fallback",
      each: function(svg) {
        // TODO do we need to do anything per svg?
      }
    }, function(count) {
      $(document).trigger("vizlab.ready");
    });
  };

  vizlab.clicklink = function(url) {
    ga('send', 'event', 'outbound', 'click', url, {
       'transport': 'beacon',
       'hitCallback': function(){document.location = url;}
     });
  };

  vizlab.ready = function(callback) {
    $(document).on("vizlab.ready", callback);
  };

  window.vizlab = vizlab;
})();

$(document).ready(vizlab.init);
