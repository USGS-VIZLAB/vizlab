(function() {
  "use strict";
  var vizlab = {};
  
  /*
   * Provides operations on the svg element parameter
   * @param {SVGSVGElement} svg
   * 
   */
  vizlab.svg = function(svg) {
    
    var cursorPoint = function(screenX, screenY) {
      var point = svg.createSVGPoint();
      point = point.matrixTransform(svg.getScreenCTM().inverse());
      point.x = Math.round(point.x);
      point.y = Math.round(point.y);
      return point;
    }
    /*
     * @param {Number} - DOM x coordinate where tooltip should be rendered
     * @param {Number} - DOM y coordinate where tooltip should be rendered
     * @param {String or Function} tooltipText - Returns text to appear in tooltip box. If tooltipText is a function,
          the function parameters will be evt, options.
     */
    var showTooltip = function(x, y, tooltipText) {
      var tooltip = document.getElementById("tooltip-text");
      var tooltipBox = document.getElementById("tooltip-box");
      var toolPoint = document.getElementById("tooltip-point");
      var text = (typeof tooltipText === "function") ? tooltipText(options) : tooltipText;
      var svgPoint = cursorPoint(x, y);
      var svgWidth = Number(svg.getAttribute("viewBox").split(" ")[2]);
      var textLength;
      var halfLength;
      var tooltipX
      
      tooltip.firstChild.data = text;
      textLength = Math.round(tooltip.getComputedTextLength());
      halfLength = textLength / 2;
      
      /* Make sure tooltip text is within the SVG */
      if (svgPoint.x - halfLength - 6 < 0)  {
        tooltipX = halfLength + 6;
      }
      else if (svgPoint.x + halfLength + 6 > svgWidth) {
        tooltipX = svgWidth - halfLength - 6;
      } 
      else 
        tooltipX = svgPoint.x;
      }
      tooltip.setAttribute("x", tooltipX);
      tooltip.setAttribute("y", svgPoint.y);
      tooltip.setAttribute("class", "shown");
      
      /* Set attributes for background box */
      tooltipBox.setAttribute("x", tooltipX - halfLength - 6);
      tooltipBox.setAttribute("y", svgPoint - 35);
      tooltipBox.setAttribute("width", textLength + 12);
      tooltipBox.setAttribute("class", "tooltip-box");
      
      /* Set attributes for the tooltip point */
      tooltipPoint.setAttribute("transform", "translate(" + svgPoint.x + "," + svgPoint.y + ")");
      tooltipPoint.setAttribute("class", "tooltip-box");
    }
    
    var hideTooltip = function() {
      var tooltip = document.getElementById("tooltip-text");
      var tooltipBox = document.getElementById("tooltip-box");
      var toolPoint = document.getElementById("tooltip-point");
      
      tooltip.firstChild.data = " ";
      tooltipBox.setAttribute("class", "hidden");
      tooltipPoint.setAttribute("class", "hidden");
    }
  
  return {
    showTooltip : showTooltip,
    hideTooltip : hideTooltip
  } 
})();
