var hoverTimer = null;
var hoverDelay = 400; //ms
  
function hovertext(text, evt){
  
  // remove all children from any tooltip group:
  d3.selectAll("#tooltip-group").selectAll("*").remove();
  // does the above work to clear out multiple tooltips from multiple svgs?
  
  if (evt === undefined){
    if(hoverTimer) {
      clearTimeout(hoverTimer); //stop when off area
    }
  } else {
    var thisSVG = evt.target.ownerSVGElement;
    var tipG = d3.select(thisSVG).select('#tooltip-group');
    if (tipG.empty()) {
      console.error("a #tooltip-group <g/> element is required");
    }  
    
    var svgPoint = cursorPoint(evt, thisSVG);
    var svgWidth = Number(thisSVG.getAttribute("viewBox").split(" ")[2]);
    var textLength;
    var halfLength;
    var tooltipX;
    var tooltip_bg = tipG.append('rect').attr("id", "tooltip-box").attr("height", 24).attr("class", "tooltip-box");
    var tool_pt = tipG.append('path').attr("id", "tooltip-point").attr("d", "M-6,-11 l6,10 l6,-11").attr("class","tooltip-box");
    var tooltip = tipG.append('text').attr("id", "tooltip-text").attr("dy","-1.1em").attr('text-anchor',"middle").attr("class","tooltip-text-label svg-text").text(text);
    
    textLength = Math.round(tooltip.node().getComputedTextLength());
    halfLength = textLength / 2;
    
    
    if (svgPoint.x - halfLength - 6 < 0)  {
      tooltipX = halfLength + 6;
    }
    else if (svgPoint.x + halfLength + 6 > svgWidth) {
      tooltipX = svgWidth - halfLength - 6;
    } 
    else {
      tooltipX = svgPoint.x;
    }
    tooltip.attr("x", tooltipX).attr("y", svgPoint.y);
    tool_pt.attr("transform","translate(" + svgPoint.x + "," + svgPoint.y + ")");
    tooltip_bg.attr("x", tooltipX - halfLength - 6).attr("y", svgPoint.y - 35).attr("width", textLength + 12);
    
    if(hoverTimer){
      clearTimeout(hoverTimer);
    }
    hoverTimer = setTimeout(function(){
      ga("send", "event", "figure", evt.target.id);
    }, hoverDelay);
  }
}

function cursorPoint(evt, thisSVG){  
  pt.x = evt.clientX; 
  pt.y = evt.clientY;
  pt = pt.matrixTransform(thisSVG.getScreenCTM().inverse());
  pt.x = Math.round(pt.x);
  pt.y = Math.round(pt.y);
  return pt;
}
