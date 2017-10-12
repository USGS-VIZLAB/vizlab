var hoverTimer = null;
var hoverDelay = 400; //ms
  
function hovertext(text, evt){
  var tipG = d3.selectAll('#tooltip-group');
  if (tipG.empty()) {
    console.error("a #tooltip-group <g/> element is required");
  }  
  // remove all children of the tooltip group:
  tipG.selectAll("*").remove();
  
  if (evt === undefined){
    if(hoverTimer) {
      clearTimeout(hoverTimer); //stop when off area
    }
  } else {
    var tooltip_bg = tipG.append('rect').attr("id", "tooltip-box").attr("height", 24).attr("class", "tooltip-box");
    var tool_pt = tipG.append('path').attr("id", "tooltip-point").attr("d", "M-6,-11 l6,10 l6,-11").attr("class","tooltip-box");
    var tooltip = tipG.append('text').attr("id", "tooltip-text").attr("dy","-1.1em").attr('text-anchor',"middle").attr("class","tooltip-text-label svg-text").text(text);
    pt = cursorPoint(evt);
    pt.x = Math.round(pt.x);
    pt.y = Math.round(pt.y);
    var svgWidth = Number(svg.getAttribute("viewBox").split(" ")[2]);
    tooltip.attr("x",pt.x).attr("y",pt.y);
    var length = Math.round(tooltip.node().getComputedTextLength());
    if (pt.x - length/2 - 6 < 0){
      tooltip.attr("x",length/2+6);
    } else if (pt.x + length/2 + 6 > svgWidth) {
      tooltip.attr("x", svgWidth-length/2-6);
    }
    tool_pt.attr("transform","translate("+pt.x+","+pt.y+")");
    tooltip_bg.attr("x", tooltip.attr("x")-length/2-6).attr("y",pt.y-35).attr("class","tooltip-box").attr("width", length+12);

    if(hoverTimer){
      clearTimeout(hoverTimer);
    }
    hoverTimer = setTimeout(function(){
      ga("send", "event", "figure", evt.target.id);
    }, hoverDelay);
  }
}
function cursorPoint(evt){  
  pt.x = evt.clientX; pt.y = evt.clientY;
  return pt.matrixTransform(svg.getScreenCTM().inverse());
}