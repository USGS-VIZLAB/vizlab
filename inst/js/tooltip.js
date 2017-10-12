var hoverTimer = null;
var hoverDelay = 400; //ms
  
function hovertext(text, evt){
  var tooltip = document.getElementById("tooltip-text");
  var tooltip_bg = document.getElementById("tooltip-box");    
  var tool_pt = document.getElementById("tooltip-point");
  if (evt === undefined){
    if(hoverTimer) {
      clearTimeout(hoverTimer); //stop when off area
    }
    tooltip.firstChild.data = ' ';
    tooltip_bg.setAttribute("class","hidden");
    tooltip_bg.setAttribute("x",0);
    tool_pt.setAttribute("class","hidden");
  } else {
    pt = cursorPoint(evt);
    pt.x = Math.round(pt.x);
    pt.y = Math.round(pt.y);
    var svgWidth = Number(svg.getAttribute("viewBox").split(" ")[2]);
    tooltip.setAttribute("x",pt.x);
    tooltip.setAttribute("y",pt.y);
    tooltip.firstChild.data = text;
    var length = Math.round(tooltip.getComputedTextLength());
    if (pt.x - length/2 - 6 < 0){
      tooltip.setAttribute("x",length/2+6);
    } else if (pt.x + length/2 + 6 > svgWidth) {
      tooltip.setAttribute("x", svgWidth-length/2-6);
    }
    tool_pt.setAttribute("transform","translate("+pt.x+","+pt.y+")");
    tooltip_bg.setAttribute("x",tooltip.getAttribute("x")-length/2-6);
    tooltip_bg.setAttribute("y",pt.y-35);
    tooltip.setAttribute("class","shown");
    tooltip_bg.setAttribute("class","tooltip-box");
    tool_pt.setAttribute("class","tooltip-box");
    tooltip_bg.setAttribute("width", length+12);
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