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
    var tooltipX;
    
    var tooltip_bg= tipG.append('path')
      .attr("id", "tooltip-box")
      .attr("class", "tooltip-box");
    var tooltip = tipG.append('text')
      .attr("id", "tooltip-text")
      .attr("dy","-1em")
      .attr('text-anchor',"middle")
      .attr("class","tooltip-text-label svg-text")
      .text(text);
    
    var textBox = tooltip.node().getBBox();
    var textLength = Math.round(textBox.width);
    var textHeight = Math.round(textBox.height);
    var halfLength = textLength / 2;
    var textBuffer = 6;
    var tipYoffset = -2; // so that the tip is slightly above the mouse location
    var tipTriangle = {x:6, y:10};
    
    
    if (svgPoint.x - halfLength - textBuffer < 0)  {
      tooltipX = halfLength + textBuffer;
    }
    else if (svgPoint.x + halfLength + textBuffer > svgWidth) {
      tooltipX = svgWidth - halfLength - textBuffer;
    } 
    else {
      tooltipX = svgPoint.x;
    }
    tooltip.attr("x", tooltipX).attr("y", svgPoint.y);
    tooltip_bg.attr("d", 
      "M"+(svgPoint.x-tipTriangle.x)+","+(svgPoint.y-tipTriangle.y)+
      " l"+tipTriangle.x+","+(tipTriangle.y+tipYoffset)+
      " l"+tipTriangle.x+",-"+(tipTriangle.y+tipYoffset)+
      " H"+(tooltipX + halfLength + textBuffer)+
      " v-"+(textHeight+textBuffer)+
      " H"+(tooltipX - halfLength - textBuffer)+
      " v"+(textHeight+textBuffer)+"Z");
    
    if(hoverTimer){
      clearTimeout(hoverTimer);
    }
    hoverTimer = setTimeout(function(){
      ga("send", "event", "figure", evt.target.id);
    }, hoverDelay);
  }
}

function cursorPoint(evt, thisSVG){  
  var pt = thisSVG.createSVGPoint();
  pt.x = evt.clientX; 
  pt.y = evt.clientY;
  pt = pt.matrixTransform(thisSVG.getScreenCTM().inverse());
  pt.x = Math.round(pt.x);
  pt.y = Math.round(pt.y);
  return pt;
}
