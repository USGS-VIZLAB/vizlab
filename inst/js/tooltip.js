var hoverTimer = null;
var hoverDelay = 400; //ms

function hovertext(text, evt){
  
  // remove all children from any tooltip group:
  d3.selectAll("#tooltip-group").selectAll("*").remove();
  
  if (evt === undefined){
    if(hoverTimer) {
      clearTimeout(hoverTimer); //stop when off area
    }
  } else {
    
    var textBuffer = 6; // px between text edge and tooltipPath border
    var tipYoffset = -1; // so that the tip is slightly above the mouse location
    var tipPointer = {x:6, y:10}; // dimensions on the tooltip pointer
    
    // the svg that this element belongs to
    var thisSVG = evt.target.ownerSVGElement;
    var tipG = d3.select(thisSVG).select('#tooltip-group');
    if (tipG.empty()) {
      console.error("a #tooltip-group <g/> element is required");
    }  
    
    // turn the mouse location into a properly scaled set of coordinates
    var svgPoint = cursorPoint(evt, thisSVG);
    var svgDims = thisSVG.getAttribute("viewBox").split(" ");
    var svgLeftBound = Number(svgDims[0]);
    var svgRightBound = Number(svgDims[2]) + svgLeftBound;
    var svgTopBound = Number(svgDims[1]);
    var tooltipX = svgPoint.x;
    var tooltipY = svgPoint.y;
    
    // create the tooltip border
    var tooltipPath = tipG.append('path')
      .attr("id", "tooltip-box")
      .attr("class", "tooltip-box");
    
    var tooltipText = tipG.append('text')
      .attr("id", "tooltip-text")
      .attr("alignment-baseline", "middle")
      .attr("dy", "0.1em")
      .attr("text-anchor","middle")
      .attr("class","tooltip-text-label svg-text")
      .text(text);
    
    var textBox = tooltipText.node().getBBox();
    var textLength = Math.round(textBox.width);
    var textHeight = Math.round(textBox.height);
    var halfLength = textLength / 2;
    
    
    // modify the border if part of it is outside of the bounds
    if (svgPoint.x - halfLength - textBuffer < svgLeftBound)  {
      tooltipX = halfLength + textBuffer;
    }
    else if (svgPoint.x + halfLength + textBuffer > svgRightBound) {
      tooltipX = svgRightBound - halfLength - textBuffer;
    } 

    var totHeight = tipPointer.y + textHeight + textBuffer;
    if (svgPoint.y - totHeight < 0){
      tipPointer.y = svgPoint.y - totHeight + tipPointer.y;
      tooltipY = tooltipY - (svgPoint.y - totHeight);
    }
    
    tooltipText.attr("x", tooltipX);
    if (tipPointer.y < - (tipYoffset)){ 
      // is a rectangle w/ no tip
      
      tooltipPath.attr("d", 
        "M" + (tooltipX - halfLength - textBuffer) + "," + svgTopBound +
        " H" + (tooltipX + halfLength + textBuffer) +
        " v" + (textHeight + textBuffer) +
        " H" + (tooltipX - halfLength - textBuffer)+"Z");
        
      tooltipText.attr("y", svgTopBound + (textHeight+textBuffer) / 2);  
    } else { 
      // is a normal tip w/ a triangle tip to it
      
      tooltipPath.attr("d", 
        "M" + (svgPoint.x - tipPointer.x) + "," + (svgPoint.y-tipPointer.y) +
        " l" + tipPointer.x + "," + (tipPointer.y + tipYoffset) +
        " l" + tipPointer.x + ",-" + (tipPointer.y + tipYoffset) +
        " H" + (tooltipX + halfLength + textBuffer) +
        " v-" + (textHeight + textBuffer) +
        " H" + (tooltipX - halfLength - textBuffer) +
        " v" + (textHeight + textBuffer) + "Z");
        
      tooltipText.attr("y", svgPoint.y-tipPointer.y - (textHeight+textBuffer) / 2);
    }
    
    
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
