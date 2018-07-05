function add_circles(data) {
  d3.select("#plotarea").selectAll("circle")
    .data(data)
    .enter()
    .append("circle")
      .attr("cx", function(d, i) { return i*100 + 20; })
      .attr("cy", function(d, i) { return Math.random()*200; })
      .attr("r", function(d) { return d*10; })
      .on("mouseover", function(d) {
        d3.select(this).attr("fill", "orange");
      })
      .on("mouseout", function() {
        d3.select(this).attr("fill", null);
      });
}

export {add_circles};
