// require d3 (can only do this is d3 has been installed, `npm install d3`)
var d3 = require('d3');

// webpack import functions
import {add_circles} from './modules/circles';

// setup
var h = 200;
var w = 700;
var margin = {
  top: 20,
  bottom: 60,
  left: 60,
  right: 60
};
var plotwidth = w - margin.left - margin.right;
var plotheight = h - margin.top - margin.bottom;

// create an svg element
var svg = d3.select("body").select("#circle_fig")
    .append("svg")
      .attr("id", "plotarea") // id == #plot in css, class == .plot in css
      .attr("width", w)
      .attr("height", h);

add_circles([1,6,7,2]);
