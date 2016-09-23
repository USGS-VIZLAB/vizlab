$(document).ready(function(){
// init controller
var controller = new ScrollMagic.Controller();
var triggers = {};
var triggerOnce = function(event) {
  if (!triggers.hasOwnProperty(event)) {
    triggers[event] = false;
  }
  if (!triggers[event]) {
      var e = new Event(event);
      document.dispatchEvent(e);
      triggers[event] = true;
    }
}

//Fish
new ScrollMagic.Scene({
	triggerElement: "#figure2"
	})
	.setClassToggle("#wally", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Stream		
new ScrollMagic.Scene({
	triggerElement: "#figure2",
	offset:500
	})
	.setClassToggle("#bass", "awake") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

	
// show pin state
function updateBox (e) {
	if (e.type === "enter") {
		$("#pin p").text("Pinned.");
	} else {
		$("#pin p").text("Unpinned.");
		}
}
		
//Figure3 appears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	offset:"50%"
	})
	.setClassToggle("#fishTemperatureFig", "awake") // add class toggle
	.on("enter leave", updateBox)
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Figure3 is pinned	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	duration:200,
	offset:"290%"
	})
	.setPin('#figure3')
	.on("enter leave", updateBox)
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//First text disappears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	offset:"490%"
	})
	.setClassToggle("#first", "gone") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

//Second text appears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	offset:"490%"
	})
	.setClassToggle("#second", "here") // add class toggle
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);
	
//Second text appears	
new ScrollMagic.Scene({
	triggerElement: "#figure3",
	duration:200,
	offset:"490%"
	})
	.setPin('#figure3')
	.on("enter leave", updateBox)
	.on("enter", function() {
    triggerOnce("sortFishTrigger");
  	})
	.addIndicators() // add indicators (requires plugin)
	.addTo(controller);

});