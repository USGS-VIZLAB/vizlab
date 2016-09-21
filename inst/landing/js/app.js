$(document).ready(function(){
	
	$.when(
		$.getJSON('json/vizzy.json')
	).then(function(json){
		var sections = [
			{file:'js/templates/header.handlebars', div: '#header', context: {}},
			{file:'js/templates/vizzies.handlebars', div: '#visualizationsContainer', context: json}
		];
		
		$.each(sections, function(index, value){
			//Gets mustache file
			$.get(value.file, function(data){
				var compiledTemplate = Handlebars.compile(data);
				var html = compiledTemplate(value.context);
				//Places mustache file in correct location
				$(value.div).html(html);
			});
		});
	});
	
	

});