$(document).ready(function(){
	$.when(
		$.getJSON('json/related.json')
	).then(function(json){
		var sections = [
			{file:'js/templates/footer.handlebars', div: 'footer', context: json},
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