$(function() {
	$(".card").each( function() {
		$(this).draggable({
			stop: function() {
				$.ajax({
					type: 'post',
					url: '/ajax',
					dataType: 'json',
					data: JSON.stringify({'name':'1234', 'age':28 }),
					contentType: 'application/json'
				});
			}
		});
	});
});



