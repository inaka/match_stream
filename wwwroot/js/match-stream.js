function connect(options) {
	if(options.port)
		socket = new io.Socket(location.hostname, {port : parseInt(options.port,10)});
	else
		socket = new io.Socket(location.hostname);
	
	socket.on('message', function(data) {
		if (data.error)
			alert(JSON.stringify(data.desc));
		else
			append(data)
	});
	
	socket.connect();
};
function append(data) {
	var clear = $('<div>');
	clear.addClass('clear');
	
	var time = $('<div>');
	time.addClass('event-time');
	time.append(fancy_minute(data.timestamp));
	
	var text = $('<div>');
	text.addClass('event-text');
	text.append($('<strong>').append(translate(data)));
	
	var detail = $('<div>');
	detail.addClass('event-detail');
	detail.append(time);
	
	var main = $('<div>');
	main.addClass('event-main');
	main.append(detail).append(text).append(clear);
	
	var box = $('<div>');
	box.addClass('event-box');
	box.append(main).append(clear);
	
	var li = $('<li>');
	li.attr('id', data.timestamp);
	li.append(box);
	
	return $('#events').append(li);
}

function fancy_minute(ts) {
	return ts + "'";
}

function translate(data) {
	return data.kind + ": " + JSON.stringify(data.data)
}