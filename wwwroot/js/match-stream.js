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

	window.mid = getMatchId();
	window.uuid = getUserId();

    socket.on('connect', function(){
        socket.send({
        	"command":"watch",
            "uid":window.uuid, 
            "mid":window.mid
        });
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

//Get the user id from local storage or cookie
function getUserId() {
    var thereIsLocalStorage = typeof window.localStorage!=='undefined';
    var uuid;
    if(thereIsLocalStorage) {  
        uuid = localStorage['uuid'];
    } else {                            
        uuid = new RegExp('(?:^|; )' +  
            encodeURIComponent('uuid') + 
            '=([^;]*)').exec(document.cookie);
        if (uuid != null) { uuid = uuid[1]; }
    }
    if (uuid == null)
    	uuid = setUserId();
    return uuid;
}

//Set user id to local storage or cookie
function setUserId() {
	var uuid = guid();
    var thereIsLocalStorage = typeof window.localStorage!=='undefined';
    if(thereIsLocalStorage) {
        localStorage['uuid'] = uuid;
    } else {
        var dt = new Date();
        dt.setTime(dt.getTime() + 31536000000);
        document.cookie="uuid=" + uuid + "; expires=" + dt.toGMTString() + "; path=/";
    }
    return uuid;
}

function guid() {
    var chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.split(''); 
    var uuid = [];

    var r;

    // rfc4122 requires these characters
    uuid[8] = uuid[13] = uuid[18] = uuid[23] = '-';
    uuid[14] = '4';

    // Fill in random data.  At i==19 set the high bits of clock sequence as
    // per rfc4122, sec. 4.1.5
    for (var i = 0; i < 36; i++) {
        if (!uuid[i]) {
            r = 0 | Math.random()*16;
            uuid[i] = chars[(i == 19) ? (r & 0x3) | 0x8 : r];
        }
    }

    return 'ms-' + uuid.join('');
}

function getMatchId() {
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i=0;i<vars.length;i++) {
        var pair = vars[i].split("=");
        if (pair[0] == 'mid') {
            return pair[1];
        }
    }
    return null;
}