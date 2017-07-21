function connect(options) {
	if(options.port)
		socket = new io.Socket(location.hostname, {rememberTransport: false, reconnect: false, port : parseInt(options.port,10)});
	else
		socket = new io.Socket(location.hostname, {rememberTransport: false, reconnect: false});
	
	socket.on('message', function(data) {
		if (data.error)
			alert(JSON.stringify(data.desc));
		else if(data.source == "event")
			append(data.content);
		else if(data.source == "twitter")
			twitter(data.content)
	});

	window.mid = getMatchId();
	window.user = getUserInfo();
	$('#commentbox').attr('readonly', !window.user.twitter);

    socket.on('connect', function(){
    	console.log("trying to watch " + window.mid),
        socket.send({
        	"command":"watch",
            "uid":window.user.id, 
            "mid":window.mid
        });
        updateClock();
    });
	socket.connect();
};
function append(data) {
	console.log(data);
	if(data.kind == "status") {
		window.match = data;
		update_status();
	} else {
		update_match(data);
		show_event(data)
	}
}

function update_match(data) {
	switch(data.kind) {
	case "start":
		window.match.period_start = data.timestamp;
		window.match.home_players = data.home_players;
		window.match.visit_players = data.visit_players;
		window.match.home_score = 0;
		window.match.visit_score = 0;
		window.match.period = "first";
		break;
	case "halftime":
		window.match.period = "halftime";
		window.match.period_start = data.timestamp;
		break;
	case "continue":
		window.match.period = "last";
		window.match.period_start = data.timestamp;
		break;
	case "penalties":
		window.match.period = "penalties";
		window.match.period_start = data.timestamp;
		break;
	case "stop":
		window.match.period = "ended";
		window.match.period_start = data.timestamp;
		break;
	case "goal":
		if(window.match.home.team_id == data.team)
			window.match.home_score++;
		else if(window.match.visit.team_id == data.team)
			window.match.visit_score++;
		break;
	case "card":
		if(data.card == "red") {
			if(window.match.home.team_id == data.team)
				for(var n in data.player) delete window.match.home_players[n];
			else if(window.match.visit.team_id == data.team)
				for(var n in data.player) delete window.match.visit_players[n];
		}
		break;
	case "substitution":
		if(window.match.home.team_id == data.team) {
			for(var n in data.player_out) delete window.match.home_players[n];
			for(var n in data.player_in) window.match.home_players[n] = data.player_in[n];
		} else if(window.match.visit.team_id == data.team) {
			for(var n in data.player_out) delete window.match.visit_players[n];
			for(var n in data.player_in) window.match.visit_players[n] = data.player_in[n];
		}
		break;
	}
	window.match.timestamp = data.timestamp;
	update_status();
}

function show_event(data) {
	var clear = $('<div>');
	clear.addClass('clear');

	var time = $('<div>');
	time.addClass('event-time');
	time.append(fancy_seconds(data.timestamp));

	var text = $('<div>');
	text.addClass('event-text');
	text.append($('<strong>').append(translate(data)));
	if(data.comment)
		text.append($('<span>').addClass('comment').append(data.comment));

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
	li.addClass(data.kind);
	li.attr('id', data.timestamp);
	li.append(box);

	$('#events').append(li);
	$('#events')[0].scrollTop = $('#events')[0].scrollHeight;
}

function update_status() {
	$('#home-crest-img')[0].src = "img/teams/" + window.match.home.team_id + ".png";
	$('#visit-crest-img')[0].src = "img/teams/" + window.match.visit.team_id + ".png";
	$('#score').text(window.match.home_score + "-" + window.match.visit_score);
	$('#time').text(fancy_period(window.match.period, window.match.timestamp, window.match.period_start));
	$('#stadium').text(window.match.stadium);
	$('#home-name').text(window.match.home.name);
	$('#visit-name').text(window.match.visit.name);
}

function fancy_period(period, current) {
	switch(period) {
	case "first": return "PT " + fancy_seconds(current);
	case "last": return "ST " + fancy_seconds(current);
	case "halftime": return "ET " + fancy_seconds(current);
	case "penalties": return "Penales";
	case "not_started": return "No iniciado";
	case "ended": return "Terminado";
	default: return period
	}
}

function fancy_seconds(current) {
	var ts = current - window.match.period_start;
	var mins = Math.floor(ts / 60000);
	var secs = Math.floor((ts - mins * 60000) / 1000);
	mins = mins < 0 ? 0 : mins;
	return (mins < 10 ? "0" : "") + mins + ":" + (secs < 10 ? "0" : "") + secs;
}

function fancy_minute(current) {
	var ts = current - window.match.period_start;
	var mins = Math.floor(ts / 60000);
	return (mins < 10 ? "0" : "") + mins + "'";
}

function translate(data) {
	switch(data.kind) {
	case "start": return "El partido ha comenzado";
	case "halftime": return "Entretiempo";
	case "continue": return "Comienza el segundo tiempo";
	case "stop": return "Final del tiempo reglamentario";
	case "penalties": return "El partido se definir&aacute; por penales";
	case "shot":
		for(var n in data.player)
			return "Disparo al arco de " + n + " (" + team_name(data.team) + ")";
	case "save":
		for(var n in data.player)
			return "Atajada de " + n + " (" + team_name(data.team) + ")";
	case "goal":
		for(var n in data.player)
			return "GOL de " + team_name(data.team) + "!! Lo hizo " +  n;
	case "corner":
		return "Tiro de esquina para " + team_name(data.team);
	case "goalkick":
		return "Saque desde el arco para " + team_name(data.team);
	case "offside":
		for(var n in data.player)
			return "Posici&oacute;n adelantada de " + n + " (" + team_name(data.team) + ")";
	case "foul":
		for(var n in data.player)
			return "Infracci&oacute;n de " + n + " (" + team_name(data.team) + ")";
	case "penalty":
		for(var n in data.player)
			return "Penal para " + team_name(data.team) + ". Lo patea " +  n;
	case "freekick":
		for(var n in data.player)
			return "Tiro libre para " + team_name(data.team) + ". Lo patea " +  n;
	case "card":
		if(data.card == "red")
			for(var n in data.player)
				return team_name(data.team) + " se queda con un hombre menos, expulsado " +  n;
		else
			for(var n in data.player)
				return n + " (" + team_name(data.team) + ") es amonestado";
	case "substitution":
		var p_in, p_out;
		for(var n in data.player_in) p_in = n;
		for(var n in data.player_out) p_out = n;
		return "Cambio en " + team_name(data.team) + ": Entra " + p_in + ". Se retira " + p_out;
	case "throwin":
		return "Lateral para " + team_name(data.team);
	default:
		return data.kind
	}
}

//Get the user id from local storage or cookie
function getUserInfo() {
    var thereIsLocalStorage = typeof window.localStorage!=='undefined';
    var user;
    if(thereIsLocalStorage) {  
        user = localStorage['user'];
    } else {
        user = new RegExp('(?:^|; )' +
            encodeURIComponent('user') + 
            '=([^;]*)').exec(document.cookie);
        if (user != null) { user = user[1]; }
    }
    if (user == null)
    	user = setUserInfo();
    return user;
}

//Set user id to local storage or cookie
function setUserInfo() {
	var user = {"id": guid()};
    var thereIsLocalStorage = typeof window.localStorage!=='undefined';
    if(thereIsLocalStorage) {
        localStorage['user'] = user;
    } else {
        var dt = new Date();
        dt.setTime(dt.getTime() + 31536000000);
        document.cookie="user=" + user + "; expires=" + dt.toGMTString() + "; path=/";
    }
    return user;
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

function updateClock() {
	if(window.match && window.match.period) {
		if(window.match.period != "not_started" &&
				window.match.period != "ended")
			window.match.timestamp += 1000;
		$('#time').text(fancy_period(window.match.period, window.match.timestamp, window.match.period_start));
	}
	return setTimeout("updateClock()",1000);
}

function team_name(team_id) {
	if(window.match && window.match.home && window.match.home.team_id == team_id && window.match.home.name)
		return window.match.home.name;
	if(window.match && window.match.visit && window.match.visit.team_id == team_id && window.match.visit.name)
		return window.match.visit.name;
}

// --------- TWITTER --------------------------------------------------
function logoutTwitter() {
	window.user.twitter = null;
	storeUserInfo();
	hideUserInfoBox();
	$('#logged-in-username').html('');
	$('div#tweets .spotlight').addClass('hidden-admin');
}