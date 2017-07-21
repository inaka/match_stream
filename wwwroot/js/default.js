if(document.domain.search(/mtv\.com/) >=0) {
    document.domain = 'mtv.com';
    API_BASE = 'http://watchwith.mtv.com:81';
} else {
    document.domain = 'inakanetworks.com';
    API_BASE = 'http://italk.inakanetworks.com:8004';
}

WEB_SOCKET_SWF_LOCATION = "js/WebSocketMain.swf";
WEB_SOCKET_DEBUG = true;
DELAY = 30;
NORMAL_DELAY = 30;
SLOW_DELAY = 1000;
messages = new Array;
messages_index = 0;

slides = new Array;
slides_index = 0;

spotlights = new Array;
spotlights_index = 0;

window.current_message_position = 0;
window.last_message_position_received = 0;
window.user_time_drift = 0;
window.is_stream_paused = false;
window.user_is_moving_slider = false;
window.is_live_button_visible = false;
window.lastMinuteShowed = 0;
window.UserCheckinDone = false;
window.got_user_geoloc_response = false;

// Function to Timezone Detect and Ignore Daylight Saving Time (DST)
function TimezoneDetect() {
    var dtDate = new Date('1/1/' + (new Date()).getUTCFullYear());
    var intOffset = 10000; //set initial offset high so it is adjusted on the first attempt
    var intMonth;
 
    //go through each month to find the lowest offset to account for DST
    for (intMonth=0;intMonth < 12;intMonth++){
        //go to the next month
        dtDate.setUTCMonth(dtDate.getUTCMonth() + 1);
 
        //To ignore daylight saving time look for the lowest offset.
        //Since, during DST, the clock moves forward, it'll be a bigger number.
        if (intOffset > (dtDate.getTimezoneOffset() * (-1))){
            intOffset = (dtDate.getTimezoneOffset() * (-1));
        }
    }
 
    return intOffset;
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

    return 'web-' + uuid.join('');
}
function xhr() {
    if(navigator.appName != 'Opera') {
        return document.getElementById('xdr_iframe').contentWindow.xhr()
    } else {
        return XMLHttpRequest()
    }
}

function pauseStream() {
    window.is_stream_paused = true;
    $('.pausebtn').addClass('paused');
}

function unPauseStream() {
    window.is_stream_paused = false;
    window.user_time_drift = $('div#scrollbar').slider('value') - ((new Date).getTime() - window.episode_info.start_time);
    $('.pausebtn').removeClass('paused');
}

function slowDownStream() {
    DELAY = SLOW_DELAY;
}

function speedUpStream() {
    DELAY = NORMAL_DELAY;
}

function findClosestItem(params) {
    var items = params.items;
    var goal = params.goal;
    var discriminator = params.discriminator || 'none';
    var closest = null;
    for (var i = items.length - 1; i >= 0; i--) {
        if (closest == null || Math.abs(items[i].position - goal) < Math.abs(closest.position - goal)) {
            if((discriminator == 'higher-or-same' && items[i].position >= goal) ||
                (discriminator == 'lower-or-same' && items[i].position <= goal) ||
                discriminator == 'none') {
                closest = items[i];
            }
        }
    }
    
    if(closest == null) {
        switch (discriminator) {
            case 'higher-or-same':
                closest = items[items.length - 1];
                break;
            case 'lower-or-same':
                closest = items[0];
                break;
            case 'none':
                closest = items[0];
                break;
        }
    }
    
    return closest;
}

function findItemByGuid(collection, guid) {
    var message = null;
    for (var i = 0; i < collection.length; i++) {
        if (collection[i].guid == guid) {
            message = collection[i];
            break;
        }
    }
    return message;
}


function moveStreamTo(value) {
    window.user_time_drift = value - ((new Date).getTime() - window.episode_info.start_time);
    var is_stream_paused_now = window.is_stream_paused;
    var closest_message, closest_slide, closest_spotlight, last_position;
    switch(value) {
        case 0:
            closest_message = messages[0];
            break;
        case window.episode_duration:
            closest_message = messages[messages.length - 1];
            break;
        default:
            closest_message = findClosestItem({
                items: messages, 
                goal: value, 
                discriminator: 'lower-or-same'
            });
    }
    
    // I try to pick the first ones when the user moves to the beginning because
    // there may be more than one slide/spotlight with position 0
    if(value == 0) {
        closest_slide = slides[0];
        closest_spotlight = spotlights[0];
    } else {
        closest_slide = findClosestItem({
            items: slides, 
            goal: value, 
            discriminator: 'lower-or-same'
        });
        closest_spotlight = findClosestItem({
            items: spotlights, 
            goal: value, 
            discriminator: 'lower-or-same'
        });
    }
    
    var div_all_messages = document.getElementById('all-messages');
    var div_featured_messages = document.getElementById('featured-messages');
    var position = 0;
    if(!is_stream_paused_now) {
        pauseStream();
    }
    window.lastMinuteShowed = Math.round(getCurrentPosition() / 60000);
    if(closest_message != null) {
        $('div#scrollbar').slider('value', closest_message.position)
        $("div#all-messages").html('');
        $("div#featured-messages").html('');
        var closest_message_index = $.inArray(closest_message, messages);
        var closest_slide_index = $.inArray(closest_slide, slides);
        if(closest_slide_index == -1) {closest_slide_index = 0};
        var closest_spotlight_index = $.inArray(closest_spotlight, spotlights);
        if(closest_spotlight_index == -1) {closest_spotlight_index = 0};
        // First, I try to add some messages before the current, to fill the list
        position = closest_message_index;
        while(!isMessagesContainerFilled(div_all_messages) && position >= 0) {
            addMessageToList({
                'list': "div#all-messages", 
                'data': messages[position], 
                'mode': 'prepend',
                'position': position
            });
            position -= 1;
        }
        position = closest_message_index;
        while(!isMessagesContainerFilled(div_featured_messages) && position >= 0) {
            if(messages[position].featured) {
                addMessageToList({
                    'list': "div#featured-messages", 
                    'data': messages[position], 
                    'mode': 'prepend',
                    'position': position
                });
            }
            position -= 1;
        }

        messages_index = closest_message_index + 1;
        slides_index = closest_slide_index;
        spotlights_index = closest_spotlight_index;
    }

    if(!is_stream_paused_now) {
        unPauseStream();
    }
}
function initializeApp() {
    window.eid = getEpisodeId();
    if(window.eid == null) {
        // We can't continue without an eid
        return
    }
    var uuid = getUserId();
    if(uuid == null) {
        initializeUserId();
        return;
    }
    if(window.user_info == null) {
        initializeUserInfo(uuid, true);
        return
    }
    if(navigator.geolocation) {
        if(!window.got_user_geoloc_response) {
            // Set up a timeout just in case the user takes too much to give or not 
            // authorization to use his geoloc data
            window.askUserGeolocTimeout = setTimeout(
                function(){
                    window.got_user_geoloc_response = true;
                    initializeApp();
                }, 
                10000
            );
            navigator.geolocation.getCurrentPosition(
                function(position) {
                    // If the timeout function already returned when the user made a
                    // decision about his geoloc data, it's too late. We discard it
                    if(!window.got_user_geoloc_response) {
                        window.position = position;
                        window.got_user_geoloc_response = true;
                        clearTimeout(window.askUserGeolocTimeout);
                        initializeApp();
                    };
                },
                function(err) {
                    if(!window.got_user_geoloc_response) {
                        window.got_user_geoloc_response = true;
                        clearTimeout(window.askUserGeolocTimeout);
                        initializeApp();
                    }
                },
                {timeout:10000}
            )
            return
        }
    } else {
        window.got_user_geoloc_response = true;
    }
    if(!window.UserCheckinDone) {
        userCheckin(true);
        return;
    }

    if(window.episode_info == null) {
        getEpisodeInfo();
        return
    }
    
    if(window.episode_checkin_info == null) {
        episodeCheckin();
        return
    }
    
    if(window.show_info == null) {
        getShowInfo();
        return
    }
    
    if(window.episode_checkin_info.method == 'play') {
        $('iframe#play-iframe')[0].src = window.episode_checkin_info.url + '&uid=' + uuid + '&format=scripts'
    }
    if(window.episode_checkin_info.method == 'tunein') {
        $('span#status').html('<span style="color: green">LIVE</span>');
        window.end_event_reached = false;
        var host = window.episode_checkin_info.host;
        var port = window.episode_checkin_info.socketio_port;
        var socket;
        if (navigator.appName == 'Microsoft Internet Explorer') {
            socket = new io.Socket(host, {
                port:port,
                rememberTransport: false,
                transports: ['jsonp-polling','htmlfile', 'xhr-multipart', 'xhr-polling']
            });
        } else {
            socket = new io.Socket(host, {
                port:port
            });
        }
        
        socket.on('message', function(data){
            if(typeof data == "object") {
                switch(data.event) {
                    case 'end':
                        setTimeout(function () {socket.disconnect()}, 10000);
                        break;
                    default:
                        p(data);
                }
            }
        });
        socket.on('connect', function(){
            socket.send({
                "uid":uuid, 
                "eid":parseInt(window.eid, 10)
            });
        });
        socket.connect();        
    }
    $('#commentbox').attr('readonly', !isLoggedInTwitter());
    window.last_time = (new Date).getTime();
    $('div#scrollbar').slider({
        min: 0,
        max: window.episode_duration,
        value: (new Date).getTime() - window.episode_info.start_time,
        animate: true,
        'start': function(event, ui) {
            window.user_is_moving_slider = true;
        },
        'stop': function(event, ui) {
            moveStreamTo(ui.value);
            window.user_is_moving_slider = false;
        }
    });
    if(window.episode_checkin_info.method == 'tunein') {
        // Set the scrollbar to the current position in time
        $("#progressbar").progressbar({
            value: 100 * (new Date() - window.episode_info.start_time) / window.episode_duration
        });
    } else {
        $("#progressbar").progressbar({
            value: 0
        });
    }
    
    $('#progressbar').mousemove(function(e){
        showTimeTooltip(this, e.pageX);
    });

    $('#scrollbar').mousemove(function(e){
        showTimeTooltip(this, 23 + e.pageX);
    });
    
    $('#progressbar,#scrollbar').mouseenter(function() {
        $('#time-tooltip').show()
    });
    $('#progressbar,#scrollbar').mouseleave(function() {
        $('#time-tooltip').hide()
    });

    $('div.tweets-cont').mouseenter(function() {
        slowDownStream()
    });
    $('div.tweets-cont').mouseleave(function() {
        speedUpStream()
    });
    setTimeout(shift, 3000);
    setTimeout(moveSlider, 3000);
}

function togglePausedStateStream() {
    if (window.is_stream_paused) {
        unPauseStream();
        
    } else {
        pauseStream();
    }
}
// Find if there is a saved user id. If there isn't, then generate one'
function initializeUserId() {
    var uuid = guid();
    var request = xhr();
    request.open('GET', API_BASE + '/2/users/info?uid=' + uuid)
    request.onreadystatechange = function () {
        if (request.readyState == 4) {
            if(request.status != 404) {
                initializeUserId();
            } else {
                setUserId(uuid);
                initializeApp();
            }
        }
    };
    request.send(null);        
}

function userIsFeatured() {
    return window.show_info != undefined && 
    window.user_info.twitter_id != undefined &&
    $.inArray(
        '@' + window.user_info.twitter_id.username.toLowerCase(), 
        window.show_info.featured
        ) >= 0;
}
// Get the user info from server
function initializeUserInfo(uuid, returnToInitializeApp) {
    var request = xhr();
    request.open('GET', API_BASE + '/2/users/info?uid=' + uuid)
    request.onreadystatechange = function () {
        if (request.readyState == 4) {
            if(request.status == 200) {
                window.user_info = jQuery.parseJSON(request.responseText);
                $('#logged-in-username').html(window.user_info.real_name);
                window.user_is_featured  = userIsFeatured();
                if(window.user_is_featured) {
                    $('div#tweets .spotlight').removeClass('hidden-admin');
                }
            } else {
                window.user_info = new Object;
            }
            if(returnToInitializeApp) {
                initializeApp();
            }
        }
    };
    request.send(null);        
}


// Get the user id from local storage or cookie
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
    return uuid;                        
}

// Set user id to local storage or cookie
function setUserId(uuid) {
    var thereIsLocalStorage = typeof window.localStorage!=='undefined';
    if(thereIsLocalStorage) {
        localStorage['uuid'] = uuid;
    } else {
        var dt = new Date();
        dt.setTime(dt.getTime() + 31536000000);
        document.cookie="uuid=" + uuid + "; expires=" + dt.toGMTString() + "; path=/";
    }
}

function getEpisodeId() {
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i=0;i<vars.length;i++) {
        var pair = vars[i].split("=");
        if (pair[0] == 'eid') {
            return pair[1];
        }
    }
    return null;
}

function episodeCheckin() {
    var uid = getUserId();
    var request = xhr();
    request.open('GET', API_BASE + '/2/episodes/checkin?uid=' + uid + '&eid=' + window.eid)
    request.onreadystatechange = function () {
        if (request.readyState == 4) {
            switch (request.status) {
                case 200:
                    window.episode_checkin_info = jQuery.parseJSON(request.responseText);
                    if(window.episode_checkin_info.method == 'play') {
                        window.episode_info.start_time = new Date();
                        window.episode_info.end_time = new Date(window.episode_info.start_time.getTime() + episode_duration)
                    } else if(window.episode_checkin_info.method == 'tunein') {
                        // HTTP 200 && tunein && <3 minutes left => show countdown
                        var secondsLeft = (window.episode_info.start_time - new Date()) / 1000;
                        if (secondsLeft > 0 && secondsLeft < 180) { 
                            $('#content').hide();
                            $('#premier-warning').show();
                            // Set countdown
                            $('#countdown').countdown({until: window.episode_info.start_time, format: 'HMS', compact: true});
                            // Reload the window when the show is ready
                            setTimeout(function () {window.location.reload()}, window.episode_info.start_time - new Date());
                        }
                    }
                    $('ul#tweet-nav a:last').click();
                    initializeApp();
                    break;
                case 404:
                    $('#content').hide();
                    $('#premier-warning').show();
                        // Set countdown
                        $('#countdown').countdown({until: window.episode_info.start_time, format: 'HMS', compact: true});
                        // Reload the window when the show is ready
                        setTimeout(function () {window.location.reload()}, window.episode_info.start_time - new Date());
                    break;
                default:
                    alert('An unexpected error has occurred while trying to obtain episode information. Please reload the page.');
                    break;
            }
        }
    };
    request.send(null);        
}

function userCheckin(returnToInitializeApp) {
    var coordsQuery = "";
    if(window.position != null) {
        if(window.position.coords.latitude != null && window.position.coords.longitude != null) {
            coordsQuery += '&latitude=' +  window.position.coords.latitude +
                '&longitude=' + window.position.coords.longitude;
            if(window.position.coords.altitude != null) {
                coordsQuery += '&altitude=' +  window.position.coords.altitude;
            }
        }
    }
    var uid = getUserId();
    var request = xhr();
    request.open('POST', API_BASE + '/2/users/checkin?uid=' + uid + '&gmt_offset=' + TimezoneDetect() + coordsQuery)
    request.onreadystatechange = function () {
        if (request.readyState == 4) {
            if(request.status == 200) {
                window.UserCheckinDone = true;
                if(returnToInitializeApp) {
                    initializeApp();
                }
            } else {
                alert('An unexpected error has occurred while trying to check in. Please reload the page.');
            }
        }
    };
    request.send(null);        
}

function getEpisodeInfo() {
    var uid = getUserId();
    var request = xhr();
    request.open('GET', API_BASE + '/2/episodes/info?uid=' + uid + '&eid=' + window.eid)
    request.onreadystatechange = function () {
        if (request.readyState == 4) {
            if(request.status == 200) {
                window.episode_info = jQuery.parseJSON(request.responseText);
                // If it's a playback, I replace the start_time and end_time with
                // current_time and current_time + show duration
                window.episode_info.start_time = new Date(Date.UTC(
                    window.episode_info.start_time.substring(0, 4),
                    window.episode_info.start_time.substring(5, 7) - 1,
                    window.episode_info.start_time.substring(8, 10),
                    window.episode_info.start_time.substring(11, 13),
                    window.episode_info.start_time.substring(14, 16)
                    ));
                window.episode_info.end_time = new Date(Date.UTC(
                    window.episode_info.end_time.substring(0, 4),
                    window.episode_info.end_time.substring(5, 7) - 1,
                    window.episode_info.end_time.substring(8, 10),
                    window.episode_info.end_time.substring(11, 13),
                    window.episode_info.end_time.substring(14, 16)
                    ));
                window.episode_duration = window.episode_info.end_time - window.episode_info.start_time;
                initializeApp();
            } else {
                alert('An unexpected error has occurred while trying to obtain episode information. Please reload the page.');
            }
        }
    };
    request.send(null);        
}

function showTimeTooltip(element, mouseX) {
    var offset = mouseX - $(element).offset().left;
    var timeTooltip = document.getElementById('time-tooltip');
    var timeTooltipText = document.getElementById('time-tooltip-text');
    var progressBar = document.getElementById('progressbar');
    var position = window.episode_duration * offset / (progressBar.clientWidth - 1);
    var secondsFromStart = Math.round(position / 1000);
    var hours = (Math.floor(secondsFromStart / 60)).toString();
    var minutes = (secondsFromStart - hours * 60).toString();
    if(hours.length == 1) {
        hours = '0' + hours;
    }
    if(minutes.length == 1) {
        minutes = '0' + minutes;
    }
    
    timeTooltipText.innerHTML = hours + ':' + minutes;
    timeTooltip.style.left = $(progressBar).offset().left + offset  - Math.round(timeTooltip.offsetWidth / 2) + 'px'
    timeTooltip.style.top = $(progressBar).offset().top - timeTooltip.offsetHeight + 'px'
}

function getShowInfo() {
    var request = xhr();
    request.open('GET', API_BASE + '/2/shows/info?sid=' + window.episode_info.show)
    request.onreadystatechange = function () {
        if (request.readyState == 4) {
            if(request.status == 200) {
                window.show_info = jQuery.parseJSON(request.responseText);
                window.user_is_featured  = userIsFeatured();
                initializeApp();
            } else {
                alert('An unexpected error has occurred while trying to obtain show information. Please reload the page.');
            }
        }
    };
    request.send(null);        
}

function logoutTwitter() {
    var request = xhr();
    var uid = getUserId();
    request.open('POST', API_BASE + '/2/users/auth/twitter?uid=' + uid)
    request.onreadystatechange = function () {
        if (request.readyState == 4 && request.status == 200) {
            hideUserInfoBox();
            window.user_info.twitter_id = null;
            $('#logged-in-username').html('');
            $('div#tweets .spotlight').addClass('hidden-admin');
        }
    };
    request.send(null);        
}

function pad(number, length) {
    var str = '' + number;
    while (str.length < length) {
        str = '0' + str;
    }
    return str;
}

function dateToServerTimestamp(date) {
    var d_names = ['Mon', 'Tues', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
    var m_names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
    "Sep", "Oct", "Nov", "Dec"];
    return d_names[date.getUTCDay()] + ' ' + m_names[date.getUTCMonth()] + ' ' +
    pad(date.getUTCDate(), 2) + ' ' + pad(date.getUTCHours(), 2) + ':' +
    pad(date.getUTCMinutes(), 2) + ':' + pad(date.getUTCSeconds(), 2) + 
    ' +0000 ' + date.getUTCFullYear();
}

function postComment() {
    var position = window.current_message_position;
    var message = $('#commentbox').val();
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        if(message != 'Write your comment here...'){
            var request = xhr();
            var uid = getUserId();
            $('#comment-button').html('<img src="images/ajax-loader.gif" />');
            request.open('POST', API_BASE + '/2/messages/send?twitter=true&uid=' + uid + '&eid=' + window.eid + '&pos=' + position)
            request.onreadystatechange = function () {
                if (request.readyState == 4) {
                    if (request.status == 200) {
                        var data = {
                            avatar: window.user_info.twitter_id.avatar,
                            timestamp: dateToServerTimestamp(new Date()),
                            position: window.current_message_position,
                            username: '@' + window.user_info.twitter_id.username,
                            comment: $('#commentbox').val()
                        }
                        // If it's a playback, add the message manually to the 
                        // list because, obviously, the server can't do it for 
                        // me
                        if(window.episode_checkin_info.method == 'play') {
                            window.messages.splice(messages_index + 1, 0, data);
                        }
                        $('#commentbox').val('Write your comment here...');
                    } else {
                        alert(request.responseText)
                    }
                    setTimeout(function () {$('#comment-button').html('COMMENT')},2000);
                }
            };
            request.send(message);        
        }
    }
}

function openTwitterLoginWindow() {
    document.getElementById('xdr_iframe').contentWindow.openTwitterAuthWindow(API_BASE + '/2/users/auth/twitter?uid=' + getUserId() + '&caller=webpage');
}

function commentboxClicked() {
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        showUserInfoBox(window.user_info);
        $('#commentbox').attr('readonly', false);
        if($('#commentbox').val() == 'Write your comment here...' || $('#commentbox').val() == '') {
            $('#commentbox').val('#teenwolf ');
        }
        limitCommentLength($('#commentbox'), $('#commentbox-remaining'), 140);
        $('#commentbox-remaining').fadeIn();
        
    }
}

function isLoggedInTwitter() {
    return window.user_info.twitter_id != null
}

function twitter_auth_status(data) {
    if(data == 'OK') {
        var uuid = getUserId();
        initializeUserInfo(uuid, false);
    }
}

function isMessagesContainerFilled(container) {
    return container.scrollTop + container.offsetHeight < container.scrollHeight
}

function getCurrentPosition() {
    return (new Date).getTime() + window.user_time_drift - window.episode_info.start_time.getTime()
}

function getCurrentPositionPercentage() {
    return 100 * getCurrentPosition() / window.episode_duration;
}

function moveSlider() {
    var current_time = (new Date).getTime();
    var positionPercentage;
    window.last_time = current_time;
    
    if(!window.is_stream_paused && !window.user_is_moving_slider) {
        $('div#scrollbar').slider('value', getCurrentPosition());
        // Convert the position in episode into a precentage, for use with the
        // scrollbar
        positionPercentage = getCurrentPositionPercentage();
        if($('div#progressbar').progressbar('value') < positionPercentage) {
            // If the progressbar is behind the current position, we update the 
            // position of the progressbar to match the current position.
            $('div#progressbar').progressbar('value', positionPercentage);
        }
    }
    setTimeout(moveSlider, 10000);
}

function shift() {
    var firstMessage;
    if(!window.is_stream_paused && !window.user_is_moving_slider) {
        var div_all_messages = document.getElementById('all-messages');
        var div_featured_messages = document.getElementById('featured-messages');
        if(div_all_messages.scrollTop < div_all_messages.scrollHeight) {
            div_all_messages.scrollTop += 1;
            div_featured_messages.scrollTop += 1;
            // If the first message just hide out, delete it from the div
            if(div_all_messages.children.length > 0 && div_all_messages.scrollTop > div_all_messages.children[0].offsetHeight) {
                div_all_messages.removeChild(div_all_messages.children[0]);
                div_all_messages.scrollTop = 0;
                // If the next message is a spotlighted one
                firstMessage = div_all_messages.children[0];
                if ($(firstMessage).find('input[name=is-spotlighted]').val() == 'true') {
                    var guid = $(firstMessage).find('input[name=message-guid]').val();
                    var message = findItemByGuid(window.messages, guid) || 
                    findItemByGuid(window.spotlights, guid);
                    
                    showSpotLight(message);
                }
                
            }
            // If we're geting to the end of the scroll, and there is more messages
            // add the next to the list. Also, update the position
            if(messages_index < messages.length) {
                if(!isMessagesContainerFilled(div_all_messages)) {
                    if(messages[messages_index].position > window.current_message_position) {
                        window.current_message_position = messages[messages_index].position;
                    }
                    addMessageToAllLists(messages[messages_index], messages_index);
                    messages_index += 1;
                }
            }
            // If there are at least 20 messages to show, and we're in 
            // live, show the go live button
            if (window.episode_checkin_info.method == 'tunein') {
                if(messages_index < (messages.length - 20)) {
                    showLiveButton();
                }
            }
        }
        var real_position = getCurrentPosition();
        if(slides[slides_index] != undefined && slides[slides_index].position <= real_position) {
            WebSlide.showWebSlide($('div#slide-body'), slides[slides_index]);
            addSlideToMessageList(slides[slides_index]);
            slides_index += 1;
        }
        // We only add a spotlight when the spotlight box is closed, to avoid
        // adding spotlights at the same time
        if(spotlights[spotlights_index] != undefined && spotlights[spotlights_index].position <= real_position && isSpotlightBoxClosed()) {
            var spotlighted_message  = findItemByGuid(window.messages, spotlights[spotlights_index].guid);
            // If the spotlighted message doesn't previously exists in the 
            // messages list, we add it to it. Also, add it when we are in live
            // because in live we'll an spotlight that is also a normal message
            // and that's fine'
            if (spotlighted_message == null || window.episode_checkin_info.method == 'tunein') {
                addMessageToList({
                    'list': "div#all-messages", 
                    'data': spotlights[spotlights_index], 
                    'mode': 'append',
                    'position': 0
                });
            }
            spotlights_index += 1;
        }      
    }

    setTimeout(shift, DELAY);
}      

function goToLive() {
    moveStreamTo(messages[messages.length - 1].position);
    $('div#scrollbar').slider('value', messages[messages.length - 1].position);
    hideLiveButton();
}
function isSpotlightBoxClosed() {
    return !$('div#fanspot').is(':visible');
}

function formatDateDifference(timestamp) {
    var m_names = new Array("Jan", "Feb", "Mar",
        "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec");
    var date = new Date();
    date.setUTCFullYear(
        timestamp.substr(26,4), 
        $.inArray(timestamp.substr(4,3), m_names), 
        timestamp.substr(8,2)
        );
    date.setUTCHours(
        timestamp.substr(11,2), 
        timestamp.substr(14,2), 
        timestamp.substr(17,2)
        );
    var now = new Date();
    // Calculate difference in hours
    var difference = (now - date) / 60000;
    if (difference < 0) { difference = 0; }
    var text;
    switch (true) {
        case (difference < 60): // less than one hour
            text = Math.floor(difference) + ' minutes ago';
            break;
        case (difference < 1440): // less than one day
            text = Math.floor(difference / 60) + ' hours ago';
            break;
        default:
            var day = date.getDate();
            if(day < 10) {
                day = '0' + day;
            }
            text = day + ' ' + m_names[date.getMonth()];
            break;
    }
    return text;

}

function addMessageToList(params) {
    var admin_link_class;
    var twitterActions;
    var list = $(params.list);
    var data = params.data;
    var mode = params.mode || 'append';
    var isSpotlighted = params.data['spotlighted-by'] != undefined;
    var isFavorite = params.data['favorite'] == true;
    var position = params.position || 0;

    var avatar, real_name;
    if(data.avatar != undefined) {
        avatar = data.avatar;
    } else {
        avatar = 'images/default-twitter-avatar.jpg';
    }

    real_name = data.real_name || '';
    var formatted_date = formatDateDifference(data.timestamp);
    var odd_row;
    odd_row = position % 2;
    if(window.user_is_featured) {
        admin_link_class = '';
    } else {
        admin_link_class = 'hidden-admin';
    }

    if(data.type == 'twitter') {
        if(isLoggedInTwitter()) {
            twitterActions = '\
                <div class="' + (isFavorite ? 'unfavorite' : 'favorite') +  ' actions"><a href="#" onclick="' + (isFavorite ? 'doUnFavorite' : 'doFavorite') +  '(\'' + data.guid + '\', this);return false">Favorite</a></div>\
                <div class="reply actions"><a href="#" onclick="doReply(\'' + data.guid + '\');return false">Reply</a></div>\
                <div class="retweet actions"><a href="#" onclick="doRetweet(\'' + data.guid + '\', this);return false">Retweet</a></div>\
            '
        } else {
            twitterActions = '';
        }
    } else {
        twitterActions = '\
            <div class="no-twitter-msg actions">Sent from MTV WatchWith</div>\
        '
    }
    var text = '<div class="message-separator"></div><div class="tweet-row' + (odd_row ? '': ' tr2') + (data.featured ? ' featured': '') + (isSpotlighted ? ' fanspot': '') + '">' +
    '<div class="trimg"><img onerror="this.src=\'images/default-twitter-avatar.jpg\'" src="' + avatar + '" alt="" />&nbsp;</div>\
                <div class="trtxt">\
                    <h3>' + data.username.replace(/^@/, '') +  ' <small>'+ real_name + '</small></h3>\
                    <h4>' + data.comment + '</h4>\
                    <h6>' + formatted_date + '</h6>\
                </div>\
                <div class="clear"></div>\
                <div class="spotlight actions ' + admin_link_class +'"><a href="#" onclick="doSpotLight(\'' + data.guid + '\', this);return false">Spotlight</a></div>' + 
    twitterActions + 
    '<div class="like actions"><a href="#" onclick="doLike(\'' + data.guid + '\', this);return false">Like</a></div>\
                <input type="hidden" class="message-guid" name="message-guid" value="' + data.guid + '" />\
                <input type="hidden" class="is-spotlighted" name="is-spotlighted" value="' + isSpotlighted + '" />\
                </div>'
    list[mode].apply(list, [text]);
    $(params.list + " div.tweet-row:last").fadeIn();
}
function addMessageToAllLists(data, position) {
    
    addMessageToList({
        'list': "div#all-messages", 
        'data': data, 
        'mode': 'append',
        'position': position
    });
    if(data.featured) {
        addMessageToList({
            'list': "div#featured-messages", 
            'data': data, 
            'mode': 'append',
            'position': position
        });
    }
}

function showSpotLight(data) {
    var avatar, real_name;
    var twitterActions;
    if(data.avatar != undefined) {
        avatar = data.avatar;
    } else {
        avatar = 'images/default-twitter-avatar.jpg';
    }

    real_name = data.real_name || '';
    var formatted_date = formatDateDifference(data.timestamp);
    if(data.type == 'twitter') {
        twitterActions = '\
            <div class="favorite actions"><a href="#" onclick="doFavorite(\'' + data.guid + '\', this);return false">Favorite</a></div>\
            <div class="reply actions"><a href="#" onclick="doReply(\'' + data.guid + '\');return false">Reply</a></div>\
            <div class="retweet actions"><a href="#" onclick="doRetweet(\'' + data.guid + '\', this);return false">Retweet</a></div>\
        '
    } else {
        twitterActions = '\
            <div class="no-twitter-msg actions">Sent from MTV WatchWith</div>\
        '
    }
    var spotlight_text = '<div class="trimg"><img src="' + avatar + '" alt="" />&nbsp;</div>\
                <div class="trtxt">\
                    <h3><a href="#">' + data.username.replace(/^@/, '')+  '</a> <small>'+ real_name + '</small></h3>\
                    <h4>' + data.comment + '</h4>\
                    <h6>' + formatted_date + '</h6>\
                </div>\
                <div class="clear"></div>'
    +   twitterActions + 
    '<div class="like actions"><a href="#" onclick="doLike(\'' + data.guid + '\', this);return false">Like</a></div>\
            </div>'
    $('div#spotlight').html(spotlight_text);
    $('div#fanspot').show().delay(20000).slideUp();
}

function doSpotLight(guid, link) {
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        var request = xhr();
        var uid = getUserId();
        
        request.open('POST', API_BASE + '/2/messages/spotlight?uid=' + uid + '&guid=' + guid + '&eid=' + window.eid);
        request.onreadystatechange = function () {
            if (request.readyState == 4 && request.status == 200) {
                $(link).html('Spotlighted');
            }
        };
        request.send(null);
    }
}

function doFavorite(guid, link) {
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        var request = xhr();
        var uid = getUserId();
        var message = findItemByGuid(window.messages, guid);
        request.open('POST', API_BASE + '/2/messages/favorite?uid=' + uid + '&guid=' + guid);
        request.onreadystatechange = function () {
            if (request.readyState == 4 && request.status == 200) {
                $(link.parentNode).addClass('unfavorite');
                $(link.parentNode).removeClass('favorite');
                link.onclick = function() {
                    doUnFavorite(guid, this);
                    return false;
                };
                setItemFavoriteStatus(guid, true);
            }
        };
        request.send(message.comment);
    }
}

function doUnFavorite(guid, link) {
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        var request = xhr();
        var uid = getUserId();
        var message = findItemByGuid(window.messages, guid);
        request.open('POST', API_BASE + '/2/messages/unfavorite?uid=' + uid + '&guid=' + guid);
        request.onreadystatechange = function () {
            if (request.readyState == 4 && request.status == 200) {
                $(link.parentNode).addClass('favorite');
                $(link.parentNode).removeClass('unfavorite');
                link.onclick = function() {
                    doFavorite(guid, this);
                    return false;
                };
                setItemFavoriteStatus(guid, false);
            }
        };
        request.send(message.comment);
    }
}

function setItemFavoriteStatus(guid, favoriteStatus) {
    var message = findItemByGuid(window.messages, guid) || findItemByGuid(window.spotlights, guid);
    if(message != null) {
        message.favorite = favoriteStatus;
    }
    
}

function doReply(guid) {
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        var message = findItemByGuid(window.messages, guid);
        $('#commentbox').focus();
        $('#commentbox').val(message.username + ' #teenwolf ');
    }
}

function doRetweet(guid, link) {
    var comment;
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        var request = xhr();
        var uid = getUserId();
        var message = findItemByGuid(window.messages, guid);
        request.open('POST', API_BASE + '/2/messages/share?twitter=true&uid=' + uid + '&guid=' + guid);
        request.onreadystatechange = function () {
            if (request.readyState == 4 && request.status == 200) {
                $(link).html('Retweeted');
            }
        };
        if (message.comment.search(/#teenwolf/) < 0 && message.comment.length <= 130) {
            comment = message.comment + ' #teenwolf';
        } else {
            comment = message.comment;
        }
        request.send(comment);
    }
}

function doLike(guid, link) {
    if(window.user_info.twitter_id == null) {
        openTwitterLoginWindow();
    } else {
        var request = xhr();
        var uid = getUserId();
        request.open('POST', API_BASE + '/2/messages/like?uid=' + uid + '&guid=' + guid);
        request.onreadystatechange = function () {
            if (request.readyState == 4 && request.status == 200) {
                $(link).html('Liked');
            }
        };
        request.send(null);        
    }
}

function showLiveButton() {
    if(!window.is_live_button_visible) {
        $('#bars').animate({
            width: '82%'
        });
        $('#live-btn').animate({
            width:'toggle'
        },550);
        window.is_live_button_visible = true;
    }
}

function showUserInfoBox(user_info) {
    $('#logged-in-username').html(user_info.real_name);
    $('#commentbox').animate({
        height:100
    }, 300, function () {
        $('div#user-info-box').fadeIn(300)
    })
}

function limitCommentLength(field, counter, limit) {
    if (field.val().length > limit) {
        field.val(field.value = field.val().substring(0, limit));
    } else {
        counter.html(limit - field.val().length + '/' + limit);
    }
}

function hideUserInfoBox() {
    $('div#user-info-box').fadeOut(300, function () {
        $('#commentbox').animate({
            height:20
        }, 300)
    })
    $('#commentbox-remaining').fadeOut();
}
function hideLiveButton() {
    if(window.is_live_button_visible) {
        $('#live-btn').animate({
            width:'toggle'
        },250);
        $('#bars').animate({
            width: '91%'
        });
        window.is_live_button_visible = false;
    }
}

function addSlideToMessageList(slide) {
    var text;
    var slideTitle;
    var thumbnail;
    switch(slide['content-type']) {
        case 'image/jpeg': // mime types
            slideTitle = 'Featured photo';
            break;
        case 'video/mpeg':
            slideTitle = 'Featured video';
            break;
        case 'inaka/ad':
            slideTitle = '';
            break;
        case 'text/html':
            slideTitle = '';
            break;
    }
    if(slide.thumbnail == undefined || slide.thumbnail == "") {
        switch(slide['content-type']) {
            case 'image/jpeg': // mime types
                thumbnail = 'images/default-thumbnail.png';
                break;
            case 'video/mpeg':
                thumbnail = 'images/video-thumbnail.png';
                break;
            case 'inaka/ad':
                thumbnail = 'images/default-thumbnail.png';
                break;
            case 'text/html':
                thumbnail = 'images/default-thumbnail.png';
                break;
            default:
                thumbnail = 'images/default-thumbnail.png';
                break;
        }
    } else {
        thumbnail = 'http://mtv.mtvnimages.com/uri/mgid:file:http:shared:' + 
        slide.thumbnail.replace(/http:\/\//, '') + 
        '?width=102&height=50&crop=true';
    }
    text = '\
    <div class="message-slide">\
        <div class="message-slide-thumbnail">\
            <img width="102" alt="" src="' + thumbnail + '">\
        </div>\
        <div class="message-slide-text"> \
            <div class="message-slide-type">' + slideTitle + '</div>\
            <div class="message-slide-title">' + slide.title + '</div>\
        </div>\
        <div class="clear"></div>\
    </div>'
    $('div#all-messages').append(text);
    $('div#all-messages div.tweet-row:last').fadeIn();  
    $('div#featured-messages').append(text);
    $('div#featured-messages div.tweet-row:last').fadeIn();
    $('#instructions').slideUp(2000);$('#feature').slideDown(2000);    
}

function p(data) {
    var lastMessagePositionPercent;
    switch (data.event) {
        case 'msgs':
            for (var i = 0; i < data.messages.length; i++) {
                if(data.messages[i].type == 'twitter' || data.messages[i].type == 'message') {
                    switch (data.messages[i].featured) {
                        case "true":
                            data.messages[i].featured = true;
                            break;
                        case "false":
                            data.messages[i].featured = false;
                            break;
                    }

                    messages.push(data.messages[i]);
                    if(data.messages[i].position > window.last_message_position_received) {
                        window.last_message_position_received = data.messages[i].position;
                    }
                }
            }
            lastMessagePositionPercent = 100 * 
                window.last_message_position_received / window.episode_duration;
            if($('div#progressbar').progressbar('value') < lastMessagePositionPercent) {
                // We only update the position of the progressbar to increment 
                // its position. We can't update it always because the 
                // progressbar is incremented by time and may happen that the 
                // bar position is greater than the last message received 
                // position
                $('div#progressbar').progressbar(
                    'value', 
                    lastMessagePositionPercent
                );
            }
            break;
        case 'spotlight':
            spotlights.push(data.message)
            break;
        case 'slide':
            slides.push(data.slide)
            break;
        case 'end':
            if(window.episode_checkin_info.method == 'play') {
                $('div#progressbar').progressbar('value',100)
            }
            break;
    }
}
$(document).ready(function() {
    //    $(document).keyup(function(e) {
    //      if (e.keyCode == 27) { 
    //          $('iframe#xdr_iframe')[0].src = $('iframe#xdr_iframe')[0].src
    //      }
    //    });
    
    $('iframe#xdr_iframe')[0].src = API_BASE + '/xdr.html'
    $('iframe#xdr_iframe').load(function(){
        // Replace internal XMLHttpRequest with the Cross-Domain-Iframe
        // method. This trick doesn't work for Opera'
        if(navigator.appName != 'Opera') {
            window.XMLHttpRequest =  document.getElementById('xdr_iframe').contentWindow.xhr
        }
        initializeApp();
    });
});

