function WebSlide() {}
// data will be an object 
// expected to insert the slide into the stream immediately
// if the slide can't be loaded, should insert a generic
// placeholder
WebSlide.prototype.showWebSlide = function(target, data)
{
  
  switch(data['content-type'])
  {
    case 'image/jpeg': // mime types
      WebSlide.showImage(target,data);

      break;
    case 'video/mpeg':
      WebSlide.showVideo(target,data);

      break;
    case 'inaka/ad':
      WebSlide.showAd(target,data);

      break;
    case 'text/html':
      WebSlide.showHTML(target,data);

      break;

  }
}

WebSlide.prototype.showAd =  function(target,data) {
  alert("mtvn must override the WebSlide.showAd function with signature 'function(target,data)'");
}

WebSlide.prototype.showImage = function(target,data) {
    $('div#slide-type').html('Featured photo');
    $('div#slide-title').html(data['title']);
    $('div#slide-description').html(data.description);
    target.html('<img width="472" src="' + data.url + '" />');
}

WebSlide.prototype.showVideo = function(target,data) {
    $('div#slide-type').html('Featured video');
    $('div#slide-title').html(data['title']);
    // var uma_id = data.uma_id;
    // var uma_id = 645398;
    var uma_id = data.uma_id.toString();
    $('div#slide-description').html(data.description);
    target.html('<object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000\
      codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=9,0,0,0"\
      width="472"\
      height="265"\
      id="tunein_player">\
      <param name="allowScriptAccess" value="always" />\
      <param name="allowFullScreen" value="true" />\
      <param name="movie" value=\
    "http://media.mtvnservices.com/mgid:uma:video:mtv.com:' + uma_id + '" />\
      <param name="quality" value="high" />\
        <embed src="http://media.mtvnservices.com/mgid:uma:video:mtv.com:' + uma_id + '"\
          type="application/x-shockwave-flash"\
          wmode="window"\
          name="tunein_player"\
          id="player1"\
          width="472"\
          height="265"\
          allowFullscreen="true"\
          flashVars=""\
          allowScriptAccess="always">\
        </embed>\
    </object>\
    ');
}

WebSlide.prototype.showHTML= function(target,data) {
    $('div#slide-type').html('Featured content');
    $('div#slide-title').html('');
    $('div#slide-description').html('');
    $('div#slide-title').html(data['title']);
    target.html('<iframe src="' + data.html_block_url + '" scrolling="no" height="279" width="472" frameBorder="0"></iframe>')    
}

WebSlide = new WebSlide();

// mtv does this to hook their ad method
WebSlide.showAd = function(target,data) {
  alert("mtv was here");
}
