var mtvnPlayers = [];

function mtvnPlayerLoaded( playerId ){
	mtvnPlayers[playerId]._onPlayerLoaded();
}
function MTVNPlayerController(id,onLoadFunctionName){
	mtvnPlayers[id] = this;
	this.onLoadFunctionName = onLoadFunctionName;
	this.playerLoaded = false;
	this.playerId = id;
	this.player = null;
	
	this._onPlayerLoaded = function(){
		this.player = (navigator.appName.indexOf("Microsoft") != -1) ? window[this.playerId] : document[this.playerId];
		this.playerLoaded = true;
		var f = eval(onLoadFunctionName);
		f(this);
	}
	
	this.onDump = function(){}
	
	this.dump = function(){
		var temp = "";
		if (this.playerLoaded){
			temp = this.player.getLogDump();
		}
		else{
			temp = "player not loaded";
		}
		this.onDump(temp);
	}
}
