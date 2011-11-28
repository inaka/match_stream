var controller = new MTVNPlayerController('tunein_player','onPlayerLoaded');
var player;
function onPlayerLoaded(controller){
   player = controller.player;
   player.addEventListener('STATE_CHANGE','onStateChange');
}
function onStateChange(state){
   switch (state) {
        case 'connected':
            pauseStream();
            break;
        case 'playing':
            pauseStream();
            break;
        case 'paused':
            unPauseStream()
            break;
        case 'stopped':
            unPauseStream()
            break;
    }

}
