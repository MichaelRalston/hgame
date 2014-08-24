function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':6116' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

$(document).ready(function () {
	var ws = createWebSocket('/');
	ws.onmessage = function(event) {
		console.log(event);
		var p = $(document.createElement('p')).text(event.data);
		$('#content').append(p);
	}
	$('#input').submit(function() {
		var text = $('#text').val();
		ws.send(text);
		$('#text').val('');
		return false;
	});
});

