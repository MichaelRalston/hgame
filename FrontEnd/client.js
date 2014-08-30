function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':6116' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function makeEntityElement(entityJson) {
	switch (entityJson.display.type) {
		case 'image':
			return $('<img id="entity-'+entityJson.entityId+'" src="'+entityJson.display.uri+'">');
		case 'text':
			return $('<div id="entity-'+entityJson.entityId+'" class="textEntity"></div>').text(entityJson.display.text);
		default:
			return $('#id_that_does_not_exist_ever');
	}
}

function entityElement(entityJson) {
	var $elem = $('#' + entityJson.entityId);
	if ($elem.length == 0) {
		$elem = makeEntityElement(entityJson);	
	}
	return $elem;		
}

function moveEntityToZone(entityId, zoneId) {
	console.log("Move entity", entityId, "to zone", zoneId);
	$('#entity-'+entityId).detach().appendTo($('#zone-'+zoneId));
}

function renderScreen(screen) {
	console.log("renderScreen", screen);
	// TODO: implement. :<
}

function showGamelog(gamelog) {
	switch (gamelog.type) {
		case 'display':
			$('<p></p>').text(gamelog.display).appendTo($('#gamelog'));
			break;
		case 'move':
			function move(entityId) {
				moveEntityToZone(entityId, gamelog.move.zone);
			}
			gamelog.move.entities.forEach(move);
			break;
		case 'action': // TODO: Implement.
			break;
		case 'targetAction': // TODO: Implement.
			break;
	}
}

function handleUpdate(data) {
	renderScreen(data.screen);
	data.gamelogs.forEach(showGamelog);
}

$(document).ready(function () {
	var ws = createWebSocket('/');
	ws.onmessage = function(event) {
		console.log("got update", event);
		handleUpdate(event.data);
	}
	$('#input').submit(function() {
		var text = $('#text').val();
		ws.send(text);
		$('#text').val('');
		return false;
	});
});

