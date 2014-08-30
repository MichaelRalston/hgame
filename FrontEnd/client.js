var sendMsg;

function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':6116' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function handleTextKey($elem, e, entityId) {
	if (e.keyCode == 13) {
		sendMsg({'action':'text','entity':entityId,'text':$elem.val()});
		$elem.val('');
	}
}

function makeEntityElement(entityJson) {
	switch (entityJson.display.type) {
		case 'image':
			var $elem = $('<img id="entity-'+entityJson.entityId+'" src="'+entityJson.display.uri+'">');
			return $elem;
		case 'text':
			var $elem = $('<div id="entity-'+entityJson.entityId+'" class="textEntity"></div>').text(entityJson.display.text);
			return $elem;
		case 'textInput':
			var $elem = $('<input type="text" id="entity-"'+entityJson.entityId+'" />');
			$elem.keyup(function(e) {
				handleTextKey($elem, e, entityJson.entityId);
			});
			return $elem;
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
		case 'action':
			$('<p></p>').text('<'+gamelog.actor+'> '+gamelog.string).appendTo($('#gamelog'));
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
	sendMsg = function(text) {
		console.log("transmitting ", text);
		ws.send(JSON.encode(text));
	}
	// Below this is hackery.
	var $input = entityElement({'entityId':'textInput','display':{'type':'text'}});
	$('#gamelog').after($input);
});

