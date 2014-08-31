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
	console.log("make element", entityJson);
	switch (entityJson.display.type) {
		case 'image':
			var $elem = $('<img id="entity-'+entityJson.entityId+'" src="'+entityJson.display.uri+'">');
			break;
		case 'text':
			var $elem = $('<div id="entity-'+entityJson.entityId+'" class="textEntity"></div>').text(entityJson.display.text);
			break;
		case 'textInput':
			var $elem = $('<input type="text" id="entity-'+entityJson.entityId+'" />');
			$elem.keyup(function(e) {
				handleTextKey($elem, e, entityJson.entityId);
			});
			break;
		default:
			$elem = $('#id_that_does_not_exist_ever');
	}
	switch (entityJson.size.type) {
		case "percent":
			$elem.width(entityJson.size.width+"%");
			$elem.height(entityJson.size.height+"%");
			break;
		default: break; // nothing.
	}
	return $elem;
}

function entityElement(entityJson) {
	var $elem = $('#entity-' + entityJson.entityId);
	if ($elem.length == 0) {
		$elem = makeEntityElement(entityJson);	
	}
	return $elem;		
}

function moveEntityToZone(entityId, zoneId) {
	console.log("Move entity", entityId, "to zone", zoneId);
	$('#entity-'+entityId).appendTo($('#zone-'+zoneId));
}

function makeZone(zoneId, zoneData) {
	switch (zoneData.display.type) {
		case "horizFill":
			$zone = $('<div id="zone-'+zoneId+'" style="height:'+zoneData.display.height+'%; width:100%"></div>');
			break;
		default: // todo: implement.
			alert("Unimplemented zone type " + zoneData.display.type);
			$zone = $('#id_that_does_not_exist_ever');			
	}
	return $zone;
}

function getZone(zoneId, zoneData) {
	var $zone = $('#zone-'+zoneId);
	if ($zone.length == 0) {
		$zone = makeZone(zoneId, zoneData);
	}
	return $zone;
}

function moveIfNeeded($parent, $elem) {
	if (!$parent.is($elem.parent())) {
		$elem.appendTo($parent);
	}
}

function placeZone($zone, zoneData) {
	switch (zoneData.display.type) {
		case "nested":
			$parent = $('#zone-'+zoneData.display.zone);			
			break;
		default:
			$parent = $('body');
			break;
	}
	moveIfNeeded($parent, $zone);
}

function renderZone(zoneData) {
	var zoneId = zoneData[0];
	var zone = zoneData[1];
	var $zone = getZone(zoneId, zone);
	placeZone($zone, zone);
	zone.entities.forEach(function(entity) {
		$entity = entityElement(entity);
		moveIfNeeded($zone, $elem);
	});
}

function renderScreen(screen) {
	console.log("renderScreen", screen);
	screen.forEach(renderZone)
}

function showGamelog(gamelog) {
	switch (gamelog.type) {
		case 'display':
			$('<p></p>').text(gamelog.display).appendTo($('#zone-'+gamelog.zone));
			break;
		case 'move':
			function move(entityId) {
				moveEntityToZone(entityId, gamelog.move.zone);
			}
			gamelog.move.entities.forEach(move);
			break;
		case 'action':
			$('<p></p>').text('<'+gamelog.actor+'> '+gamelog.string).appendTo($('#zone-'+gamelog.zone));
			break;
		case 'targetAction': // TODO: Implement.
			alert("unimplemented gamelog type; targetAction");
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
		handleUpdate(JSON.parse(event.data));
	}
	sendMsg = function(text) {
		console.log("transmitting ", text);
		ws.send(JSON.stringify(text));
	}
});


