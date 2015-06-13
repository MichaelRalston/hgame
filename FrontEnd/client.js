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

function makeClickable($elem, entityId) {
	$elem.click(function() {
		sendMsg({'action':'click','entity':entityId});
	});
}

function makeDraggable($elem, entityId, dropOnEntities) {
	$elem.draggable({
		helper: function(e) { return $(e.currentTarget).clone().height("70%").width("auto")[0];
			},
		opacity: 0.8,
		revert: true,
		snap: true
	});
	if (dropOnEntities) {
		$elem.addClass("dropOnEntities");
	}
}

function makeEntityDroppable($elem, entityId) {
	$elem.droppable({
		drop: function(event, ui) {
			if (!$elem.is(ui.draggable.parent())) {
				var id = ui.draggable.attr('id');
        var ydelta = ui.offset.top - $elem.offset().top;
        var xdelta = ui.offset.left - $elem.offset().left;
        var ypercent = Math.floor(ydelta / $elem.height());
        var xpercent = Math.floor(xdelta / $elem.width());
				sendMsg({'action':'dragEntity','target':entityId,'entity':id.slice(7),'topOffset':ypercent,'leftOffset':xpercent});
			}
		},
		tolerance: 'pointer',
		greedy: true,
		accept: ".dropOnEntities",
		hoverClass:  'drophover'
	});
}

function makeEntityElement(entityJson) {
	console.log("make element", entityJson);
	switch (entityJson.display.type) {
		case 'image':
			var $elem = $('<div class="entity has-nesting" id="entity-'+entityJson.entityId+'"><img><div class="nesting-holder"></div></div>');
			if (entityJson.clickable) {
				makeClickable($elem, entityJson.entityId);
			}
			if (entityJson.draggable) {
				makeDraggable($elem, entityJson.entityId, entityJson.dropOnEntities);
			}
			break;
		case 'text':
			var $elem = $('<div class="entity" id="entity-'+entityJson.entityId+'" class="textEntity"></div>');
			if (entityJson.clickable) {
				makeClickable($elem, entityJson.entityId);
			}
			if (entityJson.draggable) {
				makeDraggable($elem, entityJson.entityId, entityJson.dropOnEntities);
			}
			break;
		case 'textInput':
			var $elem = $('<input class="entity" type="text" id="entity-'+entityJson.entityId+'" />');
			$elem.keyup(function(e) {
				handleTextKey($elem, e, entityJson.entityId);
			});
			break;
		default:
			$elem = $('#id_that_does_not_exist_ever');
			break;
	}
	return $elem;
}

function styleEntity($elem, entityJson) {
	$elem[0].className.split(/\s+/).forEach(function(cl) {
		if (cl != 'entity' && cl != 'has-nesting' && cl != "dropOnEntities" && cl.substring(0,3) != "ui-") {
			$elem.removeClass(cl);
		}
	});
	entityJson.classes.forEach(function(cn) {
		$elem.addClass(cn);
	});
  if (entityJson.position) {
    $elem.css('left', entityJson.position.left + '%');
    $elem.css('top', entityJson.position.top + '%');
    $elem.css('position', 'absolute');
  }
	switch (entityJson.size.type) {
		case "percent":
			$elem.width(entityJson.size.width+"%");
			$elem.height(entityJson.size.height+"%");
			break;
		case "autoWidth":
			$elem.height(entityJson.size.height+"%");
			$elem.width("auto");
			break;
		default: break; // nothing.
	}
	if (entityJson.entitiesDropOn && !$elem.droppable('instance')) {
		makeEntityDroppable($elem, entityJson.entityId);
	}
	if (!entityJson.entitiesDropOn && $elem.droppable('instance')) {
		$elem.droppable('destroy');
	}
	switch (entityJson.display.type) {
		case 'text':
			$elem.text(entityJson.display.text)
			break;
		case 'image':
			$elem.children('img').attr('src', entityJson.display.uri);
			break;
		default:
			break;
	}

}

function entityElement(entityJson) {
	console.log("Rendering entity", entityJson.entityId);
	var $elem = $('#entity-' + entityJson.entityId);
	if ($elem.length == 0) {
		$elem = makeEntityElement(entityJson);
	}
	styleEntity($elem, entityJson);
	return $elem;
}

function moveEntityToZone(entityId, zoneId) {
	console.log("Move entity", entityId, "to zone", zoneId);
	$('#entity-'+entityId).appendTo($('#zone-'+zoneId));
}

function makeZone(zoneId, displayData) {
	var $zone = $('');
	switch (displayData.type) {
		case "horizFill":
			$zone = $('<div class="zone" id="zone-'+zoneId+'" style="height:'+displayData.height+'%; width:auto"></div>');
			break;
		case "floatRight":
			$zone = $('<div class="zone" id="zone-'+zoneId+'" style="width:'+displayData.width+'%; height:100%; float: right;"></div>');
			break;
		case "floatLeft":
			$zone = $('<div class="zone" id="zone-'+zoneId+'" style="width:'+displayData.width+'%; height:100%; float: left;"></div>');
			break;
		case "nested":
			return makeZone(zoneId, displayData.display);
		case "shelf":
			$zone = $('<div class="zone" id="zone-'+zoneId+'"></div>');
			$zone.hide();
			$zone.dialog(
				{ autoOpen: false
				, width: "80%"
				, height: 400
				, modal: true
				});
			$('#entity-' + displayData.entity).click(function() {
				$zone.dialog("open");
			});
			break;
		default: // todo: implement.
			alert("Unimplemented zone type " + displayData.type);
			$zone = $('#id_that_does_not_exist_ever');
	}
	return $zone;
}

function getZone(zoneData) {
	var zoneId = zoneData.zoneId;
	var $zone = $('#zone-'+zoneId);
	if ($zone.length == 0) {
		$zone = makeZone(zoneId, zoneData.display.display);
		if (zoneData.display.droppable) {
			$zone.droppable({
				drop: function(event, ui) {
					if (!$zone.is(ui.draggable.parent())) {
						var id = ui.draggable.attr('id');
						if (id) {
							sendMsg({'action':'drag','zone':zoneId,'entity':id.slice(7)});
						}
					}
				},
				tolerance: 'pointer',
				greedy: true,
			});
		}
		zoneData.display.classNames.forEach(function(cn) {
			$zone.addClass(cn);
		});
	}
	return $zone;
}

function moveIfNeeded($parent, $elem) {
	//if (!$parent.is($elem.parent())) {
		var atBottom = false;
		if ($parent.length && $parent.scrollTop() + $parent.innerHeight() + 1 >= $parent[0].scrollHeight) {
			atBottom = true;
		}
		$elem.appendTo($parent);
		if (atBottom) {
			$parent.scrollTop($parent[0].scrollHeight - $parent.innerHeight());
		}
	//}
}

function placeZone($zone, zoneData) {
	switch (zoneData.display.display.type) {
		case "nested":
			$parent = $('#zone-'+zoneData.display.display.zone);
			break;
		case "shelf":
			return;
    case "absolute":
			$parent = $('body');
      $zone.css('left', zoneData.display.display.left + '%');
      $zone.css('top', zoneData.display.display.top + '%');
      $zone.css('position', 'absolute');
		default:
			$parent = $('body');
			break;
	}
	moveIfNeeded($parent, $zone);
}

function renderZone(zoneData) {
	console.log("Rendering zone", zoneData.zoneId);
	var $zone = getZone(zoneData);
	placeZone($zone, zoneData);
	zoneData.entities.forEach(function(entity) {
		$entity = entityElement(entity);
		if (entity.nestedEntity) {
			moveIfNeeded($('#entity-' + entity.nestedEntity + '>.nesting-holder'), $entity)
		} else {
			moveIfNeeded($zone, $entity);
		}
	});
}

function renderScreen(screen) {
	console.log("renderScreen", screen);
	screen.sort(function(zoneData1, zoneData2) {
		return zoneData1.display.order - zoneData2.display.order;
	});
	screen.forEach(renderZone)
	var zoneIds = $.map(screen, function(zoneData) {
		return "zone-" + zoneData.zoneId;
	});
	var entityIds = [];
	$.map(screen, function(zoneData) {
		$.merge(entityIds, $.map(zoneData.entities, function(entity) {
			return 'entity-' + entity.entityId;
		}));
	});
	$('.zone').each(function() {
		if (zoneIds.indexOf($(this).attr('id')) === -1) {
			$(this).remove();
		}
	});
	$('.entity').each(function() {
		if (entityIds.indexOf($(this).attr('id')) === -1) {
			$(this).remove();
		}
	});
}

function showGamelog(gamelog) {
	switch (gamelog.type) {
		case 'display':
			moveIfNeeded($('#zone-'+gamelog.zone), $('<p></p>').text(gamelog.display));
			break;
		case 'move':
			function move(entityId) {
				//moveEntityToZone(entityId, gamelog.move.zone);
			}
			gamelog.move.entities.forEach(move);
			break;
		case 'action':
			moveIfNeeded($('#zone-'+gamelog.zone), $('<p></p>').text('<'+gamelog.actor+'> '+gamelog.string));
			break;
		case 'targetAction': // TODO: Implement.
			alert("unimplemented gamelog type; targetAction");
			break;
	}
}

function handleUpdate(data) {
	data.gamelogs.forEach(showGamelog);
	renderScreen(data.screen);
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
