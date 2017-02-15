Shiny.addCustomMessageHandler("generalErrorMessage",
	function(message){
		alert(JSON.stringify(message));
	}
);
