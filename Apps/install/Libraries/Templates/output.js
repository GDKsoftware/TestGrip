function htmlEntities(str) {
    return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}
$(function(){
	$("div.b64").each( function() {
		$(this).html( htmlEntities(window.atob($(this).html())) );
	});

	var $expandButton = $("span.expand-button");
	var $showTests = $("span.show-tests");
	
	// tests van een functie weergeven of verbergen
	$showTests.click(function(){
		var $this = $(this);
		var $currentFunction = $this.closest("div.function");
		var $functionTests = $currentFunction.find("div.tests");
		$functionTests.stop(true, true).slideToggle(100);
	});
	
	// weergave in-/uitklap knopje regelen
	$expandButton.click(function(){
		$(this).find("span").toggle();
	});
	
	// aangeven of functie gelukt is of niet (passed/failed)
	$("div.function").each(function(i){
		var $this = $(this);
		var $functionResult = $this.find("span.function-result");
		var functionFailed = $this.has("div.test.false").length;
		if (functionFailed) {
			$functionResult.text("failed").addClass("false");
		} else {
			$functionResult.text("passed").addClass("true");
		}
	});
	
});