var year = new Date().getFullYear();
var copyright = "Copyright &copy; " + year + " by Brigham Young University - Idaho. All Rights Reserved.";

function jumpTo(idIn) {
	var place, bar, barHeight;
	place = document.getElementById(idIn);
	bar = window.parent.document.getElementById('d2l_minibar');
	if (typeof bar === 'undefined') {
   		barHeight = bar.scrollHeight;
	} else {
		barHeight = 35;
	}
	// scroll to right place
	place.scrollIntoView();

	// scroll up because the back bar at the
	// top of the screen is on top of the node
	window.parent.scrollBy(0, -1 * barHeight  - 15);
}
 
$(document).ready(function() {
	// Insert copyright footer on all pages
	$("<footer></footer>")
		.html(copyright)
	.appendTo("main");
	
	$("<div id='footer'></div>")
		.html(copyright)
	.appendTo("#main");
	
	// Open and close accordian lists
	$("dt").click(function() {
		if ($(this).hasClass("open")) {
			$(this).removeClass("open");
		} else {
			$(this).addClass("open");
		}
	});
});