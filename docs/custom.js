/**
 * Dynamic Navigation Bars. See [[Wikipedia:NavFrame]]
 * 
 * Based on script from en.wikipedia.org, 2008-09-15.
 *
 * @source www.mediawiki.org/wiki/MediaWiki:Gadget-NavFrame.js
 * @maintainer Helder.wiki, 2012–2013
 * @maintainer Krinkle, 2013
 */
( function () {

// Set up the words in your language
var collapseCaption = 'hide';
var expandCaption = 'show';

var navigationBarHide = '[' + collapseCaption + ']';
var navigationBarShow = '[' + expandCaption + ']';

/**
 * Shows and hides content and picture (if available) of navigation bars.
 *
 * @param {number} indexNavigationBar The index of navigation bar to be toggled
 * @param {jQuery.Event} e Event object
 */
function toggleNavigationBar( indexNavigationBar, e ) {
	var navChild,
		navToggle = document.getElementById( 'NavToggle' + indexNavigationBar ),
		navFrame = document.getElementById( 'NavFrame' + indexNavigationBar );

	// Prevent browser from jumping to href "#"
	e.preventDefault();

	if ( !navFrame || !navToggle ) {
		return false;
	}

	// If shown now
	if ( navToggle.firstChild.data === navigationBarHide ) {
		for ( navChild = navFrame.firstChild; navChild !== null; navChild = navChild.nextSibling ) {
			if ( $( navChild ).hasClass( 'NavContent' ) || $( navChild ).hasClass( 'NavPic' ) ) {
				navChild.style.display = 'none';
			}
		}
		navToggle.firstChild.data = navigationBarShow;

	// If hidden now
	} else if ( navToggle.firstChild.data === navigationBarShow ) {
		for ( navChild = navFrame.firstChild; navChild !== null; navChild = navChild.nextSibling ) {
			if ( $( navChild ).hasClass( 'NavContent' ) || $( navChild ).hasClass( 'NavPic' ) ) {
				navChild.style.display = 'block';
			}
		}
		navToggle.firstChild.data = navigationBarHide;
	}
}

/**
 * Adds show/hide-button to navigation bars.
 *
 * @param {jQuery} $content
 */
function createNavigationBarToggleButton( $content ) {
	var i, j, navChild, navToggle, navToggleText, isCollapsed,
		indexNavigationBar = 0;
	// iterate over all < div >-elements
	var $divs = $content.find( 'div.NavFrame' );
	$divs.each( function ( i, navFrame ) {
		indexNavigationBar++;
		navToggle = document.createElement( 'a' );
		navToggle.className = 'NavToggle';
		navToggle.setAttribute( 'id', 'NavToggle' + indexNavigationBar );
		navToggle.setAttribute( 'href', '#' );
		$( navToggle ).on( 'click', $.proxy( toggleNavigationBar, null, indexNavigationBar ) );

		isCollapsed = $( navFrame ).hasClass( 'collapsed' );
		// backwards compatibility for old technique where the collapsed class was not yet used
		for ( navChild = navFrame.firstChild; navChild !== null && !isCollapsed; navChild = navChild.nextSibling ) {
			if ( $( navChild ).hasClass( 'NavPic' ) || $( navChild ).hasClass( 'NavContent' ) ) {
				if ( navChild.style.display === 'none' ) {
					isCollapsed = true;
				}
			}
		}
		if ( isCollapsed ) {
			for ( navChild = navFrame.firstChild; navChild !== null; navChild = navChild.nextSibling ) {
				if ( $( navChild ).hasClass( 'NavPic' ) || $( navChild ).hasClass( 'NavContent' ) ) {
					navChild.style.display = 'none';
				}
			}
		}
		navToggleText = document.createTextNode( isCollapsed ? navigationBarShow : navigationBarHide );
		navToggle.appendChild( navToggleText );

		// Find the NavHead and attach the toggle link (Must be this complicated because Moz's firstChild handling is borked)
		for ( j = 0; j < navFrame.childNodes.length; j++ ) {
			if ( $( navFrame.childNodes[j] ).hasClass( 'NavHead' ) ) {
				navToggle.style.color = navFrame.childNodes[j].style.color;
				navFrame.childNodes[j].appendChild( navToggle );
			}
		}
		navFrame.setAttribute( 'id', 'NavFrame' + indexNavigationBar );
	} );
}

mw.hook( 'wikipage.content' ).add( createNavigationBarToggleButton );

}());