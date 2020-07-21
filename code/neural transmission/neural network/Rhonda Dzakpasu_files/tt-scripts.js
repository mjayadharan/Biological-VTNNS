jQuery(function($) {

$(document).ready(function(e) {

	/*****
		Clickable tabs
	*****/

	$('.select-tab:nth-of-type(1), .select-tabs li:nth-of-type(1)').addClass('active');
	$('.select-tabs li a')
	.append('<span>&#9658;</span>')
	.live('click', function() {
		var intTab = $(this).parent('li').index();
		var strTab = $(this).attr('href');
		$('.select-tabs.clone').remove();
		$('.select-tab, .select-tabs li').removeClass('active');
		$('.select-tabs li').eq(intTab).addClass('active');
		$(strTab).addClass('active');
		return false;
	});
	$('.select-tabs span').click(function() {
		$('.select-tabs').clone().addClass('clone').insertBefore('.select-tabs');
		return false;
	});



	/*****
		Full Menu navigation
	*****/

	// hide/show the full menu
	$('#button-menu').click(function() {
		$('body').toggleClass('menu-open');
		return false;
	});

	// expandable subnavs
	var fullnavMain = '#menu-full li.expanded';
	$(fullnavMain).find('> a').append('<span>&#9658;</span>');
	$(fullnavMain).find('> a.active-trail span').addClass('open');
	$(fullnavMain).find('> a span').live('click', function() {
		$(this).toggleClass('open').parent('a').next('ul.menu').slideToggle();
		return false;
	});



	/*****
		Search bar: hide/show
	*****/

	$('#button-search').click(function() {
		$('#gu-search').slideToggle();
		$('#header, #menu-full').toggleClass('search-open');
		return false;
	});



	/*****
		Subnav menus
	*****/

	// SUBPAGES: copy subnav from full menu to sidebar only on subpages
	if( $('body').hasClass('not-front') ) {
		// check if the page is part of a menu
		if( $('ul.menu li.menu-depth-1').hasClass('expanded active-trail') ) {
			// check if the page already has a sidebar
			if( $('body').hasClass('no-sidebars') ) {
				// change the body classes for sidebars
				$('body').removeClass('no-sidebars').addClass('one-sidebar sidebar-first');
				// add the sidebar wrapper
				$('#content-wrapper #columns .columns-inner').append('<div class="region region-sidebar-first sidebar"><div class="region-inner clearfix">');
			}
			// create the container for the menu
			$('#content-wrapper .region-sidebar-first .region-inner').prepend('<section id="block-menu-menu-drawer" role="navigation">');
			// clone the active-trail subnav
			$('ul.menu li.expanded.active-trail.menu-depth-1').find('> ul.menu').clone().prependTo('#block-menu-menu-drawer').find('a span').remove();
			// check that main content is as tall as subnav
			hSidebar = $('#columns .region-sidebar-first').height();
			hContent = $('#columns #content-column .content-inner').height();
			if( hSidebar > hContent ) {
				$('#columns #content-column .content-inner').css('min-height', hSidebar+'px');
			}
		}
	}

	// HOMEPAGE: copy subnav from full menu to pagelets only on the homepage
	// see below for scripts in homepage functions



	/*****
		Aside region
	*****/
	var aside = '#content-column .region.region-content-aside',
		asideBlock = '.content.block-content',
		intBlocks = 0;
	$(aside).find(asideBlock).each(function() {
		if( $.trim( $(this).text() ).length || $(this).find('iframe').length ) {
			intBlocks++;
		}
	});
	if( intBlocks > 0 ) {
		// wrap main and secondary content in one container
		$('#content-column').addClass('aside');
		$('#main-content').before('<div id="body-content">');
		$('#main-content, #content-column .region-content-related').appendTo( $('#body-content') );
	} else {
		// remove empty related content
		$(aside).remove();
	}



	/*****
		Homepage news rotator
	*****/

	var homeNews = $('#block-views-v11-related-content-block-3 .listing.related-listing');
	$(homeNews).find('ul').addClass('slides').find('li').addClass('slide');

	var wNewsItem,
		wHomeNews = $(homeNews).width(),
		wWindow = window.outerWidth;
	if( wWindow <= 520 ) { wNewsItem = wHomeNews/2; }
	else if( wWindow > 520 && wWindow <= 768 ) { wNewsItem = wHomeNews/3; }
	else if( wWindow > 768 && wWindow <= 1024 ) { wNewsItem = wHomeNews/4; }
	else if( wWindow > 1024 ) { wNewsItem = wHomeNews/5; }

	$(homeNews).flexslider({
		animation: 'slide',
		slideshow: false,
		directionNav: true,
		controlNav: false,
		itemWidth: 218,
		minItems: 1,
		maxItems: 5,
		useCss: true,
		touch: true
	});



	/*****
		Homepage menus and scrolling
	*****/

	// only do this on the homepage
	if( $('body').hasClass('front') ) {

		// pagelet
		var pagelet = '#block-views-pagelets-block .views-row';
		$(pagelet).each(function() {

			// set anchor IDs on pagelet container
			var pageletID = 'menu-item-' + $(this).find('.views-field-mlid span').html();
			$(this).attr('id',pageletID);
			$(this).find('.views-field-mlid').remove();

			// copy subnav from full menu to pagelets
			$(this).addClass('subnav');
			var subnavMenu = '#menu-full ul.menu li.menu-depth-1.' + pageletID + ' > ul.menu';
			$(subnavMenu).clone().insertAfter( $(this).find('.views-field-field-generic-body1') ).wrap('<div class="subnav-menu">');

			// does the pagelet have a related content block?
			var relatedBlock = $(this).find('.views-field-view');
			if( $(relatedBlock).length ) {
				if( !$(relatedBlock).find('.listing').length ) {
					$(relatedBlock).remove();
				} else {
					$(this).addClass('related');
				}
			}

		});

		// add expandable function to pagelet subnav
		var pageletsSubnav = $(pagelet).find('.subnav-menu li');
		$(pageletsSubnav).find('> a span').live('click', function() {
			$(this).toggleClass('open').parent('a').next('ul.menu').slideToggle();
			return false;
		});

		// change links to anchors in menu
		var menuMain = '#block-georgetown-header-footer-gu-top-menu ul.menu';
		var menuMainItem = menuMain + ' li';
		$(menuMainItem).each(function() {
			var menuItem;
			var classes = $(this).attr('class').split(' ');
			for( var i = 0; i < classes.length; i++ ) {
				if( classes[i].substr(0,10) == 'menu-item-' ) {
					menuItem = classes[i];
					break;
				}
			}
			$(this).find('a').attr('href','#'+menuItem);
		});

		// copy main menu to pagelets
		$(menuMain).clone().insertBefore(pagelet + ' .views-field-title').wrap('<div class="pagelets-menu">');

		// initiate smooth scrolling plugin
		var menuPageletItem = pagelet + ' .pagelets-menu ul.menu li';
		$(menuMainItem + ', ' + menuPageletItem).find('a').addClass('smooth');
		$('ul.menu a.smooth').click(function() {
			$.smoothScroll({
				scrollTarget: $(this).attr('href'),
				offset: ( -1 * $('#header').outerHeight() )
			});
			return false;
		});

	}

});

});
