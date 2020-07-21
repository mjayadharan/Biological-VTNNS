/**
 * @file
 * Fire callbacks for media query breakpoints
 *
 * To use this file enable the OnMediaQuery.js polyfill in your subthemes
 * appearance settings - this will load the required plugin and this file.
 *
 * This allows you to write context (media query) specific JS without hard
 * coding the media queries, aka like matchMedia. Each context matches the
 * media queries you set in theme settings (by adding the font-family
 * declarations to the responsive layout CSS).
 *
 * SEE: https://github.com/JoshBarr/js-media-queries (really, go look, lots of
 * useful documentation available).
 *
 * IMPORTANT: do not rename or move this file, or change the directory name!
 */



//



function moveNavToDrawer(){
  var isSchoolFront = jQuery('body').hasClass('school-site') && jQuery('body').hasClass('front') ? true : false;
  if(isSchoolFront){return;}
  jQuery('#block-menu-menu-drawer').prependTo('#navigation_drawer .region-inner:first');
}
function moveNavToSide(){
  var isSchoolFront = jQuery('body').hasClass('school-site') && jQuery('body').hasClass('front') ? true : false;
  if(isSchoolFront){return;}
  jQuery('#block-menu-menu-drawer').prependTo('.region-sidebar-first .region-inner:first');
}



// create columns for broswers without CSS3 column support
function createColumns(columns){
  if( jQuery('html').hasClass('no-csscolumns') && jQuery('.columns').length ) {
    if( jQuery(this).hasClass('related') ) { columns -= 1; }
    jQuery('div.columns').each(function() {
      jQuery('div.columns ul li').each(function() {
         jQuery('div.columns ul:first').append(jQuery(this));
         jQuery('div.columns ul').not(':first').remove();
      });
      jQuery('div.columns ul').removeClass().addClass('columns').unwrap();
    });
    jQuery('ul.columns').each(function() {
      jQuery(this).columnizeList({
        columns: columns,
        wrapper_class: 'columns'
      });
      jQuery('div.columns ul').addClass('columns'+columns);
    });
  }
}



var queries = [

  // ****Smartphone****
  {
    context: ['smartphone_portrait', 'smartphone_landscape'],
    call_in_each_context: false,
    callback: function() {
      // Debug
      //console.log('smartphone');
      jQuery('.region-content-aside.floated:first').removeClass('floated').appendTo('.region-sidebar-first  .region-inner:first'); 
      jQuery('#main-content').removeClass('with-aside');
      jQuery('#footer-wrapper').not('.moved').addClass('moved').appendTo('.region-sidebar-first .region-inner:first');
    }
  },
  // portrait only
  {
    context: 'smartphone_portrait',
    callback: function() {
      // Debug
      //console.log('smartphone portrait');
    }
  },
  // landscape only
  {
    context: 'smartphone_landscape',
    callback: function() {
      // Debug
      //console.log('smartphone_landscape ');

      // create columns for older browsers
      createColumns(2);
    }
  },
  

  // ****Tablet****
  {
    context: ['tablet_portrait', 'tablet_landscape'],
    call_in_each_context: false,
    callback: function() {
      // Debug
      //console.log('tablet');
      
      jQuery('.region-content-aside:first').removeClass('floated').appendTo('.region-sidebar-first .region-inner:first');
    
      jQuery('#main-content').removeClass('with-aside');
    }
  },
  
  // portrait only
  {
    context: 'tablet_portrait',
    callback: function() {
      // Debug
      //console.log('tablet_portrait');
      
      //move left rail nav into drawer
      moveNavToDrawer();
      
      //move the footer content to the bottom
      jQuery('#footer-wrapper').not('.moved').addClass('moved').appendTo('.region-sidebar-first .region-inner:first');

      // create columns for older browsers
      createColumns(2);
    }
  },
  // landscape only
  {
    context: 'tablet_landscape',
    callback: function() {
      // Debug
      //console.log('tablet_landscape');
      
      //move the footer content to the main content area
      jQuery('.region-sidebar-first #footer-wrapper.moved').removeClass('moved').appendTo('#content-column .content-inner:first');
      
      //move the local nav to the left rail
      moveNavToSide();

      // create columns for older browsers
      createColumns(3);
    }
  },


  // ****Standard desktop context****
  {
    context: 'standard',
    callback: function() {
      // Debug
      //console.log('standard desktop');
      //move the aside content up

      if( jQuery.trim( jQuery('.region-content-aside').text() ).length && jQuery('.region-content-aside').height() > 50 ){
        jQuery('.region-content-aside:first').addClass('floated').insertBefore('#main-content');
        jQuery('#main-content').addClass('with-aside');
      }

      jQuery('.region-sidebar-first #footer-wrapper.moved').removeClass('moved').appendTo('#content-column .content-inner:first');
      
      //move the local nav to the left rail
      moveNavToSide();
      //add the shadow for taller left nav
      jQuery('.region-sidebar-first #block-menu-menu-drawer').height() > 180 ? jQuery('.region-sidebar-first #block-menu-menu-drawer').addClass('shadowed') : 0;

      // create columns for older browsers
      createColumns(4);
      

    }
  }
];

// Go!
MQ.init(queries);



//