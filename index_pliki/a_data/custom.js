jQuery(function($) {

  // Define Theme specific functions
  var Theme = {
    // Swiping mobile galleries wwith Hammer.js
    swipeGallery: function() {
      setTimeout(function() {
        var touchGallery = document.getElementsByClassName("fancybox-wrap")[0];
        var mc = new Hammer(touchGallery);
        mc.on("panleft panright", function(ev) {
          if (ev.type == "panleft") {
            $("a.fancybox-next").trigger("click");
          } else if (ev.type == "panright") {
            $("a.fancybox-prev").trigger("click");
          }
          Theme.swipeGallery();
        });
      }, 500);      
    },
    swipeInit: function() {
      if ('ontouchstart' in window) {
        $("body").on("click", "a.w-fancybox", function() {
          Theme.swipeGallery();
        });
      }
      // Add fullwidth class to gallery thumbs if less than 6
      $('.imageGallery').each(function(){
        if ($(this).children('div').length <= 6) {
          $(this).children('div').addClass('fullwidth-mobile');
        }
      });
    },
    toggleClick: function(click, target, classname){
      $(click).click(function(){
        $(target).toggleClass(classname);
      });
    },
    // Interval function to execute post-post-load events
    interval: function(condition, action, duration, limit) {
      var counter = 0;
      var looper = setInterval(function(){
        if (counter >= limit || Theme.checkElement(condition)) {
          clearInterval(looper);
        } else {
          action();
          counter++;
        }
      }, duration);
    },
    checkElement: function(selector) {
      return $(selector).length;
    },
    moveLogin: function() {
      var login = $("#member-login");
      login.detach().appendTo("#navmobile .wsite-menu-default");
    },
    setPosition: function(){
      $("#wrapper").height() < $(window).height() ? $("#wrapper").css({"position": "absolute"}) : $("#wrapper").css({"position": "relative"});
    },
    cartDisplay: function() {
      if (Number($('#wsite-mini-cart .wsite-subtotal-wrapper .wsite-price').text()) > 0 ) {
        $('#wsite-mini-cart').addClass('full');
        $('#footer').addClass('footer-full');
      }
      else {
          $('#wsite-mini-cart').removeClass('full');
          $('#footer').removeClass('footer-full');
      }
    }
  }

  $(document).ready(function() {
    $("body").addClass("postload");
    Theme.swipeInit();
    Theme.toggleClick(".wsite-com-sidebar", ".wsite-com-sidebar", "sidebar-expanded");
    Theme.toggleClick(".hamburger", "body", "menu-open");
    Theme.interval("#wsite-mini-cart.full", Theme.cartDisplay, 800, 10);
    $('.wsite-product-button, #wsite-com-product-add-to-cart, .wsite-product-item .wsite-remove-button').on('click', function(){
      setTimeout(function() { Theme.cartDisplay(); }, 800);
    });
    if ($(window).width() <= 1024) {
      Theme.moveLogin();
      Theme.setPosition();
      // Add fullwidth class to gallery thumbs if less than 6
      $('.imageGallery').each(function(){
        if ($(this).children('div').length <= 6) {
          $(this).children('div').addClass('fullwidth-mobile');
        }
      });
    }
    
    
  });
});