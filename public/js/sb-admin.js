//$(function() {
//
//    $('#side-menu').metisMenu();
//
//});

//Loads the correct sidebar on window load,
//collapses the sidebar on window resize.
$(function() {
	localStorage['strategy']=''
		
    $(window).bind("load resize", function() {
        width = (this.window.innerWidth > 0) ? this.window.innerWidth : this.screen.width;
        if (width < 768) {
            $('div.sidebar-collapse').addClass('collapse')
        } else {
            $('div.sidebar-collapse').removeClass('collapse')
        }
    });
  

    function routing2root(stra){
    	if (localStorage['strategy'] == undefined || localStorage['strategy'] != stra){  
    		localStorage['strategy']=stra
    		$('#liveUpdate').trigger('click');
    		$('#historyUpdate').trigger('click');
    	}
    		
    }
    
    function growl(message, type){
 	   $.bootstrapGrowl(message, {
		  	  ele: 'body', // which element to append to
		  	  type: type, // (null, 'info', 'error', 'success')
		  	  offset: {from: 'bottom', amount: 20}, // 'top', or 'bottom'
		  	  align: 'right', // ('left', 'right', or 'center')
		  	  width: 'auto', // (integer, or 'auto')
		  	  delay: 30000, // Time while the message will be displayed. It's not equivalent to the *demo* timeOut!
		  	  allow_dismiss: true, // If true then will display a cross to close the popup.
		  	  stackup_spacing: 10 // spacing between consecutively stacked growls.
		  	});
    }
    
    
    //adding links
    $('#liveUpdate').click( function(event ){
    	event.preventDefault();
    	var strategy= localStorage['strategy'] || 'VIX'
    	growl('Live update initiated!', 'info');
    	$.ajax({url: '/strategy/'+strategy+'?action=update&section=live&stale=60000',
    		dataType:'json',
			success: function(data){
				if (data.updated == true) 
					growl('Live update finished at '+data.timestamp +'!', 'success');
				else
					growl('Live update was performed '+data.updated.toFixed(1) + ' min(s) ago.', 'info');
				
				$('#liveSection').text('updated by R @'+data.timestamp)
			}
    	})
    })  
    
    $('#historyUpdate').click( function(event ){
    	event.preventDefault();
	    growl("Backtesting historical performance initiated, be patient!", 'info');
    	var strategy= localStorage['strategy'] || 'VIX'
    	//alert(strategy+ ' historyupdate')
    	$.ajax({url: '/strategy/'+strategy+'?action=update&section=history&stale=28800000',
    		dataType:'json',
			success: function(data){
				if (data.updated == true) 
					growl('Backtesting finished at '+data.timestamp +'!', 'success');
				else
					growl('Backtesting was performed '+data.updated.toFixed(1) + ' min(s) ago.', 'info');
				updateHistory(data.data);
			}
    	})
    }) 
    
    $('#VIX').click(function(event){
    	event.preventDefault();
    	$.ajax({url: '/',
			success: function(){
				routing2root('VIX')
			}
    	})
    })  
    
    $('#HiLo').click(function(event){
    	event.preventDefault();
    	$.ajax({url: '/',
			success: function(){
				routing2root('HiLo')
			}
    	})
    })  
    
    $('#Combo').click(function(event){
    	event.preventDefault();
    	$.ajax({url: '/',
			success: function(){
				routing2root('Combo')			
			}
    	})
    })  
    
    function updateHistory(d) {
    	$('#historySection').empty();
    	var ys=d.traits.slice(1);
	    Morris.Area({
	        element: 'historySection',
	        data: d.values,
	        xkey: 'date',
	        labels: ys,
	        ykeys: ys,
	        pointSize: 1,
	        hideHover: 'auto',
	        resize: true
	    });
    } 
    
});

