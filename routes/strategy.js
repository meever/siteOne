/* GET home page. */

var R=require('../R.js')
var util=require('../util.js')


exports.index = function(req, res){

		var Rcalls = [['--no-restore','--no-save','test1.R']]
	    var s=req.params[0].slice(1) //url.parse(req.url, true);	
	    
//		console.log(R)
		var data=R.call(Rcalls,1)
		    
		console.log('data ' + data[0])
		while(data[0]==null ) {console.log('waiting...')}
		res.render('strategy', { HP:  util.toPercentage(data[0].cumret) } );

};
