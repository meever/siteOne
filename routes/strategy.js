/* GET home page. */

var R=require('../R.js')
var util=require('../util.js')

exports.index = function(req, res){

		var Rcalls = [['--no-restore','--no-save','test1.R']]
	    var s=req.params[0].slice(1) //url.parse(req.url, true);	
	    
//		console.log(R)
		var callback=function(data){
			var newres=res
			newres.render('strategy', { HP:  util.toPercentage(data.cumret) } );
		} 

		var data=R.call(Rcalls,1,callback)


};
