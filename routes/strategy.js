/* GET home page. */

var fs = require('fs');
var R=require('../R.js')
var myUtil=require('../myUtil.js')
var rPrograms={'VIX':'VIXhedging.R', 'HiLo':'test1.R', 'Combo':'test1.R'}
var lastAction={'updated' :{'live':{},'history':{}}, 'saved':{'live':{},'history':{}}}

exports.strategy = function(req, res){
		
	    var stra= req.params[0]
	    var rProg= rPrograms[stra]
	    var stale= req.query['stale'];
	    var section =req.query.section;
		var calltime= new Date();
		var outputNameprefix= stra+'-'+section+'-update-'
		var outputName= outputNameprefix+ myUtil.timetonumber(calltime)
		var Rcalls = [['--no-restore','--no-save',rProg, stra, section, 'update',outputName ]]
	
	    
		var update=function(path, fn, lapse){
			if (fn==undefined) newUpdate=true; else newUpdate=lapse
	    	fn= fn || outputName
        	var file=path + fn + '.json' // this name needs to be changed.        	
			console.log('from strategy.js:\tfinished updating,reading file '+file+'...')
        	fs.readFile(file,'utf8', function (err, data) {
        		  if (err) {
        			    console.log('from strategy.js:\tjson reading Error: ' + err);
        		   }  
        		  if (newUpdate == true) lastAction.updated[section][outputNameprefix]=calltime;
        		  var sendData= JSON.parse(data)
        		  sendData.updated=newUpdate
        		  res.json(sendData)
        	});
		}
	    
		var save=function(data){
			console.log('from strategy.js:\tfinished saving...');
		}		
		
		var callbacks ={'update':update, 'save':save}
		
		switch (req.query.action) {
		case 'update':
			var last= lastAction.updated[section][outputNameprefix];
			if (last== undefined || 
					((new Date()) - last)> stale) {
				R.call(Rcalls,1,callbacks[req.query.action])
			} else {
				callbacks['update']('.//Rscript//returns/',outputNameprefix+ myUtil.timetonumber(last),
						(new Date() - last)/60000)
			}
			break;
		}					
};
