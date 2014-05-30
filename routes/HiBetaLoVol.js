/* GET home page. */

var spawn = require('child_process').spawn
var env = process.env
var fs = require('fs');
var async = require('async')
var _ = require('underscore')

var RCalls = [['--no-restore','--no-save','spLVHB.R']]

var opts = { cwd: 'Y:\\R-Proj\\Private\\',   
             env: process.env
           }

var retdata=null;

function setup_R_job(args,done){
	var rcall=args.rcall
	var opts=args.opts 
    var R  = spawn('Rscript', rcall, opts)
    R.on('exit',function(code){
        console.log('got exit code: '+code)
        if(code==1){
            // do something special
            done()
        }else{
        	var file=opts.cwd + 'tmp.json'
        	fs.readFile(file,'utf8', function (err, data) {
        		  if (err) {
        			    console.log('json reading Error: ' + err);
        			    return;
        			  }
        			 
        		  retdata = JSON.parse(data);
        		  console.log(retdata)
        	});
            done()
        }
        return null
    })
    return null
}



var toPercentage=function(n){
	return ((Math.floor(n / .5) * .5)* 100.0).toFixed(2)+'%'
}

var queue = async.queue
var jobs = 1 
var basin_queue=queue(setup_R_job, jobs)

_.each(RCalls,function(rcall){	
        var o = _.clone(opts,true)
        var args={opts:o, rcall:rcall}
        basin_queue.push(args)
}) 

exports.index = function(req, res){
		res.render('HiBetaLoVol', { HP:  toPercentage(retdata.cumret) } );
};
