
var spawn = require('child_process').spawn
var env = process.env
var fs = require('fs');
var async = require('async')
var _ = require('underscore')


var opts = { cwd: './/Rscript//',   
             env: process.env
           }


function setup_R_job(args,done){
	var rcall=args.rcall
	var opts=args.opts 
    var caller  = spawn('Rscript', rcall, opts)
    caller.on('exit',function(code){
        console.log('got exit code: '+code)
        if(code==1){
            // do something special
            done()
        }else{
        	var file=opts.cwd + '//test1.json' // this name needs to be changed.
        	fs.readFile(file,'utf8', function (err, data) {
        		  if (err) {
        			    console.log('json reading Error: ' + err);
        		   }   
        		  opts.callback(JSON.parse(data))
        		  console.log(data)
        	});
            done()
        }
        return null;
    })
    return null;
}

var queue = async.queue


exports.call=function(Rcalls, jobs, callback){
	console.log('r called with...'+Rcalls)
	var basin_queue=queue(setup_R_job, jobs)
	var d=new Date()
	_.each(Rcalls,
			function(rcall, i){	
				        var o = _.clone(opts,true);
				        o.outputName= rcall[2].slice(0,-2)+d.toDateString()
				        o.callback=callback
				        console.log(o.outputName)
				        var args={opts:o, rcall:rcall};
				        basin_queue.push(args);
	}) 
}
