
var spawn = require('child_process').spawn
var env = process.env
var async = require('async')
var _ = require('underscore')


var opts = { cwd: './/Rscript//', 
			 cwdout: './/Rscript//returns//',
             env: process.env
           }


function setup_R_job(args,done){
	var rcall=args.rcall
	var opts=args.opts 
    var caller  = spawn('Rscript', rcall, opts)
    caller.stdout.on('data', function(data) {console.log(data.toString())})
    caller.on('exit',function(code){
        console.log('from R.js:\tgot exit code: '+code)
        if(code==1){
            // do something special
            done()
        }else{
        	opts.callback(opts.cwdout)
            done()
        }
        return null;
    })
    return null;
}

var queue = async.queue


exports.call=function(Rcalls, jobs, callback){
	console.log('from R.js:\tr called with '+Rcalls)
	var basin_queue=queue(setup_R_job, jobs)
	var d=new Date()
	_.each(Rcalls,
			function(rcall, i){	
				        var o = _.clone(opts,true);
				        o.callback=callback
				        var args={opts:o, rcall:rcall};
				        basin_queue.push(args);
	}) 
}
