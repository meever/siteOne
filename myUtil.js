
exports.toPercentage=function(n){
	return ((Math.floor(n / .5) * .5)* 100.0).toFixed(2)+'%'
}

exports.timetonumber=function(t){
	t=t || new Date();
	return t.toISOString().replace(/[^0-9]/g, "")
}