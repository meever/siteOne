

exports.toPercentage=function(n){
	return ((Math.floor(n / .5) * .5)* 100.0).toFixed(2)+'%'
}

