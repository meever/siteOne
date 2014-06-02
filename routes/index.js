/* GET home page. */
exports.index = function(req, res){
  console.log('from home.js\trouting to \\...')
  res.render('index');
};
