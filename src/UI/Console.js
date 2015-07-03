// module UI.Console

exports.exportGlobal = function (fname) {return function(f) {return function() {window[fname] = f; return {}; } } }
