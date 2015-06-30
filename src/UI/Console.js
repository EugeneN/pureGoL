// module UI.Console

function exportGlobal(fname) {return function(f) {return function() {window[fname] = f; return {}; } } }