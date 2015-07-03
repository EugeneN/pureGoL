// module UI.Canvas

exports.fromUiEvent = function (el) {return function(ev) {return Rx.Observable.fromEvent(el, ev) } }
