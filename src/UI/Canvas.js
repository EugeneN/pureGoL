// module UI.Canvas

function fromUiEvent(el) {return function(ev) {return Rx.Observable.fromEvent(el, ev) } }