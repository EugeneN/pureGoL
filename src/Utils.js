// module Utils
exports.timeDelta = function (a) { return function(b) { return b - a } }
exports.toFixed = function (x) { return function(n) { return +x.toFixed(n) } }
exports.hex = function (n) {return n.toString(16) }
exports.newSubject = function () { return new Rx.Subject() }
exports.fromEvent = function (ev) { return function() {return Rx.Observable.fromEvent(document.body, ev)} }
exports.getIntervalStream = function (interval) { return Rx.Observable.interval(interval) }
exports.onNext = function (obs){ return function(val) { return function () { return obs.onNext(val); } } }
exports.pausable = function (obs){ return function (pauser) { return obs.pausable(pauser); } }
exports.scan = function (f) {
    return function(seed) {
        return function(ob) {
            return function() {
                return ob.scan(seed, function(acc, value) {
                    return f(value)(acc)();
                });
            };
        };
    };
}
exports.getElementOffsetLeft = function (el){ return function() { return document.getElementById(el).offsetLeft } }
exports.getElementOffsetTop = function (el){ return function() { return document.getElementById(el).offsetTop } }
exports.getParameterByName = function (name) {
    return function() {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec(location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    }
}
exports.displayBlock = function (elid) {return function() {document.getElementById(elid).style.display = "block"} }
exports.which = function (ev) { return ev.which; }
