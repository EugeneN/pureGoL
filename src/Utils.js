// module Utils
function timeDelta(a) { return function(b) { return b - a } }
function toFixed(x) { return function(n) { return +x.toFixed(n) } }
function hex(n) {return n.toString(16) }
var newSubject = function () { return new Rx.Subject() }
function fromEvent(ev) { return function() {return Rx.Observable.fromEvent(document.body, ev)} }
function getIntervalStream(interval) { return Rx.Observable.interval(interval) }
function onNext(obs){ return function(val) { return function () { return obs.onNext(val); } } }
function pausable(obs){ return function (pauser) { return obs.pausable(pauser); } }
function scan(f) {
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
function getElementOffsetLeft(el){ return function() { return document.getElementById(el).offsetLeft } }
function getElementOffsetTop(el){ return function() { return document.getElementById(el).offsetTop } }
function getParameterByName(name) {
    return function() {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec(location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    }
}
function displayBlock(elid) {return function() {document.getElementById(elid).style.display = "block"} }
function which(ev) { return ev.which; }
