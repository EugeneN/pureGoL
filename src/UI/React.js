// module UI.React

exports.setProps = function (view) { return function(props) { return function(){ return view.setProps(props); } } }
