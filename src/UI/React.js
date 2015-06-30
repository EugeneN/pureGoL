// module UI.React

function setProps(view) { return function(props) { return function(){ return view.setProps(props); } } }
