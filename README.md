# pureGoL

This is a Game of Life written in Purescript using Rx and React/Canvas/Console UIs.

Demo: http://eugenen.github.io/pureGoL-demo/

Console UI video: https://www.youtube.com/watch?v=8G-d8XcT6pM&feature=youtu.be

The main goal of this project is to define a clean and highly decoupled interface
between *stateful* business logic and *stateful* UI. Both business logic and UI
are state machines, black boxes, which communicate using only 2 data types and 2 streams:

```purescript
data State = ...
data Action = ...

type BL2UIChannel = Rx.Observable State
type UI2BLChannel = Rx.Observable Action
```

See http://eugenen.github.io/pureGoL-demo/ for demo.

# Run

- `git clone https://github.com/EugeneN/pureGoL.git`
- `cd pureGoL/`
- `pulp dep install`
- *optional if `browserify` can't find `npm` module for `rx`: `npm install rx`
- `pulp browserify > public/GoL.js`
- `open public/index.html`


