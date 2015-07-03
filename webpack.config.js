var path = require('path');

var config
  = { entry: './entrypoint'
    , output: { path: path.join(__dirname, 'public')
              , filename: 'GoL.js'
              }
    , module: { loaders: [ { test: /\.purs$/, loader: 'purs-loader?src[]=src' } ] }
    , resolve: { modulesDirectories: [ 'node_modules',
                                       'output'
                                     ]
               , extensions: ['', '.js', '.purs']
               }
    , resolveLoader: { root: path.join(__dirname, 'node_modules') }
    }
    ;

module.exports = config;
