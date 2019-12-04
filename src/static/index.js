// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed

import { Elm } from '../elm/Main'

Elm.Main.init({
  node: document.getElementById('main'),
})
