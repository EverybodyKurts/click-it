// pull in desired CSS/SASS files
require( './styles/main.scss' );

import { Elm } from '../elm/Main'

Elm.Main.init({
  node: document.getElementById('main'),
})
