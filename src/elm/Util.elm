module Util exposing (flip)


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b
