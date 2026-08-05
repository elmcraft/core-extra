module Array.Extra.Singleton exposing (repeat1, initialize1, pushEmpty, fromList, mapExistingSingleton)

import Array exposing (Array)


repeat1 : a -> Array a
repeat1 element =
    Array.repeat 1 element


initialize1 : a -> Array a
initialize1 element =
    Array.initialize 1 (\_-> element)


pushEmpty : a -> Array a
pushEmpty element =
    Array.push element Array.empty


fromList : a -> Array a
fromList element =
    Array.fromList [ element ]


mapExistingSingleton : a -> Array a
mapExistingSingleton element =
    Array.map (\() -> element) existingSingleton


existingSingleton : Array ()
existingSingleton =
    Array.push () Array.empty
