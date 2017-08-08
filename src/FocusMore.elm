module FocusMore exposing (..)

{-|


# Some useful Lenses/Foci/Setters or however you want to call them

Ment to be imported together with SwiftNamesake/please-focus's `Focus` module like this:

    import Focus exposing (Setter, (&), (.=), (=>), ($=))
    import FocusMore as Focus exposing (FieldSetter)

@docs FieldSetter


# Setters for Touples

@docs first, second


# Setters for Lists

@docs index, indexConcat


# Utility

@docs when

-}

import Focus exposing (..)


{-| A type alias for a simpler kind of Setters.
When the setter can't change the type of a subpart of your record.
For example:

    type alias Model a =
        { isBeautiful : Bool
        , somePolymorphicContent : a
        }

Then a Setter for `isBeautiful` would have the type

    isBeautiful : Focus.FieldSetter (Model a) Bool

However, a Setter for somePolymorphicContent could change the inner type:

    somePolymorphicContent : Setter (Model a) (Model b) a b

from `a` to `b` (and therefore change the Model type from `Model a` to `Model b`)

-}
type alias FieldSetter record field =
    Setter record record field field


{-| A `Setter` for the first (or left) part of a Touple.
-}
first : Setter ( a, c ) ( b, c ) a b
first f ( x, y ) =
    ( f x, y )


{-| A `Setter` for the second (or right) part of a Touple.
-}
second : Setter ( c, a ) ( c, b ) a b
second f ( x, y ) =
    ( x, f y )


{-| A `Setter` for a specific index of a list. If the index
is outside the list, no element is updated.

    [1, 2, 3, 4] & Focus.index 1 .= 42

Would result in

    [1, 42, 3, 4]

However

    [1, 2, 3, 4] & Focus.index -1 .= 42

Would result in

    [1, 2, 3, 4]

-}
index : Int -> FieldSetter (List a) a
index index f list =
    let
        applyAtIndex i elem =
            if i == index then
                f elem
            else
                elem
    in
    List.indexedMap applyAtIndex list


{-| Similar to `Focus.index`, but you update each element to a whole
list of elements. Using this you can delete elements from the list like so:

    [1, 2, 3, 4] & Focus.index 2 .= []

becomes

    [1, 2, 4]

Or you can introduce new items to the list, relative to the position you are
focusing on:

    [1, 2, 3, 4] & Focus.index 2 $= (\element -> [1337, element, 42])

becomes

    [1, 2, 1337, 3, 42, 4]

-}
indexConcat : Int -> Setter (List a) (List a) a (List a)
indexConcat index f list =
    let
        applyAtIndex i elem =
            if i == index then
                f elem
            else
                [ elem ]
    in
    List.concat (List.indexedMap applyAtIndex list)


{-| A Utility function to conditionally apply Setters in a pipeline:

    model
        |> Focus.when shouldRemoveElement
            (Focus.indexConcat index .= [])
        |> Focus.when flipBeautyModifier
            (isBeautiful $= not)

-}
when : Bool -> (a -> a) -> a -> a
when shouldApply setter sth =
    if shouldApply then
        setter sth
    else
        sth
