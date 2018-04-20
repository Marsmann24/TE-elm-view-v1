module Slots exposing (..)

import Array exposing (Array)

type alias Slots a =
    { left : List a
    , focus : Array a
    , right : List a
    , null : a --nullelement
    }

setFocusSize : Slots a -> Int -> Slots a
setFocusSize slots focusSize =
    let currentFocus = Array.length (getFocus slots)
        newFocus =
            if (currentFocus == focusSize)
            then
                (getFocus slots)
            else if (currentFocus > focusSize)
            then
                Array.slice 0 focusSize (getFocus slots)
            else
                Array.append
                    (getFocus slots)
                    (Array.fromList
                        (List.take
                            (focusSize - currentFocus)
                            (if (List.length slots.right >= (focusSize - currentFocus))
                            then slots.right
                            else
                                (List.append
                                    slots.right
                                    (List.repeat (focusSize - currentFocus) slots.null)
                                )
                            )
                        )
                    )
        newRight =
            if (currentFocus == focusSize)
            then
                slots.right
            else if (currentFocus < focusSize)
            then
                List.drop (focusSize - currentFocus) slots.right
            else
                List.append
                    (Array.toList
                        (Array.slice
                            focusSize
                            currentFocus
                            (getFocus slots)
                        )
                    )
                    slots.right
    in
    { slots
        | focus = newFocus
        , right = newRight
    }

focusLength : Slots a -> Int
focusLength slots =
    let count : a -> Int -> Int
        count item counter =
            if (isNull slots item)
            then
                counter
            else
                counter + 1
    in
    Array.foldl count 0 slots.focus

isNull : Slots a -> a -> Bool
isNull slots item =
    (item == slots.null)

isNotNull : Slots a -> a -> Bool
isNotNull slots item =
    not (isNull slots item)

focusIsFull : Slots a -> Bool
focusIsFull slots =
    ((getFocus slots) == slots.focus)

getFirstIdOf : Slots a -> a -> Maybe Int
getFirstIdOf slots item =
    let check : a -> { n : Int, b : Bool} -> { n : Int, b : Bool}
        check value counter =
            if ((value == item) || counter.b)
            then
                { counter | b = True}
            else
                { counter | n = counter.n + 1}
        result : {n : Int, b : Bool}
        result =
            Array.foldl check {n = 0, b = False} slots.focus
    in
    if (result.b)
    then Just result.n
    else Nothing

getFocus : Slots a -> Array a
getFocus slots =
    Array.filter (isNotNull slots) slots.focus

getFocusFirst : Slots a -> a
getFocusFirst slots =
    getById slots 0

getFocusFirstNotNull : Slots a -> a
getFocusFirstNotNull slots =
    Maybe.withDefault slots.null (Array.get 0 (getFocus slots))

getById : Slots a-> Int -> a
getById slots id =
    Maybe.withDefault slots.null (Array.get id slots.focus)

getFocusLast : Slots a -> a
getFocusLast slots =
    getById slots ((Array.length slots.focus) - 1)

getFocusLastNotNull : Slots a -> a
getFocusLastNotNull slots =
    Maybe.withDefault slots.null (Array.get (Array.length (getFocus slots) - 1) (getFocus slots))

leftAdd : Slots a -> a -> List a
leftAdd slots item =
    if (isNull slots item)
    then
        slots.left
    else
        item :: (slots.left)

leftDrop : Slots a -> List a
leftDrop slots =
    Maybe.withDefault [] (List.tail slots.left)

leftHead : Slots a -> a
leftHead slots =
    Maybe.withDefault slots.null (List.head slots.left)

rightAdd : Slots a -> a -> List a
rightAdd slots item =
    if (isNull slots item)
    then
        slots.right
    else
        item :: (slots.right)

rightDrop : Slots a -> List a
rightDrop slots =
    Maybe.withDefault [] (List.tail slots.right)

rightHead : Slots a -> a
rightHead slots =
    Maybe.withDefault slots.null (List.head slots.right)

setAt : Slots a -> a -> Int -> Slots a
setAt slots value id =
    { slots
        | focus = Array.set id value slots.focus
    }

moveLeft : Slots a -> Slots a
moveLeft slots =
    let
        next : Int -> a -> a
        next id view =
            if (id < ((Array.length slots.focus) - 1))
            then
                Maybe.withDefault slots.null (Array.get (id + 1) slots.focus)
            else
                rightHead slots
    in
    { slots
        | left = leftAdd slots (getFocusFirst slots)
        , focus = Array.indexedMap next slots.focus
        , right = rightDrop slots
    }

moveRight : Slots a -> Slots a
moveRight slots =
    let
        next : Int -> a -> a
        next id view =
            if (id > 0)
            then
                Maybe.withDefault slots.null (Array.get (id - 1) slots.focus)
            else
                leftHead slots
    in
    { slots
        | left = leftDrop slots
        , focus = Array.indexedMap next slots.focus
        , right = rightAdd slots (getFocusLast slots)
    }

insertAt : Slots a -> a -> Int -> Slots a
insertAt slots value id =
    let calcPosition : Int
        calcPosition =
            case (getFirstIdOf slots slots.null) of
                Just firstNull ->
                    if id > firstNull
                    then firstNull
                    else id
                _ ->
                    id
        next : Int -> Int -> a -> a
        next useId pointer view =
            if (pointer < useId)
            then
                Maybe.withDefault slots.null (Array.get pointer slots.focus)
            else if (pointer == useId)
            then
                value
            else
                Maybe.withDefault slots.null (Array.get (pointer - 1) slots.focus)
    in
    if (isNull slots value)
    then
        slots
    else if (focusIsFull slots)
    then
        { slots
            | focus = Array.indexedMap (next id) slots.focus
            , right = rightAdd slots (getFocusLast slots)
        }
    else
        { slots
            | focus =  Array.indexedMap (next calcPosition) slots.focus
        }

insertAfter : Slots a -> a -> Int -> Slots a
insertAfter slots value id =
    if ((focusIsFull slots) && (id + 1 == (Array.length slots.focus)))
    then
        insertAt (moveLeft slots) value id
    else
        insertAt slots value (id + 1)

insertEnd : Slots a -> a -> Slots a
insertEnd slots value =
    if ((not (List.isEmpty slots.right)) && (focusIsFull slots))
    then
        insertEnd (moveLeft slots) value
    else
        insertAfter slots value ((focusLength slots) - 1)

removeAt : Slots a -> Int -> Slots a
removeAt slots id =
    let
        next : Int -> a -> a
        next pointer view =
            if (pointer < id)
            then
                Maybe.withDefault slots.null (Array.get pointer slots.focus)
            else if (pointer < focusLength slots - 1)
            then
                Maybe.withDefault slots.null (Array.get (pointer + 1) slots.focus)
            else
                rightHead slots
        newSlots : Slots a
        newSlots =
            { slots
                | focus = Array.indexedMap next slots.focus
                , right = rightDrop slots
            }
        alignLeft : Slots a -> Slots a
        alignLeft slots =
            if (focusIsFull slots)
            then
                slots
            else if (List.isEmpty slots.left)
            then
                slots
            else
                alignLeft (moveRight slots)
    in
    alignLeft newSlots
