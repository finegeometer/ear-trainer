module NoteSet exposing (NoteSet, add, fold, isEmpty, member, new, remove, toggle)

import Note exposing (Note)
import Set.Any


type NoteSet
    = NoteSet (Set.Any.AnySet ( Int, Int, Int ) Note)


new : NoteSet
new =
    NoteSet (Set.Any.empty (\{ threes, fives, sevens } -> ( threes, fives, sevens )))


add : Note -> NoteSet -> NoteSet
add n (NoteSet s) =
    NoteSet (Set.Any.insert n s)


remove : Note -> NoteSet -> NoteSet
remove n (NoteSet s) =
    NoteSet (Set.Any.remove n s)


toggle : Note -> NoteSet -> NoteSet
toggle n (NoteSet s) =
    NoteSet (Set.Any.toggle n s)


member : Note -> NoteSet -> Bool
member n (NoteSet s) =
    Set.Any.member n s


fold : (Note -> x -> x) -> x -> NoteSet -> x
fold f x (NoteSet s) =
    Set.Any.foldr f x s


isEmpty : NoteSet -> Bool
isEmpty (NoteSet s) =
    Set.Any.isEmpty s
