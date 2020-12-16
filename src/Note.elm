module Note exposing (Note, name, ratio)


type alias Note =
    { threes : Int
    , fives : Int
    , sevens : Int
    }


ratio : Note -> ( Int, Int )
ratio { threes, fives, sevens } =
    let
        numerator =
            3 ^ max threes 0 * 5 ^ max fives 0 * 7 ^ max sevens 0

        denominator =
            3 ^ max -threes 0 * 5 ^ max -fives 0 * 7 ^ max -sevens 0

        twos =
            (toFloat numerator / toFloat denominator)
                |> logBase 2
                |> floor
                |> negate
    in
    ( numerator * 2 ^ max twos 0, denominator * 2 ^ max -twos 0 )


name : Note -> String
name { threes, fives, sevens } =
    let
        noteree =
            1 + modBy 7 (4 * threes + 2 * fives - sevens)

        sharps =
            divBy 7 <| threes + 4 * fives - 2 * sevens + 1

        plusses =
            divBy 7 <| 2 * threes + fives + 3 * sevens + 2
    in
    if sevens < 0 then
        ""

    else
        String.concat
            [ String.repeat sevens "₇"
            , if sharps < 0 then
                String.repeat -sharps "♭"

              else
                String.repeat sharps "♯"
            , String.fromInt noteree
            , if plusses < 0 then
                String.repeat -plusses "-"

              else
                String.repeat plusses "+"
            ]


divBy : Int -> Int -> Int
divBy modulus n =
    let
        m =
            modBy modulus n
    in
    (n - m) // modulus
