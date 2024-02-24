module String.RemoveAccentsTest exposing (removeAccentsTest)

import Expect
import String.Extra exposing (removeDiacritics)
import Test exposing (Test, describe, test)


removeAccentsTest : Test
removeAccentsTest =
    describe "removeAccents"
        [ test "Should result string without accents" <|
            \() ->
                removeDiacritics "ąáàãâäćęéèêëíìîïłóòõôöśúùûüçźżĄÁÀÃÂÄĆĘÉÈÊËÍÌÎÏŁÓÒÕÖÔŚÚÙÛÜÇŹŻ"
                    |> Expect.equal "aaaaaaceeeeeiiiilooooosuuuuczzAAAAAACEEEEEIIIILOOOOOSUUUUCZZ"
        , test "Should result in phrase without accents" <|
            \() ->
                removeDiacritics "andré JOÂO"
                    |> Expect.equal "andre JOAO"
        , test "Should produce a Polish phrase without accents" <|
            \() ->
                removeDiacritics "Cześć! Jak się masz? Śmiało usuń akcenty!"
                    |> Expect.equal "Czesc! Jak sie masz? Smialo usun akcenty!"
        ]
