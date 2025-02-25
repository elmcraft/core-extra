module List.Extra.NotMember exposing (notMemberOriginal, notMemberSimple)


notMemberOriginal : a -> List a -> Bool
notMemberOriginal x =
    not << List.member x


notMemberSimple : a -> List a -> Bool
notMemberSimple x list =
    not (List.member x list)
