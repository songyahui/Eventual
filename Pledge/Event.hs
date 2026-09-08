-- | Events and the pattern language matched against them.
module Pledge.Event
    ( Event(..)
    , subsumesEvent
    ) where

data Event t
    = Atom String t
    | Not String t
    | Wildcard
    | NotUse t
    deriving (Eq)

instance Show t => Show (Event t) where
    show (Atom name arg) = name ++ "(" ++ show arg ++ ")"
    show Wildcard        = "_"
    show (Not name arg)  = "¬" ++ name ++ "(" ++ show arg ++ ")"
    show (NotUse arg)    = "¬_(" ++ show arg ++ ")"

-- | Does the concrete event @e@ match the pattern @p@?
subsumesEvent :: Eq t => Event t -> Event t -> Bool
subsumesEvent _            Wildcard     = True
subsumesEvent (Atom n1 a1) (Atom n2 a2) = n1 == n2 && a1 == a2
subsumesEvent (Atom n1 a1) (Not n2 a2)  = not (n1 == n2 && a1 == a2)
subsumesEvent (Atom _  a1) (NotUse a2)  = a1 /= a2
subsumesEvent (Not n1 a1)  (Not n2 a2)  = n1 == n2 && a1 == a2
subsumesEvent (NotUse a1)  (NotUse a2)  = a1 == a2
subsumesEvent _            _            = False
