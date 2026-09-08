-- | Events and their payloads: the alphabet over which temporal specifications
-- ('RE', 'GuardedRE', 'WRE', …) are written.
module Pledge.Event
    ( Term(..)
    , Event(..)
    , subsumesEvent
    ) where

import Data.List (intercalate)

-- | A structured payload carried by an 'Event'.
data Term
    = Var  String   -- ^ a variable, referenced by name
    | Num  Int      -- ^ an integer
    | List [Term]   -- ^ a tuple of payloads
    deriving (Eq)

instance Show Term where
    show (Var s)   = s
    show (Num n)   = show n
    show (List ts) = "[" ++ intercalate ", " (map show ts) ++ "]"

-- | A single event, or a pattern matching a set of events, over payload type @t@.
data Event t
    = Atom String t  -- ^ the named event @name(arg)@
    | Not String t   -- ^ any event other than @name(arg)@
    | Wildcard       -- ^ any event
    | NotUse t       -- ^ any named event whose payload is not @arg@
    deriving (Eq)

instance Show t => Show (Event t) where
    show (Atom name arg) = name ++ "(" ++ show arg ++ ")"
    show Wildcard        = "_"
    show (Not name arg)  = "¬" ++ name ++ "(" ++ show arg ++ ")"
    show (NotUse arg)    = "¬_(" ++ show arg ++ ")"

-- | @subsumesEvent e p@: does event @e@ match pattern @p@?
subsumesEvent :: Eq t => Event t -> Event t -> Bool
subsumesEvent _            Wildcard     = True
subsumesEvent (Atom n1 a1) (Atom n2 a2) = n1 == n2 && a1 == a2
subsumesEvent (Atom n1 a1) (Not n2 a2)  = not (n1 == n2 && a1 == a2)
subsumesEvent (Atom _  a1) (NotUse a2)  = a1 /= a2
subsumesEvent (Not n1 a1)  (Not n2 a2)  = n1 == n2 && a1 == a2
subsumesEvent (NotUse a1)  (NotUse a2)  = a1 == a2
subsumesEvent _            _            = False