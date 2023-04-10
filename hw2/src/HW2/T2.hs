module HW2.T2 where
import HW2.T1 (Option (..),
               Pair (..),
               Quad (..),
               Annotated (..),
               Except (..),
               Stream (..),
               List (..),
               Fun (..),
               Prioritised (..))


distOption      :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

distPair        :: (Pair a, Pair b) -> Pair (a, b)
distPair ((P l1 r1), (P l2 r2)) = P (l1, l2) (r1, r2)

distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distQuad ((Q l1 r1 a1 b1), (Q l2 r2 a2 b2)) = Q (l1, l2) (r1, r2) (a1, a2) (b1, b2)

distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated ((a :# e1), (b :# e2)) = (a, b) :# (e1 <> e2)

distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised ((High a), (High b)) = High (a, b)
distPrioritised ((High a), (Medium b)) = High (a, b)
distPrioritised ((High a), (Low b)) = High (a, b)
distPrioritised ((Medium a), (High b)) = High (a, b)
distPrioritised ((Medium a), (Medium b)) = Medium (a, b)
distPrioritised ((Medium a), (Low b)) = Medium (a, b)
distPrioritised ((Low a), (High b)) = High (a, b)
distPrioritised ((Low a), (Medium b)) = Medium (a, b)
distPrioritised ((Low a), (Low b)) = Low (a, b)

distStream      :: (Stream a, Stream b) -> Stream (a, b)
distStream ((a :> stream1), (b :> stream2)) = (a, b) :> distStream (stream1, stream2)

concat' :: a -> List b -> List (a, b)
concat' a Nil = Nil
concat' a (b :. tail2) = (a, b) :. (concat' a tail2)

concat2 :: List a -> List a -> List a
concat2 Nil Nil = Nil
concat2 Nil (a :. tail) = a :. tail
concat2 (a :. tail1) Nil = a :. tail1
concat2 (a :. tail1) (b :. tail2) = a :. (concat2 tail1 (b :. tail2))

distList        :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList ((a :. tail1), (b :. tail2)) = concat2 (concat' a (b :. tail2)) (distList (tail1, (b :. tail2)))


distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun ((F f1), (F f2)) = F (\i -> (f1 i, f2 i))




wrapOption      :: a -> Option a
wrapOption a = Some a

wrapPair        :: a -> Pair a
wrapPair a = P a a

wrapQuad        :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept      :: a -> Except e a
wrapExcept a = Success a

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a

wrapStream      :: a -> Stream a
wrapStream a = a :> (wrapStream a)

wrapList        :: a -> List a
wrapList a = a :. Nil

wrapFun         :: a -> Fun i a
wrapFun a = F (\x -> a)
