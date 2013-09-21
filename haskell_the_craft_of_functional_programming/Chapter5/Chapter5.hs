module Chapter5 where
import Prelude
import Test.QuickCheck

straightLineIntersection :: (Int, Int) -> (Int, Bool)
straightLineIntersection (a, 0) = (0, True)
straightLineIntersection (0, b) = (0, False)
straightLineIntersection ((0-b)/a, True)

data People = Person Name Age
              deriving (Eq, Show)
type Name = String
type Age = Int

data Shape = Circle Float |
             Rectangle Float Float
             deriving(Eq, Show, Ord)
