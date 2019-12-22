{-# LANGUAGE FlexibleInstances #-}

module BitSet where

import           Control.DeepSeq (NFData (..))
import           Data.Bits       (Bits (..), clearBit, popCount, setBit,
                                  testBit, (.|.))
import           Data.Ix         (Ix (..))
import           Data.List       (intercalate)
import           Data.Semigroup  (Semigroup (..))

data BitSet i w = BitSet (i,i) w

instance Eq w => Eq (BitSet i w) where
  (BitSet _ a) == (BitSet _ b) = a == b

-- compare :: a -> a -> Ordering
instance Ord w => Ord (BitSet i w) where
  compare (BitSet _ a) (BitSet _ b) = compare a b

instance NFData (BitSet i w) where
  rnf (BitSet _ a) = a `seq` ()

bitSet :: (Bits w, Ix i) => (i,i) -> BitSet i w
bitSet ins
  | rangeSize ins > 32 = error "e too big"
  | otherwise = BitSet ins zeroBits

insert :: (Bits w, Ix i) => i -> BitSet i w -> BitSet i w
insert i (BitSet r x) = BitSet r $ x `setBit` index r i

null :: Bits w => BitSet i w -> Bool
null = (== 0) . BitSet.length

length :: Bits w => BitSet i w -> Int
length (BitSet _ x) = popCount x

member :: (Bits w, Ix i) => i -> BitSet i w -> Bool
member i (BitSet r x) = testBit x (index r i)

notMember :: (Bits w, Ix i) => i -> BitSet i w -> Bool
notMember i = not . member i

delete :: (Bits w, Ix i) => i -> BitSet i w -> BitSet i w
delete i (BitSet r x) = BitSet r $ x `clearBit` index r i

isSubsetOf :: Bits w => BitSet i w -> BitSet i w -> Bool
isSubsetOf (BitSet _ a) (BitSet _ b) = b == b .|. a

toList :: (Bits w, Ix i) => BitSet i w -> [i]
toList bs@(BitSet r _) = filter (`member` bs) $ range r

instance Bits w => Semigroup (BitSet i w) where
  (BitSet r a) <> (BitSet _ b) = BitSet r (a .|. b)

instance (Bits w, Show i, Ix i) => Show (BitSet i w) where
  show bs@(BitSet r _) = "fromList " <> show r <> " [" <> intercalate ", " (map show (toList bs)) <> "]"

fromList :: (Bits w, Ix i) => (i,i) -> [i] -> BitSet i w
fromList r = foldr insert (bitSet r)