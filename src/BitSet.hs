{-# LANGUAGE FlexibleInstances #-}

module BitSet where

import           Control.DeepSeq (NFData (..))
import           Data.Bits       (clearBit, popCount, setBit, testBit, (.|.))
import           Data.Ix         (Ix (..))
import           Data.List       (intercalate)
import           Data.Semigroup  (Semigroup (..))
import           Data.Word       (Word32)

data BitSet i = BitSet (i,i) Word32

instance Eq (BitSet i) where
  (BitSet _ a) == (BitSet _ b) = a == b

-- compare :: a -> a -> Ordering
instance Ord (BitSet i) where
  compare (BitSet _ a) (BitSet _ b) = compare a b

instance NFData (BitSet i) where
  rnf (BitSet _ a) = a `seq` ()

bitSet :: Ix i => (i,i) -> BitSet i
bitSet ins
  | rangeSize ins > 32 = error "e too big"
  | otherwise = BitSet ins 0

insert :: Ix i => i -> BitSet i -> BitSet i
insert i (BitSet r x) = BitSet r $ x `setBit` index r i

null :: BitSet i -> Bool
null (BitSet _ x) = x == 0

length :: BitSet i -> Int
length (BitSet _ x) = popCount x

member :: Ix i => i -> BitSet i -> Bool
member i (BitSet r x) = testBit x (index r i)

notMember :: Ix i => i -> BitSet i -> Bool
notMember i = not . member i

delete :: Ix i => i -> BitSet i -> BitSet i
delete i (BitSet r x) = BitSet r $ x `clearBit` index r i

isSubsetOf :: BitSet i -> BitSet i -> Bool
isSubsetOf (BitSet _ a) (BitSet _ b) = b == b .|. a

toList :: Ix i => BitSet i -> [i]
toList bs@(BitSet r _) = filter (`member` bs) $ range r

instance Semigroup (BitSet i) where
  (BitSet r a) <> (BitSet _ b) = BitSet r (a .|. b)

instance (Show i, Ix i) => Show (BitSet i) where
  show bs@(BitSet r _) = "fromList " <> show r <> " [" <> intercalate ", " (map show (toList bs)) <> "]"

fromList :: Ix i => (i,i) -> [i] -> BitSet i
fromList r = foldr insert (bitSet r)
