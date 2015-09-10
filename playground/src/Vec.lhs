> {-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving, TypeFamilies #-}

Playing with vectors  
  
> module Vec where

> import GHC.TypeLits  

Natural numbers

> data Nat = Zero | Suc Nat          


Vector definition

> data Vec (a :: *)(n :: Nat) where
>     VNil  :: Vec a Zero
>     VCons :: a -> Vec a n -> Vec a (Suc n)        


> infixr 5 `VCons`    

A little test

> type Three = Suc (Suc (Suc Zero))
  
> vabc :: Vec Char Three
> vabc = 'a' `VCons` 'b' `VCons` 'c' `VCons` VNil

Deriving show instance
  
> deriving instance Show a => Show (Vec a n)        

Tail for vectors

> vtail :: Vec a (Suc n) -> Vec a n
> vtail (VCons _ xs) = xs         

Map for vectors

> vmap :: (a -> b) -> Vec a n -> Vec b n
> vmap _ VNil = VNil
> vmap f (VCons x xs) = f x `VCons` vmap f xs              

> vapp :: Vec (a -> b) n -> Vec a n -> Vec b n
> vapp VNil VNil = VNil
> vapp (VCons f fs) (VCons x xs) = VCons (f x) (vapp fs xs)                 

Replicate, take one

> class VReplicate (n :: Nat) where
>    vreplicate :: a -> Vec a n

> instance VReplicate Zero where
>    vreplicate _ = VNil

> instance VReplicate n => VReplicate (Suc n) where
>    vreplicate x = x `VCons` vreplicate x

Replicate, take two

> data SNat (n :: Nat) where
>    SZero :: SNat Zero
>    SSuc  :: SNat n -> SNat (Suc n)

> vreplicate1 :: a -> SNat n -> Vec a n
> vreplicate1 x SZero = VNil
> vreplicate1 x (SSuc n) = x `VCons` vreplicate1 x n
     

Vector concatenation

> type family Plus (a :: Nat) (b :: Nat) :: Nat

> type instance Plus Zero m = m
> type instance Plus (Suc n) m = Suc (Plus n m)

> vappend :: Vec a n -> Vec a m -> Vec a (Plus n m)
> vappend VNil m = m
> vappend (VCons x xs) m = x `VCons` vappend xs m                 

