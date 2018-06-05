module Ring (
	RingMod,
	ringMod,
	ginv,
	rmul,
	runit
	) where
class Monoid a => Group a where
	ginv ::   a -> a
    
class Group a => Ring a where
	rmul :: a -> a -> a
	runit :: a

data RingMod = RingMod Int Int | Nothing' deriving (Show, Eq)
ringMod :: Int -> Int -> RingMod
ringMod a m = RingMod (mod a m)  m
	
instance Monoid RingMod where
	mempty = RingMod 1 1
	mappend (RingMod a m) (RingMod b m') | m==m' = RingMod (mod(a+b) m) m
					     | m==1 = RingMod (mod b m') m'
					     | m'==1 = RingMod (mod a m) m
					     | otherwise = Nothing'
instance Group RingMod where
	ginv (RingMod a m) = RingMod (mod(a - m) m) m

instance Ring RingMod where
	runit = RingMod 1 2
	rmul (RingMod a m) (RingMod b m') | m==m' = RingMod (mod(a * b) m) m
	                                  | a==1 && m==2 = RingMod (mod(a * b) m') m'
	                                  | b==1 && m'==2 = RingMod (mod(a * b) m) m
	                                  | otherwise = Nothing'
    

	
