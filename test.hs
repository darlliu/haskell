import Data.List (isPrefixOf)
inc :: Int -> Int
inc 0 = 0
inc x = (inc (x-1))+x


--filter2::Ord a=>(a-> Bool) ->[a]->[a]
filter2 p = foldr (\x acc -> if p x then x :acc else acc ) 1


fib :: Int -> Int
fib 0=0
fib 1=1
fib x=fib(x-1)+fib(x-2)

term :: Int -> Double
term 0=0 
term 1=1
term 2=2
term (-1)=1
term (-2)=2
term x=if x<(-2) then term(x+1)* term (x+2) 
    else if (x>2) then term(x-1)*term(x-2) 
    else 0
termfib x = (term . fib)

dlts= foldr step [] . lines
    where step l ds
            |"#define" `isPrefixOf` l = secondWord l:ds
            |otherwise                = ds
          secondWord= head. tail. words

myfun 0=0
myfun 1=1
myfun 2=2
myfun x=if x>0 then myfun(x-1)^myfun(x-2)
    else 0
mylist []=[]
mylist (x:xs)=not(head xs):mylist(tail xs)

mylist3 Nope=Nope
mylist3 (Add x xs)=xs

myreduc Nope = 0
myreduc (Add x y)=x + myreduc (y)
mylist2 []=[]
mylist2 xs=d++mylist2(tail xs)
    where d=mylist xs

mymerge xs =foldl step [] xs
    where step x ys = ys:x

mymap f xs= foldr step [] xs
    where step x ys = x `seq` f x:ys
mymap2 f xs= foldr step [] xs
    where step x ys = f x:ys
mycomp = (odd `mymap`) . mymerge 
mycomp2 = mymerge . (odd `mymap`)  
mycomp3 = length.mycomp2
--note : composite operator is right evaluated.
myfoldadd xs=foldl (\s t->s `seq` t `seq` s*t) 1 xs
myfoldadd2 xs=foldl (\s t-> s*t) 1 xs

class MyClass a where
    eql::a->a->Bool
    add::a->a->a
    fibz::a->a
instance MyClass Bool where
    eql True True = True
    eql False False = True
    eql _ _ = False
    add True False = True
    add False True = True
    add True True = True
    add _ _ = False
    fibz True = True
    fibz False = False

instance MyClass Int where
    eql x y = x==y
    add x y = x+y
    fibz x = fib x
        --deriving (Eq, Ord, Show)

data Tryouts = Try1 {
    tries1::String,
    tries2::String}
    |Try2 Int String
    |Try3 Int Int3
    deriving (Show)

instance MyClass Tryouts where
    eql x y = tries1 x==tries1 y
    add x y = Try1 (tries1 x) (tries2 y)
    fibz x = Try1 (tries1 x++tries2 x) (tries2 x ++ tries1 x)

data Flags=Flag1 Int Float | Flag2 Float Int
           deriving (Show)


data Marks=Mark1 Int [Int] 
    | Mark2 [Int] Int
    | Mark3 [Int] [Int]
    | Mark4 [Float] [Int]
           deriving (Show)

-- closure
--foo :: Num -> Num -> (Num -> Num)
--foo x y = let r = x / y
--          in (\z -> z + r)
--let binds r to the lambda which contains another argument z 
--f :: Num -> Num
--f = foo 1 0
--here invokes with x y
--foo is the closure, f is the function
--main = print (f 123)
--here invokes with z


data MyMaybe a = MyJust a
    |Nope2
    deriving(Show)
data Templated a b c=Templated a b c
    |Templated2 {
        geta::a, 
        getb::b, 
        getc::c}
    |Templated3 a String c
    deriving (Show)
data Mylist a =Add a (Mylist a)
    | Nope
    deriving (Show)
type Banners=(Flags,Marks)
type Int3=Int
myadd 0 _ = 1
myadd _ 0 = 1
myadd a b 
 | a<0||b<0   = if b==0 then 0 else 0
 | otherwise  = a+b+ (myadd (a-1) (b-1))
derp=herp
    where {
        herp=kerp
            where {
                kerp=1;
                herp=2
            };
        derp=1;
        derp2=case derp of{
            1 -> 1;
            2 -> 0}
    }

mysum xs
    | xs==[]    =1
mysum xs = length xs + mysum(tail(xs))

ran1 :: Int->Int->Int
ran1 times seed
    |times==1    =seed
    |times>1     =(1122345223*(ran1 newtimes seed)+12345) `mod` 54321
    where newtimes=times-1
main=print (term 35 , fib 35)

