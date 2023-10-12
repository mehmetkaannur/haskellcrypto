module Crypto ( gcd, smallestCoPrimeOf, phi, computeCoeffs, inverse
              , modPow, genKeys, rsaEncrypt, rsaDecrypt, toInt, toChar
              , add, subtract, ecbEncrypt, ecbDecrypt
              , cbcEncrypt, cbcDecrypt ) where

import Data.Char

import Prelude hiding (gcd, subtract)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

-- | Returns the greatest common divisor of its two arguments
gcd :: Int -> Int -> Int
gcd m n
  | n == 0    = m
  | otherwise = gcd n (m `mod` n)

-- | Euler Totient function
phi :: Int -> Int
phi m = length [c | c <- [1..m], gcd m c == 1]

{-|
Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
such that au + bv = d
-}
computeCoeffs :: Int -> Int -> (Int, Int)
computeCoeffs a b
  | b == 0    = (1, 0)
  | otherwise = (v, u - q*v)
  where
    (q, r) = quotRem a b
    (u, v) = computeCoeffs b r

-- | Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m = u `mod` m
  where
    u = fst (computeCoeffs a m)

-- | Calculates (a^k mod m)
modPow :: Int -> Int -> Int -> Int
modPow a k m
  | k == 0    = 1 `mod` m
  | even k    = modPow ((a^2) `mod` m) j m `mod` m
  | otherwise = a * modPow a (k-1) m `mod` m
  where
    j = k `div` 2

-- | Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf a = coPrime 2
  where coPrime b
         | gcd b a == 1 = b
         | otherwise    = coPrime (b+1)

{-|
Generates keys pairs (public, private) = ((e, n), (d, n))
given two "large" distinct primes, p and q
-}
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q = ((e, n), (d, n))
  where
    n = p * q
    e = smallestCoPrimeOf ((p-1) * (q-1))
    d = inverse e ((p-1)*(q-1))

-- | This function performs RSA encryption
rsaEncrypt :: Int        -- ^ value to encrypt
           -> (Int, Int) -- ^ public key
           -> Int
rsaEncrypt x (e, n) = modPow x e n

-- | This function performs RSA decryption
rsaDecrypt :: Int        -- ^ value to decrypt
           -> (Int, Int) -- ^ public key
           -> Int
rsaDecrypt c (d, n) = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- | Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt a = ord a - 97

-- | Returns the n^th letter
toChar :: Int -> Char
toChar a = chr (a + 97)

-- | "adds" two letters
add :: Char -> Char -> Char
add a b = toChar ((toInt a + toInt b) `mod` 26)

-- | "subtracts" two letters
subtract :: Char -> Char -> Char
subtract a b = toChar ((toInt a - toInt b) `mod` 26)

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- | ecb (electronic codebook) encryption with block size of a letter
ecbEncrypt :: Char -> [Char] -> [Char]
ecbEncrypt _ []     = []
ecbEncrypt k (m:ms) = add m k : ecbEncrypt k ms

-- | ecb (electronic codebook) decryption with a block size of a letter
ecbDecrypt :: Char -> [Char] -> [Char]
ecbDecrypt _ []     = []
ecbDecrypt k (m:ms) = subtract m k : ecbDecrypt k ms

-- | cbc (cipherblock chaining) encryption with block size of a letter
cbcEncrypt :: Char   -- ^ public key
           -> Char   -- ^ initialisation vector `iv`
           -> [Char] -- ^ message `m`
           -> [Char]
cbcEncrypt _ _ []     = []
cbcEncrypt k v (m:ms) = a : cbcEncrypt k a ms
  where a = add (add v m) k


-- | cbc (cipherblock chaining) decryption with block size of a letter
cbcDecrypt :: Char   -- ^ private key
           -> Char   -- ^ initialisation vector `iv`
           -> [Char] -- ^ message `m`
           -> [Char]
cbcDecrypt _ _ []     = []
cbcDecrypt k v (c:cs) = a : cbcDecrypt k c cs
  where
    a = subtract (subtract c k) v