module MQGrover (oracle1, oracle2) where

import Quipper
import Data.Bits
import Data.List
import Control.Monad
import Control.Applicative

--
-- First oracle
--

-- Compute y_i^(k) from the coefficients lambda_ii^(k), ..., lambda_in^(k)
-- and the assignment x_1, ..., x_n.
compute_y :: [Bool] -> [Qubit] -> Circ (Qubit)
compute_y cs xs = withM (qinit False) $ \t ->
  unless (null cs) $ do
    when (head cs) $ qnot_at t
    zipWithM_ (\c x ->  when c (qnot_at t `controlled` x)) (tail cs) (tail xs)

-- Computes E^(k) from the "triangle" lambda^(k)_ij (1 <= j <= n)
-- and the assignment x_1, ..., x_n.
compute_e :: [Qubit] -> [[Bool]] -> Circ (Qubit)
compute_e xs css =
  withM (qinit False) $ \e ->
    forM_ (zip css (init $ tails xs))  $ \(cs, xs') ->
      with_computed (compute_y cs xs') $ \t ->
        qnot_at e `controlled` (t, head xs')

-- The first (straight-forward) oracle.  Computes whether the
-- assignment x_1, ..., x_n satisfies the given system of equations.
oracle1 :: [[[Bool]]] -> [Qubit] -> Circ (Qubit)
oracle1 csss xs =
  withM (qinit False) $ \r -> do
    label (r:xs) ("r":["x" ++ (show i) | i <- [1..length xs]])
    es <- mapM (compute_e xs) csss
    label es ["E" ++ (show i) | i <- [1..length es]]
    qnot_at r `controlled` es

--
-- Second oracle that uses only n + ceil(log_2 m) + 3 registers, but requires
-- more gates.
--

oracle2 :: [[[Bool]]] -> [Qubit] -> Circ (Qubit)
oracle2 csss xs = do
  ctr <- init_counter $ length csss
  label xs ["x" ++ (show i) | i <- [1..length xs]]
  forM_ csss $ \css ->
    with_computed (compute_e xs css) $ controlled $ inc_counter ctr
  check_counter ctr

--
-- Helpers for the second oracle.
--

-- Rotates qubits around one turn
qrotate :: [Qubit] -> Circ()
qrotate qs = zipWithM_ swap qs' $ tail qs' where qs' = reverse qs

-- Turns polynomial into corresponding circuit
apply_polynomial :: [Bool] -> [Qubit] -> Circ()
apply_polynomial cs qs = do
  qrotate qs
  zipWithM_ (\c q -> do
        when c $ qnot_at q `controlled` head qs) (init $ tail cs) (tail qs) 
  return ()

-- Returns number of bits in the binary expansion of a given integer
bits_required :: Int -> Int
bits_required 0 = 0
bits_required n = 1 + bits_required (n `shiftR` 1)

type QCounter = (Int, [Qubit])
bound :: QCounter -> Int
bound (n, qs) = n
qbits :: QCounter -> [Qubit]
qbits (n, qs) = qs

-- Creates a new counter to count up to the given integer.
init_counter :: Int -> Circ(QCounter)
init_counter n = do
  qs <- qinit $ iterate (class_inc_counter zero_poly) (replicate nbits True)
                !!  (2^nbits - n)
  return (n, qs)
    where
      nbits = bits_required n
      zero_poly = prim_poly nbits
      class_inc_counter :: [Bool] -> [Bool] -> [Bool]
      class_inc_counter zero_poly (False:cs) = cs ++ [False]
      class_inc_counter zero_poly (True:cs) 
        = zipWith xor (cs ++ [False]) (tail zero_poly)

-- Increment the counter by one.
inc_counter :: QCounter -> Circ ()
inc_counter (n,qs) = prim_poly (bits_required n) `apply_polynomial` qs

-- Check whether the counter has reached the desired value
check_counter :: QCounter -> Circ (Qubit)
check_counter qc = withM (qinit False) $ \t -> qnot_at t `controlled` qbits qc
  
-- Returns a primitive polynomial over F_2 of given degree
prim_poly :: Int -> [Bool]
prim_poly n = map (`elem` 0:n:(watson_prim_polies!!n)) [0..n]

-- Contains for each n <= 32  a primitive polynomial of order n modulo F_2.
-- The number are the non-trivial powers of x that occur: for instance
-- the list [4,3,2] at index 8 represents the primitive polynomial
--      x^8 + x^4 + x^3 + x^2 + 1.
-- List taken from E. J. Watson, 1961.
watson_prim_polies = [
  [], [], [1], [1], [1], [2], [1], [1], [4, 3, 2], [4], [3], [2], [6,
  4, 1], [4, 3, 1], [5, 3, 1], [1], [5, 3, 2], [3], [5, 2, 1], [5,
  2, 1], [3], [2], [1], [5], [4, 3, 1], [3], [6, 2, 1], [5, 2, 1],
  [3], [2], [6, 4, 1], [3], [7, 5, 3, 2]]

withM :: Monad m =>  m a -> (a -> m ()) -> m a
withM f g = do
  t <- f
  g t
  return t
