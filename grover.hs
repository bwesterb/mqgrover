module Grover (grover) where

import Quipper

import Control.Monad

-- |Reflects (in place) over the standard uniform superposition of qubits.
-- This is used as the second part of the Grover iteration.
reflect_over_a :: [Qubit] -> Circ ()
reflect_over_a qs = do
  with_basis_change (forM_ qs hadamard_at) $ do
    with_basis_change (forM_ qs qnot_at) $ do
      with_basis_change (hadamard_at $ head qs) $ do
        qnot_at (head qs) `controlled` tail qs

-- |Grover's algorithm.  'oracle' is a circuit that should map exactly
-- 'm' 'n'-qubit-words to |1> and the others to |0>.  'grover' returns
-- a circuit that creates a superposition over all 'n'-qubit words that
-- are mapped to |1> by the oracle.
grover :: ([Qubit] -> Circ (Qubit)) -> Int -> Int -> Circ ()
grover oracle n m = do
  -- Create a standard uniform superposition over all qubits.
  qs <- qinit $ replicate n False
  forM_ qs hadamard_at
  iterations qs
  return ()
    where
      n_iters = floor $ pi / 4 / asin(sqrt((fromIntegral m) / (2^n)))
      iterations :: [Qubit] -> Circ ([Qubit])
      iterations = nbox "grover-iteration" n_iters $ \qs -> do
        -- First, the adapted oracle.  The following will send a computational
        -- basis vector w to -w if its tagged by the original oracle and
        -- leave it in place otherwise.
        with_computed (oracle qs) $ \r -> do
          gate_Z_at (head qs) `controlled` r
        -- Then, reflect over the standard uniform superposition.
        reflect_over_a qs
        return qs
