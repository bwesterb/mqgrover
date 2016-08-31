-- Generate upper bounds used in the paper

import Data.List

import Quipper

import MQGrover
import Grover

main = do
  putStrLn "-----------------------------------------------------------"
  putStrLn "First oracle"
  putStrLn "-----------------------------------------------------------"
  print_simple GateCount $ grover (oracle1 sqe) n 1
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "-----------------------------------------------------------"
  putStrLn "Second oracle"
  putStrLn "-----------------------------------------------------------"
  print_simple GateCount $ grover (oracle2 sqe) n 1
  
    where
      n = 81
      m = 85
      sqe = replicate m $ reverse $ tail $ inits $ replicate n True
  -- print_generic Preview (oracle1
  --                                 [[[True, True, False],
  --                                   [False, True],
  --                                   [False]],
  --                                  [[False, False, False],
  --                                    [True, True],
  --                                    [False]]]
  --                 ) [qubit, qubit, qubit]
  -- print_generic Preview (oracle2
  --                                 [[[True, True, False],
  --                                   [False, True],
  --                                   [False]],
  --                                  [[False, False, False],
  --                                    [True, True],
  --                                    [False]]]
  --                 ) [qubit, qubit, qubit]
