-- Generates pictures for the note
--
import Quipper
import System.Environment

import MQGrover

genpic "1" = print_generic PDF (oracle1
                                  [[[True, True, False],
                                    [False, True],
                                    [False]],
                                   [[False, False, False],
                                     [True, True],
                                     [False]]]
                  ) [qubit, qubit, qubit]

genpic "2" = print_generic PDF (oracle2
                                  [[[True, True, False],
                                    [False, True],
                                    [False]],
                                   [[False, False, False],
                                     [True, True],
                                     [False]]]
                  ) [qubit, qubit, qubit]

main = do
  args <- getArgs
  genpic $ head args
