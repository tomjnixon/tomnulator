
import Stump

import System( getArgs )
import Data.IntMap

main = do
	args <- getArgs
	let file_name = last args
	
	mem <- read_mem file_name
	let initial_state = state_from_mem mem
	
	run_sim initial_state 0

-- Tests for test 1.
test_flags a b c d = (\s -> (flags s) == Flags a b c d)
conditions = fromList [ (79, test_flags True False False False)
                      , (85, test_flags False False True True)
                      , (91, test_flags True False True False)
                      , (94, test_flags False True False False)
                      , (100, test_flags True False False False)
                      , (106, test_flags False False True False)
                      , (112, test_flags True False False True)
                      , (118, test_flags True False False False)
                      , (124, test_flags False False False True)
                      ]


run_sim state time = do
	let clock = time * 3 + 1
	putStrLn $ "Time: " ++ (show time) ++ " (" ++ (show clock) ++ ")"
	if clock `member` conditions 
		then
			if (conditions ! clock) state 
				then
					putStrLn "##### Test passed!"
				else
					putStrLn "##### Test FAILED!"
		else
			return ()
	print state
	putStrLn ""
	getLine
	run_sim (step state) (time + 1)
