module Staged3(main) where
import Staged
import StagedLib

fourth :: Int -> Int
fourth x = ~(power 4 [| x |])

sumSq :: Int
sumSq = ~(unrollSum 4 [| \ i -> i * i |])

dot :: Int -> Int -> Int -> Int
dot a b c = ~(vsum (vmap (\ x -> [| ~x * 2 |]) (VCons [| a |] (VCons [| b |] (VCons [| c |] VNil)))))

main :: IO ()
main = do
  print (fourth 3)
  print sumSq
  print (dot 1 2 3)
