module Main where

import MobilTypy

--                 d1           d2

--        |--------------|-------|

--        |                           |

--

--                  hmotnost              lavy pravy d1 d2

--data Mobil = List Int | Rameno Mobil Mobil Int Int

import qualified Vyvazeny as F
import Test.HUnit

main = do
  runTestTT $ 
    TestList [ 
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m1))
                                      True 
                                      (F.vyvazenyMobil m1)
               ,
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m2))
                                      True 
                                      (F.vyvazenyMobil m2)
               ,
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m3))
                                      True 
                                      (F.vyvazenyMobil m3)
               ,
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m4))
                                      False 
                                      (F.vyvazenyMobil m4)
                ,
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m5))
                                      False 
                                      (F.vyvazenyMobil m5)
               ,
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m6))
                                      True 
                                      (F.vyvazenyMobil m6)
               ,
               TestCase $ assertEqual ("vyvazenyMobil " ++ (show m13))
                                      True 
                                      (F.vyvazenyMobil m13)
              
             ]
        

m1 = List 10
m2 = Rameno m1 (List 5) 5 10
m3 = Rameno m2 m1 10 15   -- vyvazeny
m4 = Rameno m2 m1 15 10   -- nevyvazeny
m5 = Rameno m3 m4 3 3
m6 = Rameno (List 5) (List 10) 4 2 -- vyvazeny, nezamota
m7 = Rameno (List 2) (List 1) 2 4  -- vyvazeny, nezamota
m8 = Rameno m6 m7 1 5 -- vyvazeny, zamota
m9 = Rameno m8 m8 10 10 -- vyvazeny, zamota
m10 = Rameno (List 2) (List 4) 4 2 -- vyvazeny, nezamota
m11 = Rameno (List 1) (List 3) 3 1 -- vyvazeny, nezamota
m12 = Rameno m10 m11 4 6 -- vyvazeny, nezamota
m13 = Rameno m12 m12 7 7 -- vyvazeny, zamota    
---- riesenie tutora
vyvazenyMobil :: Mobil -> Bool
vyvazenyMobil = snd . vyvaz'

vyvaz' :: Mobil -> (Int, Bool)
vyvaz' (List h) = (h, True)
vyvaz' (Rameno m1 m2 d1 d2) = 
    (h1 + h2, ok1 && ok2 && (h1 * d1 == h2 * d2))
  where
    (h1, ok1) = vyvaz' m1
    (h2, ok2) = vyvaz' m2
