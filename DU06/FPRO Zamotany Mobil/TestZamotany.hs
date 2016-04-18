module Main where

import MobilTypy

--                 d1           d2

--        |--------------|-------|

--        |                           |

--

--                  hmotnost              lavy pravy d1 d2

--data Mobil = List Int | Rameno Mobil Mobil Int Int

import qualified Zamotany as F
import Test.HUnit

main = do
  runTestTT $ 
    TestList [ 
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m1))
                                      False 
                                      (F.zamotanyMobil m1)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m2))
                                      False
                                      (F.zamotanyMobil m2)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m3))
                                      False 
                                      (F.zamotanyMobil m3)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m4))
                                      True 
                                      (F.zamotanyMobil m4)
                ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m5))
                                      True 
                                      (F.zamotanyMobil m5)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m6))
                                      False 
                                      (F.zamotanyMobil m6)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m7))
                                      False 
                                      (F.zamotanyMobil m7)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m8))
                                      True 
                                      (F.zamotanyMobil m8)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m9))
                                      True 
                                      (F.zamotanyMobil m9)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m10))
                                      False 
                                      (F.zamotanyMobil m10)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m11))
                                      False 
                                      (F.zamotanyMobil m11)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m12))
                                      False 
                                      (F.zamotanyMobil m12)
               ,
               TestCase $ assertEqual ("zamotanyMobil " ++ (show m13))
                                      True 
                                      (F.zamotanyMobil m13)
              
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

-- trik: vysledok 0 znamena nevyvazeny
vyvazenyMobil :: Mobil -> Bool
vyvazenyMobil = (==0) . vyvazenyMobil' 

vyvazenyMobil' :: Mobil -> Int
vyvazenyMobil' (List h) = h
vyvazenyMobil' (Rameno m1 m2 d1 d2) = if h1 * d1 == h2 * d2 then h1 + h2 else 0
  where
    h1 = vyvazenyMobil' m1
    h2 = vyvazenyMobil' m2
    
zamotanyMobil :: Mobil -> Bool
zamotanyMobil = not . nezamota

nezamota :: Mobil -> Bool
nezamota m = vyvazenyMobil m  &&  nezamota' m

nezamota' :: Mobil -> Bool
nezamota' (List _) =  True
nezamota' (Rameno (List _) (List _) _ _ ) = True
nezamota' (Rameno (List _) (Rameno m1 m2 d1 d2) r1 r2) = 
  max d1 d2 < r1 + r2 && nezamota' (Rameno m1 m2 d1 d2)
nezamota' (Rameno (Rameno m1 m2 d1 d2) (List _) r1 r2) = 
  max d1 d2 < r1 + r2 && nezamota' (Rameno m1 m2 d1 d2)
nezamota' (Rameno (Rameno m11 m12 d11 d12) (Rameno m21 m22 d21 d22) r1 r2) = 
  max d11 d12 + max d21 d22 < r1 + r2 && 
  nezamota' (Rameno m11 m12 d11 d12) &&
  nezamota' (Rameno m21 m22 d21 d22) &&
  nezamota' (Rameno m12 m21 0 (r1 + r2 - d12 - d21)) && -- 4 fiktivne ramena medzi pravym a lavym mobilom
  nezamota' (Rameno m12 m22 0 (r1 + r2 - d12 - d22)) &&
  nezamota' (Rameno m11 m21 0 (r1 + r2 - d11 - d21)) &&  
  nezamota' (Rameno m11 m22 0 (r1 + r2 - d11 - d22)) 