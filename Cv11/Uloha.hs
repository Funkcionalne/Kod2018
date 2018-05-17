module Uloha where

import Mparser

data LExp = LAMBDA String LExp | ID String | APL LExp LExp deriving (Show)


lambda    :: Parser LExp
lambda = do {  char '\\'
             ; id <- identifier
             ; char '.'
             ; exp <- lambda
             ; return (LAMBDA id exp)
         } +++
         do {  char '('
             ; e1 <- lambda
             ; char ' '
             ; e2 <- lambda
             ; char ')'
             ; return (APL e1 e2)
         } +++
         do {  id <- identifier; return (ID id) }

palindrom    :: Parser String
palindrom = foldr (\p -> \ps -> p +++ ps) 
                  (return "")
                  (
                   [ do { char i; xs <- palindrom; char i; return ([i] ++ xs ++ [i]) } | i <- ['0'..'9']]
                    ++
                   [ do {c <- digit; return [c]} ]
                  )

morseCodes = [(".-","A"), ("-...", "B"), ("-.-.","C"), ("-..","D"), (".","E"), ("..-.","F"),
              ("--.","G"), ("....","H"), ("..","I"), (".---","J"), ("-.-","K"), (".-..","L"), 
              ("--","M"), ("-.","N"), ("---","O"), (".--.","P"), ("--.-","Q"), (".-.","R"),
              ("...","S"), ("-","T"), ("..-","U"), ("...-","V"), (".--","W"), ("-..-","X"),
              ("-.--","Y"), ("--..","Z")
           -- ,("-----","0"), (".----","1"), ("..---","2"), ("...--","3"), ("....-","4"),
           -- (".....","5"), ("-....","6"), ("--...","7"), ("---..","8"), ("----.","9")
             ]
morse    :: Parser String
morse =  foldr (\p -> \ps -> p `plus` ps) 
               (return "")
               [ do { string' code; xs <- morse; return (chr ++ xs)} | (code, chr) <- morseCodes]

just :: [(a,[b])] -> [(a,[b])]
just = filter (null . snd)

{-
 xs = [("EEAGEE",""),("EEAGI",""),("EEAMEEE",""),("EEAMEI",""),("EEAMIE",""),
         ("EEAMS",""),("EEATB",""),("EEATDE",""),("EEATNEE",""),("EEATNI",""),
         ("EEATTEEE",""),("EEATTEI",""),("EEATTIE",""),("EEATTS",""),("EEAZE",""),
         ("EEEMB",""),("EEEMDE",""),("EEEMNEE",""),("EEEMNI",""),("EEEMTEEE",""),
         ("EEEMTEI",""),("EEEMTIE",""),("EEEMTS",""),("EEEOEEE",""),("EEEOEI",""),
         ("EEEOIE",""),("EEEOS",""),("EEETGEE",""),("EEETGI",""),("EEETMEEE",""),
         ("EEETMEI",""),("EEETMIE",""),("EEETMS",""),("EEETTB",""),("EEETTDE",""),
         ("EEETTNEE",""),("EEETTNI",""),("EEETTTEEE",""),("EEETTTEI",""),
         ("EEETTTIE",""),("EEETTTS",""),("EEETZE",""),("EEJEEE",""),("EEJEI",""),
         ("EEJIE",""),("EEJS",""),("EEWB",""),("EEWDE",""),("EEWNEE",""),("EEWNI",""),
         ("EEWTEEE",""),("EEWTEI",""),("EEWTIE",""),("EEWTS",""),("EIMB",""),("EIMDE",""),
         ("EIMNEE",""),("EIMNI",""),("EIMTEEE",""),("EIMTEI",""),("EIMTIE",""),
         ("EIMTS",""),("EIOEEE",""),("EIOEI",""),("EIOIE",""),("EIOS",""),("EITGEE",""),
         ("EITGI",""),("EITMEEE",""),("EITMEI",""),("EITMIE",""),("EITMS",""),("EITTB",""),
         ("EITTDE",""),("EITTNEE",""),("EITTNI",""),("EITTTEEE",""),("EITTTEI",""),
         ("EITTTIE",""),("EITTTS",""),("EITZE",""),("EUGEE",""),("EUGI",""),
         ("EUMEEE",""),("EUMEI",""),("EUMIE",""),("EUMS",""),("EUTB",""),("EUTDE",""),
         ("EUTNEE",""),("EUTNI",""),("EUTTEEE",""),("EUTTEI",""),("EUTTIE",""),
         ("EUTTS",""),("EUZE",""),("IAGEE",""),("IAGI",""),("IAMEEE",""),("IAMEI",""),
         ("IAMIE",""),("IAMS",""),("IATB",""),("IATDE",""),("IATNEE",""),("IATNI",""),
         ("IATTEEE",""),("IATTEI",""),("IATTIE",""),("IATTS",""),("IAZE",""),
         ("IEMB",""),("IEMDE",""),("IEMNEE",""),("IEMNI",""),("IEMTEEE",""),
         ("IEMTEI",""),("IEMTIE",""),("IEMTS",""),("IEOEEE",""),("IEOEI",""),
         ("IEOIE",""),("IEOS",""),("IETGEE",""),("IETGI",""),("IETMEEE",""),
         ("IETMEI",""),("IETMIE",""),("IETMS",""),("IETTB",""),("IETTDE",""),
         ("IETTNEE",""),("IETTNI",""),("IETTTEEE",""),("IETTTEI",""),("IETTTIE",""),
         ("IETTTS",""),("IETZE",""),("IJEEE",""),("IJEI",""),("IJIE",""),("IJS",""),
         ("IWB",""),("IWDE",""),("IWNEE",""),("IWNI",""),("IWTEEE",""),("IWTEI",""),
         ("IWTIE",""),("IWTS",""),("SMB",""),("SMDE",""),("SMNEE",""),("SMNI",""),
         ("SMTEEE",""),("SMTEI",""),("SMTIE",""),("SMTS",""),("SOEEE",""),("SOEI",""),
         ("SOIE",""),("SOS",""),("STGEE",""),("STGI",""),("STMEEE",""),("STMEI",""),
         ("STMIE",""),("STMS",""),("STTB",""),("STTDE",""),("STTNEE",""),("STTNI",""),
         ("STTTEEE",""),("STTTEI",""),("STTTIE",""),("STTTS",""),("STZE",""),("VGEE",""),
         ("VGI",""),("VMEEE",""),("VMEI",""),("VMIE",""),("VMS",""),("VTB",""),("VTDE",""),
         ("VTNEE",""),("VTNI",""),("VTTEEE",""),("VTTEI",""),("VTTIE",""),("VTTS",""),("VZE","")]

filter (\x -> not (elem x xs)) (just $ parse morse "...---...")         
-}

{-
([{][][}{][)(][}{][][}{][)(][}{][)(][}{][)(}{)(}{][)}]
([{][][][)(}{)(][][][}{)()(}{)(][][)(}{}{][}{)(][}])
([{][)(][}{)(][}{][}{)(}{)(][}{}{)(][}{)(]})
-}

{-
   - Moje riesenie ():
   - ako keby sme citali vstup "odzadu"
   - lavu zatvorku mozeme dat iba ak je prislusny counter 0
   - pravu zatvorku dame iba ak prislusny counter je 1 (najblizsia zatvorka tohto typu je prava zatvorka)
-}

lopata' :: Parser (String, Int, Int, Int)
lopata' = do {  char '('
             ; (xs,g,h,k) <- lopata'
             ; if g /= 1 then return zero ("",0,0,0) else return ('(':xs, 0, h, k)
            }
         +++
         do {  char '['
             ; (xs,g,h,k) <- lopata'
             ; if h /= 1 then return zero ("",0,0,0) else return ('[':xs, g, 0, k)
            }
         +++
         do {  char '{'
             ; (xs,g,h,k) <- lopata'
             ; if k /= 1 then return zero ("",0,0,0) else return ('{':xs, g, h, 0)
            }
         +++
         do {  char ')'
             ; (xs,g,h,k) <- lopata'
             ; if g /= 0 then return zero ("",0,0,0) else return (')':xs, 1, h, k)
            }
         +++
         do {  char ']'
             ; (xs,g,h,k) <- lopata'
             ; if h /= 0 then return zero ("",0,0,0) else return (']':xs, g, 1, k)
            }
         +++
         do {  char '}'
             ; (xs,g,h,k) <- lopata'
             ; if k /= 0 then return zero ("",0,0,0) else return ('}':xs, g, h, 1)
            }
         +++
         do {  char '('
             ; return zero ("", 0, 0, 0)
            }
         +++
         do {  char '['
             ; return zero ("", 0, 0, 0)
            }
         +++
         do {  char '['
             ; return zero ("", 0, 0, 0)
            }
         +++
         do {  char ')'
             ; return (")", 1, 0, 0)
            }
         +++
         do {  char ']'
             ; return ("]", 0, 1, 0)
            }
         +++
         do {  char '}'
             ; return ("}", 0, 0, 1)
            }
         +++
         return ("", 0, 0, 0)


{-
    - parse lopata "([{])}"
    - parse lopata "([{)}]"
    - parse lopata "([{)}]]"
-}
lopata :: Parser String
lopata = do { (xs,g,h,k) <- lopata'; return xs }
