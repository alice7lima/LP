module Interpreter where

import AbsLI
import Prelude hiding (lookup)

type ErrorMessage = String

executeP :: RContext -> Program  -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
   SAss id exp ->
      case eval context exp of
         Left e -> Left e
         Right x -> Right (update context (getStr id) (x))
   SBlock [] -> Right context
   SBlock (s:stms) ->
      case execute context s of
         Left e -> Left e
         Right x -> execute (x) (SBlock stms) 
   SWhile exp stm -> 
      case eval context exp of
         Left e -> Left e
         Right x -> if ((i(x)) /= 0)
                     then
                        case execute context stm of
                           Left e -> Left e
                           Right x -> execute (x) (SWhile exp stm)
                     else Right context
   SdoWhile stm exp ->
     case execute context stm of
            Left e -> Left e
            Right x -> execute (x) (SWhile exp stm)
 
   STry [] _ stmF -> execute context (SBlock stmF)
   STry (t:stmT) stmC stmF -> 
      case execute context t of
         Left e -> 
            case execute context (SBlock stmC) of
               Left e -> Left e
               Right y -> execute (y) (SBlock stmF)
      
         Right x -> execute x (STry stmT stmC stmF) 

data Valor = ValorStr String |
             ValorInt Integer |
             ValorBool Bool

s :: Valor -> String             
s (ValorStr str) = str
i :: Valor -> Integer
i (ValorInt vint) = vint 
b (ValorBool vbool) = vbool


instance Show Valor where
 show (ValorInt vint) = show vint
 show (ValorStr vstr) = vstr
 show (ValorBool vb) = show vb


instance Eq Valor where
 (ValorInt i1) == (ValorInt i2) =  i1 == i2
 (ValorStr s1) == (ValorStr s2) =  s1 == s2
 (ValorBool b1) == (ValorBool b2) = b1 == b2


eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
    EAdd exp0 exp  ->
      case eval context exp0 of
         Left msg -> Left msg 
         Right x ->
            case eval context exp of
               Left msg -> Left msg
               Right y -> Right (ValorInt (i(x) + i(y)))
    ESub exp0 exp  ->
      case eval context exp0 of
         Left msg -> Left msg
         Right x ->
            case eval context exp of
               Left msg -> Left msg
               Right y -> Right (ValorInt (i(x) - i(y)))
    EMul exp0 exp  ->
      case eval context exp0 of
         Left msg -> Left msg 
         Right x ->
            case eval context exp of
               Left msg -> Left msg
               Right y -> Right (ValorInt (i(x) * i(y)))
    EDiv exp0 exp -> case eval context exp0 of 
                    Right ve1 -> case eval context exp of 
                                   Right ve2 -> if ((i(ve2)) == 0)
                                                 then Left ("divisao por 0")
                                                 else Right (ValorInt (i(ve1) `div` i(ve2)))
                                   Left msg -> Left msg  
                    Left msg -> Left msg  
    ECon exp0 exp -> 
        case eval context exp0 of
         Left msg -> Left msg 
         Right x ->
            case eval context exp of
               Left msg -> Left msg
               Right y -> Right (ValorStr (s(x) ++ s(y)))
    EInt n  ->  Right (ValorInt n )
    EVar id  -> Right (lookup context (getStr id))
    EStr str -> Right (ValorStr str )
    EOr exp0 exp -> 
        case eval context exp0 of
         Left msg -> Left msg 
         Right x ->
            case eval context exp of
               Left msg -> Left msg
               Right y -> Right (ValorBool(b(x) || b(y)))
    EAnd exp0 exp -> 
        case eval context exp0 of
         Left msg -> Left msg 
         Right x ->
            case eval context exp of
               Left msg -> Left msg
               Right y -> Right (ValorBool (b(x) && b(y)))        
    
    ENot exp ->
        case eval context exp of
            Left msg -> Left msg 
            Right x -> Right (ValorBool (not(b(x))))
  
    ETrue -> Right (ValorBool True)
    EFalse -> Right (ValorBool False)


type RContext = [(String,Valor)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv

