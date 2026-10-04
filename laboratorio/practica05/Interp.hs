module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [p] b = Just (Fun p b)
curryFun (p : ps) b = Fun p <$> curryFun ps b
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f [a] = Just (App f a)
curryApp f (a : as) = curryApp (App f a) as
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op [a, b] = Just (op a b)
binaryOp op (a : b : rest) = binaryOp op (op a b : rest)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] e = desugar e
desugarCond ((c, t) : cs) e =
  If <$> desugar c <*> desugar t <*> desugarCond cs e

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS es) = mapM desugar es >>= binaryOp Add
desugar (SubS es) = mapM desugar es >>= binaryOp Sub
desugar (NotS e) = Not <$> desugar e
desugar (LetS x v b) = (\v' b' -> App (Fun x b') v') <$> desugar v <*> desugar b
desugar (LetStarS [] b) = desugar b
desugar (LetStarS ((x, v) : bs) b) = desugar (LetS x v (LetStarS bs b))
desugar (FunS ps b) = desugar b >>= curryFun ps
desugar (AppS f as) = desugar f >>= \f' -> mapM desugar as >>= curryApp f'
desugar (IfS c t e) = If <$> desugar c <*> desugar t <*> desugar e
desugar (CondS cs e) = desugarCond cs e
desugar (LetRecS f d b) =
  desugar (LetS f (AppS (IdS "Y") [FunS [f] d]) b)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv = lookup

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (ExprV e env) = bigStep env e >>= strict
strict v = Just v

exige :: Env -> ASA -> Maybe Value
exige env e = bigStep env e >>= strict

numVal :: Value -> Maybe Int
numVal (NumV n) = Just n
numVal _ = Nothing

boolVal :: Value -> Maybe Bool
boolVal (BooleanV b) = Just b
boolVal _ = Nothing

operando :: Env -> ASA -> Maybe Int
operando env e = exige env e >>= numVal

aplica :: Value -> ASA -> Env -> Maybe Value
aplica (ClosureV p b envF) a env = bigStep ((p, ExprV a env) : envF) b
aplica _ _ _ = Nothing

elige :: Env -> ASA -> ASA -> Bool -> Maybe Value
elige env t _ True = bigStep env t
elige env _ e False = bigStep env e

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Add a b) =
  (\x y -> NumV (x + y)) <$> operando env a <*> operando env b
bigStep env (Sub a b) =
  (\x y -> NumV (max 0 (x - y))) <$> operando env a <*> operando env b
bigStep env (Not e) = (BooleanV . not) <$> (exige env e >>= boolVal)
bigStep env (Fun p b) = Just (ClosureV p b env)
bigStep env (App f a) = exige env f >>= \vf -> aplica vf a env
bigStep env (If c t e) = exige env c >>= boolVal >>= elige env t e
