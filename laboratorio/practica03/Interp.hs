module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura

sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados [] = []
sinDuplicados (x:xs) = x : sinDuplicados (filter (/= x) xs)

freeVars :: ASA -> [String]
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And es) = sinDuplicados (concatMap freeVars es)
freeVars (Or es) = sinDuplicados (concatMap freeVars es)
freeVars (Add es) = sinDuplicados (concatMap freeVars es)
freeVars (Sub es) = sinDuplicados (concatMap freeVars es)
freeVars (Mul es) = sinDuplicados (concatMap freeVars es)
freeVars (Div es) = sinDuplicados (concatMap freeVars es)
freeVars (Lt es) = sinDuplicados (concatMap freeVars es)
freeVars (Gt es) = sinDuplicados (concatMap freeVars es)
freeVars (Le es) = sinDuplicados (concatMap freeVars es)
freeVars (Ge es) = sinDuplicados (concatMap freeVars es)
freeVars (Expt e1 e2) = sinDuplicados (freeVars e1 ++ freeVars e2)
freeVars (EqP e1 e2) = sinDuplicados (freeVars e1 ++ freeVars e2)
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (Let binds body) =
  let boundVars = map fst binds
      freeInBinds = concatMap (freeVars . snd) binds
      freeInBody = filter (`notElem` boundVars) (freeVars body)
  in sinDuplicados (freeInBinds ++ freeInBody)
freeVars (LetStar [] body) = freeVars body
freeVars (LetStar ((x, e):binds) body) =
  sinDuplicados (freeVars e ++ filter (/= x) (freeVars (LetStar binds body)))

names :: ASA -> [String]
names (Id x) = [x]
names (Num _) = []
names (Boolean _) = []
names (And es) = sinDuplicados (concatMap names es)
names (Or es) = sinDuplicados (concatMap names es)
names (Add es) = sinDuplicados (concatMap names es)
names (Sub es) = sinDuplicados (concatMap names es)
names (Mul es) = sinDuplicados (concatMap names es)
names (Div es) = sinDuplicados (concatMap names es)
names (Lt es) = sinDuplicados (concatMap names es)
names (Gt es) = sinDuplicados (concatMap names es)
names (Le es) = sinDuplicados (concatMap names es)
names (Ge es) = sinDuplicados (concatMap names es)
names (Expt e1 e2) = sinDuplicados (names e1 ++ names e2)
names (EqP e1 e2) = sinDuplicados (names e1 ++ names e2)
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (Let binds body) =
  sinDuplicados (map fst binds ++ concatMap (names . snd) binds ++ names body)
names (LetStar binds body) =
  sinDuplicados (map fst binds ++ concatMap (names . snd) binds ++ names body)

freshName :: [String] -> String
freshName used = head [ "v" ++ show i | i <- [0..], ("v" ++ show i) `notElem` used ]

sust :: ASA -> String -> ASA -> ASA
sust e x s = sustMany e [(x, s)]

sustMany :: ASA -> [Binding] -> ASA
sustMany e [] = e
sustMany (Id y) subs = case lookup y subs of
                         Just s -> s
                         Nothing -> Id y
sustMany (Num n) _ = Num n
sustMany (Boolean b) _ = Boolean b
sustMany (And es) subs = And (map (`sustMany` subs) es)
sustMany (Or es) subs = Or (map (`sustMany` subs) es)
sustMany (Add es) subs = Add (map (`sustMany` subs) es)
sustMany (Sub es) subs = Sub (map (`sustMany` subs) es)
sustMany (Mul es) subs = Mul (map (`sustMany` subs) es)
sustMany (Div es) subs = Div (map (`sustMany` subs) es)
sustMany (Lt es) subs = Lt (map (`sustMany` subs) es)
sustMany (Gt es) subs = Gt (map (`sustMany` subs) es)
sustMany (Le es) subs = Le (map (`sustMany` subs) es)
sustMany (Ge es) subs = Ge (map (`sustMany` subs) es)
sustMany (Expt e1 e2) subs = Expt (sustMany e1 subs) (sustMany e2 subs)
sustMany (EqP e1 e2) subs = EqP (sustMany e1 subs) (sustMany e2 subs)
sustMany (Not e) subs = Not (sustMany e subs)
sustMany (Add1 e) subs = Add1 (sustMany e subs)
sustMany (Sub1 e) subs = Sub1 (sustMany e subs)
sustMany (ZeroP e) subs = ZeroP (sustMany e subs)

sustMany (Let binds body) subs =
  let newBinds = [(v, sustMany e subs) | (v, e) <- binds]
      boundVars = map fst binds
      validSubs = filter (\(x, _) -> x `notElem` boundVars) subs
  in if null validSubs
     then Let newBinds body
     else
       let freeInSubs = concatMap (freeVars . snd) validSubs
           captures = filter (`elem` freeInSubs) boundVars
       in if null captures
          then Let newBinds (sustMany body validSubs)
          else
            let avoid = freeInSubs ++ names body ++ boundVars ++ map fst validSubs
                (newBoundVars, _) = foldl (\(acc, av) _ ->
                                              let n = freshName av
                                              in (acc ++ [n], n : av))
                                          ([], avoid) binds
                renameSubs = zip boundVars (map Id newBoundVars)
                renamedBody = sustMany body renameSubs
                finalBinds = zip newBoundVars (map snd newBinds)
            in Let finalBinds (sustMany renamedBody validSubs)

sustMany (LetStar binds body) subs =
  let helper [] s = ([], sustMany body s)
      helper ((x, e):bs) s =
          let newE = sustMany e s
              s' = filter (\(k, _) -> k /= x) s
          in if null s' then
                let (restBs, restBody) = helper bs []
                in ((x, newE) : restBs, restBody)
             else
                let freeInS = concatMap (freeVars . snd) s'
                in if x `elem` freeInS then
                   let avoid = freeInS ++ names (LetStar bs body) ++ map fst s'
                       newX = freshName avoid
                       renamedRest = sustMany (LetStar bs body) [(x, Id newX)]
                       (rBs, rBody) = case renamedRest of
                                        LetStar bss bod -> (bss, bod)
                                        _ -> (bs, body)
                       (finalBs, finalBody) = helper rBs s'
                   in ((newX, newE) : finalBs, finalBody)
                else
                   let (finalBs, finalBody) = helper bs s'
                   in ((x, newE) : finalBs, finalBody)
      (resBinds, resBody) = helper binds subs
  in LetStar resBinds resBody

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.

extrNum :: ASA -> Maybe Int
extrNum (Num n) = Just n
extrNum _ = Nothing

extrBool :: ASA -> Maybe Bool
extrBool (Boolean b) = Just b
extrBool _ = Nothing

cadeComp :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
cadeComp op es = do
  vs <- mapM bigStep es
  ns <- mapM extrNum vs
  if length ns < 2
    then Nothing
    else Just (Boolean (and (zipWith op ns (tail ns))))

bigStep :: ASA -> Maybe ASA
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _) = Nothing

bigStep (And es) = do
  vs <- mapM bigStep es
  bs <- mapM extrBool vs
  Just (Boolean (and bs))
bigStep (Or es) = do
  vs <- mapM bigStep es
  bs <- mapM extrBool vs
  Just (Boolean (or bs))

bigStep (Add es) = do
  vs <- mapM bigStep es
  ns <- mapM extrNum vs
  Just (Num (sum ns))
bigStep (Mul es) = do
  vs <- mapM bigStep es
  ns <- mapM extrNum vs
  Just (Num (product ns))
bigStep (Sub es) = do
  vs <- mapM bigStep es
  ns <- mapM extrNum vs
  case ns of
    [] -> Nothing
    [_] -> Nothing
    (x:xs) -> Just (Num (foldl (\acc n -> max 0 (acc - n)) x xs))
bigStep (Div es) = do
  vs <- mapM bigStep es
  ns <- mapM extrNum vs
  case ns of
    [] -> Nothing
    [_] -> Nothing
    (x:xs) -> if 0 `elem` xs
              then Nothing
              else Just (Num (foldl div x xs))

bigStep (Lt es) = cadeComp (<) es
bigStep (Gt es) = cadeComp (>) es
bigStep (Le es) = cadeComp (<=) es
bigStep (Ge es) = cadeComp (>=) es

bigStep (Expt e1 e2) = do
  Num n <- bigStep e1
  Num m <- bigStep e2
  Just (Num (n ^ m))
bigStep (EqP e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num n1, Num n2) -> Just (Boolean (n1 == n2))
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 == b2))
    _ -> Nothing

    bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean b -> Just (Boolean (not b))
    Num _ -> Just (Boolean False)
    _ -> Nothing
bigStep (Add1 e) = do
  n <- bigStep e >>= extrNum
  Just (Num (n + 1))
bigStep (Sub1 e) = do
  n <- bigStep e >>= extrNum
  Just (Num (max 0 (n - 1)))
bigStep (ZeroP e) = do
  n <- bigStep e >>= extrNum
  Just (Boolean (n == 0))

  bigStep (Let binds body) = do
  evalBinds <- mapM (\(x, e) -> do
    v <- bigStep e
    return (x, v)) binds
  bigStep (sustMany body evalBinds)

  bigStep (LetStar [] body) = bigStep body
bigStep (LetStar ((x, e):binds) body) = do
  v <- bigStep e
  let substituted = sust (LetStar binds body) x v
  bigStep substituted