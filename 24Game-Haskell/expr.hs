module Expression where
import Utils

--------------------------- Expr --------------------------------
data Expr = Const Float | Sum Expr Expr | Dif Expr Expr | Mult Expr Expr
                        | Div Expr Expr deriving Eq

foldExpr :: (Float -> b) ->
            (b -> b -> b) ->
            (b -> b -> b) ->
            (b -> b -> b) ->
            (b -> b -> b) ->
            Expr ->
            b
foldExpr fconst fsum fdif fmult fdiv expr = case expr of
                                              Const x -> fconst x
                                              Sum e1 e2 -> fsum (rec e1)
                                                                (rec e2)
                                              Dif e1 e2 -> fdif (rec e1)
                                                                (rec e2)
                                              Mult e1 e2 -> fmult (rec e1)
                                                                  (rec e2)
                                              Div e1 e2 -> fdiv (rec e1)
                                                                (rec e2)
                                        where rec = foldExpr fconst fsum fdif
                                                    fmult fdiv

recExpr :: (Float -> b) ->
            (b -> Expr -> b -> Expr -> b) ->
            (b -> Expr -> b -> Expr -> b) ->
            (b -> Expr -> b -> Expr -> b) ->
            (b -> Expr -> b -> Expr -> b) ->
            Expr ->
            b
recExpr fconst fsum fdif fmult fdiv expr = case expr of
                                              Const x -> fconst x
                                              Sum e1 e2 -> fsum (rec e1) e1
                                                                (rec e2) e2
                                              Dif e1 e2 -> fdif (rec e1) e1
                                                                (rec e2) e2
                                              Mult e1 e2 -> fmult (rec e1) e1
                                                                  (rec e2) e2
                                              Div e1 e2 -> fdiv (rec e1) e1
                                                                (rec e2) e2
                                        where rec = recExpr fconst fsum fdif
                                                    fmult fdiv
instance Show Expr where
    show e = foldExpr (\x -> show (round x))
                      (\rec1 rec2 -> rec1 ++ " + " ++ rec2)
                      (\rec1 rec2 -> rec1 ++ " - " ++ "(" ++ rec2 ++ ")")
                      (\rec1 rec2 -> "(" ++ rec1 ++ ") * (" ++ rec2 ++ ")")
                      (\rec1 rec2 -> "(" ++ rec1 ++ ") / (" ++ rec2 ++ ")")
                      e
--instance Show Expr where
--    show e = foldExpr show (\rec1 rec2 -> "sum(" ++ rec1 ++ "," ++ rec2 ++ ")")
--                           (\rec1 rec2 -> "dif(" ++ rec1 ++ "," ++ rec2 ++ ")")
--                           (\rec1 rec2 -> "mult(" ++ rec1 ++ "," ++ rec2 ++ ")")
--                           (\rec1 rec2 -> "div(" ++ rec1 ++ "," ++ rec2 ++ ")")
--                           e

---- Properties ----
areCommutation :: Expr -> Expr -> Bool
areCommutation e1 e2 = case e1 of
                            Sum a b ->
                                case e2 of
                                    Sum x y -> (areEquivalents x b ||
                                                areEquivalents b x) &&
                                                (areEquivalents a y ||
                                                 areEquivalents y a)
                                    otherwise -> False
                            Mult a b ->
                                case e2 of
                                    Mult x y -> (areEquivalents x b ||
                                                areEquivalents b x) &&
                                                (areEquivalents a y ||
                                                 areEquivalents y a)
                                    otherwise -> False
                            otherwise -> False

areAssociation :: Expr -> Expr -> Bool
areAssociation e1 e2 = case e1 of
                            Sum (Sum a b) c ->
                                case e2 of
                                    Sum x y -> (areEquivalents x (Sum a b) ||
                                                areEquivalents (Sum a b) x) &&
                                                (areEquivalents y c ||
                                                 areEquivalents c y) ||
                                                 (areEquivalents x a &&
                                                  areEquivalents y (Sum b c)) ||
                                                  (areEquivalents a x &&
                                                   areEquivalents (Sum b c) y)
                                    otherwise -> False
                            Sum c (Sum a b) ->
                                case e2 of
                                    Sum x y -> (areEquivalents x c ||
                                                areEquivalents c x) &&
                                                (areEquivalents y (Sum a b) ||
                                                 areEquivalents (Sum a b) y) ||
                                                 (areEquivalents x (Sum c a) &&
                                                  areEquivalents y b) ||
                                                  (areEquivalents (Sum c a) x &&
                                                   areEquivalents b y)
                                    otherwise -> False
                            Mult (Mult a b) c ->
                                case e2 of
                                    Mult x y -> (areEquivalents x (Mult a b) ||
                                                areEquivalents (Mult a b) x) &&
                                                (areEquivalents y c ||
                                                 areEquivalents c y) ||
                                                 (areEquivalents x a &&
                                                  areEquivalents y (Mult b c)) ||
                                                  (areEquivalents a x &&
                                                   areEquivalents (Mult b c) y)
                                    otherwise -> False
                            Mult c (Mult a b) ->
                                case e2 of
                                    Mult x y -> (areEquivalents x c ||
                                                areEquivalents c x) &&
                                                (areEquivalents y (Mult a b) ||
                                                 areEquivalents (Mult a b) y) ||
                                                 (areEquivalents x (Mult c a) &&
                                                  areEquivalents y b) ||
                                                  (areEquivalents (Mult c a) x &&
                                                   areEquivalents b y)
                                    otherwise -> False
                            otherwise -> False

areDistribution :: Expr -> Expr -> Bool
areDistribution e1 e2 = case e1 of
                            Mult a (Sum b c) ->
                                case e2 of
                                    Sum (Mult x1 y1) (Mult x2 y2) ->
                                        (areEquivalents x1 a ||
                                         areEquivalents a x1) &&
                                         (areEquivalents y1 b ||
                                          areEquivalents b y1) &&
                                          (areEquivalents x2 a ||
                                           areEquivalents a x2) &&
                                           (areEquivalents y2 c ||
                                            areEquivalents c y2)
                                    otherwise -> False
                            Mult (Sum b c) a ->
                                case e2 of
                                    Sum (Mult x1 y1) (Mult x2 y2) ->
                                        (areEquivalents x1 b ||
                                         areEquivalents b x1) &&
                                         (areEquivalents y1 a ||
                                          areEquivalents a y1) &&
                                          (areEquivalents x2 c ||
                                           areEquivalents c x2) &&
                                           (areEquivalents y2 a ||
                                            areEquivalents a y2)
                                    otherwise -> False
                            otherwise -> False

areEquivalents :: Expr -> Expr -> Bool
areEquivalents = recExpr fconst fsum fdif fmult fdiv
        where fconst = \x e -> e == Const x
              fsum = \b1 ex1 b2 ex2 e -> case e of
                                            Sum a b ->
                                                (e == Sum ex1 ex2)
                                                || (e == Sum ex2 ex1)
                                                || (b1 a && b2 b)
                                                || (b1 b && b2 a)
                                            otherwise -> False
              fdif = \b1 ex1 b2 ex2 e -> case e of
                                            Dif a b ->
                                                (e == Dif ex1 ex2)
                                                || (b1 a && b2 b)
                                            otherwise -> False
              fmult = \b1 ex1 b2 ex2 e -> case e of
                                            Mult a b ->
                                                (e == Mult ex1 ex2)
                                                || (e == Mult ex2 ex1)
                                                || (b1 a && b2 b)
                                                || (b1 b && b2 a)
                                            Sum a b ->
                                                case ex2 of
                                                    Sum s1 s2 ->
                                                        case ex1 of
                                                            Sum s3 s4 ->
                                                                case a of
                                                                    Mult a1 a2 ->
                                                                        case b of
                                                                            Mult c1 c2 ->
                                                                                ((areEquivalents a1 s3 || areEquivalents s3 a1) &&
                                                                                (areEquivalents a2 s1 || areEquivalents s1 a2) &&
                                                                                (areEquivalents c1 s4 || areEquivalents s4 c1) &&
                                                                                (areEquivalents c2 s2 || areEquivalents s2 c2)) ||
                                                                                (b1 a1 && b1 c1 && (areEquivalents s1 a1 ||
                                                                                    areEquivalents a1 s1) && (areEquivalents s2 c2 ||
                                                                                    areEquivalents c2 s2))
                                                                            otherwise -> False
                                                                    otherwise -> False
                                                            otherwise ->
                                                                case a of
                                                                    Mult a1 a2 ->
                                                                        case b of
                                                                            Mult c1 c2 ->
                                                                                    b1 a1 &&
                                                                                    (areEquivalents s1 a2
                                                                                    || areEquivalents a2 s1)
                                                                                    && b1 c1 &&
                                                                                    (areEquivalents s2 c2
                                                                                    || areEquivalents c2 s2)
                                                                            otherwise -> False
                                                                    otherwise -> False
                                                    otherwise ->
                                                        
                                            otherwise -> False
              fdiv = \b1 ex1 b2 ex2 e -> case e of
                                            Div a b -> e == Div ex1 ex2 ||
                                                       e == Div ex2 ex1 ||
                                                        (b1 a && b2 b)
                                            otherwise -> False
--------------------

exprResult :: Expr -> Maybe Float
exprResult = foldExpr fconst fsum fdif fmult fdiv
    where fconst = \x -> Just x
          fsum = maybeValue (+) (const True)
          fdif = maybeValue (-) (const True)
          fmult = maybeValue (*) (const True)
          fdiv = maybeValue (/) (\x -> x /= 0)
