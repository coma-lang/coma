module Core 
  ( Core.read
  , join
  , double
  , get
  , select
  , value
  , firstOr
  , merge
  , given
  ) where

import Data.Maybe
import qualified Data.HashMap as HM

import qualified Csv
import qualified Ast



-- INVALID INPUT


invalidInput fn expr = error $ "Invalid input to '" ++ fn ++ "': " ++ show expr



-- DOUBLE
-- Now that all Core functions are curried, use double to uncurry them back.


double :: Ast.Fn -> Ast.Coma -> Ast.Coma -> IO Ast.Coma
double fn xs ys = do
  Ast.Lambda i lenv fn <- fn 0 HM.empty xs
  fn i lenv ys



-- READ


read :: Ast.Fn
read _ _ (Ast.StrAtom filename) = 
  readFile filename >>= (return . convert . Csv.parse)
  where 
    convert = Ast.List . map convertRow
    convertRow = Ast.List . map Ast.StrAtom
read _ _ err = invalidInput "read" err



-- JOIN (CARTESIAN PRODUCT)
-- Combine 2 CSV tables by creating a list of Csv.Row pairs. The length of the
-- output list is X * Y where X and Y are respective length of the input tables.


join :: Ast.Fn

join 0 env list@(Ast.List ys) 
  = return 
  $ Ast.Lambda 1 (HM.insert "xs" list env) join

join 1 env list@(Ast.List ys) =
  let Just (Ast.List xs) = HM.lookup "xs" env in
  return
    $ Ast.List 
    $ map (Ast.List . uncurry (++)) 
    $ [(x,y) | (Ast.List x) <- xs, (Ast.List y) <- ys]

join _ _ err = invalidInput "join" err
    


-- GET
-- Create a Csv.Row with just the elements under selected indices.


--  :: [Int] -> Csv.Row -> Csv.Row
get :: Ast.Fn

get 0 env ids@(Ast.List _) = return $ Ast.Lambda 1 (HM.insert "ids" ids env) get

get 1 env (Ast.List row) =
  let Just (Ast.List ids) = HM.lookup "ids" env in
  return $ Ast.List (map aux ids)
  where aux (Ast.IntAtom i) = row !! i



-- SELECT
-- Create a Csv.Table with just the columns under selected indices.


--     :: [Int] -> Csv.Table -> Csv.Table
select :: Ast.Fn

select 0 env ids@(Ast.List _) 
  = return 
  $ Ast.Lambda 1 (HM.insert "ids" ids env) select

select 1 env (Ast.List rows) =
  let Just ids@(Ast.List _) = HM.lookup "ids" env in do
  list <- mapM (double get ids) rows
  return $ Ast.List list 



-- VALUE
-- Look at the value at given index in row.


--    :: Int -> Csv.Row -> String
value :: Ast.Fn

value 0 env index@(Ast.IntAtom _)
  = return
  $ Ast.Lambda 1 (HM.insert "index" index env) value

value 1 env (Ast.List row) =
  let Just (Ast.IntAtom index) = HM.lookup "index" env in do
  return $ row !! index 



-- FIRST OR


firstOr :: Ast.Coma -> Ast.Coma -> Ast.Coma
firstOr (Ast.StrAtom x) (Ast.StrAtom y) = Ast.StrAtom $ if null x then y else x



-- MERGE
-- Merge two rows, giving priority to the first one. Use values from the second
-- row if the value in the first row is an empty string.


-- :: Csv.Row -> Csv.Row -> Csv.Row
merge :: Ast.Fn

merge 0 env p@(Ast.List _)
  = return
  $ Ast.Lambda 1 (HM.insert "p" p env) merge

merge 1 env (Ast.List q) =
  let Just (Ast.List p) = HM.lookup "p" env in do
  return $ Ast.List $ map (uncurry firstOr) $ Prelude.zip p q



-- GIVEN
-- Filter out table rows that don't fit the predicate.


--    :: (Csv.Row -> Bool) -> Csv.Table -> Csv.Table
given :: Ast.Fn

given 0 env predicate@(Ast.Lambda _ _ _)
  = return
  $ Ast.Lambda 1 (HM.insert "predicate" predicate env) given

given 1 env (Ast.List rows) =
  let 
    Just predicate@(Ast.Lambda _ _ _) = HM.lookup "predicate" env 
  in do
  maybes <- mapM (selector env predicate) rows
  return $ Ast.List $ catMaybes maybes


selector :: Ast.Env -> Ast.Coma -> Ast.Coma -> IO (Maybe Ast.Coma)
selector env fn@(Ast.Lambda _ _ _) row = do
  Ast.BoolAtom ok <- Ast.execWithEnv env (Ast.Call fn row)
  return $ if ok then Just row else Nothing
