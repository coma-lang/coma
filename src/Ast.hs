{-# OPTIONS_GHC -w #-}
module Ast where 
import qualified Data.HashMap as HM
import Data.List (intercalate)
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Coma)
	| HappyAbsSyn8 ([Coma])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,94) ([1280,1864,0,4,0,1,0,61440,7,32848,112,0,20480,28800,1280,1800,0,16,0,0,0,0,0,0,1,2053,32775,0,61952,7,0,0,2053,20487,28800,1280,1800,32848,112,2053,20487,28800,1280,1800,0,2,2053,20487,28800,1280,1800,32848,112,2053,20487,28800,1280,1800,32848,112,0,0,0,0,0,32848,112,0,0,2175,1280,1864,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Let","Expr","Call","Literal","List","'('","')'","'['","']'","'!='","'='","'<'","'<='","'>'","'>='","'++'","'\\\\'","'->'","':='","let","in","ident","string","integer","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (20) = happyShift action_9
action_0 (23) = happyShift action_2
action_0 (25) = happyShift action_10
action_0 (26) = happyShift action_11
action_0 (27) = happyShift action_12
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (23) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (25) = happyShift action_25
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (28) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (13) = happyShift action_18
action_4 (14) = happyShift action_19
action_4 (15) = happyShift action_20
action_4 (16) = happyShift action_21
action_4 (17) = happyShift action_22
action_4 (18) = happyShift action_23
action_4 (19) = happyShift action_24
action_4 _ = happyReduce_2

action_5 (9) = happyShift action_7
action_5 (11) = happyShift action_8
action_5 (20) = happyShift action_9
action_5 (25) = happyShift action_10
action_5 (26) = happyShift action_11
action_5 (27) = happyShift action_12
action_5 (7) = happyGoto action_17
action_5 _ = happyReduce_10

action_6 _ = happyReduce_12

action_7 (9) = happyShift action_7
action_7 (11) = happyShift action_8
action_7 (20) = happyShift action_9
action_7 (25) = happyShift action_10
action_7 (26) = happyShift action_11
action_7 (27) = happyShift action_12
action_7 (5) = happyGoto action_16
action_7 (6) = happyGoto action_5
action_7 (7) = happyGoto action_6
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (9) = happyShift action_7
action_8 (11) = happyShift action_8
action_8 (20) = happyShift action_9
action_8 (25) = happyShift action_10
action_8 (26) = happyShift action_11
action_8 (27) = happyShift action_12
action_8 (7) = happyGoto action_14
action_8 (8) = happyGoto action_15
action_8 _ = happyReduce_19

action_9 (25) = happyShift action_13
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_15

action_11 _ = happyReduce_14

action_12 _ = happyReduce_13

action_13 (21) = happyShift action_37
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_7
action_14 (11) = happyShift action_8
action_14 (20) = happyShift action_9
action_14 (25) = happyShift action_10
action_14 (26) = happyShift action_11
action_14 (27) = happyShift action_12
action_14 (7) = happyGoto action_14
action_14 (8) = happyGoto action_36
action_14 _ = happyReduce_19

action_15 (12) = happyShift action_35
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (10) = happyShift action_34
action_16 (13) = happyShift action_18
action_16 (14) = happyShift action_19
action_16 (15) = happyShift action_20
action_16 (16) = happyShift action_21
action_16 (17) = happyShift action_22
action_16 (18) = happyShift action_23
action_16 (19) = happyShift action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_11

action_18 (9) = happyShift action_7
action_18 (11) = happyShift action_8
action_18 (20) = happyShift action_9
action_18 (25) = happyShift action_10
action_18 (26) = happyShift action_11
action_18 (27) = happyShift action_12
action_18 (6) = happyGoto action_33
action_18 (7) = happyGoto action_6
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (9) = happyShift action_7
action_19 (11) = happyShift action_8
action_19 (20) = happyShift action_9
action_19 (25) = happyShift action_10
action_19 (26) = happyShift action_11
action_19 (27) = happyShift action_12
action_19 (6) = happyGoto action_32
action_19 (7) = happyGoto action_6
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (9) = happyShift action_7
action_20 (11) = happyShift action_8
action_20 (20) = happyShift action_9
action_20 (25) = happyShift action_10
action_20 (26) = happyShift action_11
action_20 (27) = happyShift action_12
action_20 (6) = happyGoto action_31
action_20 (7) = happyGoto action_6
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (9) = happyShift action_7
action_21 (11) = happyShift action_8
action_21 (20) = happyShift action_9
action_21 (25) = happyShift action_10
action_21 (26) = happyShift action_11
action_21 (27) = happyShift action_12
action_21 (6) = happyGoto action_30
action_21 (7) = happyGoto action_6
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (9) = happyShift action_7
action_22 (11) = happyShift action_8
action_22 (20) = happyShift action_9
action_22 (25) = happyShift action_10
action_22 (26) = happyShift action_11
action_22 (27) = happyShift action_12
action_22 (6) = happyGoto action_29
action_22 (7) = happyGoto action_6
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (9) = happyShift action_7
action_23 (11) = happyShift action_8
action_23 (20) = happyShift action_9
action_23 (25) = happyShift action_10
action_23 (26) = happyShift action_11
action_23 (27) = happyShift action_12
action_23 (6) = happyGoto action_28
action_23 (7) = happyGoto action_6
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (9) = happyShift action_7
action_24 (11) = happyShift action_8
action_24 (20) = happyShift action_9
action_24 (25) = happyShift action_10
action_24 (26) = happyShift action_11
action_24 (27) = happyShift action_12
action_24 (6) = happyGoto action_27
action_24 (7) = happyGoto action_6
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (22) = happyShift action_26
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (9) = happyShift action_7
action_26 (11) = happyShift action_8
action_26 (20) = happyShift action_9
action_26 (25) = happyShift action_10
action_26 (26) = happyShift action_11
action_26 (27) = happyShift action_12
action_26 (5) = happyGoto action_39
action_26 (6) = happyGoto action_5
action_26 (7) = happyGoto action_6
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_7
action_27 (11) = happyShift action_8
action_27 (20) = happyShift action_9
action_27 (25) = happyShift action_10
action_27 (26) = happyShift action_11
action_27 (27) = happyShift action_12
action_27 (7) = happyGoto action_17
action_27 _ = happyReduce_9

action_28 (9) = happyShift action_7
action_28 (11) = happyShift action_8
action_28 (20) = happyShift action_9
action_28 (25) = happyShift action_10
action_28 (26) = happyShift action_11
action_28 (27) = happyShift action_12
action_28 (7) = happyGoto action_17
action_28 _ = happyReduce_8

action_29 (9) = happyShift action_7
action_29 (11) = happyShift action_8
action_29 (20) = happyShift action_9
action_29 (25) = happyShift action_10
action_29 (26) = happyShift action_11
action_29 (27) = happyShift action_12
action_29 (7) = happyGoto action_17
action_29 _ = happyReduce_7

action_30 (9) = happyShift action_7
action_30 (11) = happyShift action_8
action_30 (20) = happyShift action_9
action_30 (25) = happyShift action_10
action_30 (26) = happyShift action_11
action_30 (27) = happyShift action_12
action_30 (7) = happyGoto action_17
action_30 _ = happyReduce_6

action_31 (9) = happyShift action_7
action_31 (11) = happyShift action_8
action_31 (20) = happyShift action_9
action_31 (25) = happyShift action_10
action_31 (26) = happyShift action_11
action_31 (27) = happyShift action_12
action_31 (7) = happyGoto action_17
action_31 _ = happyReduce_5

action_32 (9) = happyShift action_7
action_32 (11) = happyShift action_8
action_32 (20) = happyShift action_9
action_32 (25) = happyShift action_10
action_32 (26) = happyShift action_11
action_32 (27) = happyShift action_12
action_32 (7) = happyGoto action_17
action_32 _ = happyReduce_3

action_33 (9) = happyShift action_7
action_33 (11) = happyShift action_8
action_33 (20) = happyShift action_9
action_33 (25) = happyShift action_10
action_33 (26) = happyShift action_11
action_33 (27) = happyShift action_12
action_33 (7) = happyGoto action_17
action_33 _ = happyReduce_4

action_34 _ = happyReduce_16

action_35 _ = happyReduce_17

action_36 _ = happyReduce_20

action_37 (9) = happyShift action_7
action_37 (11) = happyShift action_8
action_37 (20) = happyShift action_9
action_37 (25) = happyShift action_10
action_37 (26) = happyShift action_11
action_37 (27) = happyShift action_12
action_37 (7) = happyGoto action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_18

action_39 (13) = happyShift action_18
action_39 (14) = happyShift action_19
action_39 (15) = happyShift action_20
action_39 (16) = happyShift action_21
action_39 (17) = happyShift action_22
action_39 (18) = happyShift action_23
action_39 (19) = happyShift action_24
action_39 (24) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (9) = happyShift action_7
action_40 (11) = happyShift action_8
action_40 (20) = happyShift action_9
action_40 (23) = happyShift action_2
action_40 (25) = happyShift action_10
action_40 (26) = happyShift action_11
action_40 (27) = happyShift action_12
action_40 (4) = happyGoto action_41
action_40 (5) = happyGoto action_4
action_40 (6) = happyGoto action_5
action_40 (7) = happyGoto action_6
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_1

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier         pos happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Equal happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (NotEqual happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Less happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Greater happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Append happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Call happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyTerminal (TokenInteger            pos happy_var_1))
	 =  HappyAbsSyn4
		 (IntAtom happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 (HappyTerminal (TokenString             pos happy_var_1))
	 =  HappyAbsSyn4
		 (StrAtom happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 (HappyTerminal (TokenIdentifier         pos happy_var_1))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (List happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 7 happyReduction_18
happyReduction_18 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier         pos happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lambda 0 HM.empty (lambda happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_0  8 happyReduction_19
happyReduction_19  =  HappyAbsSyn8
		 ([]
	)

happyReduce_20 = happySpecReduce_2  8 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 28 28 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLeftParen          pos -> cont 9;
	TokenRightParen         pos -> cont 10;
	TokenLeftBrace          pos -> cont 11;
	TokenRightBrace         pos -> cont 12;
	TokenNotEqual           pos -> cont 13;
	TokenEqual              pos -> cont 14;
	TokenLessThan           pos -> cont 15;
	TokenLessThanOrEqual    pos -> cont 16;
	TokenGreaterThan        pos -> cont 17;
	TokenGreaterThanOrEqual pos -> cont 18;
	TokenAppend             pos -> cont 19;
	TokenLambda             pos -> cont 20;
	TokenArrow              pos -> cont 21;
	TokenAssign             pos -> cont 22;
	TokenLet                pos -> cont 23;
	TokenIn                 pos -> cont 24;
	TokenIdentifier         pos happy_dollar_dollar -> cont 25;
	TokenString             pos happy_dollar_dollar -> cont 26;
	TokenInteger            pos happy_dollar_dollar -> cont 27;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 28 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 



-- ENV


type Env = HM.Map String Coma



-- COMA


data Coma
  = IntAtom Int
  | StrAtom String
  | Ident String
  | List [Coma]
  | Lambda Int Env (Int -> Env -> Coma -> IO Coma)
  | Equal Coma Coma
  | NotEqual Coma Coma
  | Less Coma Coma
  | LessEqual Coma Coma
  | Greater Coma Coma
  | GreaterEqual Coma Coma
  | Append Coma Coma
  | Call Coma Coma
  | Let String Coma Coma
  


-- EQ


instance Eq Coma where
  IntAtom i == IntAtom j = i == j
  StrAtom i == StrAtom j = i == j
  Ident   i == Ident   j = i == j
  List    i == List    j = j == j
  _         == _         = False



-- SHOW COMA


instance Show Coma where
  show (IntAtom i) = "#" ++ show i
  show (StrAtom s) = s
  show (Ident idt) = "$" ++ idt
  show (List list) = "[ " ++ unwords (map show list) ++ " ]"
  show (Lambda i env _) = "<lambda/" ++ show i ++ "/" ++ show env ++ ">"
  show (Equal e1 e2) = show e1 ++ " = " ++ show e2
  show (NotEqual e1 e2) = show e1 ++ " != " ++ show e2
  show (Less e1 e2) = show e1 ++ " < " ++ show e2
  show (LessEqual e1 e2) = show e1 ++ " <= " ++ show e2
  show (Greater e1 e2) = show e1 ++ " > " ++ show e2
  show (GreaterEqual e1 e2) = show e1 ++ " >= " ++ show e2
  show (Append e1 e2) = show e1 ++ " ++ " ++ show e2
  show (Call e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Let idt e1 e2) = "let " ++ idt ++ " := " ++ show e1 ++ " in " ++ show e2



-- EXEC WITH ENV


execWithEnv :: Env -> Coma -> IO Coma

execWithEnv _ int@(IntAtom _) = return int
execWithEnv _ str@(StrAtom _) = return str
execWithEnv _ lst@(List    _) = return lst

execWithEnv env ident@(Ident name) = 
  case HM.lookup name env of
    Just coma -> return coma
    Nothing   -> error $ "Unknown identifier: '" ++ name ++ "'"

execWithEnv env (Lambda i lenv fn) = 
  return $ Lambda i (HM.union lenv env) fn
    
execWithEnv env (Call e1 e2) = do
  Lambda i lenv fn <- execWithEnv env e1
  arg <- execWithEnv env e2
  fn i (HM.union env lenv) arg

execWithEnv env (Let ident expr inexpr) = do
  evaluated <- execWithEnv env expr
  execWithEnv (HM.insert ident evaluated env) inexpr 
 
execWithEnv _ code = return code



-- LAMBDA


lambda :: String -> Ast.Coma -> Int -> Ast.Env -> Ast.Coma -> IO Ast.Coma
lambda param expr _ env arg = execWithEnv (HM.insert param arg env) expr
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
