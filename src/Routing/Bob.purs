module Routing.Bob where

import Prelude
import Data.Array as Data.Array
import Control.Error.Util (note, hush)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Except (except, ExceptT)
import Control.Monad.Except.Trans (throwError, runExceptT, withExceptT)
import Control.Monad.State (runState, State)
import Control.Monad.State.Class (put, get)
import Control.Monad.Trans (lift)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldr)
import Data.Generic (toSpine, class Generic, GenericSpine(SString, SInt, SBoolean, SRecord, SProd), fromSpine, toSignature)
import Data.Identity (Identity)
import Data.List (List(Cons, Nil), null, singleton, head, fromFoldable, (:))
import Data.Maybe (fromJust, Maybe(Just, Nothing), fromMaybe)
import Data.NonEmpty (foldMap1, (:|))
import Data.StrMap (insert, pop, StrMap, empty)
import Data.String (split)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Routing.Bob.Boomerangs (arrayFromList, lazy)
import Routing.Bob.Query.Boomerangs (ValueBoomerang, toOptionalValueBoomerang, toSimpleValueBoomerang, toQueryBoomerang)
import Routing.Bob.RecursionSchemes (anaM, para, RAlgArg, Fix(Fix))
import Routing.Bob.Signature (SigRecValueF(SigRecOptionalValueF, SigRecValueF), fromGenericSignature, SigF(SigStringF, SigProdF, SigIntF, SigBooleanF, SigRecordF))
import Routing.Bob.UrlBoomerang (UrlSerializer, Url, printURL, parseURL, int, str, boolean, liftStringBoomerang, liftStringPrs, UrlBoomerang)
import Text.Boomerang.Combinators (pureBmg, maph, nil, cons, duck1)
import Text.Boomerang.HStack (hSingleton, hNil, hHead, HNil, type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), Boomerang(Boomerang), runSerializer)
import Text.Boomerang.String (lit)
import Text.Parsing.Parser (parseFailed, fail, ParseError(ParseError), PState(PState), ParserT(ParserT), runParser)
import Text.Parsing.Parser.Pos (Position(Position))
import Text.Parsing.Parser.String (eof)
import Type.Proxy (Proxy(Proxy))

type UrlBoomerangForGenericSpine r = UrlBoomerang r (GenericSpine :- r)

maybeIsoToSpineBoomerang ::
  forall r.
    String -> String -> UrlBoomerang r (GenericSpine :- r) -> UrlBoomerang r (GenericSpine :- r)
maybeIsoToSpineBoomerang just nothing value =
  (maph justPrs justSer <<< lazy <<< value ) <> (pureBmg (SProd nothing [] :- _) nothingSer)
 where
  justPrs v = SProd just [v]

  justSer (SProd c [v]) | c == just = Just v
  justSer _ = Nothing

  nothingSer (SProd c _ :- r) | c == nothing = Just r
  nothingSer _ = Nothing


type MaybeMatch =
  { nothing :: String
  , just :: String
  , sigF :: Fix SigF
  , value :: Fix SigF
  }

matchMaybe :: Fix SigF -> Maybe MaybeMatch
matchMaybe sigF@(Fix (SigProdF _ (j@{ sigValues: (v : Nil) } :| (n@{ sigValues: Nil } : Nil)))) =
  Just { sigF: sigF, just: j.sigConstructor, nothing: n.sigConstructor, value: v}
matchMaybe sigF@(Fix (SigProdF _ (n@{ sigValues: Nil } :| (j@{ sigValues: (v : Nil) } : Nil)))) =
  Just { sigF: sigF, just: j.sigConstructor, nothing: n.sigConstructor, value: v}
matchMaybe _ = Nothing

serializeNothing :: MaybeMatch -> String
serializeNothing m =
  let sb = para toSpineBoomerang (m.sigF)
  in unsafePartial $ fromJust $ (serialize sb (SProd m.nothing [])) >>= printURL

type UrlParser a = ParserT Url Identity a

maybeValuePrs :: forall a. Fix SigF -> UrlParser a -> ParserT (List String) Identity a
maybeValuePrs sigF valuePrs =
  case matchMaybe sigF of
    Just m -> prs m
    Nothing -> (fail "Production doesn't match a maybe structure")
 where
  prs :: MaybeMatch -> ParserT (List String) Identity a
  prs maybeMatch =
    ParserT prs'
   where
    prs' :: PState (List String) -> Identity { input :: (List String), result :: Either ParseError a, consumed:: Boolean, position:: Position }
    prs' (PState s) =
      case parseMaybeValue s.input valuePrs maybeMatch of
        parseError@(Left _) ->
          pure
            { input: s.input
            , result: parseError
            , consumed: false
            , position: s.position
            }
        result ->
          pure
            { input: Nil
            , result: result
            , consumed: true
            , position: Position { column: 1, line: 2}
            }

    parseMaybeValue :: List String -> UrlParser a -> MaybeMatch -> Either ParseError a
    parseMaybeValue input valuePrs maybeMatch = do
      value <- case input of
        Nil ->
          pure $ serializeNothing maybeMatch
        (v : Nil) ->
          pure v
        _ ->
          throwError (parseError "Maybe iso parser can handle only single value" 2)
      url <- note (parseError ("Incorrect uri encoded in query param value: \"" <> value <> "\"") 1) (parseURL value)
      runParser url valuePrs
     where
      parseError message column =
        ParseError { message: message, position: Position {column: column, line: 1}}

toSpineBoomerang ::
  forall r.
    RAlgArg SigF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
toSpineBoomerang (SigProdF _ (j@{ sigValues: (v : Nil) } :| (n@{ sigValues: Nil } : Nil))) =
  maybeIsoToSpineBoomerang j.sigConstructor n.sigConstructor v.a
toSpineBoomerang (SigProdF _ (n@{ sigValues: Nil } :| (j@{ sigValues: (v : Nil) } : Nil))) =
  maybeIsoToSpineBoomerang j.sigConstructor n.sigConstructor v.a
toSpineBoomerang (SigRecordF l) =
  -- add/drop record constructor
  maph SRecord ser <<<
  -- convert array <-> list
  arrayFromList <<<
  -- for every record field use appropriate boomerang with appropriate label
  -- and join this boomerangs into list
  fieldsBmg
 where
  step e r =
    cons <<< duck1 fieldBmg <<< r
   where
    fieldBmg =
      maph { recLabel: e.recLabel, recValue: _} (Just <<< _.recValue) <<< lazy <<< fromRecValue e.recLabel e.recValue

  ser (SRecord a) = Just a
  ser _ = Nothing

  fieldsBmg = foldr step nil l

  fromRecValue ::
    forall r'. String -> SigRecValueF { a :: UrlBoomerangForGenericSpine r', f :: Fix SigF } -> UrlBoomerangForGenericSpine r'
  fromRecValue key (SigRecOptionalValueF just nothing v@{ a: valueBmg }) =
    toQueryBoomerang key <<< toOptionalValueBoomerang just nothing $ valueBmg
  fromRecValue key (SigRecValueF v@{ a: valueBmg }) =
    toQueryBoomerang key <<< toSimpleValueBoomerang $ valueBmg

toSpineBoomerang (SigProdF _ cs@(h :| t)) =
  foldMap1 fromConstructor cs
 where
  -- fromConstructor :: DataConstructorF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
  fromConstructor constructor =
    bmg
   where
    valuesBmg =
      intersperce (liftStringBoomerang (lit "/")) <<< map (lazy <<< _) <<< map _.a <<< _.sigValues $ constructor
     where
      intersperce :: forall tok a t. (forall r. Boomerang tok r r) ->
                                     List (Boomerang tok t (a :- t)) ->
                                     Boomerang tok t (List a :- t)
      intersperce _ Nil = nil
      intersperce sep (Cons b t) =
        cons <<< duck1 b <<< foldr step nil t
       where
        step e l = sep <<< cons <<< duck1 e <<< l

    constructorBmg =
      maph prs ser <<< arrayFromList <<< valuesBmg
     where
      prs = SProd constructor.sigConstructor
      ser (SProd c values)
        | c == constructor.sigConstructor = Just values
        | otherwise = Nothing
      ser _ = Nothing

    constructorNameBmg =
      liftStringBoomerang (lit constructorName)
     where
      constructorName = serializeConstructorName constructor.sigConstructor

    bmg
      | null t = constructorBmg
      | null constructor.sigValues = constructorNameBmg <<< constructorBmg
      | otherwise = constructorNameBmg <<< liftStringBoomerang (lit "/") <<< constructorBmg
toSpineBoomerang SigBooleanF =
  maph SBoolean ser <<< boolean
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
toSpineBoomerang SigIntF =
  maph SInt ser <<< int
 where
  ser (SInt b) = Just b
  ser _        = Nothing
toSpineBoomerang SigStringF =
  maph SString ser <<< str
 where
  ser (SString s) = Just s
  ser _        = Nothing

parse :: forall a. UrlBoomerang HNil (a :- HNil) -> Url -> Maybe a
parse (Boomerang b) tok = do
  f <- hush (runParser tok (do
    r <- b.prs
    -- we have to consume whole input
    liftStringPrs eof
    pure r))
  pure (hHead (f hNil))

serialize :: forall a. UrlBoomerang HNil (a :- HNil) -> a -> Maybe Url
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  pure (f { path: "", query: empty })

bob :: forall a r. (Generic a) => Proxy a -> Maybe (UrlBoomerang r (a :- r))
bob p = do
  sb <- para toSpineBoomerang <$> (anaM fromGenericSignature (toSignature p))
  pure (maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< sb)
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

foreign import camelsToHyphens :: String -> String

serializeConstructorName :: String -> String
serializeConstructorName n =
  camelsToHyphens (fromMaybe n (Data.Array.last <<< split "." $ n))

genericToUrl :: forall a. (Generic a) => a -> Maybe String
genericToUrl a = do
  route <- bob (Proxy :: Proxy a)
  url <- serialize route a
  printURL url

genericFromUrl :: forall a. (Generic a) => String -> Maybe a
genericFromUrl s = do
  url <- parseURL s
  route <- bob (Proxy :: Proxy a)
  parse route url

data Router a = Router (UrlBoomerang HNil (a :- HNil))

router :: forall a. (Generic a) => Proxy a -> Maybe (Router a)
router p = do
  b <- bob p
  pure $ Router b

toUrl :: forall a. Router a -> a -> String
toUrl (Router bmg) a =
  unsafePartial (case serialize bmg a >>= printURL of Just s -> s)

fromUrl :: forall a. Router a -> String -> Maybe a
fromUrl (Router bmg) url =
  parseURL url >>= parse bmg
