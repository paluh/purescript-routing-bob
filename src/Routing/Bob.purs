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
import Data.Either (Either(Left))
import Data.Foldable (foldr)
import Data.Generic (class Generic, GenericSpine(SString, SInt, SBoolean, SRecord, SProd), toSpine, fromSpine, toSignature)
import Data.Identity (Identity)
import Data.List (List(Cons, Nil), null, singleton, head, fromFoldable, (:))
import Data.Maybe (fromJust, Maybe(Just, Nothing), fromMaybe)
import Data.NonEmpty (foldMap1, (:|))
import Data.StrMap (insert, pop, StrMap, empty)
import Data.String (split)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Routing.Bob.Boomerangs (arrayFromList, lazy)
import Routing.Bob.RecursionSchemes (anaM, para, RAlgArg, Fix(Fix))
import Routing.Bob.Signature (fromGenericSignature, SigF(SigStringF, SigProdF, SigIntF, SigBooleanF, SigRecordF))
import Routing.Bob.UrlBoomerang (Url, printURL, parseURL, int, str, boolean, liftStringBoomerang, liftStringPrs, UrlBoomerang)
import Text.Boomerang.Combinators (pureBmg, maph, nil, cons, duck1)
import Text.Boomerang.HStack (hSingleton, hNil, hHead, HNil, type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), Boomerang(Boomerang), runSerializer)
import Text.Boomerang.String (lit)
import Text.Parsing.Parser (fail, ParseError(ParseError), PState(PState), ParserT(ParserT), runParser)
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

-- maybeIsoBmg :: valueBmg -
-- 
-- 
-- -- parse' :: forall a r. _ -> String -> Fix SigF -> ExceptT String (State (StrMap (List String))) (r -> a :- r)
-- parse' prs name (Fix f) = do
--   query <- lift get
--   value <-
--     -- XXX: any type isomorphic to maybe should be handled here
--     case parseMaybeIso f of
--       Just c ->
--         case pop name query of
--           -- XXX: don't use arbitrary empty value encoding: ""
--           --      serialize Nothing to get this value
--           Nothing -> pure "" -- (serializeNothing c.nothing c.sigF)
--           Just (Tuple Nil query') -> do
--             lift (put query')
--             pure "" -- (serializeNothing c.nothing c.sigF)
--           Just (Tuple (v : Nil) query') -> do
--             lift (put query')
--             pure v
--           Just (Tuple vs@(v : v' : _) query') ->
--             throwError (parseError ("Multiple values for param: " <> name <> " (" <> show vs <> ")"))
--       Nothing -> do
--         Tuple valuesList query' <- note' ("Mising param: " <> name <> ".") (pop name query)
--         lift (put query')
--         note' ("Param required: " <> name <> ".") (head valuesList)
--         -- XXXX: Add similar case as above for multiple params
--         --  Just (Tuple vs@(v : v' : _) query') ->
--         --    throwError ("Multiple values for param: " <> name <> " (" <> show vs <> ")")
--   url <- note' ("Incorrect uri encoded in param: " <> name <> ".") (parseURL value)
--   withExceptT
--     (\(ParseError pe) ->
--         (parseError ("Fail to parse param \"" <> name <> "\": " <> pe.message <> ".")))
--     (except $ (runParser url prs))
--  where
--   note':: forall m s. (Monad m) => String -> (Maybe s) -> ExceptT String m s
--   note' m v =
--     except $ note m v
--   parseError message = message

-- maybeValuePrs :: Fix SigF -> UrlBoomerang r (a :- r) -> UrlBoomerang r (a :- r)
-- maybeValuePrs :: forall a. _ -> ParserT String Identity a -> ParserT (Maybe (List String)) Identity a
-- maybeValuePrs (Fix f) valuePrs =
--   case matchMaybeProdF f of
--     Just m -> prs m
--     Nothing -> fail "Production doesn't match a maybe structure"
--  where
--   matchMaybeProdF sigF@(SigProdF _ (j@{ sigValues: (v : Nil) } :| (n@{ sigValues: Nil } : Nil))) =
--     Just { sigF: sigF, just: j.sigConstructor, nothing: n.sigConstructor, value: v}
--   matchMaybeProdF sigF@(SigProdF _ (n@{ sigValues: Nil } :| (j@{ sigValues: (v : Nil) } : Nil))) =
--     Just { sigF: sigF, just: j.sigConstructor, nothing: n.sigConstructor, value: v}
--   matchMaybeProdF _ = Nothing
-- 
--   serializeNothing constructorName sigF =
--     let sb = para toSpineBoomerang (Fix sigF)
--     in unsafePartial $ fromJust $ (serialize sb (SProd constructorName [])) >>= printURL
-- 
--   prs :: _ -> ParserT (Maybe (List String)) Identity a
--   prs maybeMatch =
--     ParserT prs
--    where
--     prs (PState s) =
--       let
--         result = do
--           rawValue <- case s.input of
--             Nothing ->
--               pure $ serializeNothing maybeMatch.nothing maybeMatch.sigF
--             Just Nil ->
--               pure $ serializeNothing maybeMatch.nothing maybeMatch.sigF
--             Just (v : Nil) ->
--               pure v
--             Just _ ->
--               throwError
--                 { message: "Maybe iso parser can handle only single value"
--                 , position: s.position
--                 }
--           url <- note' ("Incorrect uri encoded in param: " <> name <> ".") (parseURL value)
--           runParser url rawValue
--       in
--         { input: (Nothing :: Maybe (List String))
--         , result
--         , consumed: true
--         , position: s.position
--         }

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


-- parse' :: forall a r. _ -> String -> Fix SigF -> ExceptT String (State (StrMap (List String))) (r -> a :- r)
-- parse' prs name (Fix f) = do


-- XXX: split this monster
-- make query parameter boomerang
-- @name - variable query name (name1=value1&name2=value2)
-- @{a, f} - a is boomerang for given value and f is current
--           node in SigF expression tree because we
--           are using (check paramorphism)
param :: forall a r. String -> { a :: UrlBoomerang r (a :- r), f :: Fix SigF } -> UrlBoomerang r (a :- r)
param name { a: (Boomerang valueBmg), f: Fix f } =
  Boomerang
    { prs: prs
    , ser: ser
    }
 where
  parseMaybeIso sigF@(SigProdF _ (j@{ sigValues: (v : Nil) } :| (n@{ sigValues: Nil } : Nil))) =
    Just { sigF: sigF, just: j.sigConstructor, nothing: n.sigConstructor, value: v}
  parseMaybeIso sigF@(SigProdF _ (n@{ sigValues: Nil } :| (j@{ sigValues: (v : Nil) } : Nil))) =
    Just { sigF: sigF, just: j.sigConstructor, nothing: n.sigConstructor, value: v}
  parseMaybeIso _ = Nothing

  serializeNothing c sigF =
    let sb = para toSpineBoomerang (Fix sigF)
    in unsafePartial $ fromJust $ (serialize sb (SProd c [])) >>= printURL

  prs :: ParserT Url Identity (r -> a :- r)
  prs =
    ParserT prs'
   where
    prs' :: PState Url -> Identity { input :: Url, result :: Either ParseError (r -> a :- r), consumed :: Boolean, position :: Position }
    prs' (PState s) =
      toParserTResult <<< flip runState s.input.query <<< runExceptT $ parse'
     where
      toParserTResult (Tuple result query) =
        pure
          { input: s.input { query = query }
          , result
          , consumed: false
          , position: s.position
          }

      parse' :: ExceptT ParseError (State (StrMap (List String))) (r -> a :- r)
      parse' = do
        query <- lift get
        value <-
          -- XXX: any type isomorphic to maybe should be handled here
          case parseMaybeIso f of
            Just c ->
              case pop name query of
                -- XXX: don't use arbitrary empty value encoding: ""
                --      serialize Nothing to get this value
                Nothing -> pure "" -- (serializeNothing c.nothing c.sigF)
                Just (Tuple Nil query') -> do
                  lift (put query')
                  pure "" -- (serializeNothing c.nothing c.sigF)
                Just (Tuple (v : Nil) query') -> do
                  lift (put query')
                  pure v
                Just (Tuple vs@(v : v' : _) query') ->
                  throwError (parseError ("Multiple values for param: " <> name <> " (" <> show vs <> ")"))
            Nothing -> do
              Tuple valuesList query' <- note' ("Mising param: " <> name <> ".") (pop name query)
              lift (put query')
              note' ("Param required: " <> name <> ".") (head valuesList)
              -- XXXX: Add similar case as above for multiple params
              --  Just (Tuple vs@(v : v' : _) query') ->
              --    throwError ("Multiple values for param: " <> name <> " (" <> show vs <> ")")
        url <- note' ("Incorrect uri encoded in param: " <> name <> ".") (parseURL value)
        withExceptT
          (\(ParseError pe) ->
              (parseError ("Fail to parse param \"" <> name <> "\": " <> pe.message <> ".")))
          (except $ (runParser url valueBmg.prs))
       where
        note':: forall m s. (Monad m) => String -> (Maybe s) -> ExceptT ParseError m s
        note' m v =
          except $ note (parseError m) v

        parseError message = ParseError { message, position: s.position }

  ser :: Serializer Url (a :- r) r
  ser =
    Serializer $ \v ->
      case runSerializer valueBmg.ser v of
        Just (Tuple sf rest) ->
          let
            url = sf { path: "", query: empty }
            v' =
              unsafePartial $
                case printURL url of
                  Just s -> s
            r' = pure (Tuple (\r -> r { query = insert name (singleton v') r.query}) rest)
          in
            case parseMaybeIso f of
              Just c ->
                if v' == serializeNothing c.nothing c.sigF
                  then
                    pure (Tuple id rest)
                  else
                    r'
              Nothing -> r'
        Nothing -> Nothing


toSpineBoomerang ::
  forall r.
    RAlgArg SigF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
toSpineBoomerang (SigProdF _ (j@{ sigValues: (v : Nil) } :| (n@{ sigValues: Nil } : Nil))) =
  maybeIsoToSpineBoomerang j.sigConstructor n.sigConstructor v.a
toSpineBoomerang (SigProdF _ (n@{ sigValues: Nil } :| (j@{ sigValues: (v : Nil) } : Nil))) =
  maybeIsoToSpineBoomerang j.sigConstructor n.sigConstructor v.a
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
toSpineBoomerang (SigRecordF l) =
  maph SRecord ser <<< arrayFromList <<< foldr step nil l
 where
  step e r =
    cons <<< duck1 fieldBmg <<< r
   where
    fieldBmg =
      maph { recLabel: e.recLabel, recValue: _} (Just <<< _.recValue) <<< lazy <<< param e.recLabel e.recValue
  ser (SRecord a) = Just a
  ser _ = Nothing
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
