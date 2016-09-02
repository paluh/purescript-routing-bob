module Routing.Bob where

import Prelude
import Data.Array as Data.Array
import Control.Error.Util (note, hush)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Except (except, ExceptT)
import Control.Monad.Except.Trans (runExceptT, withExceptT)
import Control.Monad.State (runState, State)
import Control.Monad.State.Class (put, get)
import Control.Monad.Trans (lift)
import Data.Array (uncons)
import Data.Foldable (class Foldable, foldr, foldlDefault, foldrDefault, fold)
import Data.Generic (class Generic, GenericSignature(SigRecord, SigString, SigBoolean, SigInt, SigProd), GenericSpine(SRecord, SString, SInt, SBoolean, SProd), toSpine, fromSpine, toSignature)
import Data.Identity (Identity)
import Data.List (List(Cons, Nil), null, fromFoldable, concatMap, (:))
import Data.Maybe (fromJust, Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.NonEmpty (fromNonEmpty, NonEmpty, (:|), foldMap1)
import Data.StrMap (insert, pop, StrMap, empty)
import Data.String (split)
import Data.Traversable (class Traversable, traverse, for)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Routing.Bob.UrlBoomerang (Url, printURL, parseURL, int, str, boolean, liftStringBoomerang, liftStringPrs, UrlBoomerang)
import Text.Boomerang.Combinators (pureBmg, maph, nil, cons, duck1)
import Text.Boomerang.HStack (hSingleton, hNil, hHead, HNil, type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), Boomerang(Boomerang), runSerializer)
import Text.Boomerang.String (lit)
import Text.Parsing.Parser (ParseError(ParseError), PState(PState), ParserT(ParserT), runParser)
import Text.Parsing.Parser.String (eof)
import Type.Proxy (Proxy(Proxy))

newtype Fix f = Fix (f (Fix f))

-- convertion between types and urls is done through
-- basic recursion schemes - cata and ana
-- don't be afraid it's nothing really fancy - it's just fold and unfold
-- but this strategy simplifies code a lot
unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix f) = f

cata :: forall a f. Functor f => (f a -> a) -> Fix f -> a
cata alg = alg <<< (cata alg <$> _) <<< unFix

type RAlgArg f a =  f { f :: Fix f, a :: a }
type RAlg f a = RAlgArg f a -> a
para :: forall a f. Functor f => RAlg f a -> Fix f -> a
para rAlg =
  rAlg <<< (g <$> _) <<< unFix
 where
  g f = let a = (para rAlg f) in { f, a }

ana :: forall a f. Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix <<< (ana coalg <$> _) <<< coalg

anaM :: forall a f m. (Functor f, Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
anaM coalgM = ((map Fix) <<< traverse (anaM coalgM)) <=< coalgM

-- subset of GenericSignature which is covered by this library
type DataConstructorF r = { sigConstructor :: String, sigValues :: List r}

data SigF r
  = SigProdF String (NonEmpty List (DataConstructorF r))
  | SigRecordF (NonEmpty List ({ recLabel :: String, recValue :: r }))
  | SigBooleanF
  | SigIntF
  | SigStringF

instance functorSigF :: Functor SigF where
  map f (SigProdF s l) = SigProdF s (map (\r -> r { sigValues = map f r.sigValues }) l)
  map f (SigRecordF l) = SigRecordF (map (\r -> r { recValue = f r.recValue }) l)
  map _ SigBooleanF = SigBooleanF
  map _ SigIntF = SigIntF
  map _ SigStringF = SigStringF

instance foldableSigF :: Foldable SigF where
  foldMap f (SigProdF s l) =
    fold <<< concatMap (\r -> map f r.sigValues) <<< (fromNonEmpty (:)) $ l
  foldMap f (SigRecordF l) =
    fold <<< map (\r -> f r.recValue) <<< (fromNonEmpty (:)) $ l
  foldMap _ _ = mempty
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableSigF :: Traversable SigF where
  traverse f (SigProdF s l) =
    SigProdF s <$> for l (\r -> r { sigValues = _ } <$> for r.sigValues f)
  traverse f (SigRecordF l) =
    SigRecordF <$> for l (\r -> r { recValue = _ } <$> f r.recValue)
  traverse _ SigBooleanF = pure SigBooleanF
  traverse _ SigIntF = pure SigIntF
  traverse _ SigStringF = pure SigStringF
  sequence = traverse id

fromGenericSignature :: GenericSignature -> Maybe (SigF GenericSignature)
fromGenericSignature (SigProd s a) = do
  {head: h, tail: t} <- uncons <<< map fromConstructor $ a
  pure $ SigProdF s (h :| fromFoldable t)
 where
  fromConstructor c = c { sigValues = fromFoldable $ map (_ $ unit) c.sigValues }
fromGenericSignature (SigRecord a) = do
  {head: h, tail: t} <- uncons <<< map fromField $ a
  pure $ SigRecordF (h :| fromFoldable t)
 where
  fromField f = f { recValue = f.recValue unit }
fromGenericSignature SigInt = Just SigIntF
fromGenericSignature SigBoolean = Just SigBooleanF
fromGenericSignature SigString = Just SigStringF
fromGenericSignature _ = Nothing

type UrlBoomerangForGenericSpine r = UrlBoomerang r (GenericSpine :- r)

lazy :: forall z y. UrlBoomerang (y :- z) ((Unit -> y) :- z)
lazy = maph const (Just <<< (_ $ unit))

arrayFromList :: forall t a tok. Boomerang tok (List a :- t) (Array a :- t)
arrayFromList =
  maph arrayFromFoldable (Just <<< fromFoldable)
 where
  -- very ineficient - will be replaced with next purescript-array release
  arrayFromFoldable = foldr Data.Array.cons []

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

      parse' :: ExceptT ParseError (State (StrMap (Maybe String))) (r -> a :- r)
      parse' = do
        query <- lift get
        value <-
          -- XXX: any type isomorphic to maybe should be handled here
          case parseMaybeIso f of
            Just c ->
              case pop name query of
                -- XXX: don't use arbitrary empty value encoding: ""
                --      serialize Nothing to get this value
                Nothing -> pure (serializeNothing c.nothing c.sigF)
                Just (Tuple Nothing query') -> do
                  lift (put query')
                  pure (serializeNothing c.nothing c.sigF)
                Just (Tuple (Just v) query') -> do
                  lift (put query')
                  pure v
            Nothing -> do
              Tuple maybeValue query' <- note' ("Mising param: " <> name <> ".") (pop name query)
              lift (put query')
              note' ("Param required: " <> name <> ".") maybeValue
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
    Serializer $ \v -> case runSerializer valueBmg.ser v of
      Just (Tuple sf rest) ->
        let
          url = sf { path: "", query: empty }
          v' =
            unsafePartial $
              case printURL url of
                Just s -> s
          r' = pure (Tuple (\r -> r { query = insert name (Just v') r.query}) rest)
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
    -- param' l { f, a } =
    --     (SProd "Data.Maybe" v) ->

    --     otherwise -> param l v

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
