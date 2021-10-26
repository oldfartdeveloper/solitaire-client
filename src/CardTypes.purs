module CardTypes
  ( Action(..)
  , Axis(..)
  , Card(..)
  , Color(..)
  , DCard(..)
  , DisplayMode(..)
  , Ext(..)
  , FaceDir(..)
  , Field(..)
  , GSt(..)
  , Pile(..)
  , PileType(..)
  , Rank(..) 
  , Suit(..)
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Enum (class Enum, toEnum)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Random.LCG (Seed)

-- CARD TYPES ------------------------------------------------------------------

data Rank    = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 

derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank
-- derive instance boundedRank :: Bounded Rank
-- derive instance genericRank :: Generic Rank _
-- derive instance enumRank :: Enum Rank

instance boundedRank :: Bounded Rank where
  bounded = bottom 

instance Show Rank where
  show RA  = "A"
  show R2  = "2"
  show R3  = "3"
  show R4  = "4"
  show R5  = "5"
  show R6  = "6"
  show R7  = "7"
  show R8  = "8"
  show R9  = "9"
  show R10 = [toEnum 0x2491] :: String -- unicode ligature for one-char width 
  show RJ  = "J"
  show RQ  = "Q"
  show RK  = "K"

data Suit    = Club | Diamond | Heart | Spade 

derive instance eqSuit :: Eq Suit
derive instance ordSuit :: Ord Suit
-- derive instance boundedSuit :: Bounded Suit
-- derive instance enumSuit :: Enum Suit

instance Show Suit where
  show Club    = [toEnum 0x2663] :: String -- unicode characters for suits
  show Diamond = [toEnum 0x2666] :: String 
  show Heart   = [toEnum 0x2665] :: String
  show Spade   = [toEnum 0x2660] :: String
  

data Card        = Card Rank Suit

derive instance eqCard :: Eq Card
-- derive instance showCard :: Show Card
derive instance ordCard :: Ord Card

instance Show Card where
  show card = (show card.rank) <> (show card.suit)

data FaceDir     = FaceUp | FaceDown

derive instance eqFaceDir :: Eq FaceDir
-- derive instance showFaceDir :: Show FaceDir
derive instance genericFaceDir :: Generic FaceDir _
derive instance ordFaceDir :: Ord FaceDir

instance showFaceDir :: Show FaceDir where -- FIX: need better output than string; do we use this?
  show = genericShow

data DCard       = DCard { _card    :: Card
                         , _facedir :: FaceDir }
                        
derive instance eqDCard :: Eq DCard                      
-- derive instance showDCard :: Show DCard                      
derive instance ordDCard :: Ord DCard

instance Show DCard where
  show dCard = (show dCard.card) <> (show dCard.facedir)

data DisplayMode = Stacked | Splayed

derive instance eqDisplayMode :: Eq DisplayMode
derive instance genericDisplayMode :: Generic DisplayMode _
derive instance ordDisplayMode :: Ord DisplayMode

instance showDisplayMode :: Show DisplayMode where
  show = genericShow

data PileType    = WasteP | TableP | FoundP

derive instance eqPileType :: Eq PileType
derive instance genericPileType :: Generic PileType _
derive instance ordPileType :: Ord PileType

instance showPileType :: Show PileType where
  show = genericShow

data Pile = Pile { _cards    :: List DCard  --   piles contain cards
                 , _display  :: DisplayMode -- , opinions on how to be drawn
                 , _rankBias :: Maybe Rank  -- , possibly opinions on base rank
                 , _suitBias :: Maybe Suit  -- , possibly opinions on base suit
                 , _pileType :: PileType    -- , and an identifier for location
                 }      

derive instance eqPile :: Eq Pile           -- since it makes canPlace simpler
-- derive instance showPile :: Show Pile

instance Show Pile where
  show pile =  show pile.cards
            <> show pile.display
            <> show pile.rankBias
            <> show pile.suitBias
            <> show pile.pileType

-- GAME TYPES ------------------------------------------------------------------

data Field = Field { _waste :: List Pile      -- fields are game wrappers for the
                   , _table :: List Pile      -- three board components
                   , _found :: List Pile
                   }

derive instance eqField :: Eq Field
-- derive instance showField :: Show Field

instance Show Field where
  show field =  show field.waste
             <> show field.tableau
             <> show field.found

                                             --   the gamestate is a record for the
data GSt = GSt { _field   :: Field           --   field as seen above
               , _seed    :: Effect Seed     -- , and a random seed to be passed thru
               , _history :: List (Tuple Field Int)  -- , and a list of previous fields
               , _score   :: Int
               , _moves   :: Int
               }

-- derive instance showGSt :: Show GSt    

instance Show GSt where
  show gst =  show gst.field
           <> show gst.seed
           <> show gst.history
           <> show gst.score
           <> show gst.moves

-- DISPLAY TYPES ---------------------------------------------------------------

data Color       = Red | Black                       -- deriving (Eq, Show, Ord)

derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color
derive instance genericColor :: Generic Color _

instance showColor :: Show Color where
  show = genericShow

data Axis = NS | EW -- data type for pile splay orientation

derive instance eqAxis :: Eq Axis
derive instance genericAxis :: Generic Axis _
  
instance showAxis :: Show Axis where
  show = genericShow

data Action = New | Undo -- deriving (Eq, Show, Ord) -- data type for button action

derive instance eqAction :: Eq Action
derive instance genericAction :: Generic Action _
derive instance ordAction :: Ord Action

instance showAction :: Show Action where
  show = genericShow

data Ext = WasteX | TableX | FoundX -- named extents for click regions
         | IdX Int | DCX DCard | ActionX Action

derive instance eqExt :: Eq Ext         
derive instance genericExt :: Generic Ext _        
derive instance ordExt :: Ord Ext

instance showExt :: Show Ext where
  show = genericShow
