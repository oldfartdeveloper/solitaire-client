module CardTypes
  -- ( Action(..)
  -- , Axis(..)
  -- , Card(..)
  -- , Color(..)
  -- , DCard(..)
  -- , DisplayMode(..)
  -- , Ext(..)
  -- , FaceDir(..)
  -- , Field(..)
  -- , GSt(..)
  -- , Pile(..)
  -- , PileType(..)
  -- , ProductType(..) -- temporary
  -- , showProductType -- temporary
  -- , Rank(..) 
  -- , Suit(..)
  -- )
  where

import Prelude

-- import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Char (toCharCode, fromCharCode)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Show (show)
import Data.Enum (class Enum, toEnum)
import Data.List (List)
import Data.Maybe (Maybe, fromMaybe)
import Data.String.CodePoints (singleton)
import Data.Tuple (Tuple)
import Effect (Effect)
import Random.LCG (Seed)

-- EXPERIMENT:

data ProductType = ProductType String Int

derive instance genericProductType :: Generic ProductType _

instance showProductType :: Show ProductType where
  show = genericShow

-- CARD TYPES ------------------------------------------------------------------

data Rank    = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 

derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank
-- derive instance boundedRank :: Bounded Rank
-- derive instance genericRank :: Generic Rank _
-- derive instance enumRank :: Enum Rank

-- instance boundedRank :: Bounded Rank where
--   bottom = genericBottom :: Rank
--   top = genericTop :: Rank

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
  show R10 = showCodePoint 0x2491 -- unicode ligature for one-char width 
  show RJ  = "J"
  show RQ  = "Q"
  show RK  = "K"

data Suit    = Club | Diamond | Heart | Spade 

derive instance eqSuit :: Eq Suit
derive instance ordSuit :: Ord Suit
-- derive instance boundedSuit :: Bounded Suit
-- derive instance enumSuit :: Enum Suit

instance Show Suit where
  show Club    = showCodePoint 0x2663
  show Diamond = showCodePoint 0x2666
  show Heart   = showCodePoint 0x2665
  show Spade   = showCodePoint 0x2660
  

data Card        = Card Rank Suit

derive instance eqCard :: Eq Card
derive instance genericCard :: Generic Card _
derive instance ordCard :: Ord Card

instance showCard :: Show Card where
  -- show = genericShow                -- Generic show yields "(Card ⒑ ♦" 
  show :: Card -> String
  show (Card r s) = show r <> show s   -- Custom show yields "⒑♦; used to display cards in game"

data FaceDir     = FaceUp | FaceDown

derive instance eqFaceDir :: Eq FaceDir
derive instance genericFaceDir :: Generic FaceDir _
derive instance ordFaceDir :: Ord FaceDir

instance showFaceDir :: Show FaceDir where
  show = genericShow

data DCard       = DCard { _card    :: Card
                         , _facedir :: FaceDir }
                        
derive instance eqDCard :: Eq DCard  
derive instance genericDCard :: Generic DCard _                    
derive instance ordDCard :: Ord DCard

instance showDCard :: Show DCard where
  show = genericShow

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
derive instance genericPile :: Generic Pile _

instance Show Pile where
  show = genericShow

-- GAME TYPES ------------------------------------------------------------------

data Field = Field { _waste :: List Pile      -- fields are game wrappers for the
                   , _table :: List Pile      -- three board components
                   , _found :: List Pile
                   }

derive instance eqField :: Eq Field
derive instance genericField :: Generic Field _

instance showField :: Show Field where
  show = genericShow

                                             --   the gamestate is a record for the
data GSt = GSt { _field   :: Field           --   field as seen above
               , _seed    :: Effect Seed     -- , and a random seed to be passed thru
               , _history :: List (Tuple Field Int)  -- , and a list of previous fields
               , _score   :: Int
               , _moves   :: Int
               }

derive instance genericGSt :: Generic GSt _

-- instance showGSt :: Show GSt where -- FIXME: can't show seed w/ genericShow
--   show = genericShow

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

showCodePoint :: Int -> String
showCodePoint codePoint = fromMaybe "" (map singleton $ toEnum codePoint)
