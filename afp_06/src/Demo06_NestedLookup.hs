module Demo06_NestedLookup where

import qualified Data.Map as M


type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = 
   Honest_Bobs_Phone_Network
 | Morrisas_Marvelous_Mobiles
 | Petes_Plutocratic_Phones
 deriving (Eq, Ord)

findCarrierBillingAddress, 
  fCBAversion1, fCBAversion2, fCBAversion3, fCBAversion4, fCBAversion5 :: 
     PersonName
  -> M.Map PersonName PhoneNumber
  -> M.Map PhoneNumber MobileCarrier
  -> M.Map MobileCarrier BillingAddress
  -> Maybe BillingAddress
findCarrierBillingAddress = undefined


fCBAversion1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap
            
fCBAversion2 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address            

fCBAversion3 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap

fCBAversion4 person phoneMap carrierMap addressMap = 
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where lookup :: Ord k => M.Map k v -> k -> Maybe v 
        lookup = flip M.lookup

fCBAversion5 person phoneMap carrierMap addressMap = 
    lookup addressMap =<< lookup carrierMap =<< lookup phoneMap person
  where lookup :: Ord k => M.Map k v -> k -> Maybe v 
        lookup = flip M.lookup
        
