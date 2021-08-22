{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib where

import Data.Csv -- hiding (decode, decodeWith, decodeByName, decodeByNameWith)
-- import Data.Csv.Parser.Megaparsec (decode, decodeWith, decodeByName, decodeByNameWith)
import GHC.Generics (Generic)
import Data.Char (ord)
import Data.Text as Text (Text, splitOn)
import Data.Function ((&))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Search as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.HashMap.Strict as Hash (HashMap, fromList, lookup)
import Data.Hashable (Hashable)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.String.Conversions (cs)
import Data.Either (partitionEithers)

-- NOTE: http://download.geonames.org/export/dump/

-- TYPES -----------------------------------

data Region = Region
  { geonameid :: Text
  , latitude :: Double
  , longitude :: Double
  , name :: Text
  , countryCode :: Code Country
  , provinceCode :: Code Province
  , countryName :: Text
  , provinceName :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToRecord Region


newtype Code a = Code { unCode :: Text }
  deriving (Show, Eq, Hashable, Generic, FromField, ToField)

data Geoname = Geoname
  { geonameid :: Text
  , name :: Text
  , latitude :: Double
  , longitude :: Double
  , countryCode :: Code Country
  , admin1Code :: Code Province
  } deriving (Show, Eq)

instance FromRecord Geoname where
  parseRecord v = Geoname
    <$> v .! 0
    <*> v .! 1
    <*> v .! 4
    <*> v .! 5
    <*> v .! 8
    <*> v .! 10



data Province = Province
  { countryCode :: Code Country
  , provinceCode :: Code Province
  , name :: Text
  } deriving (Show, Eq)

-- try the generic instance
instance FromRecord Province where
  parseRecord v = do
    -- split oh the "."
    code <- v .! 0
    name <- v .! 1
    (c, p) <- parseDualCode code
    pure $ Province (Code c) (Code p) name
    where
      parseDualCode t = 
        case Text.splitOn "." t of
          [c, p] -> pure (c, p)
          x -> fail $ "Could not parse dual code: " ++ show x


data Country = Country
  { code :: Code Country
  , name :: Text
  } deriving (Show, Eq)

instance FromRecord Country where
  parseRecord v = Country
    <$> v .! 0
    <*> v .! 4


dualCode :: Code Country -> Code Province -> Code (Country, Province)
dualCode (Code c) (Code p) = Code $ c <> "." <> p



-- DECODING -----------------------------------
decodeOptions =
  defaultDecodeOptions
    { decDelimiter = fromIntegral (ord '\t') }

encodeOptions =
  defaultEncodeOptions
    { encDelimiter = fromIntegral (ord '\t') }





readClean :: FilePath -> IO BSL.ByteString
readClean f = do
  s <- BSL.readFile f
  pure $ preprocess $ s

preprocess :: BSL.ByteString -> BSL.ByteString
preprocess s = BSL.replace "\"" ("\'" :: BSL.ByteString) s


-- | The countries file has a bunch of comments at the top prefixed by "#"
-- I don't know if it's safe to tread all the files 
dropComments :: BSL.ByteString -> BSL.ByteString
dropComments s =
    BSL.lines s
  & filter (not . BSL.isPrefixOf ("#"))
  & BSL.unlines


parseGeodump :: FromRecord a => BSL.ByteString -> IO (Vector a)
parseGeodump s = do
  case decodeWith decodeOptions NoHeader s of
    Left e -> fail $ show e
    Right rs -> pure rs

-- download the database?
loadProvinces :: IO (Vector Province)
loadProvinces =
  readClean "data/admin1CodesASCII.txt" >>= parseGeodump


loadCities :: IO (Vector Geoname)
loadCities = do
  readClean "data/cities500.txt" >>= parseGeodump


loadCountries :: IO (Vector Country)
loadCountries = do
  -- only this file can safely be treated as Char8
  s <- dropComments <$> readClean "data/countryInfo.txt" 
  parseGeodump s


countryMap :: Vector Country -> HashMap (Code Country) Country
countryMap cs = Hash.fromList $ Vector.toList $ fmap toItem cs
  where toItem c = (c.code, c)

provinceMap :: Vector Province -> HashMap (Code (Country, Province)) Province
provinceMap cs = Hash.fromList $ Vector.toList $ fmap toItem cs
  where toItem c = (dualCode c.countryCode c.provinceCode, c)





data RegionError
  = NoCountry Geoname
  deriving (Show)

-- we can't leave out regions because they don't have a province

regions :: HashMap (Code Country) Country -> HashMap (Code (Country, Province)) Province -> [Geoname] -> ([RegionError], [Region])
regions hcs hps gs =
  partitionEithers $ map region gs
  
  where

    region :: Geoname -> Either RegionError Region
    region g = do

      let p = lookupProvince g

      -- fail if the country isn't found
      c <- lookupCountry g

      pure $ Region
        { geonameid = g.geonameid
        , latitude = g.latitude
        , longitude = g.longitude
        , name = g.name
        , provinceCode = g.admin1Code
        , provinceName = (.name) <$> p
        , countryCode = c.code
        , countryName = c.name
        }

    lookupCountry :: Geoname -> Either RegionError Country
    lookupCountry g = 
      maybe (Left $ NoCountry g) Right $ Hash.lookup g.countryCode hcs 

    lookupProvince :: Geoname -> Maybe Province
    lookupProvince g =
      let dual = dualCode g.countryCode g.admin1Code
       in Hash.lookup dual hps 







generateRegions :: IO ()
generateRegions = do
  putStrLn "Generating Regions..."
  hcs <- countryMap <$> loadCountries
  hps <- provinceMap <$> loadProvinces
  gs <- loadCities

  let (errs, rs) = regions hcs hps (Vector.toList gs)

  mapM_ print errs
  putStrLn "-----------------"
  mapM_ print $ take 10 rs

  let out = encodeWith encodeOptions rs
  BSL.writeFile "data/regions.tsv" out




{- ADMIN CODES

    The main 'geoname' table has the following fields :
    ---------------------------------------------------
    geonameid         : integer id of record in geonames database
    name              : name of geographical point (utf8) varchar(200)
    asciiname         : name of geographical point in plain ascii characters, varchar(200)
    alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
    latitude          : latitude in decimal degrees (wgs84)
    longitude         : longitude in decimal degrees (wgs84)
    feature class     : see http://www.geonames.org/export/codes.html, char(1)
    feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
    country code      : ISO-3166 2-letter country code, 2 characters
    cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
    admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
    admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
    admin3 code       : code for third level administrative division, varchar(20)
    admin4 code       : code for fourth level administrative division, varchar(20)
    population        : bigint (8 byte int) 
    elevation         : in meters, integer
    dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
    timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
    modification date : date of last modification in yyyy-MM-dd format


    AdminCodes:
    Most adm1 are FIPS codes. ISO codes are used for US, CH, BE and ME. UK and Greece are using an additional level between country and fips code. The code '00' stands for general features where no specific adm1 code is defined.
    The corresponding admin feature is found with the same countrycode and adminX codes and the respective feature code ADMx.

-}

