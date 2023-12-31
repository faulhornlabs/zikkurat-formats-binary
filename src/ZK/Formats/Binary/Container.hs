
-- | Parsing the generic iden3 binary container format.
--
-- Cf. <https://github.com/iden3/binfileutils/>
--

{-# LANGUAGE StrictData, PackageImports #-}
module ZK.Formats.Binary.Container where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Word
import Data.List
import Data.Ord

import Control.Monad
import Control.Applicative

import System.IO

import Data.ByteString.Lazy (ByteString) ; import qualified Data.ByteString.Lazy as L

import "binary" Data.Binary.Get

import ZK.Formats.Helpers

--------------------------------------------------------------------------------

type Msg = String

--------------------------------------------------------------------------------

data GlobalHeader = GlobalHeader
  { _magicWord     :: Word32 
  , _magicString   :: String
  , _globalVersion :: Word32
  }
  deriving Show

magicString :: GlobalHeader -> String
magicString = magicWordToString . _magicWord

magicWordToString :: Word32 -> String
magicWordToString w = [ chr (fromIntegral (shiftR w (k*8) .&. 255)) | k <- [0..3] ]

data SectionHeader = SectionHeader
  { _sectionType :: Word32    -- ^ section id
  , _dataOffset  :: Word64    -- ^ offset of the section data in the file
  , _dataSize    :: Word64    -- ^ size of the section data
  }
  deriving Show

sortSectionHeaders :: [SectionHeader] -> [SectionHeader]
sortSectionHeaders = sortBy (comparing _sectionType)

readSection :: Handle -> SectionHeader -> IO ByteString
readSection h (SectionHeader _ ofs siz) = do
  hSeek h AbsoluteSeek (fromIntegral ofs)
  L.hGet h (fromIntegral siz)

data Container = Container
  { _globalHeader :: GlobalHeader
  , _sections     :: [SectionHeader]
  }
  deriving Show

--------------------------------------------------------------------------------

parseContainerFile :: FilePath -> IO (Either Msg Container)
parseContainerFile fname = 
  do
    h    <- openBinaryFile fname ReadMode 
    flen <- hFileSize h
    ei   <- readContainer h flen
    hClose h
    return ei

  where

    readContainer :: Handle -> Integer -> IO (Either Msg Container)
    readContainer h flen 
      | flen < 32  = return $ Left "file is too small"
      | otherwise  = do
          bytes <- L.hGet h 12
          let (magicWord,version,nsections) = flip runGet bytes ((,,) <$> getWord32le <*> getWord32le <*> getWord32le)
          let magicStr = magicWordToString magicWord
          let global = GlobalHeader magicWord magicStr version
          case all isAlphaNum magicStr of
            False -> return $ Left "expecting an alphanumeric magic word"
            True  -> do
              mbSections <- worker (fromIntegral nsections)
              return (Container <$> pure global <*> mbSections)

      where
        worker :: Int -> IO (Either Msg [SectionHeader])
        worker 0 = return (Right [])
        worker n = do
          pos <- hTell h
          if flen - pos < 12 then return (Left "unexpected end of file") else do
            bytes <- L.hGet h 12
            let (sectyp,seclen) = flip runGet bytes ((,) <$> getWord32le <*> getWord64le)
            let sect = SectionHeader 
                  { _sectionType = sectyp
                  , _dataOffset  = fromIntegral (pos + 12)
                  , _dataSize    = seclen
                  }
            hSeek h RelativeSeek (fromIntegral seclen)
            rest <- worker (n-1)
            return ((sect:) <$> rest)

--------------------------------------------------------------------------------

