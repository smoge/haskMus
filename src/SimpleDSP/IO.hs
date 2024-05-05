{-# LANGUAGE ImportQualifiedPost #-}

module SimpleDSP.IO where

import Data.ByteString (toStrict)
import Data.ByteString.Internal (toForeignPtr0)
import Data.Vector.Storable (unsafeFromForeignPtr0)
import Foreign (Storable (sizeOf))
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import SimpleDSP.Samples
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Typed (proc, readProcessStdout)

decodeFile :: FilePath -> IO Samples
decodeFile fname = do
  (ExitSuccess, pcmBuf) <- readProcessStdout $ proc "ffmpeg" args
  let (wordPtr, wordSZ) = toForeignPtr0 (toStrict pcmBuf)
      pcmPtr = castForeignPtr wordPtr :: ForeignPtr Float
      pcmSZ = wordSZ `div` sizeOf (0 :: Float)
      arr = unsafeFromForeignPtr0 pcmPtr pcmSZ
  pure arr
  where
    args =
      ["-hide_banner", "-loglevel", "info"] -- quiet
        <> ["-i", fname] -- input
        <> ["-ac", "1"] -- convert to mono
        <> ["-ar", "44100"] -- sample at 44100 (fit for 60 fps)
        <> ["-f", "f32le"] -- use float
        <> ["-"] -- output
