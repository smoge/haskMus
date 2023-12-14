-- Modified from:
-- Copyright (C) 2007 Bart Massey

{- | This module implements reading and writing of the most
  common kinds of WAVE files.  WAVE files are Microsoft
  RIFF audio sample files originally based on the AIFF
  format, and commonly have the .wav filename extension.
  This module currently supports reading and writing
  single-section little-endian PCM audio files containing
  up to 32-bit samples encoded according to the well-known WAVE
  sample encoding.  The interface audio stream format is a
  list of frames of 32-bit (`Int32`) left-justified signed
  PCM samples; each frame has one sample per channel.  The
  audio reader and writer are sufficiently lazy that files
  larger than memory can be processed.
-}
module Data.WAVE (
    WAVE (..),
    WAVEHeader (..),
    WAVESample,
    WAVESamples,
    hGetWAVE,
    getWAVEFile,
    hPutWAVE,
    putWAVEFile,
    sampleToDouble,
    doubleToSample,
) where

import Control.Monad (unless, when)
import Data.Bits (Bits (shift, (.&.)))
import qualified Data.ByteString.Lazy as BS
import Data.Char (chr)
import Data.Int (Int32, Int8)

--import Data.List hiding (words)
import Data.Word (Word8)
import System.IO (
    BufferMode (BlockBuffering),
    Handle,
    IOMode (ReadMode, WriteMode),
    SeekMode (RelativeSeek),
    hClose,
    hPutStr,
    hSeek,
    hSetBinaryMode,
    hSetBuffering,
    openFile,
 )

-- TODO 
-- Change String to Text
-- import qualified Data.Text as T

{- | For internal use only; the header as it appears on-disk.
  The interface cleans this up to remove redundancy and
  make things easier to understand.
-}
data WAVERawHeader = WAVERawHeader
    { rawNumChannels :: Int     -- Number of channels in the audio
    , rawSampleRate :: Int      -- Sample rate of the audio
    , rawByteRate :: Int        -- Byte rate of the audio
    , rawBlockAlign :: Int      -- Block alignment of the audio
    , rawBitsPerSample :: Int   -- Bits per sample of the audio
    , rawFrames :: Maybe Int    -- Number of frames in the audio stream (if present)
    }

-- | Descriptive information for the audio source.
data WAVEHeader = WAVEHeader
    { -- | Samples per frame.
      waveNumChannels :: Int
    , -- | Frames per second.
      waveFrameRate :: Int
    , -- | Number of
      --  significant bits of left-justified value.
      waveBitsPerSample :: Int
    , -- | If present,
      --  number of frames in the stream.
      --  Otherwise, can be (inefficiently)
      --  inferred from the length of the
      --  stream.
      waveFrames :: Maybe Int
    }

{-
TODO: why not use Float?
type WAVESample = Float

import Data.Binary.IEEE754 (wordToFloat, floatToWord)
import Data.Word (Word32)

convertWordToFloat :: Word32 -> Float
convertWordToFloat = wordToFloat

convertFloatToWord :: Float -> Word32
convertFloatToWord = floatToWord
-}


{- | Each sample is a left-justified signed integer, with
  significant bits as given in the header.
-}
type WAVESample = Int32

{- | A stream is a list of frames, each of which is a list of
  samples with one sample per channel.
-}
type WAVESamples = [[WAVESample]]

-- | The header and stream read or written.
data WAVE = WAVE
    { waveHeader :: WAVEHeader
    , waveSamples :: WAVESamples
    }

bits_to_bytes :: (Integral a) => a -> a
bits_to_bytes n = (n + 7) `div` 8

collect :: Int -> [a] -> [[a]]
collect _ [] = []
collect n s = h : collect n s'
  where
    (h, s') = splitAt n s

{- | Utility routine for working with audio data in floating
  point format.

  Convert a sample value to a Double. This function normalizes the sample value
  to the range -1.0 to 1.0 for ease of use in floating-point computations.
-}
sampleToDouble :: WAVESample -> Double
sampleToDouble v =
    let maxb = toInteger (maxBound :: WAVESample)
        minb = toInteger (minBound :: WAVESample)
     in if v >= 0
            then fromInteger (toInteger v) / fromInteger maxb
            else -fromInteger (toInteger v) / fromInteger minb

{- | Utility routine for working with audio data in floating
  point format.
-}
doubleToSample :: Double -> WAVESample
doubleToSample v =
    let maxb = toInteger (maxBound :: WAVESample)
        minb = toInteger (minBound :: WAVESample)
     in if v >= 0
            then fromInteger (floor (v * fromInteger maxb))
            else fromInteger (ceiling (v * fromInteger minb))

{-|
   Convert a `ByteString` to a `String`.
-}
bs_to_string :: BS.ByteString -> String
bs_to_string b = fmap (chr . fromIntegral) (BS.unpack b)

{-|
   Check if the given `Handle` matches the provided string `s`.
   If not, throw an error with a mismatched format string message.
-}
match :: Handle -> String -> IO ()
match h s = do
    b <- BS.hGet h (length s)
    unless
        (bs_to_string b == s)
        (error ("mismatched format string '" <> (s <> "'")))

{-|
   Convert a list of `Word8` values to a number of type `a`.
   The type `a` must be an instance of the `Num` class.
-}
convert_nbytes_lend :: (Num a) => [Word8] -> a
convert_nbytes_lend bs =
    foldl accum 0 (reverse bs)
  where
    accum a b = 256 * a + fromIntegral b

{-|
   Read `n` bytes from the given `Handle`, convert them to a number and return the result.
-}
get_nbytes_lend :: Handle -> Int -> IO Int
get_nbytes_lend h n = do
    bytes <- BS.hGet h n
    pure (convert_nbytes_lend (BS.unpack bytes))

{-|
   Read a 4-byte word from the given `Handle` and return the result.
-}
get_word_lend :: Handle -> IO Int
get_word_lend h = get_nbytes_lend h 4

{-|
   Read a 2-byte halfword from the given `Handle` and return the result.
-}
get_halfword_lend :: Handle -> IO Int
get_halfword_lend h = get_nbytes_lend h 2

{-|
   Read the wave header from the given `Handle` and return it.
-}
get_wave_header :: Handle -> IO WAVERawHeader
get_wave_header h = do
    size <- get_word_lend h
    audio_format <- get_halfword_lend h
    unless
        (audio_format == 1)
        (error "PCM only for now")
    unless
        (size == 16)
        (error "bad PCM chunk size")
    num_channels <- get_halfword_lend h
    frame_rate <- get_word_lend h
    byte_rate <- get_word_lend h
    block_align <- get_halfword_lend h
    bits_per_sample <- get_halfword_lend h
    pure
        ( WAVERawHeader
            { rawNumChannels = num_channels
            , rawSampleRate = frame_rate
            , rawByteRate = byte_rate
            , rawBlockAlign = block_align
            , rawBitsPerSample = bits_per_sample
            , rawFrames = Nothing
            }
        )

skip_chunk :: Handle -> IO ()
skip_chunk h = do
    size <- get_word_lend h
    hSeek h RelativeSeek (fromIntegral size)

get_wave_data :: Handle -> WAVERawHeader -> IO (WAVERawHeader, WAVESamples)
get_wave_data h hd = do
    size <- get_word_lend h
    let bits_per_sample = rawBitsPerSample hd
    let bytes_per_sample = bits_to_bytes bits_per_sample
    when
        (rawBlockAlign hd /= bytes_per_sample * rawNumChannels hd)
        (error "internal error: align and bits disagree")
    let frames = size `div` rawBlockAlign hd
    let count = frames * rawNumChannels hd
    samples <- case bytes_per_sample of
        1 -> do
            bytes <- BS.hGet h count
            pure (fmap convert_byte (BS.unpack bytes))
        n | n <= 4 -> do
            bytes <- BS.hGet h (count * n)
            let words = collect n (BS.unpack bytes)
            pure (fmap (convert_multibyte n) words)
        _ -> error "max 32 bits per sample for now"
    let samples' = fmap (mask bits_per_sample) samples
    pure
        ( hd{rawFrames = Just frames}
        , collect (rawNumChannels hd) samples'
        )
  where
    convert_byte =
        (`shift` 24)
            . (fromIntegral :: Int8 -> WAVESample)
            . (fromIntegral :: Word8 -> Int8)
    convert_multibyte n =
        (`shift` (32 - 8 * n))
            . (convert_nbytes_lend :: [Word8] -> WAVESample)
    mask bits v =
        v .&. (((1 `shift` bits) - 1) `shift` (32 - bits))

cook_header :: WAVERawHeader -> WAVEHeader
cook_header
    ( WAVERawHeader
            { rawNumChannels = nc
            , rawSampleRate = sr
            , rawBitsPerSample = bps
            , rawBlockAlign = ba
            , rawFrames = Just s
            }
        ) =
        WAVEHeader
            { waveNumChannels = nc
            , waveFrameRate = sr
            , waveBitsPerSample = bps
            , waveFrames = Just s
            }
cook_header
    ( WAVERawHeader
            { rawNumChannels = nc
            , rawSampleRate = sr
            , rawBitsPerSample = bps
            , rawBlockAlign = ba
            , rawFrames = Nothing
            }
        ) =
        WAVEHeader
            { waveNumChannels = nc
            , waveFrameRate = sr
            , waveBitsPerSample = bps
            , waveFrames = Nothing
            }


{- 

-- Replace get_chunks with attoparsec????
import Data.Attoparsec.ByteString (Parser, anyWord8, word8, string, take, parseOnly)
import Data.Attoparsec.Binary (anyWord32le, anyWord16le, anyWord32be, anyWord16be)

parseChunkHeader :: Parser String
parseChunkHeader = do
    header <- take 4
    return $ BS.unpack header

parseFmtChunk :: Parser WAVERawHeader
parseFmtChunk = do
    size <- anyWord32le
    audioFormat <- anyWord16le
    unless (audioFormat == 1) (fail "PCM only for now")
    unless (size == 16) (fail "bad PCM chunk size")
    numChannels <- anyWord16le
    sampleRate <- anyWord32le
    byteRate <- anyWord32le
    blockAlign <- anyWord16le
    bitsPerSample <- anyWord16le
    return $ WAVERawHeader numChannels sampleRate byteRate blockAlign bitsPerSample Nothing

parseDataChunk :: WAVERawHeader -> Parser WAVESamples
parseDataChunk header = do
    let bytesPerSample = bits_to_bytes (rawBitsPerSample header)
    let bytesPerFrame = bytesPerSample * rawNumChannels header
    let frames = bytesPerFrame `div` rawBlockAlign header
    let count = frames * rawNumChannels header
    samples <- replicateM count anyWord8
    let samples' = map (mask (rawBitsPerSample header)) samples
    return $ collect (rawNumChannels header) samples'

parseWAVE :: Parser WAVE
parseWAVE = do
    _ <- string "RIFF"
    _ <- anyWord32le -- file size
    _ <- string "WAVE"
    _ <- string "fmt "
    _ <- anyWord32le -- chunk size
    header <- parseFmtChunk
    _ <- string "data"
    dataSize <- anyWord32le
    samples <- parseDataChunk header
    return $ WAVE (cook_header header) samples

getWAVE :: BS.ByteString -> Either String WAVE
getWAVE bs = parseOnly parseWAVE bs

-- Usage example:
-- wav <- getWAVE <$> BS.readFile "path/to/wave/file.wav"
-- case wav of
--     Left err -> putStrLn $ "Error parsing WAVE file: " ++ err
--     Right wave -> print wave

 -}


get_chunks :: Handle -> Maybe WAVERawHeader -> Maybe WAVESamples -> IO WAVE
get_chunks _ (Just hd) (Just s) =
    pure
        WAVE
            { waveHeader = cook_header hd
            , waveSamples = s
            }
get_chunks h mh ms = do
    s <- get_chunk_header
    process_chunk s mh ms
  where
    get_chunk_header = do
        bs <- BS.hGet h 4
        pure (bs_to_string bs)
    process_chunk "fmt " Nothing Nothing = do
        nh <- get_wave_header h
        get_chunks h (Just nh) ms
    process_chunk "fmt " _ _ =
        error "duplicate fmt chunk in WAVE"
    process_chunk "data" (Just nh) Nothing = do
        (nh', nd) <- get_wave_data h nh
        get_chunks h (Just nh') (Just nd)
    process_chunk "data" _ _ =
        error "no fmt chunk or duplicate data chunk in WAVE"
    process_chunk _ nh ms = do
        skip_chunk h
        get_chunks h nh ms

-- | Read the WAVE file at the given handle and return the audio data.
hGetWAVE :: Handle -> IO WAVE
hGetWAVE h = do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering Nothing)
    match h "RIFF"
    size <- get_word_lend h
    match h "WAVE"
    get_chunks h Nothing Nothing

-- | Read the WAVE file at the given path and return the audio data.
getWAVEFile :: FilePath -> IO WAVE
getWAVEFile fn = do
    h <- openFile fn ReadMode
    wav <- hGetWAVE h
    hClose h
    pure wav

unconvert_nbytes_lend :: Int -> Int -> [Word8]
unconvert_nbytes_lend 0 _ = []
unconvert_nbytes_lend n v =
    fromIntegral (v .&. 255) :
    unconvert_nbytes_lend (n - 1) (v `shift` (-8))

put_nbytes_lend :: Handle -> Int -> Int -> IO ()
put_nbytes_lend h n v = do
    let bytes = BS.pack (unconvert_nbytes_lend n v)
    BS.hPut h bytes

put_word_lend :: Handle -> Int -> IO ()
put_word_lend h = put_nbytes_lend h 4

put_halfword_lend :: Handle -> Int -> IO ()
put_halfword_lend h = put_nbytes_lend h 2

put_wave_header :: Handle -> WAVEHeader -> IO ()
put_wave_header h hd = do
    put_halfword_lend h 1 --- PCM
    let num_channels = waveNumChannels hd
    put_halfword_lend h num_channels
    let frame_rate = waveFrameRate hd
    put_word_lend h frame_rate
    let bytes_per_sample = bits_to_bytes (waveBitsPerSample hd)
    let block_align = bytes_per_sample * num_channels
    let byte_rate = frame_rate * block_align
    put_word_lend h byte_rate
    put_halfword_lend h block_align
    put_halfword_lend h (waveBitsPerSample hd)

put_wave_data :: Handle -> WAVEHeader -> [WAVESample] -> IO ()
put_wave_data h hd sa = do
    let nb = bits_to_bytes (waveBitsPerSample hd)
    when
        (nb <= 0 || nb > 4)
        (error "supported sample sizes 1..32 bits for now")
    let saa = fmap (`shift` (8 * nb - 32)) sa
    let ba =
            if nb == 1
                then fmap (fromIntegral . (.&. 255) . (+ 128)) saa
                else concatMap (unconvert_nbytes_lend nb . fromIntegral) saa
    let bytes = BS.pack ba
    BS.hPut h bytes

-- | Write the given audio data to the given handle as a WAVE file.
hPutWAVE :: Handle -> WAVE -> IO ()
hPutWAVE h wav = do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering Nothing)
    ---
    let header = waveHeader wav
    let samples = waveSamples wav
    let frame_count = case waveFrames header of
            Just n -> n
            Nothing -> length samples
    let frame_samples = frame_count * waveNumChannels header
    let header_size = 2 + 2 + 4 + 4 + 2 + 2
    let bytes_per_sample = bits_to_bytes (waveBitsPerSample header)
    let data_size = frame_samples * bytes_per_sample
    ---
    hPutStr h "RIFF"
    put_word_lend h (4 + header_size + 8 + data_size + 8)
    hPutStr h "WAVE"
    ---
    hPutStr h "fmt "
    put_word_lend h header_size
    put_wave_header h header
    ---
    hPutStr h "data"
    put_word_lend h data_size
    put_wave_data h header (concat samples)

-- | Write the given audio data to the given path as a WAVE file.
putWAVEFile :: FilePath -> WAVE -> IO ()
putWAVEFile fn wav = do
    h <- openFile fn WriteMode
    hPutWAVE h wav
    hClose h