module Time.SeqDur where

import           Data.Foldable (toList)
import           Data.List     (intercalate)
import           Data.Sequence
import qualified Data.Sequence as S
import           Time.Dur


type Durs = Seq Dur

showDurs :: Seq Dur -> String
showDurs durs = "Durs[ " <> (intercalate ", " . fmap (show . unDur) . toList $ durs) <> "]"

frontAdd :: Dur -> Durs -> Durs
frontAdd d ds = d <| ds

endAdd :: Dur -> Durs -> Durs
endAdd d ds = ds |> d

concatSeq :: Durs -> Durs -> Durs
concatSeq = (><)

leftMost :: Durs -> Maybe Dur
leftMost ds = case viewl ds of
  EmptyL -> Nothing
  d :< _ -> Just d

rightMost :: Durs -> Maybe Dur
rightMost ds = case viewr ds of
  EmptyR -> Nothing
  _ :> d -> Just d

getAtIndex :: Int -> Durs -> Maybe Dur
getAtIndex i ds = if i < S.length ds then Just (S.index ds i) else Nothing

modifyAtIndex :: Int -> (Dur -> Dur) -> Durs -> Durs
modifyAtIndex i f = adjust' f i

setAtIndex :: Int -> Dur -> Durs -> Durs
setAtIndex = update

insertElemAt :: Int -> Dur -> Durs -> Durs
insertElemAt = insertAt

deleteElemAt :: Int -> Durs -> Durs
deleteElemAt = deleteAt

-- examples

seqDurs :: Durs
seqDurs = fromList [1 / 2, 1 / 6, 1 / 6, 1 / 6, 1 / 4]

seqDurs' :: Durs
seqDurs' = seqDurs |> (5 %/ 6)
