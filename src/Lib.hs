module Lib
    ( someFunc
    ) where

import Euterpea as E
import System.Random

someFunc :: IO ()
someFunc = E.playDev 0 $ duet (20, 60, 50) (70, 80, 70) (10, 30, 40)

duet :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Music (Pitch, Volume)
duet (minVol1, minVol2, minVol3) (maxVol1, maxVol2, maxVol3) (threshold1, threshold2, threshold3) = do
--  let pitches1 = pitches [60, 62, 63, 65, 67, 68, 70, 72, 74, 75, 77, 79] -- c4minor
  let pitches1 = pitches [60, 62, 63, 66, 67, 68, 71, 72, 74, 75, 78, 79] -- c4 hungarian
      pitches2 = pitches [36, 36, 43, 43, 46, 48]
      pitches3 =
        pitches [60, 60, 60, 60, 63, 63, 63, 66, 66, 66, 67, 67, 60 + 12, 60 + 12, 63 + 12, 66 + 12, 67 + 12, 70 + 12]
      mel1 = randomMel pitches1 [qn, en, en, en] minVol1 maxVol1 threshold1 (mkStdGen 500)
      mel2 = randomMel pitches2 [hn, qn, qn, qn] minVol2 maxVol2 threshold2 (mkStdGen 501)
      mel3 = randomMel pitches3 [wn, wn, wn, wn, wn, hn, hn, qn] minVol3 maxVol3 threshold3 (mkStdGen 502)
  E.tempo
    0.9 -- global tempo times 0.9
    (E.tempo 2 -- double melody tempo
      (instrument Marimba mel1 :=:
      instrument E.AcousticBass mel2 :=:
      instrument TremoloStrings mel3) :=:
      --instrument StringEnsemble2 mel3) :=:
    E.tempo 0.5 (percKick E.BassDrum1 :=: -- half percussion tempo
    percKick E.AcousticBassDrum :=:
    percSnare2 E.ElectricSnare :=:
    percSnare E.ElectricSnare :=:
    percHat E.ClosedHiHat :=:
    percOh E.OpenHiHat :=:
    percSnare E.AcousticSnare))

randomMel :: [AbsPitch] -> [Dur] -> Int -> Int -> Int -> StdGen -> Music (Pitch, Volume)
randomMel pitches durs minVol maxVol thresh g0 =
  let (p, g1) = choose pitches g0
      (d, g2) = choose durs g1
      (v, g3) = randomR (0, 127) g2
      x =
        if v < thresh
          then rest d
          else note d (pitch p, min maxVol (max v minVol))
   in x :+: randomMel pitches durs thresh minVol maxVol g3

percKick :: PercussionSound -> Music (Pitch, Volume)
percKick ps = addVolume 85
  (perc ps (wn/4) :+:

  rest (wn/4) :+:

  rest (wn / 8) :+:
  perc ps (wn / 8)) :+:

  rest (wn/4) :+: percKick ps

percSnare :: PercussionSound -> Music (Pitch, Volume)
percSnare ps = addVolume 80 (rest (wn/4) :+: perc ps (wn/4) :+: rest (wn/4) :+: perc ps (wn/4)) :+: percSnare ps

percSnare2 :: PercussionSound -> Music (Pitch, Volume)
percSnare2 ps =
  addVolume
    80
    (rest (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16) :+:

    rest (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16) :+:

    rest (wn / 16) :+:
    perc ps (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16) :+:

    rest (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16) :+:
    rest (wn / 16)) :+:
  percSnare2 ps

percHat :: PercussionSound -> Music (Pitch, Volume)
percHat ps =
  addVolume
    70
    (perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32) :+:

    perc ps (wn / 32) :+:
    perc ps (wn / 32) :+:
    perc ps (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:

    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:

    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32) :+:
    rest (wn / 32) :+:
    perc ps (wn / 32) :+:
    perc ps (wn / 32) :+:
    rest (wn / 32)) :+:
  percHat ps

percOh :: PercussionSound -> Music (Pitch, Volume)
percOh ps = addVolume 70
    (rest (wn/8) :+:
    rest (wn/8) :+:
    rest (wn/8) :+:
    rest (wn/8) :+:
    rest (wn/8) :+:
    rest (wn/8) :+:
    rest (wn/8) :+:
    perc ps (wn/8)) :+:
    percOh ps

choose :: [a] -> StdGen -> (a, StdGen)
choose [] g = error "nothing to choose"
choose xs g =
  let (i, g') = next g
   in (xs !! (i `mod` length xs), g')

pitches :: [Int] -> [AbsPitch]
pitches y = y
