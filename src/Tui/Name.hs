-- | Brick resource names: one per scrollable area and one per form input.
module Tui.Name
  ( Name (..)
  ) where

data Name
  = CommitList
  | Details
  | FDatesAuthor
  | FDatesCommitter
  | FDatesBoth
  | FSpread
  | FShift
  | FJitter
  | FSeed
  | FWeekdays
  | FWorkHours
  | FTz
  | FKeepClock
  | FReplace
  | FReplaceRegex
  | FPath
  | FDelete
  | FRename
  | FPrune
  deriving (Eq, Ord, Show)
