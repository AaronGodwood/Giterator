-- | The transform form. Every field goes through the same parser as its CLI
-- flag, so the TUI and CLI accept exactly the same inputs.
module Tui.Form
  ( FormInput (..)
  , emptyInput
  , mkForm
  , PreviewSpec (..)
  , specPlan
  , buildSpec
  ) where

import Brick (Padding (..), Widget, hLimit, padRight, str, (<+>))
import Brick.Forms
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Git.Rewrite (Plan)
import Lens.Micro (Lens', lens)
import Transform.Content
import Transform.Time
import Tui.Name

data FormInput = FormInput
  { fiDates       :: Which
  , fiSpread      :: Text
  , fiShift       :: Text
  , fiJitter      :: Text
  , fiSeed        :: Text
  , fiWeekdays    :: Bool
  , fiWorkHours   :: Text
  , fiTz          :: Text
  , fiKeepClock   :: Bool
  , fiReplace     :: Text
  , fiReplaceRegex :: Text
  , fiPath        :: Text
  , fiDelete      :: Text
  , fiRename      :: Text
  , fiPrune       :: Bool
  }
  deriving (Eq, Show)

emptyInput :: FormInput
emptyInput = FormInput Both "" "" "" "giterator" False "" "" False "" "" "" "" "" False

data PreviewSpec = PreviewSpec
  { psTime    :: TimeOptions
  , psContent :: ContentOptions
  , psPrune   :: Bool
  }

specPlan :: PreviewSpec -> Plan
specPlan s = contentPlan (psContent s) <> timePlan (psTime s)

-- | Per-field errors, or Nothing when the form asks for no change at all.
buildSpec :: FormInput -> Either [(Name, String)] (Maybe PreviewSpec)
buildSpec fi
  | not (null errors) = Left errors
  | not (timeActive || contentActive) = Right Nothing
  | otherwise = Right (Just (PreviewSpec time content (fiPrune fi)))
  where
    spread = field FSpread parseRange (fiSpread fi)
    shift = field FShift parseDuration (fiShift fi)
    jitter' = field FJitter parseDuration (fiJitter fi)
    window = field FWorkHours parseWindow (fiWorkHours fi)
    tz = field FTz parseTz (fiTz fi)
    replace = field FReplace literalReplacement (fiReplace fi)
    replaceRegex = field FReplaceRegex regexReplacement (fiReplaceRegex fi)
    rename = field FRename parseArrow (fiRename fi)
    errors =
      concat
        [ problem spread, problem shift, problem jitter', problem window, problem tz
        , problem replace, problem replaceRegex, problem rename
        ]

    value = either (const Nothing) id
    time =
      TimeOptions
        { toWhich = fiDates fi
        , toSpread = value spread
        , toShift = value shift
        , toJitter = value jitter'
        , toSeed = utf8 (T.unpack (fiSeed fi))
        , toWeekdays = fiWeekdays fi
        , toWindow = value window
        , toTz = (,if fiKeepClock fi then KeepWallClock else KeepInstant) <$> value tz
        }
    content =
      ContentOptions
        { coReplace = [r | Just r <- [value replace, value replaceRegex]]
        , coPaths = glob (fiPath fi)
        , coDelete = glob (fiDelete fi)
        , coRename = [r | Just r <- [value rename]]
        }
    timeActive =
      fiWeekdays fi
        || isJust (value spread)
        || isJust (value shift)
        || isJust (value jitter')
        || isJust (value window)
        || isJust (value tz)
    contentActive = not (null (coReplace content) && null (coDelete content) && null (coRename content))

    glob t = [utf8 (T.unpack s) | let s = T.strip t, not (T.null s)]

-- | A blank field means "not set"; otherwise it must parse.
field :: Name -> (String -> Either String a) -> Text -> Either (Name, String) (Maybe a)
field name parse t
  | T.null s = Right Nothing
  | otherwise = either (\e -> Left (name, e)) (Right . Just) (parse (T.unpack s))
  where
    s = T.strip t

problem :: Either e a -> [e]
problem = either pure (const [])

mkForm :: FormInput -> Form FormInput e Name
mkForm =
  newForm
    [ label "Dates" @@= radioField (lens fiDates (\s v -> s {fiDates = v})) [(Author, FDatesAuthor, "author"), (Committer, FDatesCommitter, "committer"), (Both, FDatesBoth, "both")]
    , label "Spread" @@= text (lens fiSpread (\s v -> s {fiSpread = v})) FSpread
    , label "Shift" @@= text (lens fiShift (\s v -> s {fiShift = v})) FShift
    , label "Jitter" @@= text (lens fiJitter (\s v -> s {fiJitter = v})) FJitter
    , label "Seed" @@= text (lens fiSeed (\s v -> s {fiSeed = v})) FSeed
    , label "" @@= checkboxField (lens fiWeekdays (\s v -> s {fiWeekdays = v})) FWeekdays "weekdays only"
    , label "Work hours" @@= text (lens fiWorkHours (\s v -> s {fiWorkHours = v})) FWorkHours
    , label "Timezone" @@= text (lens fiTz (\s v -> s {fiTz = v})) FTz
    , label "" @@= checkboxField (lens fiKeepClock (\s v -> s {fiKeepClock = v})) FKeepClock "keep wall clock"
    , label "Replace" @@= text (lens fiReplace (\s v -> s {fiReplace = v})) FReplace
    , label "Regex" @@= text (lens fiReplaceRegex (\s v -> s {fiReplaceRegex = v})) FReplaceRegex
    , label "Only paths" @@= text (lens fiPath (\s v -> s {fiPath = v})) FPath
    , label "Delete" @@= text (lens fiDelete (\s v -> s {fiDelete = v})) FDelete
    , label "Rename" @@= text (lens fiRename (\s v -> s {fiRename = v})) FRename
    , label "" @@= checkboxField (lens fiPrune (\s v -> s {fiPrune = v})) FPrune "drop commits left empty"
    ]
  where
    text :: Lens' FormInput Text -> Name -> FormInput -> FormFieldState FormInput e Name
    text l name = editTextField l name (Just 1)
    label :: String -> Widget Name -> Widget Name
    label s w = hLimit 12 (padRight Max (str s)) <+> w
