{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
module Nix.LicenseType where

import Nix.Common

data LicenseType
  = GPL
  | GPLv2
  | GPLv2Plus
  | GPLv3
  | GPLv3Plus
  | Free
  | FreeCopyleft
  | UnfreeRedistributable
  | Unfree
  | UnfreeRedistributableFirmware
  deriving (Show, Eq)
