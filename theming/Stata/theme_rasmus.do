/*******************************************************************************
Rasmus Duret
theme_rasmus.do - graph scheme / colour constants matching the shared palette
defined in theming/palette.yaml. Values here must be kept in sync with that
file by hand (Stata cannot parse YAML natively).

STATUS: placeholder - theming/palette.yaml is not finalized yet, so the values
below are stand-ins. Fill in theming/palette.yaml first, then mirror the real
values here.

Usage: `do "theming/Stata/theme_rasmus.do"` near the top of a script to define
the globals below, then reference them in graph commands, e.g.
    line y x, lcolor("$rasmus_c1")
*******************************************************************************/

global rasmus_c1 "TODO1"
global rasmus_c2 "TODO2"
global rasmus_c3 "TODO3"
global rasmus_c4 "TODO4"

global rasmus_positive "TODO"
global rasmus_negative "TODO"
global rasmus_neutral  "TODO"
