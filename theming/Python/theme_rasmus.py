"""
Rasmus Duret
theme_rasmus - matplotlib rcParams + colour palette matching the shared
palette defined in theming/palette.yaml. Values here must be kept in sync
with that file by hand.

STATUS: placeholder - theming/palette.yaml is not finalized yet, so the
values below are stand-ins. Fill in theming/palette.yaml first, then mirror
the real values here.

Usage:
    from theme_rasmus import apply_theme, QUALITATIVE
    apply_theme()
"""

QUALITATIVE = ["#TODO1", "#TODO2", "#TODO3", "#TODO4"]

POSITIVE = "#TODO"
NEGATIVE = "#TODO"
NEUTRAL = "#TODO"

FONT_FAMILY = "TODO"


def apply_theme():
    import matplotlib.pyplot as plt

    plt.rcParams.update({
        "font.family": FONT_FAMILY,
        "axes.prop_cycle": plt.cycler(color=QUALITATIVE),
        "axes.grid": True,
        "grid.alpha": 0.3,
        "axes.spines.top": False,
        "axes.spines.right": False,
    })
