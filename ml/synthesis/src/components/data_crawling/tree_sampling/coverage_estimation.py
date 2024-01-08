from __future__ import annotations

from math import exp, log


def estimate_tree_coverage_based_on_collision_ratio(collision_ratio: float, n_nodes: int) -> float | None:
    """
    This is a bit sketchy yet state-of-the-art ad hoc heuristic estimation of tree coverage based on collision ratio.

    It's based on the following hypotheses:
        1) tree coverage is in pure functional (mathematical) dependency on collision ratio
        2) this dependency is tree-independent and is explained purely by random descent algorithm definition,
           probability theory and statistics.

    All approximation functions and coefficients are derived empirically from a set of sampling runs.
    All formulas are arbitrarily chosen to mathematically describe empirical data as close as my math skills allow.

    Error is estimated to be up to 10-15%, but it may depend on the tree peculiarities.

    UPD: found not suitable for large trees (leading to very small cron value) :(
    More data and runs needed, but I'm out of time.
    """
    # collision ratio in percents
    cr = collision_ratio * 100
    n = n_nodes  # number of unique nodes sampled from the tree
    cron = cr / n  # short for "cr over n". like cr, but independent of n.

    if cron < 0.02:
        return None

    # nr - tree coverage, i.e. number of nodes sampled from the tree in this run

    # goal - find "approx(cron*nr)", i.e. approximate value of cron*nr
    # empirical data shows that approximation function heavily depends on whether cr > 100% or not

    # approximation function for cr < 100%, found empirically
    approx_lt100 = -0.019 * log(cron) - 0.0394

    # approximation function for cr > 100%, found empirically
    approx_gt100 = 1.0225 * cron - 0.0288

    # we will blend these two functions with a sigmoid
    blender_width_coef = 0.02  # found empirically
    blender_center = 100.0
    blender_value = 1 / (1 + exp(-blender_width_coef * (cr - blender_center)))

    # blend the two approximation functions (as sigmoid goes 0..1, we go lt100..gt100)
    approx = approx_lt100 * (1 - blender_value) + approx_gt100 * blender_value

    # (approx = cron * nr)
    approx_nr = max(0, min(1, approx / cron))

    return approx_nr
