"""
Gene expression brain maps on the Desikan-Killiany (DK) surface atlas.

Extracts Allen Human Brain Atlas (AHBA) microarray expression with `abagen`
and renders single-gene cortical maps on the fsaverage5 surface with `nilearn`.

Why this replaces the earlier notebook approach
------------------------------------------------
The previous notebook built the DK parcellation by hand from FreeSurfer
`aparc.annot` files and offset the right hemisphere by +34. That had two bugs:
  1. `corpuscallosum` (a medial-wall, non-cortical label) was NOT masked, so it
     was painted as a real parcel.
  2. the +34 offset collided (LH insula == RH bankssts), merging two regions.
Both distorted the parcel borders relative to the abagen paper (Fig. 4c).

Here we instead use abagen's own, officially-supported DK *surface* atlas
(`fetch_desikan_killiany(surface=True)`), which masks the medial wall correctly
(label 0) and gives 68 cortical parcels (LH ids 1-34, RH ids 42-75) with no
collisions and no dependency on a local FreeSurfer install.

Usage
-----
    from gene_brain_map_dk import compute_expression, plot_gene_expression
    expr = compute_expression()                       # downloads AHBA once, caches
    plot_gene_expression("GRIN2A", expr)              # full map
    plot_gene_expression("GRIN2A", expr, threshold=0.5)  # only strongly expressed
"""

import os
import tempfile
import warnings

import numpy as np
import pandas as pd
import nibabel as nib
import matplotlib.pyplot as plt

import abagen
from abagen.images import check_atlas
from nilearn import datasets, plotting
from nilearn.surface import load_surf_mesh

warnings.filterwarnings("ignore")

HERE = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(HERE, "..", "outputs", "gene_maps")
EXPR_CACHE = os.path.join(OUT_DIR, "dk_surface_expression.csv")


# ---------------------------------------------------------------------------
# Atlas helpers
# ---------------------------------------------------------------------------
def _write_surf_gii(mesh, path):
    """Write an fsaverage geometry (coords, faces) to a .surf.gii file."""
    coords, faces = load_surf_mesh(mesh)
    darrs = [
        nib.gifti.GiftiDataArray(coords.astype(np.float32),
                                 intent="NIFTI_INTENT_POINTSET"),
        nib.gifti.GiftiDataArray(faces.astype(np.int32),
                                 intent="NIFTI_INTENT_TRIANGLE"),
    ]
    nib.save(nib.GiftiImage(darrays=darrs), path)
    return path


def load_dk_surface_atlas(template="fsaverage5"):
    """abagen DK *surface* atlas, ready for both extraction and plotting.

    Uses abagen's DK surface label files (medial wall = 0, LH ids 1-34, RH ids
    42-75; no collisions), but hands abagen a per-vertex label *vector* plus the
    fsaverage geometry. That mirrors the extraction path the original notebook
    used, and avoids abagen 0.1.3's GIFTI-labeltable parser (which calls the
    pandas<2 `DataFrame.append` and errors on modern pandas).

    Returns a dict with:
      info     : DataFrame (id, label, hemisphere, structure)
      labels   : (lh_labels, rh_labels) per-vertex int arrays (0 = medial wall)
      atlas_obj: abagen atlas object (label vector + fsaverage geometry)
    """
    dk = abagen.fetch_desikan_killiany(surface=True)
    info = pd.read_csv(dk["info"])
    lh = nib.load(dk["image"][0]).darrays[0].data.astype(np.int32)
    rh = nib.load(dk["image"][1]).darrays[0].data.astype(np.int32)
    labels_vec = np.hstack([lh, rh])

    # fsaverage geometry -> gifti (abagen needs vertex coordinates to place
    # AHBA tissue samples). pial surface, same standard mesh as the labels.
    fs = datasets.fetch_surf_fsaverage(template)
    tmp = tempfile.mkdtemp(prefix="dk_geom_")
    lh_surf = _write_surf_gii(fs["pial_left"], os.path.join(tmp, "lh.surf.gii"))
    rh_surf = _write_surf_gii(fs["pial_right"], os.path.join(tmp, "rh.surf.gii"))

    atlas_obj = check_atlas(labels_vec, geometry=(lh_surf, rh_surf),
                            space="fsaverage")
    return {"info": info, "labels": (lh, rh), "atlas_obj": atlas_obj}


# ---------------------------------------------------------------------------
# Expression extraction
# ---------------------------------------------------------------------------
def compute_expression(atlas=None, cache=EXPR_CACHE, recompute=False,
                       probe_selection="max_intensity",
                       donor_probes="aggregate"):
    """Regional AHBA expression on the DK surface atlas (region-by-gene).

    The first call downloads ~4 GB of AHBA microarray data (cached by abagen in
    ~/abagen-data) and is slow; the resulting region-by-gene matrix is cached to
    `cache` as CSV so later calls are instant. Expression values are abagen's
    default scaled-robust-sigmoid normalised, i.e. in (0, 1).
    """
    if atlas is None:
        atlas = load_dk_surface_atlas()
    if cache and os.path.isfile(cache) and not recompute:
        return pd.read_csv(cache, index_col=0)

    expr = abagen.get_expression_data(
        atlas["atlas_obj"],
        probe_selection=probe_selection, donor_probes=donor_probes)

    os.makedirs(os.path.dirname(cache), exist_ok=True)
    expr.to_csv(cache)
    return expr


# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------
def _parcels_to_vertices(values_by_id, labels):
    """Map a {region_id: value} series onto a per-vertex float array.

    Vertices whose region has no value (medial wall, or masked below a
    threshold) are set to NaN so nilearn renders them as background.
    """
    out = np.full(labels.shape, np.nan, dtype=float)
    for rid, val in values_by_id.items():
        if np.isnan(val):
            continue
        out[labels == rid] = val
    return out


def plot_gene_expression(gene, expr, atlas=None, threshold=None,
                         cmap="viridis", surf="infl", views=("lateral", "medial"),
                         vmin=None, vmax=None, save=True, out_dir=OUT_DIR,
                         darkness=0.5):
    """Plot one gene's DK-parcellated expression on the fsaverage5 surface.

    Parameters
    ----------
    gene : str
        Column in `expr` (e.g. "GRIN2A").
    expr : DataFrame
        Region-by-gene matrix from `compute_expression` (index = DK region id).
    threshold : float or None
        If given, parcels with expression < threshold are greyed out (shown as
        background), so only the most-expressed regions are coloured. Because
        abagen values are in (0, 1), threshold=0.5 highlights the upper half.
    cmap, surf, views, vmin, vmax, darkness :
        Passed through to nilearn's surface plotting.

    Returns the matplotlib Figure (2 hemispheres x len(views) panels).
    """
    if atlas is None:
        atlas = load_dk_surface_atlas()
    if gene not in expr.columns:
        raise KeyError(f"{gene!r} not in expression matrix "
                       f"(have {expr.shape[1]} genes).")

    info = atlas["info"]
    lh_lab, rh_lab = atlas["labels"]
    fs = datasets.fetch_surf_fsaverage("fsaverage5")

    # per-region values mapped onto vertices (medial wall stays NaN -> shown as
    # background). Thresholding is handled natively by plot_surf_stat_map below.
    vals = expr[gene].astype(float)
    if vmin is None:
        vmin = 0.0 if threshold is None else float(threshold)
    if vmax is None:
        vmax = float(np.nanmax(expr[gene]))

    # split region values by hemisphere and map onto vertices
    ctx = info[info.structure == "cortex"]
    lh_ids = ctx[ctx.hemisphere == "L"].id.values
    rh_ids = ctx[ctx.hemisphere == "R"].id.values
    lh_map = _parcels_to_vertices({i: vals.get(i, np.nan) for i in lh_ids}, lh_lab)
    rh_map = _parcels_to_vertices({i: vals.get(i, np.nan) for i in rh_ids}, rh_lab)

    hemis = [("left", lh_map, fs[f"{surf}_left"], fs["sulc_left"]),
             ("right", rh_map, fs[f"{surf}_right"], fs["sulc_right"])]

    fig, axes = plt.subplots(
        len(views), 2, figsize=(5.2 * 2, 3.6 * len(views)),
        subplot_kw={"projection": "3d"})
    axes = np.atleast_2d(axes)

    for r, view in enumerate(views):
        for c, (hemi, vmap, mesh, bg) in enumerate(hemis):
            plotting.plot_surf_stat_map(
                mesh, stat_map=vmap, hemi=hemi, view=view,
                bg_map=bg, bg_on_data=True,
                cmap=cmap, vmin=vmin, vmax=vmax, symmetric_cbar=False,
                threshold=threshold, avg_method="median",
                axes=axes[r, c], figure=fig, colorbar=False)

    # single shared colourbar
    sm = plt.cm.ScalarMappable(cmap=cmap,
                               norm=plt.Normalize(vmin=vmin, vmax=vmax))
    cbar = fig.colorbar(sm, ax=axes.ravel().tolist(), shrink=0.6, pad=0.02)
    cbar.set_label(f"{gene} expression", fontsize=12)

    thr = "" if threshold is None else f"  (threshold ≥ {threshold})"
    fig.suptitle(f"{gene} — AHBA expression on DK atlas{thr}", fontsize=14)

    if save:
        os.makedirs(out_dir, exist_ok=True)
        tag = "" if threshold is None else f"_thr{threshold}"
        fname = os.path.join(out_dir, f"{gene}_dk_surface{tag}.png")
        fig.savefig(fname, dpi=200, bbox_inches="tight")
        print(f"saved: {fname}")
    return fig


if __name__ == "__main__":
    atlas = load_dk_surface_atlas()
    expr = compute_expression(atlas)
    for g in ["GRIN2A"]:
        if g in expr.columns:
            plot_gene_expression(g, expr, atlas)
            plot_gene_expression(g, expr, atlas, threshold=0.5)
