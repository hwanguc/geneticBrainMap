# import libraries
import numpy as np
import pandas as pd
import nibabel as nib
from nilearn import datasets, surface, plotting
import abagen


# fetch microarray data and atlas using abagen
microarrayfiles = abagen.fetch_microarray(donors='all', verbose=1)
atlas = abagen.fetch_desikan_killiany()

# get expression data for atlas
expression = abagen.get_expression_data(atlas['image'],atlas['info'],probe_selection='max_intensity',donor_probes='aggregate')

map = abagen.get_interpolated_map('SNCA',atlas['image'])


# --- inputs you already have ---
labels_img = atlas['image']            # NIfTI label image
region_info = pd.read_csv(atlas['info'])   # this is already a pandas DataFrame
print(region_info.head())

# get the names in the correct order
region_ids = region_info['id'].to_numpy()
region_names = region_info['label'].to_list()

# pick a gene to visualize
gene = "GRIN2A"                          # example; choose any gene present in `expression.index`
vals = expression[gene] # shape: (n_regions,)

# make a volume with the gene's values
labels_img = nib.load(atlas['image'])   # load the NIfTI
labdata = labels_img.get_fdata()        # now works
out = np.zeros_like(labdata, dtype=np.float32)

# (optional) restrict to cortex
cort_ids = set(region_info.loc[region_info['structure'] == 'cortex', 'id'].to_list())

# assign per-region value -> voxels
# use .get to handle any IDs present in the image but not in the Series (rare)
unique_ids = np.unique(labdata).astype(int)
for rid in unique_ids:
    if rid == 0:    # background
        continue
    if len(cort_ids) and (rid not in cort_ids):
        out[labdata == rid] = np.nan  # or 0.0 if you prefer
    else:
        val = vals.get(rid, np.nan)   # region IDs match expression.index
        out[labdata == rid] = val

expr_img = nib.Nifti1Image(out, labels_img.affine)

# project to fsaverage and plot on inflated surfaces
fsavg = datasets.fetch_surf_fsaverage()  # FreeSurfer fsaverage space (not MNI152)

tex_left  = surface.vol_to_surf(expr_img, fsavg.pial_left)
tex_right = surface.vol_to_surf(expr_img, fsavg.pial_right)

plotting.plot_surf_stat_map(
    fsavg.infl_left, tex_left, hemi="left", bg_map=fsavg.sulc_left,
    colorbar=True, title=f"{gene} (left hemisphere)"
)
plotting.plot_surf_stat_map(
    fsavg.infl_right, tex_right, hemi="right", bg_map=fsavg.sulc_right,
    colorbar=True, title=f"{gene} (right hemisphere)"
)
plotting.show()