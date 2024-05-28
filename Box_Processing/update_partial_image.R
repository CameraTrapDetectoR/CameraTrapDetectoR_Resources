## update culled image review in master list

# load latest labels
lbl_path <- "path/to/latest/varified_df"

df <- data.table::fread(lbl_path)

# set path to review dir
review_dir <- "C:/Users/amira.burns/OneDrive - USDA/Image Verification"

# get names of finalized dfs
