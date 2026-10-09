# Changelog

## [Unreleased]

**Added**
- export to .txt file 
- optional "Contribute data" wizard: turns the uploaded peat depths into files following the
  Data Exchange Standard for Peat Surveys v1.0 (Natural England) with a Norwegian profile, for download
    - new `standard/` folder with the machine-readable standard and the Norwegian profile
    - the corrected profile writes yes/no attributes as `true`/`false` (TDWG Boolean Controlled Vocabulary, change C7)
    - works right after the upload, without the volume calculation
    - new dependencies: `shinyvalidate`, `yaml`

**Changed**
- peat depth unit changed from m to cm
    - updated the columns of the input data 
        - `torvdybde_cm` instead of `Dybde`
        - `peat_depth_cm` instead of `Dybde`
    - updated the calculations to still calc. volume in m3
- allow for input data with different coord. systems.
    - all coordinates are transformed to `ETRS89 UTM zone 33N` (EPSG:25833).
    - update default to `ETRS89 UTM zone 33N` (EPSG:25833) 
- new order UI-elements in the sidebar
- reading the uploaded zip moved into `read_dataset_zip()` (`global.R`), shared by the volume calculation and the "Contribute data" wizard
- updated documentation and inline comments

**Removed**
- empty mapframe in the output interpolation map is deleted.

## Version 1.0.0 (2023-16-01)

*Official release*

**Added**
- visulization of the power parameter

**Changed**
- better error handling
- improved peat depth interpolation 

## Version 0.1.0-alpha (2022-10-25)

*Pre-release*