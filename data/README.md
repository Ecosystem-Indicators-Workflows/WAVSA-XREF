# README `./data/`

All data for the repository belongs under `./data/`, which consists of the following sub-directories:

```plaintext
├── data/
  ├── xreffdata/ # Original data, never edited
  ├── processed_data/ # Analysis-ready data
  └── metadata/ # e.g. data dictionary, provenance
```

- `xreffdata/` contains data that is *never* edited, any modified versions of those files, such as from data cleaning and processing (whether from hand-editing or programmatic manipulation), is to be written to `processed_data`.
- `processed_data` contains data files that is ready for analysis, following any data cleaning or processing.
- `metadata/` contains metadata like data dictionaries or provenance information
