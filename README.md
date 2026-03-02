# septa_d_line_cbtc
An investigation into how CBTC is affecting the SEPTA D Line and what its consequences are.

## Substack
Please follow along my investigation on [Substack](https://adenyacobi.substack.com/). 
- [Part 1](https://adenyacobi.substack.com/p/septa-slow-down-on-the-d-lines-part) is out!

## File Structure
- **input**: The data files used for the analyses. These are zip files so they can be stored on GitHub. To run the code, you will need to unzip them. Note that `Network Dataset.zip` (which holds GTFS files and Open Street Map file) and `Census TIGER Line Shapefiles.zip` (which holds shapefiles of Pennsylvania block groups) are not currently uploaded because their file sizes are too large. I will work on getting this data uploaded. Until then, `3_accessibility.R` will not be able to be run unless you provide the data.
- **intermediate**: Here are where code outputs are stored if they are not directly used in the Substack article(s).
- **output**: Here are where code outputs are stored if they are directly used in the Substack articles(s), such as the `maps`. `part1_compilation` compiles all the intermediate Excel files for Part 1. The `tables` folder contains images of the Excel tables.
- **code**: The programming scripts live here.
   - `1_parse_gtfs.R`: This script parses pre-CBTC and post-CBTC GTFS to compare SEPTA D1 and D2 trips. It also outputs the file `intermediate/trip_time_comparisons.xlsx`.
   - `2_quantify_additional_costs.R`: This script quantifies the additional costs of CBTC, both to riders and SEPTA. The script is downstream of `1_parse_gtfs.R`. The script outputs `intermediate/added_costs.xlsx`.
   - `3_accessibility.R`: This script creates transit isochrone comparison maps and figures out how many jobs certain origins have access to in the pre-CBTC and post-CBTC scenarios. The script outputs `intermediate/accessbility.xlsx` and creates the accessibility maps in the `output/maps` folder.
