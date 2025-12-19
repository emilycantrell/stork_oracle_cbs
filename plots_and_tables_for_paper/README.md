## OVERVIEW

The code in this directory creates figures and tables for our paper in the PreFer special issue, using results that we exported from the remote access environment for the Centraal Bureau voor de Statistiek (CBS) which manages the Dutch registry. The exported results are available in `stork_oracle_cbs/exports/`. See the README in the root directory of this repository for information about how we generated those results.

## SPECS & VERSION NUMBERS

The code to generate the figures and tables was run with the following specs:

|Component            |Specification   |
|:--------------------|:---------------|
|**Computer**         |2023 MacBook Pro|
|**Processor**        |Apple M2 Max    |
|**Memory**           |96 GB           |
|**Operating System** |Sequoia 15.7.2  |
|**Disk Space**       |2 TB            |
|**R**                |4.5.2           |
|**Groundhog**        |3.2.3           |

The Groundhog package will automatically load other dependencies using the most up-to-date version of those dependencies as of 2025-11-15.

Running the code on a computer with these specs took less than 30 seconds.

## INSTRUCTIONS FOR RUNNING THE CODE TO GENERATE FIGURES AND TABLES

To generate the figures and tables, follow the steps below. Please note that the code in this directory does not retrain the models from scratch; it simply produces figures and tables with our existing results files.

1. Clone the `december_export` branch of this repository to your local machine. If you prefer to download a ZIP file instead of cloning, be sure to download the `december_export` branch.

2. Install the versions of R and Groundhog specified above. (You may be able to skip this step if you already have other versions of R and Groundhog; it is possible the code will run on other versions.)

3. Open the `run_all` file in this directory and choose the desired `target_test_set` by uncommenting the appropriate line. For the validation set, choose `evaluation_test_50_percent_split`. For the holdout set, choose `official_holdout_set`. (Note: we have not yet tested the code with the `official holdout set` option because we are waiting until after reviewer feedback to touch the holdout set results.)

4. Run `run_all`. You will find all figures and tables in `stork_oracle_cbs/plots_and_tables_for_paper/plots_and_tables_output`.

Note: The visual abstract (Figure 1) was assembled in Freeform using the outputs from the "Figure 1" scripts. Additionally, Figure 6 was manually edited in Freeform to include brackets showing the size of the gap between curves.
