# This script executes the CHIME simulation projecting covid-19 related hospitalizations and admissions based on observed hospitalization data
# For more details please read the chime_sims repo from Penn Medicine (https://github.com/pennsignals/chime_sims)
import os 
os.system('conda install -c anaconda configargparse')
os.system('python Python/_01_GOF_sims.py -p Python/data/OTT_parameters.csv -t Python/data/OTT_ts.csv  --reopen_day 14 --reopen_caps 1 1.1 0.9 --save_reopening_csv --one_reopen --reopen_from_today')