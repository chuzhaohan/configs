#!/bin/bash

# cron requires environment variables to be set
export SGE_ROOT=/share/apps/gridengine/62u5_20110621
export SGE_CLUSTER_NAME=scylla
export SGE_CELL=default
export SGE_ARCH=lx-amd64

# cron requires absolute path because environment isn't set
QSTAT=/share/apps/gridengine/62u5_20110621/bin/lx-amd64/qstat
GREP=/bin/grep
AWK=/usr/bin/awk

# parse qstat output. Use GREP instead of -u because that results in a title
# line passing checks. I do not want that
OUT=$($QSTAT | $GREP "dcherian" | $GREP "\bR\|\bS\|\bt" | $AWK '{print $3'})

# check that cron is working
#DATE=$(date)
#echo "running on cron: $DATE" >> ~/croncheck
#echo "$QSTAT" >> ~/croncheck
#echo $OUT >> ~/croncheck

# if not an empty string then, email OUT to me
if [[ !( -z "$OUT" ) ]]; then
    echo $OUT | mail -s "queuecheck" dcherian@mit.edu
fi