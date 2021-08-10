#!/bin/bash
# shut down all R processes
USED=`tail -n 1 list_of_computers`
for computer in $USED
do
   (ssh -f $computer "killall R && exit")
done
