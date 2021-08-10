#!/bin/bash
# USAGE: ./MultipleLogIns.sh Rfile.R
# list of computers in the department
COMPUTERS="adrastea aitne albatross albiorix alf alnitak amalthea anake analepsy anice antiknight aoede araf arche atria autonoe bak barnowl basil battleships beans bicorn bittern blackcap bluetail boltzmann bonxie bowtie brahms brough buffoon caliban callirrhoe callisto capacitor capsules cardioid carina carme catbird catenary cave cayenne chaldene chiffchaff chopin citril clouds concave conceal concede conceit concent conch concord concur condor conduit confab confect coot coral cordelia corvus coulomb crane crow crumble cubic cuckoo curlew custard cyllene defeat delice deltoid demeter democrates demonfan density diogenes diver dominoes dotterel dowitcher drab draft drat drip droid drudge drummer emaciate emanant embattle embedded embellish ember emblaze embroil emery emission emitter empassion emperor emphatic empirical emplace employ emporium emptor epicurus epsilon equinox eridanus erinome eris erriapo euanthe eukelade eurydome farad fillomino finite floggle-toggle folium fourwinds fulmar futoshiki gallinule gargoyle garnish glencoe goldcrest goldfinch goshawk grouse grumpy guillemot gull gyrfalcon harrier hashi hawkowl haydn haystack hegemone helike heraclius heron herse heyawake hiccup himalia hippo hitori hornbill hyena ibis impulse inertia inglorious iocaste isonoe janus jigsaw jubilee juno jupiter kakuro kale kallichore kappa kite kiviuq kore krisskross kropki lambda lapwing leda lemma linnet linoleum lits longtail luminosity lune lysithea magnets magnitude mallard markab masyu mathrax megaclite mercury metis minesweeper mneme mockingbird modulus momentum moon mozart murrelet mustard neso neverland nightjar nix nonogram nordic norinori nosey numberlink nurikabe nuthatch nymph oblong ohm oriole orthosie orwell osmosis osprey othello otter oval ovenbird paaliaq parakeet parquet parrot parsec part3 pasithee pedal pentagon pentopia persaeus phi plato platypus plover pluto polaris porridge praxidike primrose psamathe ptarmigan pudding puffin pursuit quail rackoon radius rainbow razorbill rectangle redgrouse redknot redwing reedling refract rhombus ricci rigel rose rossini roulette rubik rubythroat sanderling sandmartin sandpiper sao sapsucker saratoga saturn scaup scramble senate setebos shearwater shelduck shoebill shrike siarnaq sigma sinope sirius siskin skadi skua skylark sloth snake snipe socrates spica spitfire spoom squad square squat squawk squeak squeal squiffy squill squint squirm squish stadium starbattle statuepark stonechat stork styx sudoku suguru sun swan syba tacitus tan tangram tapir taygete tents tern thales thebe thelxinoe themisto theta thrasher thrush thrym tomtom torsion tractrix trapezium treecreeper triangle trifle trinculo triton truffle twite tyone unfair ungiwg unruly uranus utopia venus verdi versine vireo volt vulture wagner wagtail wallcreeper walls warbler waxwing wheatear whinchat whitethroat witchens woodlark wryneck yajilin ymir yogi"

#MINCOMP="20"  # minimum number of machines to run in parallel
#MAXCOMP="100" # maximum number of machines to run in parallel
AVAILABLE=""
UNAVAILABLE=""

# ping computer to check availability, timeout after 10ms.
# exit status is 0 if the ping command successfully completes within 10ms, if so, add the computer to $AVAILABLE list, otherwise add it to $UNAVAILABLE list (if timed out, exit status is 124)
for computer in $COMPUTERS
do
    timeout 0.05 ping -c 1 $computer >/dev/null
    if [ "$?" -eq "0" ]
    then
	AVAILABLE="$AVAILABLE $computer"
    else
	UNAVAILABLE="$UNAVAILABLE $computer"
    fi
done
n_avail=$(echo "$AVAILABLE" | wc -w)
echo "Number of computers available: $n_avail"
echo $AVAILABLE >> list_of_computers


##============================##
## run script in parallel     ##
##============================##

rm -rf outfiles/
mkdir outfiles/

Rfile=$1 # R file provided by the first argument in the command line
# ssh all departmental computers and execute the R script
for computer in $AVAILABLE
do
    (nohup ssh -f $computer "cd ~/path_to_my_simulation_folder/ && nice -n 19 Rscript $Rfile && exit")
done



