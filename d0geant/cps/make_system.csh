#
# set up a production submission system under directory $PROD_DIR
#
#  %make_system production_system project nprocesses
#
#   where:
#
#     production_system = name of the production system to use
#			  (this is the same as the CPS production system name)
#     project		= project name (d0geant, d0reco, test etc)
#     nprocesses	= number of production processes to run. If not
#                         specified, number is taken from cps_showps.
#

set prod  = $1
set proj  = $2

set lines = `cps_showps $prod|wc -l`
if ( $lines == 1 ) then
  echo "Production system $prod is not known to CPS on this node"
  exit
endif

if ( $3 == '' ) then
  set nproc = `cps_showps $prod|awk '$1~/Total/ {npr=$6-2;print npr}'`
else
  set nproc = $3
endif

if ( $?D0FLAVOR ) then
  if ( $D0FLAVOR == 'SIUNIX' ) set cpu = 'SGI'
  if ( $D0FLAVOR == 'IBMAIX' ) set cpu = 'AIX'
else
  echo "Set D0FLAVOR to appropriate type and try again."
  exit
endif

if ( ! $?PROD_DIR ) then
  echo "Set PROD_DIR to point to production area and try again."
  exit
else
  if ( ! -e $PROD_DIR ) then
    echo "Production directory $PROD_DIR does not exist."
    exit
  endif
endif

if ( ! -e $PROD_DIR/${proj} ) mkdir $PROD_DIR/${proj}
cd $PROD_DIR/${proj}
if ( ! -e $prod ) mkdir $prod
if ( -e standard ) cp ./standard/* ./$prod
cd $prod
/bin/rm ${proj}_*class

echo " "						>> ${proj}_pclass
echo "CLASS = 2"					>> ${proj}_pclass
echo " NUMBER OF PROCESSES = $nproc"			>> ${proj}_pclass
echo " CPU TYPE = $cpu"					>> ${proj}_pclass
echo " TASK TYPE = COMPUTE"				>> ${proj}_pclass

echo " "						>> ${proj}_iclass
echo "CLASS = 1"					>> ${proj}_iclass
echo " NUMBER OF PROCESSES = 1"				>> ${proj}_iclass
echo " CPU TYPE = $cpu"					>> ${proj}_iclass
echo " TASK TYPE = IO"					>> ${proj}_iclass

echo " "						>> ${proj}_oclass
echo "CLASS = 3"					>> ${proj}_oclass
echo " NUMBER OF PROCESSES = 1"				>> ${proj}_oclass
echo " CPU TYPE = $cpu"					>> ${proj}_oclass
echo " TASK TYPE = IO"					>> ${proj}_oclass

echo "JDF file templates created in $PROD_DIR/$proj/$prod"
echo "for system with $nproc production nodes"
