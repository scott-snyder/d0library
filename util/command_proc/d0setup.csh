#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#========================================================================
#
# Name      : D0SETUP
#
# Purpose   : Execute SETUP procedures for D-ZERO products.
#              Search for *SETUP*.CSH until one of the following succeeds:
#              
#                  o   If D0 
#                      Search ONLINE disk for setup_xxxx.csh
#                  o   Search d0$xxxx: for setup_xxxx.csh
#                  o   Search D0$UTIL:D0PRODUCTS.LIST
#                  o   Search using FERMILAB SETUP
#
# Arguments : P1   Product name
#            [P2]  Sub-product name
#            [P3-P8]  arguments
#
# Created  30-OCT-1991   Harrison B. Prosper
# Modified 12-MAR-1992   Harrison B. Prosper 
#      Improve search
# Modified 25-MAR-1992   Harrison B. Prosper 
#      Search only top-most area of online area
# Updated  24-Sep-1992   Herbert Greenlee
#      Adapted from DCL procedure of the same name
#
#========================================================================

set product = $1
set sub_product = $2

set arg1    = $3
set arg2    = $4
set arg3    = $5
set arg4    = $6
set arg5    = $7

set command_procedure   = ''
set used_product_list   = 0

set product_list    = `vff 'D0$UTIL:D0PRODUCTS.LIST'`
set product_file    = ~/d0products.tmp

#================================================
#   Get Product Name
#================================================

if( $product == '' )then
  echo "D-ZERO product Name : \c"
  set product = `line`
  if( product == '' )goto exit
endif

#================================================
#
#   Search D0Library for setup procedures
#   
#================================================

set csh_files = (`vfl 'D0$'"$product"':*SETUP*.CSH'`)
set NameList = '@'
set NumberFound = 0
set outfile1 = ${product_file}_1
touch $outfile1
set outfile2 = ${product_file}_2
touch $outfile2

foreach csh_file ( $csh_files )
  set csh_name = $csh_file:t
  set csh_name = $csh_name:r

#================================================
#   If sub_product name is specified then look
#   only for those procedures which contain the
#   sub_product name.
#================================================

  if( $csh_name !~ *${sub_product}* )continue

#================================================
#   Check Procedure Name
#================================================

  if( $NameList =~ *@${csh_name}@* )continue

#================================================
#   Add new procedure name to NameList
#================================================

  set NameList = ${NameList}${csh_name}@
  @ NumberFound = $NumberFound + 1

  if( $csh_name =~ setup* )then
    echo ${product}@$csh_file >> $outfile1
  else
    echo ${product}@$csh_file >> $outfile2
  endif
end

cat $outfile1 $outfile2 >! $product_file
rm -f ${product_file}_* >& /dev/null

if( $NumberFound > 0 )goto Get_Procedure_Names

#================================================
#
#  Search of D0library failed so search
#  for product in product list
#
#================================================

if( $product_list != '' )then
  grep -i ${product}/ $product_list | tr '/' '@' | sed 's/.COM/.CSH/' >! $product_file
endif
if( `cat $product_file | wc -l` == 0 )goto Execute_FNAL_SETUP

#================================================
#   Get Procedure Names
#================================================

Get_Procedure_Names:
set NumberFound = 0

foreach record (`cat $product_file`)
  set csh_file = `echo $record | cut -d@ -f2 | vff`
  if( $csh_file == '' )continue
  @ NumberFound = $NumberFound + 1

#================================================
#   If SETUP.CSH is amongst this list then execute
#   it only.
#================================================

  set csh_name = $csh_file:t
  set csh_name = $csh_name:r
  if( $csh_name == setup )then
    set NumberFound = 1
    set csh_files = $csh_file
    break
  endif
  set csh_files = ( $csh_files $csh_file )
end

if( $NumberFound == 0 )goto Execute_FNAL_SETUP

#================================================
#   Present list of procedures
#================================================

if( $NumberFound == 1 )then
  set command_procedure = $csh_files[1]
else
  set i   = 0
DO_LOOP2:
  @ i   = $i + 1
  echo $i $csh_files[$i]
  if( $i < $NumberFound )goto DO_LOOP2
  echo "\nWhich one [1] : \c"
  set which_one = `line`
  if( $which_one  == '' )set which_one = 1
  set command_procedure = $csh_files[$which_one]
endif

#================================================
#   execute procedure
#================================================

Execute_Procedure:

if( $command_procedure == '' )then
  echo "%D0SETUP-E-BADPROD, Unknown D-ZERO Product"
  goto exit
endif
if( `vff $command_procedure` == '' )then
  echo "%D0SETUP-E-NOTFOUND, Cannot find procedure $command_procedure"
  goto exit
endif

set com = "$command_procedure $arg1 $arg2 $arg3 $arg4 $arg5"
echo "source $com"
source $com
goto exit

Execute_FNAL_SETUP:

set setup_file = ''
echo "Executing Fermi setup"
if( $?UPS_DIR )then
  if( -f $UPS_DIR/bin/ups_setup )then
    set setup_file =  `$UPS_DIR/bin/ups_setup $product $sub_product $arg1`
  endif
endif
if( $setup_file != '' )then
  source $setup_file
endif

exit:

rm -f $product_file >& /dev/null
exit
