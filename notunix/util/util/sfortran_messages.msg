!--------------------------------------------------------------
! Name   :      SFORTRAN_MESSAGES.MSG
! Purpose:      Message definitions for SFORTRAN.EXE
! Created:       7-Oct-1990   Harrison B. Prosper
!--------------------------------------------------------------
.FACILITY       SFORTRAN, 1780 /PREFIX=SFO_
.SEVERITY       FATAL
INVCLINE        <Error in the command line>
INVMACRO        <Invalid !SQL macro in line !AS>/FAO=1
ERRPROC         <Error processing line !AS>/FAO=1
OPENFAILED      <Unable to open file !AS> /FAO=1
NOTFOUND        <Unable to open include file in line !AS> /FAO=1
NODECLAR         <No declaration exists for variable in line !AS> /FAO=1

.SEVERITY       SUCCESS
SUCCESS         <SFORTRAN successful completion>

.END
