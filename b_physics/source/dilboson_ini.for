      FUNCTION DILBOSON_INI
C--------------------------------------------------------------
C
C  Initialization routine for DILBOSON package.
C  Read control parameters.
C
C  11-NOV-1991 Daria Zieminska 
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL DILBOSON_INI,DILBOSON_DDF,OK,FLGVAL,VEEINI,DO_KSHORT
      INTEGER IER 
      CHARACTER*(*) RCPFIL
      PARAMETER( RCPFIL = 'DILBOSON_RCP' )   ! Logical name of control file
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
      OK=IER.EQ.0
      OK=DILBOSON_DDF().AND.OK       !  Get list of banks to dump
      OK=OK.AND.VEEINI()
      DILBOSON_INI=.TRUE.
  999 RETURN
      END
