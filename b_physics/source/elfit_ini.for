      FUNCTION ELFIT_INI
C--------------------------------------------------------------
C
C  Initialization routine for ELFIT package.
C  Read control parameters.
C
C  12-JUL-1993 Andrzej Zieminski, Daria Zieminska 
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL ELFIT_INI,ELFIT_DDF,OK 
      INTEGER IER 
      CHARACTER*(*) RCPFIL
      PARAMETER( RCPFIL = 'ELFIT_RCP' )   ! Logical name of control file
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
      OK=IER.EQ.0
      OK=ELFIT_DDF().AND.OK       !  Get list of banks to dump
      ELFIT_INI=.TRUE.
  999 RETURN
      END
