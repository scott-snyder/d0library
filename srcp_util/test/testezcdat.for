      PROGRAM TESTEZCDAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Test of EZCDAT.
C-   
C-                          Do DEFINE DATA TESTDATA.DAT and
C-                             DEFINE SRCP TESTSRCP.DAT
C-                             
C-                          to define logicals DATA and SRCP.
C-
C-   Created  15-SEP-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNIN,LUNOUT
      PARAMETER( LUNIN = 10 )
      PARAMETER( LUNOUT= 20 )
C----------------------------------------------------------------------
      PRINT*,'Working...'
C
      OPEN (UNIT=LUNIN, FILE='DATA',STATUS='OLD')
      OPEN (UNIT=LUNOUT,FILE='SRCP',STATUS='NEW')
C
      CALL EZCDAT (LUNIN,LUNOUT)
C
      CLOSE (LUNIN)
      CLOSE (LUNOUT)
C
      PRINT*,'Done!'
      END
