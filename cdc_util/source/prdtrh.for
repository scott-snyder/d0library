      SUBROUTINE PRDTRH(PRUNIT,KDTRH,NDTRH,CARFL,IPRFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the bank DTRH 
C-
C-   Inputs  : PRUNIT [I] : The fortran output unit
C-             KDTRH  [I] : Bank pointer if CARFL = 'SINGLE'
C-             NDTRH  [I] : Bank number  if CARFL = 'SINGLE'
C-             CARFL [C*] : Flag, either 'ALL' or 'SINGLE'
C-             IPRFL  [I] : Print level
C-   Outputs : on the specified unit
C-
C-   Created  12-FEB-1993   Qizhong Li-Demarteau   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT, KDTRH, NDTRH, IPRFL
      INTEGER LDTRH, GZDTRH
      INTEGER PLCDCH, GZCDCH
      CHARACTER*(*) CARFL
C----------------------------------------------------------------------
C
      LDTRH = GZDTRH()
      IF (LDTRH .LE. 0) GOTO 999
C
      WRITE (PRUNIT, 1000) IQ(LDTRH+7),IQ(LDTRH+2)
 1000 FORMAT
     &  (5X,'Total',I6,' CDC tracks are built',I6,' DTRK banks kept')
      IF (IPRFL .LE. 1) GOTO 999
      WRITE (PRUNIT, 1001) IQ(LDTRH+8), IQ(LDTRH+9)
 1001 FORMAT(5X,'Total CDC hits number =',I6,/,5X,
     &  ' Total length of CDC raw data =',I8,' words')
C
  999 RETURN
      END
