      SUBROUTINE PRZTMP(PRUNIT, LZTMP, NZTMP, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of bank 'ZTMP' 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LZTMP  [I] : Pointer to the bank ( CFL = 'ONE' ) 
C-                          Unused if CFL = 'ALL'.
C-             NZTMP  [I] : Bank number, used only if CFL='ONE' and LZTMP = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' OR 'ALL' 
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-
C-   Created  17-FEB-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTMP.LINK'
      INTEGER PRUNIT, LZTMP, NZTMP, IFL
      INTEGER LZTMP1, GZZTMP, LZTRH, GZZTRH
      CHARACTER*3 CFL
C----------------------------------------------------------------------
C
      IF (CFL .EQ. 'ONE') THEN
        IF (LZTMP .GT. 0) THEN
          LZTMP1 = LZTMP
        ELSE
          LZTMP1 = GZZTMP(NZTMP)
        ENDIF
        IF (LZTMP1 .GT. 0) THEN
          WRITE (PRUNIT,1001) 
          WRITE (PRUNIT,1002) IQ(LZTMP1-5), IQ(LZTMP1+3), IQ(LZTMP1+4),
     &      IQ(LZTMP1+5), Q(LZTMP1+6), Q(LZTMP1+8)
        ENDIF
        GOTO 999
      END IF
C
      IF (CFL .EQ. 'ALL') THEN 
        LZTRH = GZZTRH()
        IF (LZTRH .LE. 0) GOTO 999
        LZTMP1 = LQ(LZTRH - IZZTMP)
        IF (LZTMP1 .LE. 0) GOTO 999
        WRITE (PRUNIT,1001) 
  100   WRITE (PRUNIT,1002) IQ(LZTMP1-5), IQ(LZTMP1+3), IQ(LZTMP1+4),
     &      IQ(LZTMP1+5), Q(LZTMP1+6), Q(LZTMP1+8)
        LZTMP1 = LQ(LZTMP1)
        IF (LZTMP1 .LE. 0) GOTO 999
        GOTO 100
      ENDIF
C
 1001 FORMAT(/,'  ZTMP    IDV    IDC    IDF   delta_phi delta_theta')
 1002 FORMAT(1X,I5,4X,I3,4X,I3,4X,I3,2X,F9.3,2X,F9.3)
C
  999 RETURN
      END
