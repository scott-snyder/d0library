      SUBROUTINE DBCLB_CALTYPE(DECT,NTYPE,CALTYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get a list of calibration types.
C-
C-   Inputs  : DECT     detector name
C-             NTYPE    number of calibratoion types
C-             
C-   Outputs : CALTYP list of calibration types
C-   Controls: 
C-
C-   Created  24-MAR-1991   SHAHRIAR ABACHI
C-   Modified 26-OCT-1992   H.Xu  Added L0
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DECT
      CHARACTER*(*) CALTYP(*)
      INTEGER NTYPE
C
      CALTYP(1) = 'PEDESTAL'
      CALTYP(2) = 'GAINS'
      NTYPE = 2
C
      IF(DECT(1:3) .NE. 'CAL') THEN
        CALTYP(3) = 'TIMES'
        NTYPE = NTYPE + 1
      ENDIF
C
      IF(DECT(1:3) .EQ. 'MUO') THEN
        CALTYP(4) = 'DTIME'
        NTYPE = NTYPE + 1
      ENDIF
C
      IF(DECT(1:3) .EQ. 'LV0') THEN
        CALTYP(2) = 'TIMES'
        CALTYP(3) = 'DTIME'
        NTYPE = 3
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
