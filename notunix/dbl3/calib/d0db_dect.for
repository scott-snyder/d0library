      SUBROUTINE D0DB_DECT(PATH,DECT,CALTYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given pathname gives detector type.
C-
C-   Inputs  : PATH  Pathname
C-   Outputs : DECT     Detector type
C-            CALTYPE   Calibration type
C-
C-   Controls:
C-
C-   Created  15-JUN-1992   SHAHRIAR ABACHI
C-   Updated  15-DEC-1992   Haowei XU Added Level0
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH,DECT,CALTYPE
      INTEGER IJK
C
      IF(PATH(9:9) .EQ. 'C') THEN
        DECT = 'CALOR'
      ELSEIF(PATH(9:9) .EQ. 'M') THEN
        DECT = 'MUON'
      ELSEIF(PATH(9:9) .EQ. 'D') THEN
        DECT = 'CDC'
      ELSEIF(PATH(9:9) .EQ. 'F') THEN
        DECT = 'FDC'
      ELSEIF(PATH(9:9) .EQ. 'V') THEN
        DECT = 'VTX'
      ELSEIF(PATH(9:9) .EQ. 'T') THEN
        DECT = 'TRD'
      ELSEIF(PATH(9:9) .EQ. 'S') THEN
        DECT = 'SAMUS'
      ELSEIF(PATH(9:9) .EQ. 'L') THEN
        DECT = 'LV0'
      ENDIF
C
      IJK = 22
      IF(PATH(16:19) .EQ. 'PEDS') THEN
        CALTYPE = 'PEDESTALS'
        IJK = 21
      ELSEIF(PATH(16:20) .EQ. 'GAINS') THEN
        CALTYPE = 'GAINS'
      ELSEIF(PATH(16:20) .EQ. 'TIMES') THEN
        CALTYPE = 'TIMES'
      ELSEIF(PATH(16:20) .EQ. 'DTIME') THEN
        CALTYPE = 'DTIMES'
      ELSEIF(PATH(16:20) .EQ. 'DRIFT') THEN
        CALTYPE = 'DRIFT'
      ELSEIF(PATH(16:20) .EQ. 'MINTM') THEN
        CALTYPE = 'MINTIME'
      ENDIF
C
      IF(PATH(IJK:IJK+4) .EQ. 'CLBH') THEN
        CALTYPE(17:17) = 'H'
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
