      SUBROUTINE DBCLB_SEQDET(DETSEQ,DETYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given detector name gives sequence number.
C-                         If DETYP is ' ', then uses TOPN in dbstp.inc
C-                         to determine.
C-
C-   Inputs  : DETSEQ    Detector sequence number
C-
C-   Outputs : DETYP     Detector type  (C*3)
C-
C-   Controls:
C-
C-   Created  08-FEB-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DETSEQ
      CHARACTER*(*) DETYP
C
      DETYP = ' '
C
      IF(DETSEQ .EQ. 1) THEN
        DETYP(1:3) = 'CAL'
      ELSEIF(DETSEQ .EQ. 2) THEN
        DETYP(1:3) = 'MUO'
      ELSEIF(DETSEQ .EQ. 3) THEN
        DETYP(1:3) = 'SAM'
      ELSEIF(DETSEQ .EQ. 4) THEN
        DETYP(1:3) = 'CDC'
      ELSEIF(DETSEQ .EQ. 5) THEN
        DETYP(1:3) = 'FDC'
      ELSEIF(DETSEQ .EQ. 6) THEN
        DETYP(1:3) = 'VTX'
      ELSEIF(DETSEQ .EQ. 7) THEN
        DETYP(1:3) = 'TRD'
      ELSEIF(DETSEQ .EQ. 8) THEN
        DETYP(1:3) = 'LV0'
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
