      SUBROUTINE DBCLB_PATH_INFO(PATH,DETYP,DETSEQ,CALTYP,HBNK,LINKC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given path name gives all info on calib data
C-
C-   Inputs  : PTAH
C-
C-   Outputs : DETYP     Detector type  (at least C*3)
C-             DETSEQ    Detector sequence NUMBER
C-             CALTYP    Calibration type  (at least C*6)
C-             HBNK      Header bank name  (at least C*4)
C-             LINKC     Link offset of header bank
C-
C-   Controls:
C-
C-   Created  19-JAN-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH
      CHARACTER*(*) DETYP
      CHARACTER*(*) CALTYP
      CHARACTER*(*) HBNK
      INTEGER DETSEQ,LINKC
      INCLUDE 'D0$LINKS:IZSLV0.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZSVTX.LINK'
      INCLUDE 'D0$LINKS:IZSCDC.LINK'
      INCLUDE 'D0$LINKS:IZSFDC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INCLUDE 'D0$LINKS:IZSSAM.LINK'
C
      CHARACTER*(6) PARSE(4)
C
      DETYP = ' '
      CALTYP = ' '
      HBNK = ' '
      LINKC = 0
      DETSEQ = 0
      CALL DBCLB_PARSE(PATH,PARSE)
      IF(PARSE(2)(1:1) .EQ. 'C' .OR. PARSE(2)(1:1) .EQ. 'c') THEN
        DETYP(1:3) = 'CAL'
        DETSEQ = 1
        LINKC = IZSCAL
      ELSEIF(PARSE(2)(1:1) .EQ. 'M' .OR. PARSE(2)(1:1) .EQ. 'm') THEN
        DETYP(1:3) = 'MUO'
        DETSEQ = 2
        LINKC = IZSMUO
      ELSEIF(PARSE(2)(1:1) .EQ. 'S' .OR. PARSE(2)(1:1) .EQ. 's') THEN
        DETYP(1:3) = 'SAM'
        DETSEQ = 3
        LINKC = IZSSAM
      ELSEIF(PARSE(2)(1:1) .EQ. 'D' .OR. PARSE(2)(1:1) .EQ. 'd') THEN
        DETYP(1:3) = 'CDC'
        DETSEQ = 4
        LINKC = IZSCDC
      ELSEIF(PARSE(2)(1:1) .EQ. 'F' .OR. PARSE(2)(1:1) .EQ. 'f') THEN
        DETYP(1:3) = 'FDC'
        DETSEQ = 5
        LINKC = IZSFDC
      ELSEIF(PARSE(2)(1:1) .EQ. 'V' .OR. PARSE(2)(1:1) .EQ. 'v') THEN
        DETYP(1:3) = 'VTX'
        DETSEQ = 6
        LINKC = IZSVTX
      ELSEIF(PARSE(2)(1:1) .EQ. 'T' .OR. PARSE(2)(1:1) .EQ. 't') THEN
        DETYP(1:3) = 'TRD'
        DETSEQ = 7
        LINKC = IZSTRD
      ENDIF
C
      CALTYP(1:4) = PARSE(3)(1:4)
      HBNK(1:4) = PARSE(4)(1:4)
C
C----------------------------------------------------------------------
  999 RETURN
      END
