      SUBROUTINE DBCLB_DETSEQ(DETYP,DETSEQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given detector name gives sequence number.
C-                         If DETYP is ' ', then uses TOPN in dbstp.inc
C-                         to determine.
C-
C-   Inputs  : DETYP     Detector type  (C*3)
C-
C-   Outputs : DETSEQ    Detector sequence number
C-
C-   Controls:
C-
C-   Created  08-FEB-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DETYP
      INTEGER DETSEQ
      INCLUDE 'D0$INC:DBSTP.INC'
      CHARACTER*3 DETYPE
C
      DETYPE = ' '
      DETSEQ = 0
      CALL UPCASE(DETYP, DETYPE)
      IF (DETYPE(1:1) .EQ. ' ') THEN
        DETYPE(1:1) = TOPN(2:2)
      ENDIF
      IF(DETYPE(1:3) .EQ. 'CAL') THEN
        DETSEQ = 1
      ELSEIF(DETYPE(1:3) .EQ. 'MUO') THEN
        DETSEQ = 2
      ELSEIF(DETYPE(1:3) .EQ. 'SAM') THEN
        DETSEQ = 3
      ELSEIF(DETYPE(1:3) .EQ. 'CDC') THEN
        DETSEQ = 4
      ELSEIF(DETYPE(1:3) .EQ. 'FDC') THEN
        DETSEQ = 5
      ELSEIF(DETYPE(1:3) .EQ. 'VTX') THEN
        DETSEQ = 6
      ELSEIF(DETYPE(1:3) .EQ. 'TRD') THEN
        DETSEQ = 7
      ELSEIF(DETYPE(1:3) .EQ. 'LV0') THEN
        DETSEQ = 8
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
