      SUBROUTINE UTAG_FROM_UDST(UDST_VERSION,LUTAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill a UTAG bank based on the UDST version
C-
C-   Inputs  : UDST_VERSION - version in UDST bank
C-   Outputs : LUTAG        - link to UTAG bank
C-   Controls: NONE
C-
C-   Created  16-MAY-1994   Ian Adam
C-   Updated  17-OCT-1994   Ian Adam  set version in UTAG to input 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:UTAG_DATA.INC'
      INCLUDE 'D0$INC:UTAG_VERSIONS.INC'
      INTEGER     NDIMG(MAX_GRP)
      INTEGER     IXGRP(MAX_GRP)
      CHARACTER*8 XTAGS(MAX_GRP,MAX_WORDS)
      INTEGER     UDST_VERSION,V
      INTEGER     LUTAG
      INTEGER     I,J
      LOGICAL     FIRST/.TRUE./
C----------------------------------------------------------------------
C     FILL THE ARRAYS

      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL FILL_UTAG_VERSIONS
      ENDIF
        
      V = UDST_VERSION

      IF (V.GT.N_VERSION) THEN
        CALL ERRMSG('NONEXISTENT UDST VERSION','UTAG_FROM_UDST',' ','F')
        GOTO 999
      ENDIF

      IDMAX = IDMAXV(V)

      DO I=1,IDMAX
        NDIMG(I) = NDIMGV(V,I)
        CALL UCTOH (XGRPV(V,I), IXGRP(I), 4, 4)
        DO J = 1, NDIMGV(V,I)
          XTAGS(I,J) = XTAGSV(V,I,J)
        ENDDO
      ENDDO

      CALL UTAGFL(IDMAX,NDIMG,XTAGS,IXGRP,MAX_GRP,MAX_WORDS,LUTAG)

      IQ(LUTAG+1) = V

  999 RETURN
      END
