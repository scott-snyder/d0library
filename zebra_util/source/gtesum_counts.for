      SUBROUTINE GTESUM_COUNTS (STYP, NFOUND, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return counts of objects found in ESUM bank
C-    Inputs:
C-             STYP     [C*4]   Summary type desired:
C-                                'FILT','TRGR','RECO','ISAE'
C-    Outputs:
C-        NFOUND(ID_ALL:LAST_TYPE) [I] Total Number of objects found, by type:
C-                              usage: NFOUND(ID_ALL) has total # objects found
C-                                     NFOUND(ID_PHOTON) has total # photons
C-
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- No ESUM bank of this type.
C-
C-   Controls: none
C-   Created  6-JAN-1992   James T. Linnemann
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*(*) STYP
      INTEGER NFOUND(ID_ALL:LAST_TYPE)
      INTEGER IER, LESUM, GZESUM, POINT, I
C----------------------------------------------------------------------
C: Set error flag
      IER = 0                   ! OK
C: Get link
      LESUM = GZESUM(STYP)
      IF ( LESUM .LE. 0 ) THEN
        IER = -1
        DO I = ID_ALL,LAST_TYPE
          NFOUND(I) = 0
        ENDDO
      ELSE
        DO I = ID_ALL,LAST_TYPE
          NFOUND(I) = IQ( LESUM + 5 + I)
        ENDDO
      END IF
  999 RETURN
      END
