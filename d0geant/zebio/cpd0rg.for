      SUBROUTINE CPD0RG(IXZDIV,LINK,IZLINK,CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy D0RG bank and it's dependents from RUNG to
C-                              ZEBCOM. We have to pass destination division
C-                              and links as arguments because GCBANK and
C-                              ZEBCOM both use Q and IQ.
C-
C-   Inputs  : IXZDIV   Division in ZEBCOM
C-             LINK     Mother bank address in ZEBCOM
C-             IZLINK   Mother link to use
C-             CHOPT    Character options
C-   Outputs :
C-   Controls:
C-
C-   Created  21-SEP-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IXZDIV,LINK,IZLINK
      CHARACTER*(*) CHOPT(*)
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$LINKS:IZD0RG.LINK'
      INTEGER LD0RG
C----------------------------------------------------------------------
      LD0RG = LQ(JRUNG-IZD0RG)
      IF ( LD0RG.GT.0 ) THEN
        CALL MZCOPY(IXCONS,LD0RG,IXZDIV,LINK,-IZLINK,CHOPT)
      ENDIF
  999 RETURN
      END
