      FUNCTION GZESUM(STYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find ESUM bank of a requested type
C-
C-   Inputs  : STYP   Character *4 bank type: 'FILT','TRGR','RECO','ISAE'
C-             if STYP = 'ANY', return the one at the start of the chain
C-   Outputs :  Link to appropriate member of linear chain
C-   Controls:
C-
C-   Created  6-JAN-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STYP
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZESUM.LINK'
      INTEGER GZHSUM, LSUP, GZESUM, ISTYP, LZFIND
C----------------------------------------------------------------------
      GZESUM = 0
      LSUP = GZHSUM()
      IF (LSUP .LE. 0) RETURN
C...first element found in possible linear chain
      GZESUM = LQ( LSUP - IZESUM )
      IF (STYP.NE.'ANY') THEN
C...now look for member with the requested type
        CALL UCTOH(STYP,ISTYP,4,4)
C...get location of type word from 1st bank found; it's last word of FIXed part
        GZESUM = LZFIND(IXCOM,GZESUM,ISTYP,IQ(GZESUM+2))
      ENDIF
  999 RETURN
      END
