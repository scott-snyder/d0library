      SUBROUTINE EVSTRM(OSTRM,ISTRM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      MAP output streams characters to output stream indices
C-   Inputs  : 
C-     OSTRM= output stream name 
C-   Outputs : 
C-     ISTRM= output stream index
C-
C-   ENTRY EVSTRM_STOR(OSTRM,ISTRM)
C-   Inputs  : 
C-     OSTRM= output stream name 
C-     ISTRM= output stream index
C-
C-   ENTRY EVSTRM_ITOC(ISTRM,OSTRM)
C-   Inputs  : 
C-     ISTRM= output stream index
C-   Outputs : 
C-     OSTRM= output stream name 
C-
C-   Created  17-NOV-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 OSTRM
      INTEGER I,ISTRM
      INTEGER NMAXO,NMAX2
      PARAMETER (NMAXO=8)
      PARAMETER (NMAX2=NMAXO-2)
      CHARACTER*3 OSTRMS(NMAXO)
      DATA OSTRMS/'STA','DST',NMAX2*'   '/
C----------------------------------------------------------------------
C
      ISTRM=0
      DO 31 I=1,NMAXO
        IF(OSTRMS(I).EQ.OSTRM) ISTRM=I
   31 CONTINUE
C
      RETURN
C 
C
      ENTRY EVSTRM_ITOC(ISTRM,OSTRM)
C
      OSTRM=OSTRMS(ISTRM)
C
      RETURN
C
C
      ENTRY EVSTRM_STOR(OSTRM,ISTRM)
C
      OSTRMS(ISTRM)=OSTRM
C
  999 RETURN
      END
