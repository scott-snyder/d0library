      SUBROUTINE BKPNUT(IPASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Book particle bank PNUT (neutrino= -missing E)
C-     The PNUT bank number is set to IPASS
C-
C-   Inputs  : 
C-     IPASS= pass no., 1 for simple sum, >1 for corrected sums
C-
C-   Created  17-JAN-1989   Serban D. Protopopescu
C-   Updated  10-JAN-1990   Harrison B. Prosper  
C-      Added extra word for scaler ET sum; set bank version=2
C-   Updated  17-MAY-1993   Stan Krzywdzinski
C-      Added 4 extra words for sig**2(Ez), <dExdEy>, <dExdEz>, <dEydEz>;
C-      set bank version=3
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPNUT.LINK'
      INTEGER IPASS
      INTEGER IOPNUT,LPNUT,GZPNUT,GZPARH,LPARH
      LOGICAL FIRST
      INTEGER NDATA,NLNKS
      DATA NDATA,NLNKS/18,1/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('PNUT','2I-F',IOPNUT)
        FIRST=.FALSE.
      ENDIF
C
C
      LPARH=GZPARH()
      LPNUT=GZPNUT(IPASS)
      IF(LPNUT.NE.0) GOTO 999   ! bank for this pass already exists
C
C   Create new PNUT bank 
C
      CALL MZBOOK(IXMAIN,LPNUT,LPARH,-IZPNUT,
     +            'PNUT',NLNKS,NLNKS,NDATA,IOPNUT,0)
C
      IQ(LPNUT-5)=IPASS         ! bank no. set to pass number
      IQ(LPNUT+1)=3             ! version number
      IQ(LPNUT+2)=11            ! e neutrino id 
  999 RETURN
      END
