      SUBROUTINE BKDHIT(LDHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book bank DHIT, which is the down link bank
C-                         from CDCH. DHIT bank contains compressed
C-                         information for all reconstructed CDC hits
C-
C-   Inputs  : none
C-   Outputs : LDHIT: address of DHIT bank
C-
C-   Created  10-JUL-1991   Qizhong Li-Demarteau
C-   Modified 13-Nov-1993   C. Klopfenstein
C-                          Change length to 3 words/hit to allow 
C-                          RECO to run from DHIT. Note - number of words/DHIT
C-                          is now stored as a parameter in d0$inc:cdpara.inc
C-   Updated  13-APR-1994   NORMAN A. GRAF   Corrected MZFORM for new format
C-   Updated   2-DEC-1994   Qizhong Li-Demarteau  fixed bank length
C-                            (after changed length to 3 words/hit, no one
C-                            changed bank length corresponding to new length!)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      include 'd0$inc:cdpara.inc'
      INCLUDE 'D0$LINKS:IZDHIT.LINK'
      INTEGER LDHIT, LCDCH, GZCDCH
      INTEGER MPDHIT(5)
      LOGICAL FIRST
C
      SAVE FIRST
      DATA MPDHIT/4HDHIT,0,0,3,0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL MZFORM('DHIT','3I /1B 2I',MPDHIT(5))
        FIRST=.FALSE.
      ENDIF
      LCDCH = GZCDCH()
      IF (LCDCH .LE. 0) THEN
        CALL BKCDCH
        LCDCH = GZCDCH()
      ENDIF
      MPDHIT(4) = IQ(LCDCH + 10) * LEN_DHIT + 3
C
C ****  Book DHIT bank
C
      CALL MZLIFT(IXMAIN, LDHIT, LCDCH, -IZDHIT, MPDHIT, 0)
C        
      IQ(LDHIT + 1) = 0               ! version number
C      IQ(LDHIT + 3) = 2               ! # of words per hit
      IQ(LDHIT + 3) = len_dhit         ! # of words per hit
C
  999 RETURN
      END
