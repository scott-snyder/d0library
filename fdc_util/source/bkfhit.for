C VAX/DEC CMS REPLACEMENT HISTORY, Element BKFHIT.FOR
C *1     4-NOV-1993 10:52:19 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element BKFHIT.FOR
      SUBROUTINE BKFHIT(LFHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book bank FHIT, which is the down link bank
C-                         from FDCH. FHIT bank contains compressed
C-                         information for all reconstructed FDC hits
C-
C-   Inputs  : none
C-   Outputs : LFHIT: address of FHIT bank
C-
C-   Created  19-AUG-1991   Robert E. Avery
C-                      based on BKDHIT by Qizhong Li-Demarteau
C-   Updated   4-NOV-1991   Robert E. Avery  use MZBOOK call.
C-   Updated  19-OCT-1993   Robert E. Avery  
C-                      New version (1), contains separate delay line hits.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFHIT.LINK'
      INTEGER LFHIT, LFDCH, GZFDCH
      INTEGER ND,IXFHIT
      LOGICAL FIRST
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL MZFORM('FHIT','3I /1B 1I',IXFHIT)
        FIRST=.FALSE.
      ENDIF
      LFDCH = GZFDCH()
      IF (LFDCH .LE. 0) CALL BKFDCH(LFDCH)
C
C ****  Book FHIT bank
C
      ND = ( IQ(LFDCH+1) + IQ(LFDCH+10) ) * 2 + 3    ! Including DL hits
      CALL MZBOOK(IXMAIN,LFHIT,LFDCH,-IZFHIT,'FHIT',0,0,ND,IXFHIT,0)
C        
      IQ(LFHIT + 1) = 1               ! version number
      IQ(LFHIT + 3) = 2               ! # of words per hit
C
  999 RETURN
      END
