      SUBROUTINE BKPTAU(NZTR,LPTAU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      book a PTAU bank
C-   
C-   Outputs : 
C-     LPTAU = pointer to created bank
C-
C-   Created  26-SEP-1990   Serban D. Protopopescu
C-   Modified 16-JUL-1993   A.Boehnlein, H.Li, increased bank version number
C-   Updated   2-NOV-1993   Qizhong Li-Demarteau  added 7 words in PTAU bank
C-                                        and increased bank version to be 3
C-   Updated  19-NOV-1993   Qizhong Li-Demarteau  corrected PTAU ID word 
C-   Updated  17-MAR-1994   Qizhong Li-Demarteau  added 3 words and increased
C-                             bank version to be 4  and update NPTAU in PARH
C-   Updated  12-DEC-1994   Qizhong Li-Demarteau   added 11 words and 
C-                                     increased bank version to be 5
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NZTR,LPTAU
      INTEGER LPARH
      INTEGER IXIO
      INTEGER GZPARH
      INTEGER NDATA
      PARAMETER( NDATA = 32 )
      INTEGER NL
      INTEGER NS
      PARAMETER( NS = 1 )
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPTAU.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LPTAU = 0
      IF(FIRST)THEN
C
        CALL MZFORM('PTAU','2I 11F 2I 3F 3I 2F 4I 5F',IXIO)  ! Describe Bank format
C
        FIRST = .FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LPARH = GZPARH()
      IF(LPARH.EQ.0) THEN
        CALL ERRMSG('CALORIMETER','BKPTAU',
     &    'PARH BANK DOES NOT EXIST ' ,'W')
C
      ELSE
        NL=NZTR+NS+1
        LPTAU = LQ(LPARH - IZPTAU)
        IF (LPTAU .LE. 0) THEN
          CALL MZBOOK
     &    (IXMAIN,LPTAU,LPARH,-IZPTAU,'PTAU',NL,NS,NDATA,IXIO,0)
          IQ(LPTAU-5) = 1
        ELSE
          CALL MZBOOK
     &    (IXMAIN,LPTAU,LPTAU,0,'PTAU',NL,NS,NDATA,IXIO,0)
        ENDIF
        IQ(LPTAU+1) = 5   ! version number
        IQ(LPTAU+2)=16  ! id for taus
        IQ(LPARH+7) = IQ(LPARH+7) + 1    ! update PARH
      ENDIF
C
  999 RETURN
      END
