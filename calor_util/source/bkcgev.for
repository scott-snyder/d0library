      SUBROUTINE BKCGEV(NR,LCGEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books CGEV. If no CGEV exist then CGEV is stand
C-           alone bank in ZEBSTP. If there exists a CGEV already then another
C-           CGEV is booked next in a linear chain.
C-
C-   Inputs  : NR   [I] Number of Repeat words/channel
C-                      NR = 1  K=AWCG only
C-                      NR = 2  plus pedestal sigmas
C-                      NR = 3  plus pedestals
C-   Outputs : LCGEV: Link of Booked CGEV Bank
C-   Controls: None
C-
C-   Created  31-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCGEV,GZCGEV
      INTEGER IXIO,NL,NS,ND,LSUP,NH,NR
      CHARACTER*5 ZFORM
C
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCGEV = 0
      IF(FIRST)THEN
        IF (NR.EQ.0) NR = 1
        NL = 2                           ! LINKS
        NS = 2                           ! STRUCT LINKS 
        NH = 4                           ! HEADER WORDS (version,x1-x8,NR)
        ND = NH+NR*(2*NETAL+1)*NPHIL*NLYRL  !slots for all LYR,PHI,ETA 
        WRITE(ZFORM,'(I1.1,''I -F'')') NH
        CALL MZFORM('CGEV',ZFORM,IXIO)   ! Describe Bank format
        CALL STP_INZLNK
        FIRST = .FALSE.
      ENDIF
      LCGEV = GZCGEV()
      IF(LCGEV.EQ.0) THEN
C
C **** Book first CGEV bank
C
        CALL MZBOOK (IDVSTP,LCGEV,0,2,'CGEV',NL,NS,ND,IXIO,0)
        IC(LCGEV+1) = NH
        IC(LCGEV+2) = 1 !VERSION
        IC(LCGEV+4) = NR
        CALL GZCGEV_INIT(LCGEV)
      ELSE
C
C **** Book next CGEV bank 
C
        LSUP = LCGEV
        CALL MZBOOK (IDVSTP,LCGEV,LSUP,0,'CGEV',NL,NS,ND,IXIO,0)
        IC(LCGEV+1) = NH
        IC(LCGEV+2) = 1 !VERSION
        IC(LCGEV+4) = NR
      END IF
  999 RETURN
      END
