      SUBROUTINE GTMSGH( NMOD, MMOD, NSCI )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return MSGH information
C-                         muon module number, number of scintillator
C-
C-   Inputs  : None
C-   Outputs : NMOD    : number of muon module has SCIBO.
C-                       if 0, error.
C-             NMOD(i) : i-th muon module number 
C-             NSCI(i) : number of scintillator belong to i-th muon module 
C-   Controls: None
C-
C-   Created   7-FEB-1992   Atsushi Taketani
C-   Updated  14-MAR-1994   Susumu Igarashi  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NMOD, MMOD(*), NSCI(*)
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER  LMSGH, GZMSGH
      INTEGER  I, LMSGM,J
C----------------------------------------------------------------------
      LMSGH = GZMSGH(0)
      IF ( LMSGH.EQ.0 ) THEN
        NMOD = 0
        GOTO 999
      END IF
C
      NMOD = IC(LMSGH+10)
      J=0
      DO 100 I=1,307
        LMSGM = LC(LMSGH-I)
        IF (LMSGM.EQ.0) GOTO 100
        IF(IC(LMSGM+10).GT.0)THEN
          J=J+1
          MMOD(J) = I
          NSCI(J) = IC(LMSGM+10)
        ENDIF
  100 CONTINUE 
C
  999 RETURN
      END
