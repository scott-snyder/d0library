      SUBROUTINE PFENDR(HALF)
C---------------------------------------------------------------------
C
C  Purpose and Method: Draw end view of FDC.
C  
C  Inputs:  none
C  Outputs: HALF = FDC Half
C
C  Created 20-JAN-1991   Jeffrey Bantly
C-   Updated  21-FEB-1991   Lupe Howell  Prompt displayed in COMPACK window 
C
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER ISECTR,HALF,LEN,MODULE,II,JJ
      REAL    INR,OUTR     ! Inner and Outer radius of a PHI chamber
      REAL    DIMENS(6),SIZ
      LOGICAL WAIMEN,TWAIMEN
      CHARACTER*15 TEXT
      CHARACTER*60 PROM
      CHARACTER*80 STRING
      DATA MODULE /2/
      DATA PROM/' Enter HALF (0 or 1; default 0)>'/
      DATA TWAIMEN / .TRUE. /
      SAVE MODULE,PROM,TWAIMEN
C---------------------------------------------------------------------
C
      CALL PFUMES(' FDC END VIEW - TRACKS ONLY')
      HALF = 0
      CALL OUTMSG('1')
      CALL GETPAR(1,PROM,'U',STRING) 
      CALL SWORDS(STRING,II,JJ,LEN)
      IF(LEN.NE.0) READ(STRING(1:LEN),*,ERR=999)HALF
      IF(HALF.NE.0 .AND. HALF.NE.1) HALF=0
      WRITE(TEXT,100) HALF
  100 FORMAT('   Half ',I4,'  ')
C
      CALL PUGETV('WAIT FOR MENU',WAIMEN)
      CALL PUSETV('WAIT FOR MENU',TWAIMEN)
      CALL GTFWAL(MODULE,DIMENS)
      IF( DIMENS(1).LT. 0.0) GOTO 999
      INR  = DIMENS(1)
      OUTR = DIMENS(2)
C
C  Cell By Cell Loop Process
C
      CALL PXCOLR('GRE')
      CALL JCIRCL(0.,0.,0.,INR-2.,0)
      CALL JCIRCL(0.,0.,0.,INR,0)
      CALL JPINTR(0)
      CALL JCIRCL(0.,0.,0.,OUTR+1.,0)       
      CALL JCIRCL(0.,0.,0.,OUTR,0)       ! Drawing the outer most circle
C-----------------------------------------------------------------------
  999 CONTINUE
      CALL PUSETV('WAIT FOR MENU',WAIMEN)
      RETURN
      END 
