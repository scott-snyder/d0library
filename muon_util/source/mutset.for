      SUBROUTINE MUTSET(IQUAD,SLBC,SLA,NHIT,X,Y,Z,ILAYR,NLAYR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    FILLS TEMPORARY ARRAYS FOR MUON TRACKING
CC    LOOKS FOR HITS IN SAME QUADRANT
CC
CC    INPUT: IQUAD  -  QUADRANT
CC           SLBC,SLA - SLOPE IN BC AND A LAYER
CC    OUTPUT: NHIT(NPLN) - NUMBER OF HITS IN PLANE
CC            X(NHIT,NPLN) - POSITION IN BEND VIEW - 2 HITS; ONE FOR
CC                           EACH DRIFT SOLUTION
CC            Y(NHIT,NPLN) - POSITION ALONG WIRE
CC            Z(NHIT,NPLN) - POSITION BETWEEN PLANES
CC            ILAYR(4)     - NUMBER HITS A,B,C,D LAYER
CC
CC    ONLY DOES FIRST TIME FOR HITS ON A GIVEN CELL
CC
CC       D. HEDIN   3-13-86 
CC     DH 11-86 NEW ZEBRA FORMATS; 4 MODULE SETUP
CC     DH 4-87 PLANES COUNT 0-2
CC       DH 12-87 REMOVE VERNIER PAD FROM NON-BEND POSITION
CC     DH 2-88 CHANGE LAYER
CC     DH 10/88 add ORENT=3,4 (forward)---SET UP FOR D0
CC     DH 12/89 FIX ORIENTATION
CC     DH 3/90 SEE IF HIT ALREADY ON A TRACK, ADD LAYER 4
CC     DH 5/90 COUNT HITS IN LAYERS
CC     DH 2/92 28 words/MUOH hit
CC     DH 3/92 if dt1=unphysical, use dt2 if available
CC     DH 9/92 track by octants in central top/bottom
CC     DH 11/92 larger arrays; kill 251,255,281,285
CC     DH 1/93 hit/track if >1 (fit solution)
CC     DH 7/94 allow for missing drift times
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IQUAD,GZMUHT,GZMUOH,LMUHT,LMUOH,NMUSRT,I,LP,IADD,NMAX
      INTEGER LMUOF,GZMUOF,IMUOF,NPL
      PARAMETER (NMAX=100)
      INTEGER NHIT(13),NMOD,NPLN,NWIR,IERR,JQUAD,LAYER,K
      INTEGER IORENT,MULAYR,MUQUAD2,ILAYR(4),NLAYR
      REAL DT,SLBC,SLA,X(NMAX,13),Y(NMAX,13),Z(NMAX,13),COST,XX,YY,ZZ
      INTRINSIC SQRT    
CC
      LMUHT=GZMUHT(0)
      LMUOH=GZMUOH(0)
      LMUOF=GZMUOF(0)
      NMUSRT=IQ(LMUHT+2)
      NLAYR=0
      DO I=1,4
        ILAYR(I)=0
      ENDDO
      DO I=1,13
        NHIT(I)=0
      ENDDO
C
      DO 100 I=1,NMUSRT
      LP=28*(I-1)+LMUOH
      IF(IQ(LP+3).GT.0) GO TO 100        ! SEE IF ALREADY USED
      IF(IQ(LP+6).EQ.0) GO TO 100        ! SKIP IF NO TIMES
      IADD=IQ(LP+1)       ! WIRE ADDRESS
      CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR)
      IF(NMOD.EQ.251.OR.NMOD.EQ.255) GO TO 100
      IF(NMOD.EQ.281.OR.NMOD.EQ.285) GO TO 100
CC   SEE IF IN RIGHT QUADRANT
      JQUAD=MUQUAD2(NMOD)
      IF(JQUAD.NE.IQUAD) GO TO 100
CC  SKIP OUT IF 1 PLANE IN MODULE
      IMUOF=LMUOF+(IQ(LMUHT+10+NMOD)-1)*10
c      IF(JQUAD.GE.5.AND.IQ(IMUOF+10).LE.1) GO TO 100
      LAYER=MULAYR(NMOD)
      ILAYR(LAYER)=ILAYR(LAYER)+1
      IORENT=IABS(IQ(LP+5))
CC  ARRANGE OUTPUT SO IN 'REVERSE' ORDER AS THIS IS WHAT DARIA DID ORIGINALLY
CC  AND IT MAKES SENSE FOR NORMAL 3 MODULE D0 SETUP. 
      IF(LAYER.EQ.1) THEN
        K= 13-NPLN
        COST=1./SQRT(1.+SLA**2)
      ELSE IF(LAYER.EQ.2) THEN
        K=9-NPLN
        COST=1./SQRT(1.+SLBC**2)
      ELSE IF(LAYER.EQ.3) THEN
        K=6-NPLN
        COST=1./SQRT(1.+SLBC**2)
      ELSE IF(LAYER.EQ.4) THEN
        K=3-NPLN
        COST=1./SQRT(1.+SLBC**2)
      ENDIF
CC
CC    CONVERT INTO TRACKING 'VIEW'
CC
      IF(IORENT.EQ.1) THEN
        XX=Q(LP+23)
        YY=Q(LP+22)
        ZZ=Q(LP+21)
      ELSE IF(IORENT.EQ.2) THEN
        XX=Q(LP+23)
        YY=Q(LP+21)
        ZZ=Q(LP+22)
      ELSE IF(IORENT.EQ.3) THEN
        XX=Q(LP+21)
        YY=Q(LP+22)
        ZZ=Q(LP+23)
      ELSE IF(IORENT.EQ.4) THEN
        XX=Q(LP+22)
        YY=Q(LP+21)
        ZZ=Q(LP+23)
      ENDIF
CC         INCLUDE BOTH DRIFT SOLUTIONS
      IF(NHIT(K).EQ.NMAX) THEN
        GO TO 100
      ENDIF
      DT=Q(LP+17)
      IF(IQ(LP+6).EQ.2.AND.ABS(DT).GT.999.) THEN
        IF(ABS(Q(LP+18)).LT.999.) DT=Q(LP+18)
      ENDIF
      NHIT(K)=NHIT(K)+1
      X(NHIT(K),K)=XX-Q(LP+15)/COST
      Y(NHIT(K),K)=YY+DT
      Z(NHIT(K),K)=ZZ
      NHIT(K)=NHIT(K)+1
      X(NHIT(K),K)=XX+Q(LP+15)/COST
      Y(NHIT(K),K)=YY+DT
      Z(NHIT(K),K)=ZZ
 100  CONTINUE
      DO I=1,4
        IF(ILAYR(I).GT.0) NLAYR=NLAYR+1
      ENDDO
      RETURN
      END
