      SUBROUTINE MU_HITPLN_CUT(LMUOT,ICUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : LMUOT, the location in MUOT of the track candidate.
C-   Outputs : ICUT, a Logical. TRUE means id'ed as junk.
C-   Controls: Possible implementation of control for L2.
C-
C-   Created  18-DEC-1992   Tom Diehl
C-   Modified 04-JAN-1993   Tom Diehl  Implement KB's new MUCPLN
C-   DH 1/93   NC=NC+ND ADDED FOR FORWARD  
C-            01-FEB-1993   Tom D.     Turn on all SAMUS layer counting 
C    DH 3/93 muse adjacent in EF/CF region
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL ICUT
      INTEGER LMUOT,I,JBIT,IFW3
      INTEGER PLANES_ALL,PLANES_FIT,IAPLN,IFPLN
      INTEGER NA,NB,NC,ND,NA_ADJ,NBC_ADJ,SA,SB,SC
      INTEGER NATOT,NBTOT,NCTOT,NSAMUS,NTOT
      INTEGER NAADJTOT,NBADJTOT,NCADJTOT
C----------------------------------------------------------------------
      ICUT = .FALSE.
      IF(BTEST(IQ(LMUOT+5),10)) RETURN        ! SA-SB-WC CANDIDATE 
      IFW3=IQ(LMUOT+6)
      CALL MUCPLN(LMUOT,PLANES_ALL,PLANES_FIT,IAPLN,IFPLN)
      CALL MUUPLN(PLANES_ALL,NA,NB,NC,ND,SA,SB,SC,NA_ADJ,NBC_ADJ)
      IF(IFW3.GE.100) THEN    ! EF/CE ADJACENT
        NA=NA+NA_ADJ
        NB=NB+NBC_ADJ
      ENDIF
      NATOT = 0
      NBTOT = 0
      NCTOT = 0

      NAADJTOT = 0
      NBADJTOT = 0
      NCADJTOT = 0  

      DO I = 0,3
        IF(BTEST(IAPLN,I)) NATOT = NATOT + 1          ! A LAYER
      ENDDO

      DO I = 4,7
        IF(BTEST(IAPLN,I)) NAADJTOT = NAADJTOT + 1    ! A'  LAYER
      ENDDO

      DO I = 0,2
        IF(BTEST(IAPLN,I+8)) NBTOT = NBTOT + 1        ! B  LAYER
        IF(BTEST(IAPLN,I+14)) NCTOT = NCTOT + 1       ! C  LAYER
        IF(BTEST(IAPLN,I+20)) NCTOT = NCTOT + 1       ! D  LAYER
      ENDDO

      DO I = 0,2
        IF(BTEST(IAPLN,I+11)) NBADJTOT = NBADJTOT + 1 ! B  LAYER
        IF(BTEST(IAPLN,I+17)) NCADJTOT = NCADJTOT + 1 ! C  LAYER
        IF(BTEST(IAPLN,I+23)) NCADJTOT = NCADJTOT + 1 ! D  LAYER
      ENDDO

      NSAMUS = IQ(LMUOT + 2)

      IF(SA.GT.0) NA = NA + 2
      IF(SB.GT.0) NB = NB + 2
      IF(SC.GT.0) NC = NC + 2

      NC=NC+ND

      NTOT = NA + NB + NC 

      IF(NA.EQ.3.AND.NB.LE.1.AND.NC.LE.1) THEN
        ICUT = .TRUE.
      ENDIF

      IF(NA.EQ.2.AND.NB.LE.1.AND.NC.LE.1) THEN
        ICUT = .TRUE.
      ENDIF

      IF(NA.EQ.1.AND.NB.LE.1.AND.NC.LE.1) THEN
        ICUT = .TRUE.
      ENDIF

      IF(NTOT.LE.5.AND.NA.LE.3) THEN
        ICUT = .TRUE.
      ENDIF     

  999 RETURN
      END
