      SUBROUTINE DILBOSON_MC(NLEP,ERRMOM,ERRPHI,ERRTHE,MATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare reconstructed tracks with their
C-                         ISAJET counterparts.      
C-   Inputs  : NLEP=1,2
C-   Outputs : ERRMOM,ERRPHI,ERRTHE,MATCH
C-   Controls: 
C-
C-   Created  14-NOV-1991   C.R.MURPHY
C-   Updated  24-MAR-1992   c.r.murphy - Added veekin.inc and dropped 
C-   some arguments in call to lep_mc_match.       
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAL.LINK'
      REAL ISLEP(3,2),ERRMOM,ERRPHI,ERRTHE
      REAL MOM1_ISAL,MOM2_ISAL,PHI1_ISAL,
     +     PHI2_ISAL,THE1_ISAL,THE2_ISAL
      REAL P1,P2,E1,E2,P1COMP(3),P2COMP(3),INVMASS,
     +     THE1,THE2,PHI1,PHI2,PT,OPANGLE,PT1_ISAL,PT2_ISAL,
     +     PT_ISALMAX,PT_MIN1,PT_MIN2,PT_MIN,AMAX1 
      INTEGER NISAL,NLEP,LISAE,GZISAE,LISAL,NZBANK
      INTEGER IOK,I1,I2,ICALL,IER
      LOGICAL FOUND,MATCH
      DATA ICALL/0/
C
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DILBOSON_RCP')
        CALL EZGET('PT_MIN1',PT_MIN1,IER)
        CALL EZGET('PT_MIN2',PT_MIN2,IER)
        CALL EZGET('PT_MIN',PT_MIN,IER)
        ICALL=1
      ENDIF
C  
      MATCH=.FALSE.
      GO TO (10,20) NLEP
   10 LISAE=GZISAE()
      IF (LISAE.EQ.0) GO TO 999
      LISAL=LQ(LISAE-IZISAL)
      NISAL=NZBANK(IXCOM,LISAL)
C
C     LOOP OVER ISAL BANKS
C
      IOK=0
      IF (NISAL.LE.1) GO TO 999
      DO I1=1,NISAL-1
        DO I2=I1+1,NISAL
          CALL DILBOSON_ISA(I1,I2,P1,P1COMP,E1,P2,P2COMP,E2,
     +                 THE1,PHI1,THE2,PHI2,INVMASS,FOUND)
          IF (.NOT.FOUND) GO TO 666
          IF (THE1.LE.0..OR.THE2.LE.0.) GO TO 666
          PT1_ISAL=SQRT(P1COMP(1)**2+P1COMP(2)**2)
          PT2_ISAL=SQRT(P2COMP(1)**2+P2COMP(2)**2)
          PT_ISALMAX=AMAX1(PT1_ISAL,PT2_ISAL)
          IF (PT_ISALMAX.LT.PT_MIN1) GO TO 666
          IF (INVMASS.GE.3.0.AND.INVMASS.LT.3.2) THEN
            IOK=IOK+1
            PHI1_ISAL=PHI1
            THE1_ISAL=THE1
            PHI2_ISAL=PHI2
            THE2_ISAL=THE2
            MOM1_ISAL=P1
            MOM2_ISAL=P2
            PT=SQRT((P1COMP(1)+P2COMP(1))**2 +
     +                   (P1COMP(2)+P2COMP(2))**2)
            OPANGLE=ACOS(1.-INVMASS**2/(2.*P1*P2))*57.296
          ENDIF
  666     CONTINUE
        ENDDO
      ENDDO
      IF (IOK.EQ.0) GO TO 999
C
      ISLEP(1,1)=MOM1_ISAL
      ISLEP(2,1)=THE1_ISAL
      ISLEP(3,1)=PHI1_ISAL
      ISLEP(1,2)=MOM2_ISAL
      ISLEP(2,2)=THE2_ISAL
      ISLEP(3,2)=PHI2_ISAL
   20 CALL LEP_MC_MATCH(NLEP,ISLEP,ERRMOM,ERRPHI,ERRTHE,MATCH)
C
  999 RETURN
      END
