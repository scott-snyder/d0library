      SUBROUTINE ISAQFL
C-------------------------------------------------------------------
C-
C-  Fill ISAQ Zebra banks (initial and final partons)
C-
C-      SDP JAN.,1986
C-   Updated  12-DEC-1989   Serban D. Protopopescu   
C-
C--------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:JETSET.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISALNK.INC'
      INTEGER LISAE
      INTEGER I,LISAQ,JOR
      REAL PHI1,THETA,ETA
C--------------------------------------------------------------------
C
      LISAQ=0
C
      IF(NJSET.NE.0) THEN
C
        DO 200 I=1,NJSET       ! loop over all partons
C   keep only stable partons and skip ones with 0 energy
C   skip also t-quarks as decay products will be added later
          IF(JDCAY(I).EQ.0.AND.PJSET(4,I).GT.0.
     &      .AND.IABS(JTYPE(I)).NE.6) THEN 
            JOR=JORIG(I)/JPACK
            CALL BKISAQ(LISAQ)
            QREF(I)=LISAQ
            LQ(LISAQ-1)=0      !  reference link is 0 for initial partons
            IF(JOR.LT.10) 
     $        LQ(LISAQ-1)=PQREF(JOR)
C   fill Zebra bank
            IQ(LISAQ+1)=JTYPE(I)                  ! parton type
            CALL UCOPY(PJSET(1,I),Q(LISAQ+2),5)   ! momenta and mass
C   calculate PHI1,theta and eta
            CALL ISPETA(PJSET(1,I),THETA,PHI1,ETA)
            Q(LISAQ+7)=PHI1
            Q(LISAQ+8)=THETA
            Q(LISAQ+9)=ETA
          ENDIF
  200   CONTINUE
C
      ENDIF
      RETURN
      END
