      SUBROUTINE DILBOSON_ISA(I1,I2,P1,P1COMP,E1,P2,P2COMP,E2,
     +                  THE1,PHI1,THE2,PHI2,INVMASS,FOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Loop over ISAL banks and record data
C-                        on leptons specified by rcp parameter
C-                        LEPTONS
C-   Inputs  :I1,I2
C-   Outputs :P1,P2,E1,E2,P1COMP,P2COMP
C-            THE1,THE2,PHI1,PHI2,INVMASS,FOUND
C-   Controls:
C-
C-   Created  24-MAR-1992   C.MURPHY
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I1,I2,I,J,L1,L2,GZMUOT,GZMUON,GZISAL,ID1,ID2,
     +        LSTART,NS1,NS2,LZTRK1,LZTRK2,LTRACK1,LTRACK2
      INTEGER LEPTONS,IER,ICALL
      REAL    P1,E1,P1COMP(3),P2,E2,P2COMP(3),THE,PHI,
     +        THE1,PHI1,THE2,PHI2,INVMASS,S,MUON,ELEC,MASSLEP
      CHARACTER*4 CHAR
      LOGICAL FOUND
      LOGICAL PSIVERT,VERT1,VERT2
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      DATA MUON/0.105658/
      DATA ELEC/0.000511/
      DATA ICALL/0/
C
      FOUND=.FALSE.
C
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DILBOSON_RCP')
        CALL EZGET('LEPTONS',LEPTONS,IER)
        IF (LEPTONS.EQ.12) MASSLEP=ELEC
        IF (LEPTONS.EQ.14) MASSLEP=MUON
        CALL EZRSET
        ICALL=1
      ENDIF
      LSTART=GZISAL()
      DO I=1,I1
        IF (I.EQ.1) THEN
          L1=LSTART
        ELSE
          L1=LQ(L1)
        ENDIF
      ENDDO
      J=I2-I1
      DO I=1,J
        IF (I.EQ.1) THEN
          L2=LQ(L1)
        ELSE
          L2=LQ(L2)
        ENDIF
      ENDDO
C
      ID1=IQ(L1+1)
      ID2=IQ(L2+1)
      IF (ID1*ID2.NE.-LEPTONS*LEPTONS) GO TO 999

      P2=Q(L2+5)
      THE1=Q(L1+8)
      THE2=Q(L2+8)
      IF (LEPTONS.EQ.12) THEN
        IF(P1.GT.0.1.AND.P2.GT.0.1.AND.THE1.GT.0.0065.AND.THE2
     c  .GT.0.0065.AND.THE1.LT.3.135 .AND. THE2.LT.3.135) THEN
          FOUND=.TRUE.
        END IF
      END IF
      IF (LEPTONS.EQ.16) THEN
        IF (P1.GT.1.AND.P2.GT.1) THEN
          FOUND=.TRUE.
        END IF
      END IF
      IF (FOUND.EQ..TRUE.) THEN
        E1=SQRT(P1**2+MASSLEP**2)
        PHI1=Q(L1+7)
        IF (PHI1.LT.0.) PHI1=PHI1+TWOPI
        E2=SQRT(P2**2+MASSLEP**2)
        PHI2=Q(L2+7)
        IF (PHI2.LT.0.) PHI2=PHI2+TWOPI
        P1COMP(1)=Q(L1+2)
        P1COMP(2)=Q(L1+3)
        P1COMP(3)=Q(L1+4)
        P2COMP(1)=Q(L2+2)
        P2COMP(2)=Q(L2+3)
        P2COMP(3)=Q(L2+4)
        S=(E1+E2)**2-(P1COMP(1)+P2COMP(1))**2-
     +      (P1COMP(2)+P2COMP(2))**2-(P1COMP(3)+P2COMP(3))**2
        INVMASS=SQRT(ABS(S))
C
      ENDIF
  999 RETURN
      END
