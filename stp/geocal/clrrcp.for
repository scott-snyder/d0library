      SUBROUTINE CLRRCP( LCLYR, LCSCN, ICD_ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obtain SRCP position data for CLYR (temp) 
C-       banks.  This bank will be transferred to the permanent
C-       CLYR bank later.
C-
C-   Inputs  :     LCLYR     pointer to temporary CLYR bank
C-                 LCSCN     pointer to CSCN bank
C-                 ICD_ETA   title to SRCP array
C-   Outputs :     in zebra banks
C-   Zebra Banks Altered:    CLYR, CSCN
C-
C-   Created   2-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
C
      CHARACTER ICD_ETA*11, ICD_DIV*11, SHAP*4
      INTEGER LCLYR, LCSCN, I, INDX, IER, ICD, LOUT
      REAL R, PHI, PHI1, PHI2
C
      DATA LOUT / 6 /
C
      CALL EZGET(ICD_ETA,IVAL,IER)  ! get icd arrays
      IF (IER .NE. 0) THEN
        WRITE (6,*) ' ICD ARRAY NOT FOUND: ', ICD_ETA
        STOP 557
      END IF
C
      ICD_DIV = ICD_ETA
      ICD_DIV(5:7) = 'DIV'
      CALL EZGET(ICD_DIV,IVAL(101),IER)  ! get icd division array
      IF (IER .NE. 0) THEN
        WRITE (6,*) ' ICD DIVISION ARRAY NOT FOUND: ', ICD_DIV
        STOP 558
      END IF
C
      WRITE (SHAP, '(A4)') IVAL(2)
      IF(SHAP .EQ. 'CONE') THEN
        R=0.25*(RVAL(13)+RVAL(14)+RVAL(15)+RVAL(16))
      ELSE IF (SHAP .EQ. 'TUBE') THEN
        R=0.5*(RVAL(12)+RVAL(13))
      ELSE
        WRITE (LOUT,*) ' only CONE and TUBE are currently accepted : ',
     &    SHAP
        STOP 559
      END IF
C
      IC(LCSCN + IGNPHI) = IVAL(106)    ! number of divisions
      IC(LCLYR + ICELID) = IC(LCSCN + IGIDEN)    ! keep identifier
      PHI1 = 0.
      PHI2 = TWOPI/IVAL(106)
      PHI = 0.5*(PHI1+PHI2)
C
      C(LCLYR + ICX) = R * COS(PHI)
      C(LCLYR + ICY) = R * SIN(PHI)
      C(LCLYR + ICZ) = RVAL(10)
C
      IC(LCLYR + ICNPAR) = IVAL(11) + 2
      CALL UCOPY(RVAL(12), C(LCLYR+ICPAR1), IVAL(11))
      SHAP(4:4) = 'S'
      READ(SHAP,'(A4)') IC(LCLYR+ICSHAP)
      C(LCLYR+ICPAR1+IVAL(11)) = PHI1/RADIAN     ! ICPAR6 (CONS) or
                                        ! ICPAR4 (TUBS)
      C(LCLYR+ICPAR2+IVAL(11)) = PHI2/RADIAN     ! ICPAR7 (CONS) or
                                        ! ICPAR5 (TUBS)
      LC(LCLYR-IXCLAY)=LCSCN
C----------------------------------------------------------------------
  999 RETURN
      END
