      SUBROUTINE CELL_VOLUME(IETA, IPHI, ILAYER, VOLUME, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE VOLUME OF
C-           A CELL.  THE CELL IS ADDRESSED BY THE PHYSICS VARIABLES.
C-           FOR A CELL THAT IS COMPOSED OF A NUMBER OF SUBCELLS THE 
C-           VOLUME IS THE SUM OF THE SUBCELL VOLUMES.  
C-
C-   Inputs  :    IETA      PHYSICS ETA NUMBER
C-                IPHI      PHYSICS PHI NUMBER
C-                ILAYER    PHYSICS LAYER NUMBER
C-   Outputs :    VOLUME    VOLUME OF CELL
C-                OK        ERROR FLAG -- 0: OK 
C-                                        1: NO VOLUME FOR GIVEN INDICES
C-                                        2: INVALID RETURN FROM CALPHI
C-   Controls: 
C-
C-   Created   5-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, IERR, OK
      REAL CELVOL, VOL, SGN, PSI, X1, VOLUME
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
      OK = 1
      IF( FIRST) THEN                  ! initialize PSI
        IF( IERR .NE. 0) THEN          ! CALPHI in INIT_SINTBL has
                                       ! invalid return
          OK = 2                       ! should not happen
          RETURN
        END IF
        FIRST = .FALSE.
      END IF
C
      JETA = ABS(IETA)                 ! magnitude of eta
      SGN = SIGN(1, IETA)              ! sign of eta
      IF( SGN.LT. 0. .AND. ILAYER .GE. LYEM3A .AND. ILAYER .LE. LYEM3D
     &  .AND. JETA .LE. 26)THEN
        JLAYER = MOD(ILAYER-1,4) + 3   ! symmetry for negative FLOOR 3
      ELSE
        JLAYER = ILAYER
      END IF
C
      LQCEDP = LC(LCGEH - IZCEDP)      ! pointer to tower dispatching
C                                      ! bank
      LQCETA = LC(LQCEDP - IZCETA - JETA + 1)    ! pointer to constant
C                                        ! eta bank
      LQCLYR = LC(LQCETA - IZCLYR - JLAYER + 1)  ! pointer to first
C                                        ! appropriate layer bank
      MPHI = IC(LQCETA + IGMPHI)         ! number of phi's present for
C                                        ! this eta
      JPHI = MOD( IPHI-1, MPHI) + 1      ! index to representative phi
      NSBLAY = JBYT( C(LQCLYR), JBNSBC, NBNSBC)  ! number of sublayers
C
      IF( NSBLAY .LE. 1) THEN
   30   IF( LQCLYR .EQ. 0) GO TO 999
          VOLUME = CELVOL( LQCLYR)
          GO TO 170
      ELSE
        VOLUME = 0.                      ! total volume
  100   IF(LQCLYR .EQ. 0) GO TO 150
          KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
          IF (KPHI .NE. JPHI) GO TO 130
          JSBLAY = JBYT( C(LQCLYR), JBSBCL, NBSBCL)
          IF( JSBLAY .EQ. 0) GO TO 130
          VOL = CELVOL( LQCLYR)        ! volume of sub-cell
          IF( VOL.LE.0. ) GO TO 130
          VOLUME = VOLUME + VOL
  130   LQCLYR = LC(LQCLYR)
        GO TO 100
C
  150   CONTINUE
        IF(VOLUME .EQ. 0) GO TO 999
      END IF
C
C     NOW ROTATE COORDINATES FOR PHI .NE. 1
C
  170 CONTINUE
      OK = 0
C----------------------------------------------------------------------
  999 RETURN
      END
