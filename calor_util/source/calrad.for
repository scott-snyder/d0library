      SUBROUTINE CALRAD(IETA, LAYER, CENRAD, DELRAD, CENZED, DELZED,
     +   ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO RETURN THE MEAN RADIAL POSITION AND THE
C-           RADIAL EXTENT OF A CELL IN THE CENTRAL CALORIMETER.
C-           THIS VERSION OF THE PROGRAM TAKES NUMBERS FROM THE
C-           CALORIMETER ZEBRA GEOMETRY FILES.
C-
C-   Inputs  :    IETA     physics ETA number
C-                LAYER    physics LAYER number
C-   Outputs :    CENRAD   radial position of the cell center
C-                DELRAD   radial extent of the cell
C-                CENZED   longitudinal position of the cell center
C-                DELZED   longitudinal extent of the cell
C-                ARGSOK   error flag -- 0: OK
C-                                       1: IETA or LAYER not in valid range
C-                                       2: cell does not exist
C-   Controls:
C-
C-   Created  12-DEC-1988   Michael W. Peters
C-   Revised  24-MAR-1989   Stephen Kahn
C-            27-SEP-1989   Stephen Kahn  -- new enhanced calling sequence
C-            10-DEC-1990   Nobu Oshima -- Put CALL EZRSET!!!
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER  IETA, LAYER, ARGSOK, IETAA, JLAYER, IL, IER
      REAL     CENRAD, DELRAD, CENZED, DELZED, SGN
      REAL     DZCC(NLYRL), ZCC(NLYRL)
      CHARACTER FLOOR*18, IFL*1
      LOGICAL  FIRST, CEXIST, EZERR
C
      CHARACTER*4 CHAR4,CHAS4
      INTEGER ICHAR4,ICHAS4
      EQUIVALENCE (ICHAR4,CHAR4)
      EQUIVALENCE (ICHAS4,CHAS4)
      DATA     CHAR4 /'TRD1'/
      DATA     CHAS4 /'TRD2'/
      DATA     FIRST  /.TRUE./
      DATA     FLOOR /'CCEM_FLOOR0_VOLUME'/
C
      IF (FIRST) THEN
        CALL EZPICK('SRCP_UCAL')       ! select CC srcp
C
        DO 20 IL = 1, NLYRL
          IF(IL.LT.LYEM3A) THEN
            WRITE(IFL,'(I1)') IL
            FLOOR(3:4) = 'EM'
          ELSE IF (IL.GE.LYEM3A .AND. IL.LE.LYEM3D) THEN
            IFL = '3'
            FLOOR(3:4) = 'EM'
          ELSE IF (IL.EQ.MXLYEM) THEN
            IFL = '4'
            FLOOR(3:4) = 'EM'
          ELSE IF (IL.GE.MNLYFH.AND.IL.LE.MXLYFH-1) THEN
            WRITE(IFL,'(I1)') IL-6
            FLOOR(3:4) = 'FH'
          ELSE IF (IL.EQ.MNLYCH) THEN
            WRITE(IFL,'(I1)') IL-7
            FLOOR(3:4) = 'CH'
          ELSE
            DZCC(IL) = 0
            ZCC(IL)=0
            GO TO 20
          END IF
          FLOOR(11:11) = IFL
          CALL EZGET(FLOOR,RVAL,IER)
          IF(IER .NE. 0) THEN
            ARGSOK = 4
            GO TO 999
          END IF
          IF(IVAL(2) .EQ. ICHAR4) THEN
            DZCC(IL) = 2.*RVAL(14)
            ZCC(IL) = 0.
          ELSE IF (IVAL(2) .EQ. ICHAS4) THEN
            DZCC(IL) = 2.*RVAL(14)
            ZCC(IL) = 0.
          END IF
   20   CONTINUE
        FIRST =.FALSE.
        CALL EZRSET
      ENDIF
C
      ARGSOK = 1
      IF(.NOT.CEXIST(IETA,1,LAYER)) GO TO 999
      IETAA = ABS( IETA)
      SGN = SIGN(1, IETA)
      IF( IETAA .LT. 1 .OR. IETAA .GT. 12) GO TO 999       ! not valid
C                                        ! ETA for Cen Cal
      IF( LAYER .LT.1 .OR. LAYER .GT. 15) GO TO 999        ! not valid
C                                        ! LAYER number
      IF( LAYER .GE. MNLYMG .AND. LAYER .LE. MXLYMG) GO TO 999  !
      ! massless gap or ICD layers
      IF( LAYER .GE. MNLYFH .AND. IETAA .GT. 21-LAYER ) GO TO 999     !
      ! fail cells that are not in CC
      ARGSOK = 2
      IF( IETA .LT. 0 .AND. LAYER .GE. LYEM3A .AND. LAYER .LE. LYEM3D)
     +   THEN
        JLAYER = MOD(LAYER-1,4) + 3      ! symmetry for negative FLOOR 3
      ELSE
        JLAYER = LAYER
      END IF
C
      LQCEDP = LC(LCGEH - IZCEDP)        ! pointer to tower dispatching
C                                        ! bank
      LQCETA = LC(LQCEDP - IZCETA - IETAA + 1)   ! pointer to constant
C                                        ! eta bank
      LQCLYR = LC(LQCETA - IZCLYR - JLAYER + 1)  ! pointer to first
C                                        ! appropriate layer bank
      IF( LQCEDP .EQ. 0 .OR. LQCETA .EQ. 0 .OR. LQCLYR .EQ. 0) GO TO 999
C
      CENRAD = SQRT(C(LQCLYR+ICX)**2 + C(LQCLYR+ICY)**2)
C
      DELRAD = 2.0 * C(LQCLYR + ICPAR4)  ! assumes CEN CAL cells are
C                                        ! described by TRAP shape
      CENZED = SGN * ZCC(LAYER)         ! center z of cell
C
      DELZED = DZCC(LAYER)              ! assumes CEN CAL cells are
C                                        ! described by TRAP shape
      ARGSOK = 0
C----------------------------------------------------------------------
  999 RETURN
      END
