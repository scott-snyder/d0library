      SUBROUTINE MUTSAM (JQUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  reconstruct muon tracks in SAMUS detector
C-                          Uses SAMTRK
C-
C-   Inputs  :              JQUAD  Quadrant number 13 is North,
C-                                                 14 is South
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  13-DEC-1993   M. Fortner   from SAANAL
C-   Updated  23-FEB-1994   Alexander Efimov
C-            6/24   MF     Use SAMUOT, MUIFW3
C-   Updated  20-JAN-1995   Andrei Mayorov  change par. list in call to SAHTFL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER JQUAD
      INTEGER LMTRH, LSAMT
      INTEGER GZMTRH, GZSAMT
      INTEGER DIR, KEYTRG, NTRK
      INTEGER IT,ITRAK
      LOGICAL OK
C
C ****  Creation RECO banks
C
      IF (JQUAD .NE. 13 .AND. JQUAD .NE. 14) GO TO 999
      DIR = JQUAD - 12                ! 1 is North and 2 is South
      LMTRH = GZMTRH (0)
      IF (LMTRH .NE. 0) THEN
        LSAMT = GZSAMT ()
        IF (LSAMT .EQ. 0) THEN
          CALL BKSAMT (LSAMT)
          CALL SAHTFL (OK)
          IF (.NOT. OK) THEN                       ! no hits
            CALL MZDROP (IXCOM, LSAMT, ' ')
            GO TO 999
          END IF
        END IF
      ELSE
        CALL ERRMSG ('NO LMTRH BANK IN MUTSAM.','MUON',' ','W')
      ENDIF
C
C   Track reconstruction and MUOT filling
C
      CALL SAMTRK (DIR, KEYTRG, NTRK)
      IF (KEYTRG.LT.0.OR.NTRK.EQ.0) GO TO 999
      DO IT = 1, NTRK
        CALL SAMUOT(DIR,IT,ITRAK)
        CALL MUIFW3(ITRAK)
      ENDDO
C
  999 CONTINUE
      RETURN
      END
