      LOGICAL FUNCTION FFICAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calorimeter FFREAD cards defined here
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      LOGICAL PRT_FFICAL
      INTEGER I
C
C     DATA LOECUT/.020/,HIECUT/.500/,X0STEP/1.0/
C     DATA DCUSER/2.76, 0.43, 0.42, 0.0, 0.23, 5*0.0/
C     DATA DCUSEI/10*0/
C----------------------------------------------------------------------
      FFICAL = .TRUE.
      IF ( DCAL .LE. 0 ) GOTO 999
C
      SCAL(1) = 1.    ! 0 -> GEOMETRICAL TOWERS, 1 -> SOFTWARE TOWERS
      SCAL(2) = 0.    ! 0 -> DROP TRACKS AFTER DIGI, 1 -> KEEP TRACKS
      SCAL(3) = 0.    ! 1 -> Save CAEP bank in output
      SCAL(4) = 0.    ! 1 -> Calculate and Save JETS bank
      SCAL(5) = .001  ! Calculate CAD banks - zero suppression in GeV
                      !  <0 -> do not calculate CAD banks
      SCAL(6) = 0.    ! 1 -> Save ICD Hits bank
      SCAL(7) = 999.    ! Pt cut for forcing SHWG = 0
      DO I = 8, 10    ! Unused
        SCAL(I) = 0.
      ENDDO
      DECA = DCAL        ! Turn on ECA
      DUCA = DCAL        ! Turn on UCA
      DCRY = DCAL        ! Turn on Cryostat (1,2,3 are all the same)
      DEAD = DCAL        ! Turn on Dead material

C
      CALL FFKEY('SCAL',SCAL,10,'REAL')
      CALL FFKEY('DECA',DECA,1,'INTEGER')
      CALL FFKEY('DUCA',DUCA,1,'INTEGER')
      CALL FFKEY('DCRY',DCRY,1,'INTEGER')
      CALL FFKEY('DEAD',DEAD,1,'INTEGER')
C
      CALL FFKEY('LOECUT',LOECUT,1,'REAL')
      CALL FFKEY('HIECUT',HIECUT,1,'REAL')
      CALL FFKEY('X0STEP',X0STEP,1,'REAL')
      CALL FFKEY('DCUSER',DCUSER,10,'REAL')
      LOECUT = .02
      HIECUT = .50
      X0STEP = 1.0
      DCUSER(1) = 2.76
      DCUSER(2) = 0.43
      DCUSER(3) = 0.42
      DCUSER(4) = 0.00
      DCUSER(5) = 0.23
      DO I = 1,10
        DCUSEI(I) = 0.
      ENDDO
      DO I = 6,10
        DCUSER(I) = 0.
      ENDDO

C
C
C ****  Define cards to change cuts for tree-pruning algorithm
C ****  and supply default values
C
      CALL FFKEY('EDRP',EDRPMAX,1,'REAL')
      CALL FFKEY('WDRP',WDRPMAX,1,'REAL')
      EDRPMAX = .05
      WDRPMAX = 500.
C
      ENTRY PRT_FFICAL
C
      PRT_FFICAL = .TRUE.
      WRITE (LOUT,9000) DECA,DUCA,DCRY,DEAD,LOECUT,HIECUT,X0STEP,
     &  (SCAL(I),I=1,10)
 9000 FORMAT(' FFICAL ** DECA ',I3,' DUCA ',I3,' DCRY ',I3,' DEAD ',I3/
     &  10X,' LOECUT ',F8.6,' HIECUT ',F8.6,' X0STEP ',F8.3/
     &  10X,' SCAL: ',10G6.1)
      WRITE (LOUT,9001) EDRPMAX*1.E6,WDRPMAX
 9001 FORMAT(11X,'EDRPMAX = ',G10.2,' keV; WDRPMAX = ',G10.2)
C
  999 RETURN
      END
