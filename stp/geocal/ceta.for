      SUBROUTINE CETA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This subroutine creates the CTOW bank.  The
C-                 CTOW bank describes the tower structure of the
C-                 calorimeter for a single eta slice.  This bank
C-                 dispatches to CLYR banks which contain the cell
C-                 geometry for phi = 0.
C-
C-   Zebra Banks Lifted:   CETA, CSHA
C-   Zebra Banks Modified: CEDP
C-   Inputs  :             None
C-   Outputs :             None
C-   Controls:             None
C-
C-   Created  25-NOV-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
C
      INTEGER MCETA(5), NEN, JDEPTH, IOCETA, LPREV, IETA, IERR
      REAL    CENETA, DELETA, CENPHI, DELPHI
C
      DATA MCETA / 'CETA', 18, 18, 11, 9 /
C
      CALL MZFORM('CETA','8I3F',IOCETA)
      MCETA(5) = IOCETA
C
      LQCEDP = LC(LCGEH-IZCEDP)
      NEN = IC(LQCEDP + IGNETA)        ! number of etas
C
      DO 100 IETA = 1, NEN             ! loop for etas
      IF(IETA .EQ. 1) THEN
        CALL MZLIFT(IDVSTP, LQCETA, LQCEDP, -IZCETA, MCETA, 0)      ! 1st eta
      ELSE
        CALL MZLIFT(IDVSTP, LQCETA, LQCETA, 0, MCETA, 0)    ! subsequent
C                                        ! etas
        LC(LQCEDP - IZCETA - IETA + 1) = LQCETA            ! fill links
C                                        ! into CEDP for rapid dispatching
      END IF
      IC(LQCETA + IGIETA) = IETA         ! eta ID
      IF(IETA .LE. 12) THEN              ! Central Calorimeter cells
        IC(LQCETA + IGMPHI) = 4          ! min # of phis required
      ELSE                               ! End Calorimeter only cells
        IC(LQCETA + IGMPHI) = 1          ! min # of phi's required
      END IF
      IC(LQCETA + IGEMDT) = JDEPTH(-2)   ! # of EM depths
      IC(LQCETA + IGMGDT) = JDEPTH(-3)   ! # of MG or IDT depths
      IC(LQCETA + IGFHDT) = JDEPTH(-4)   ! # of FH depths
      IC(LQCETA + IGCHDT) = JDEPTH(-5)   ! # of CH depths
      IC(LQCETA + IGCCDT) = JDEPTH(-6)   ! # of CC depths
      IC(LQCETA + IGECDT) = JDEPTH(-7)   ! # of EC depths
      CALL CALETA( IETA, CENETA, DELETA, IERR) ! obtain eta, delta eta
      C(LQCETA + IGETA) = CENETA         ! nominal eta at tower center
      C(LQCETA + IGDETA) = DELETA        ! delta eta of tower
      CALL CALPHI( 1, IETA, CENPHI, DELPHI, IERR) ! obtain phi, delta phi
      C(LQCETA + IGDFI) = DELPHI        ! delta phi of tower
C
      CALL TOWSHP(LQCETA)                ! contruct shape for tower
  100 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
