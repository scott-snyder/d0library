      FUNCTION L2CR_READ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read from STP banks: L2CR the calorimeter
C-                         geometry info needed to fill L2CRCAL common
C-
C-   Returned value  : TRUE if we completed without error
C-   Inputs  :
C-   Outputs :
C-   Controls: See D0$INC:L2CRCAL_CONT.INC
C-
C-   Created  20-NOV-1990   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! ZEBSTP COMMON
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'      ! tool common
      INCLUDE 'D0$INC:L2CRCALS.INC'     ! strings
      LOGICAL L2CR_READ
      INTEGER GZL2CR,LL2CR,I,J,IPOINT
      INTEGER NCIETA1,NCLAYER1          ! sizes of arrays
      INTEGER NDATA1                    ! # of data words
C----------------------------------------------------------------------
      L2CR_READ = .FALSE.               ! Assume the worst
C
C---Get link to L2CR bank which should be read in for us from .STP file
C
      LL2CR = GZL2CR()
      IF (LL2CR .LE. 0) THEN
        MUMSG = ' Could not find L2CR bank '
        GOTO 800
      END IF
C
C---Fill arrays from bank. See D0$INC:L2CRCAL_CONT.INC as well as L2CR.ZEB
C---Note I read 2D arrays assuming the first argument runs the fastest.
C---See L2CR_BUILD.
C
      NCIETA1 = IC( LL2CR + 2)
      NCLAYER1= IC( LL2CR + 3)
C---Check that these parameters as written in the L2CR match the ones
C---expected from the common block:
      IF (NCIETA1 .NE. NCIETA .OR. NCLAYER1 .NE. NCLAYER) THEN
        MUMSG = ' L2CR bank parameters does not match L2CRCAL common '
        GOTO 800
      END IF

C
      IPOINT = 5
C---Read in layer radii:
      DO I = 1, 2*NCLAYER + 2
        RBEG( I ) = C( LL2CR + IPOINT + I - 1)
      END DO
      IPOINT = IPOINT + 2*NCLAYER + 2
C---Read in Z begin and end positions of cells:
      DO I = 1, 2*NCLAYER + 2
        DO J = 1, 2*NCIETA + 2
          ZBEG(J,I) = C( LL2CR + IPOINT + J - 1)
        END DO
        IPOINT = IPOINT + (2*NCIETA+2)
      END DO
      DO I = 1, 2*NCLAYER + 2
        DO J = 1, 2*NCIETA + 2
          ZEND(J,I) = C( LL2CR + IPOINT + J - 1)
        END DO
        IPOINT = IPOINT +(2*NCIETA+2)
      END DO
C---Read in the ILAYER Id for each of our 'layer' indices:
      DO I = 1,2*NCLAYER + 2
        IDLAYER(I) = IC( LL2CR + IPOINT + I - 1)
      END DO
      IPOINT = IPOINT + 2*NCLAYER + 2
C---Read in the IETA id for each of our eta indices:
      DO I = 1,2*NCIETA + 2
        IDIETA(I) = IC( LL2CR + IPOINT + I - 1)
      END DO
      IPOINT = IPOINT + 2*NCIETA + 2
C---Fill logical array : OUT_OF_CC
      CALL VZERO(OUT_OF_CC,(2*NCIETA+2)*(2*NCLAYER+2))
      DO I = 1, 2*NCLAYER + 2
        DO J = 1,2*NCIETA + 2
          IF ( J .EQ. 1 .OR. J .EQ. 2*NCIETA+2) OUT_OF_CC(J,I) = .TRUE.
          IF ( I .EQ. 1 .OR. I .EQ. 2*NCLAYER+2) OUT_OF_CC(J,I) = .TRUE.
          IF (ZBEG(J,I) .LT. -200. .OR. ZBEG(J,I) .GT. 200.)
     &      OUT_OF_CC(J,I) = .TRUE.
          IF (ZEND(J,I) .LT. -200. .OR. ZBEG(J,I) .GT. 200.)
     &      OUT_OF_CC(J,I) = .TRUE.
        END DO
      END DO
C---Count number of words of this bank and compare:
      IPOINT = IPOINT - 1
      NDATA1 = IC( LL2CR + 4)
      IF (IPOINT .NE. NDATA1) THEN
        MUMSG = ' # data words in L2CR is unexpected '
        GOTO 800
      END IF
C---We are done
      L2CR_READ = .TRUE.
      RETURN
  800 CONTINUE
      CALL ERRMSG('L2CRCAL','L2CR_READ',MUMSG,'W')

  999 RETURN
      END
