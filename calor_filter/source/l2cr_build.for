      FUNCTION L2CR_BUILD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill L2CR stp bank with CC geometry info
C                          Use SCAL info.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: use CAL_STP file
C-
C-   Created  12-JUN-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'       ! STP common
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'      ! Cosmic Ray Cal Common
      INCLUDE 'D0$INC:L2CRCALS.INC'
      LOGICAL L2CR_BUILD,JOIN
      INTEGER I,J,K,M,IER,NS,LAY,IETA,GZL2CR,IPOINT,LL2CR
      REAL ZS(2,32),XS(2,32),YS(2,32)
      REAL ZMIN,ZMAX,RMIN,RMAX,R
      LOGICAL OK,CHTINI,CALOR_INI
C----------------------------------------------------------------------
      L2CR_BUILD = .TRUE.
C--- Do we need to build L2CR?
      IF (GZL2CR() .GT. 0) RETURN
C
      L2CR_BUILD = .FALSE.              ! Assume the worst

C---*************** FILL COMMON BLOCK ARRAYS FIRST ***************
C
C--- First fill the arrays that map the array elements to the ieta and
C--- ilayer's that they represent.
C
      IDLAYER( 1) = 0                   ! boundary
      IDLAYER( 2) = 15                  ! CH first muon may see
      IDLAYER( 3) = 13                  ! FH3
      IDLAYER( 4) = 12                  ! FH2
      IDLAYER( 5) = 11                  ! FH1
      IDLAYER( 6) =  7                  ! EM4
      IDLAYER( 7) =  3                  ! start of EM3
      IDLAYER( 8) =  2                  ! EM2
      IDLAYER( 9) =  1                  ! EM1
      IDLAYER(10) =  1
      IDLAYER(11) =  2
      IDLAYER(12) =  3
      IDLAYER(13) =  7
      IDLAYER(14) =  11
      IDLAYER(15) =  12
      IDLAYER(16) =  13
      IDLAYER(17) =  15
      IDLAYER(18) =  0                  ! boundary
C
C---Fill IETA id's
C
      IDIETA(1) = 0                      ! boundary
      IDIETA( 2*NCIETA+ 2) = 0           ! boundary
      J = 1
      IDIETA( 1 ) = 0
      DO I = -12,12
        IDIETA( J+1 ) = I
        IF ( I .NE. 0) J = J+1
      END DO
C
C--- Okay, now we are ready to start putting some values into this:
C--- First read in the calorimeter STP file:
      OK = CALOR_INI()
      IF (.NOT. OK) THEN
        MUMSG = ' CALOR_INI  failed '
        GOTO 800
      END IF
      OK = CHTINI()
      IF (.NOT. OK) THEN
        MUMSG = ' CHTINI failed '
        GOTO 800
      END IF
C--- First lets fill RBEG which are the outside radii of the cells that
C--- might be hit.
      DO I = 2,NCLAYER+1             ! cycle over possible layers
        LAY = IDLAYER( I )
        CALL CELVEC(1,4,LAY,XS,YS,ZS,NS,IER)
        IF (IER .NE. 0) THEN
          MUMSG = ' Invalid cell given to CELVEC'
          GOTO 800
        END IF
        RMAX = 0.
        RMIN = 700.
        DO M = 1,NS
          DO K = 1,2
            R = SQRT(XS(K,M)**2 + YS(K,M)**2)
            IF (R .GT. RMAX) RMAX = R
            IF (R .LT. RMIN) RMIN = R
          END DO
        END DO
C---Now fill RBEG:
        IF (I .EQ. 2) RBEG(1) = RMAX
        IF (I .EQ. 2) RBEG(2*NCLAYER+2) = -RMAX
        RBEG(I) = RMIN
        RBEG( 2*NCLAYER + 2 - I + 1) = -RMIN
      END DO

C---Now lets fill ZBEG and ZEND which is more difficult as some dont exist
C---and as the eta range in Z shortens at lower depths as we are just
C---interested in the CC. Also, we must worry about the EM3 layer.
      DO I = 2,2*NCLAYER + 1
        DO 1000 J = 2,2*NCIETA + 1
          IETA = IDIETA(J)
          LAY  = IDLAYER(I)
C---Handle OUT of CC cells:
          IF (LAY.EQ. 15 .AND. IABS(IETA) .GE. 7) GOTO 900
          IF (LAY.EQ. 13 .AND. IABS(IETA) .GE. 9) GOTO 900
          IF (LAY.EQ. 12 .AND. IABS(IETA) .GE.10) GOTO 900
          IF (LAY.EQ. 11 .AND. IABS(IETA) .GE.11) GOTO 900
          IF (LAY.EQ. 7  .AND. IABS(IETA) .GE.12) GOTO 900
C-- em3 consists of depths 3,4,5,6. At IETA=12 there is only 3 and 4.
C-- At IETA=-12 there is only 5 and 6. So we need a special call. We
C-- will join these non contiguous cells in such a way as to define a
C-- EM3 cell. (consisting of all 4 depths).
          IF (LAY.EQ.3 .AND. IETA .EQ. -12) THEN
            CALL CELVEC(IETA,4,5,XS,YS,ZS,NS,IER)
            GOTO 700
          END IF
          CALL CELVEC(IETA,4,LAY,XS,YS,ZS,NS,IER)

  700     IF (IER .NE. 0) THEN
            MUMSG = ' Invalid cell given to CELVEC'
            GOTO 800
          END IF
          ZMAX = -700.
          ZMIN = 700.
          DO M = 1,NS
            DO K = 1,2
              IF (ZS(K,M) .LT. ZMIN) ZMIN = ZS(K,M)
              IF (ZS(K,M) .GT. ZMAX) ZMAX = ZS(K,M)
            END DO
          END DO
          ZBEG(J,I) = ZMIN
          ZEND(J,I) = ZMAX
C---Handle the OUT of CC cells:
          GOTO 901
  900     CONTINUE
          OUT_OF_CC(J,I) = .TRUE.
          ZBEG(J,I) = LO
          ZEND(J,I) = HI
  901     CONTINUE
 1000   CONTINUE
      END DO
C---Now lets 'join' the calorimeter to the outside 'no man's land' in
C---such a way that if we 'leave' the calorimeter we will hit a very
C---large or very small number and so be trapped.
      DO I = 1,2*NCLAYER + 2
        DO J = 1,2*NCIETA + 2
          IETA = IDIETA(J)
          LAY = IDLAYER(I)
          JOIN = .FALSE.
          IF (LAY.EQ. 15 .AND. IABS(IETA) .EQ. 7) JOIN = .TRUE.
          IF (LAY.EQ. 13 .AND. IABS(IETA) .EQ. 9) JOIN = .TRUE.
          IF (LAY.EQ. 12 .AND. IABS(IETA) .EQ.10) JOIN = .TRUE.
          IF (LAY.EQ. 11 .AND. IABS(IETA) .EQ.11) JOIN = .TRUE.
          IF (LAY.EQ. 7  .AND. IABS(IETA) .EQ.12) JOIN = .TRUE.
          IF (LAY .LT. 7 .AND. J .EQ. 1 .OR. J .EQ. 2*NCIETA+2) JOIN =
     &      .TRUE.
          IF (JOIN) THEN
            IF (LAY .GE. 7) THEN
              IF (IETA .GT. 0) ZBEG(J,I) = ZEND(J-1,I)
              IF (IETA .GT. 0) ZEND(J,I) = HI
              IF (IETA .LT. 0) ZEND(J,I) = ZBEG(J+1,I)
              IF (IETA .LT. 0) ZBEG(J,I) = LO
              OUT_OF_CC(J,I) = .TRUE.
            ELSE
              IF (J .EQ. 1) ZEND(1,I) = ZBEG(2,I)
              IF (J .EQ. 1) ZBEG(1,I) = LO
              IF (J .NE. 1) ZBEG(J,I) = ZEND(J-1,I)
              IF (J .NE. 1) ZEND(J,I) = HI
              OUT_OF_CC(J,I) = .TRUE.
            END IF
          END IF
        END DO
      END DO

C---Now we want to make ILAYER = 1 and NCLAYER+2 the same as the layers
C---near to them. We also want to fix the EM3 cells. Currently, the
C---configuration has gaps between the end (ZEND) of an EM3 cell and
C---the next EM3 cell. This is not true for IETA=12,-12 as they only
C---consisted of one EM3 layer.
      DO I = 1,2*NCLAYER + 2
        DO  J = 1,2*NCIETA + 2
C---Set eta border:
          IF ( I .EQ. 1) THEN           ! Copy the same as I=2 (layer=
c                                       ! 15)
            ZBEG(J,I) = ZBEG(J,2)
            ZEND(J,I) = ZEND(J,2)
            OUT_OF_CC(J,I) = .TRUE.
          ELSE IF (I.EQ. 2*NCLAYER+2) THEN      ! Copy same as other
c                                               ! layer=15
            ZBEG(J,I) = ZBEG(J,2*NCLAYER+1)
            ZEND(J,I) = ZEND(J,2*NCLAYER+1)
            OUT_OF_CC(J,I) = .TRUE.
          END IF
C---Now do EM3 joining. Dont worry about IETA=12,-12 as they were single
C---cells.
          IF (IDLAYER(I) .EQ. 3) THEN         ! EM3
            IETA = IDIETA(J)
            IF (ABS(IETA) .NE. 12 .AND. IETA .NE. 0) THEN
              ZEND(J,I) = ZBEG(J+1,I)
            END IF
          END IF

      END DO
      END DO
C*****************************************************************
C---Find number of data words:
      IPOINT = 2*NCLAYER + 2 + 2*(2*NCIETA+2)*(2*NCLAYER+2)
      IPOINT = IPOINT + 2*NCLAYER + 2 + 2*NCIETA + 2
      IPOINT = IPOINT + 4               ! 4 header words
C---Now book the bank.
      CALL BKL2CR(LL2CR,IPOINT)
      IF (LL2CR .LE. 0) THEN
        MUMSG = ' could not book L2CR '
        GOTO 800
      END IF
C---Now fill L2CR
      IC( LL2CR + 2) = NCIETA
      IC( LL2CR + 3) = NCLAYER
C---Fill starting with +5
      IPOINT = 5
C---Read in layer radii:
      DO I = 1,2*NCLAYER + 2
        C( LL2CR + IPOINT + I - 1) = RBEG( I )
      END DO
      IPOINT = IPOINT + 2*NCLAYER + 2
C---Read in Z begin and end positions of cells:
      DO I = 1, 2*NCLAYER + 2
        DO J = 1, 2*NCIETA + 2
          C( LL2CR + IPOINT + J - 1) = ZBEG(J,I)
        END DO
        IPOINT = IPOINT + (2*NCIETA + 2)
      END DO
      DO I = 1, 2*NCLAYER + 2
        DO J = 1, 2*NCIETA + 2
          C( LL2CR + IPOINT + J - 1) = ZEND(J,I)
        END DO
        IPOINT = IPOINT + (2*NCIETA+2)
      END DO
C---Read in the ILAYER Id for each of our 'layer' indices:
      DO I = 1,2*NCLAYER + 2
        IC( LL2CR + IPOINT + I - 1) = IDLAYER(I)
      END DO
      IPOINT = IPOINT + 2*NCLAYER + 2
C---Read in the IETA id for each of our eta indices:
      DO I = 1,2*NCIETA + 2
        IC( LL2CR + IPOINT + I - 1) = IDIETA(I)
      END DO
      IPOINT = IPOINT + 2*NCIETA + 2
C---Count number of words of this bank and compare
      IPOINT = IPOINT - 1
      IF (IPOINT .NE. IC( LL2CR + 4) ) THEN
        MUMSG = ' L2CR lengths do not agree '
        GOTO 800
      END IF
C---We are done
      L2CR_BUILD = .TRUE.
      RETURN
  800 CONTINUE
      CALL ERRMSG('L2CRCAL','L2CR_BUILD',MUMSG,'W')
  999 RETURN
      END
