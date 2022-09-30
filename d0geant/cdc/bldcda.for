      SUBROUTINE BLDCDA ( HIT, NDIM, NHIT, IFADC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the Zebra structure
C-                         (CDCH -- DLYR -- DSEC)--DCDA from the hits HIT
C-                         given by GEANT ( JHITS )
C-
C-   Inputs  : HIT ( NDIM, NHIT )  = Array containing the data stored for
C-                                   each hit in JHITS
C-   Outputs : none ; The bank DCDA is filled
C-
C-   Created  27-JAN-1988   Ghita Rahal-Callot
C-   Updated  30-JUN-1988   Ghita Rahal-Callot  correct the influence on
C-                                              the delay line signal 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NDIM, NHIT, IFADC(*)
      REAL HIT ( NDIM, * )
C
      INTEGER NWORD, NMAX
      PARAMETER (NWORD = 18, NMAX = 50)
      INTEGER I, J, IHIT
      INTEGER ICELL, NDELAY, NHITOT(2)
      REAL HITDL(NWORD, 2 * NMAX)
C
C ****  Influence of each Sense Wire on the Delay Line
C
      REAL AINFL(2)
      DATA AINFL / 1. , .11 /
C
C----------------------------------------------------------------------
C
      ICELL = IFADC ( 3 )
      IF ( ICELL .EQ. 0 .OR. ICELL .EQ. 5 ) THEN
        NDELAY = 0
        CALL VZERO ( HITDL, NWORD*2*NMAX )
        NHITOT(1) = NHIT
      ELSEIF (ICELL .EQ. 1 .OR. ICELL .EQ. 6 ) THEN
          NHITOT(2) = NHIT
      ENDIF
C
C ****  Prepare and store the datas for the Sense Wires
C
      IF ( NHIT .LE. 0 ) THEN
        IF ( NDELAY .LE. 0 ) GO TO 999
        IF ( ICELL .NE. 1 .AND. ICELL .NE. 6 ) GO TO 999
C
C ****  Perform digitization for each end of the Delay Line
C
        CALL CDATDL ( IFADC, HITDL, NHITOT )
        CALL VZERO_i ( NHITOT,2)
        GO TO 999
      ENDIF
      CALL CDATSW ( HIT, NDIM, NHIT, IFADC )
C
C  ****  Prepare the datas for the Delay lines
C
      IF ( ICELL .LE. 1 .OR. ICELL .GE. 5 ) THEN
        DO 100 IHIT=1,NHIT
C
C ****  Get information of cells (1, 2) and (6, 7) for the DELAY LINES
C
          CALL UCOPY ( HIT(1,IHIT), HITDL(1,NDELAY + IHIT), NDIM)
          J=1
          IF ( ICELL .EQ. 1 .OR. ICELL .EQ. 5 ) J=2
C
C ****  Correct the pulse height by the influence of the wire on the Delay
C ****  line ( 1 for the closest wire, .11 for the next one )
C
          HITDL ( 7, NDELAY+IHIT ) = HITDL ( 7, NDELAY+IHIT )
     &                             * AINFL ( J )
  100   CONTINUE
        NDELAY = NDELAY + NHIT
C ****  DELAY LINES :
C ****        Digitization is stored in 4 FADC one for each
C ****        end of the 2 delay lines.
C ****        DL close to the first  (last) sense wire :
C ****             #7   (#9)  for the +z end
C ****             #8   (#10)  for the -z end
C
        IF ( NDELAY .LE. 0 ) GO TO 999
        IF ( ICELL .NE. 1 .AND. ICELL .NE. 6 ) GO TO 999
C
C ****  Perform digitization for each end of the Delay Line
C
        CALL CDATDL ( IFADC, HITDL, NHITOT )
        CALL VZERO_i ( NHITOT,2)
      ENDIF
  999 CONTINUE
      RETURN
      END
