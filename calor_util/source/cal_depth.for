      SUBROUTINE CAL_DEPTH(IETA, LAYER, DEPTH, RAD_LEN, ABS_LEN, IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO RETURN THE RADIAL (AXIAL) CELL DEPTH
C-               IN THE CENTRAL (END) CALORIMETER. THE LENGTH TRAVERSE
C-               IN THE CELL BY A PARTICLE ORIGINATING AT Z=0 IS
C-               DEPTH/SIN(THETA) [DEPTH/COS(THETA)] IN THE CENTRAL
C-               (END) CALORIMETER.
C-
C-   Inputs  :   IETA       Physics ETA number
C-               LAYER      Physics LAYER number
C-   Outputs :   DEPTH      Radial or Axial Depth
C-               RAD_LEN    Radiation Length in Cell Material
C-               ABS_LEN    Absorption Length in Cell Material
C-               IFLAG      Flag indicating:   1:  Central Calorimeter
C-                                             2:  End Calorimetr
C-                                             3:  Cell does not exist
C-                                             4:  Other error condition
C-   Controls: 
C-
C-   Created  12-SEP-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:CMAT.DEF'
C
      INTEGER IETA, LAYER, IFLAG, IETAA, IOK, JLAYER
      REAL    DEPTH, RAD_LEN, ABS_LEN, CENRAD, DELRAD, CENZED, DELZED
      REAL    TILT
      LOGICAL CEXIST
C
      IETAA = ABS(IETA)
      IFLAG = 3
      IF(.NOT. CEXIST(IETA,1,LAYER)) GO TO 999  ! Cell not valid
      IF(IETAA .LE. 12 .AND. LAYER.LE.MNLYCH .AND. IETAA.LE.21-LAYER .
     &  AND. (LAYER.LE.MXLYEM .OR. LAYER.GE.MNLYFH))
     &  THEN                              ! cen cal cells
        IFLAG = 1
        CALL CALRAD(IETA,LAYER,CENRAD,DELRAD,CENZED,DELZED,IOK)
        IF(IOK.NE.0 ) GO TO 990
        DEPTH = DELRAD
      ELSE
        IFLAG = 2
        CALL CALZED(IETA,LAYER,CENZED,DELZED,CENRAD,DELRAD,TILT,IOK)
        IF(IOK.NE.0) GO TO 999
        DEPTH = DELZED
      END IF
C
      JLAYER = LAYER
      IF(IETA.EQ.-12) THEN
        IF(JLAYER.EQ.5) JLAYER=3
        IF(JLAYER.EQ.6) JLAYER=4
      ENDIF
      IF(IETA.EQ.-14) THEN
        IF(JLAYER.EQ.3) JLAYER=5
        IF(JLAYER.EQ.4) JLAYER=6
      ENDIF
      LQCEDP = LC(LCGEH - IZCEDP)      ! pointer to towerr dispatching
C                                      ! bank
      LQCETA = LC(LQCEDP - IZCETA - IETAA + 1)  ! pointer to constant
C                                      ! eta bank
      LQCLYR = LC(LQCETA - IZCLYR - JLAYER + 1) ! pointer to first
C                                      ! appropriate layer bank
      LQCLAY = LC(LQCLYR - IXCLAY)     ! pointer to CLAY layer banks
C
      LQCMAT = LC(LQCLAY - IZLMAT)     ! pointer to CMAT material banks
C
      RAD_LEN = C(LQCMAT + IGRADL)     ! radiation length
      ABS_LEN = C(LQCMAT + IGABSL)     ! interaction lenght
      GO TO 999
C
  990 IFLAG = 4
C----------------------------------------------------------------------
  999 RETURN
      END
