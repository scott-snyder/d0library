      SUBROUTINE MODVEC(IETA, IPHI, ILAYER, XS, YS, ZS, NS, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE VECTORS THAT REPRESENT THE 
C-           SIDES OF THE MODULE WHICH CONTAINS A CELL ADDRESSED BY 
C-           THE ABOVE PHYSICS VARIABLES.  XS, YS, AND
C-           ZS ARE (2, NS) ARRAYS GIVING THE BEGINNING AND END POINTS
C-           OF LINES REPRESENTING THE SIDES OF A CELL.  THE CELL 
C-           IS ADDRESSED BY THE PHYSICS VARIABLES.  THIS ROUTINE
C-           LOOKS FOR THE 'CLGA' AND 'CSHA' BANK DESCRIBING THE MODULE.
C-
C-   Inputs  :    IETA      PHYSICS ETA NUMBER
C-                IPHI      PHYSICS PHI NUMBER
C-                ILAYER    PHYSICS LAYER NUMBER
C-   Outputs :    XS        X COORDINATE SIDE VECTOR
C-                YS        Y COORDINATE SIDE VECTOR 
C-                ZS        Z COORDINATE SIDE VECTOR
C-                NS        NUMBER OF SIDES OF THE CELL
C-                IERR      ERROR FLAG -- 0: OK
C-                                        1: NO CLYR BANK FOR GIVEN INDICES
C-   Controls: 
C-
C-   Created   20-JAN-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER MSIDES, MCORNS
      PARAMETER (MSIDES = 32)          ! maximum number of sides
      PARAMETER (MCORNS = 20)          ! maximum number of corners
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, JERR, IERR, NS, I, J, K, NC, N1
      REAL SGN, PSI, PHIMD, PHI
      REAL X(MCORNS), Y(MCORNS), Z(MCORNS)
      REAL XS(2,MSIDES), YS(2,MSIDES), ZS(2,MSIDES) 
      INTEGER IUPPER(5), ILOWER(5), IMIDDLE(5)
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
      IERR =  1
C
      IF( ILAYER .GE. MNLYMG .AND. ILAYER .LE. MXLYMG) GO TO 999    !
                                        ! exclude massless gaps and ICD
C
C ... GET MODULE CORRESPONDING TO THE CELL
C
      CALL LMODUL( IETA, IPHI, ILAYER, LQCLGA, PHIMD, SGN, IERR)
      IF( IERR .NE. 0) GO TO 999
C
C ... GET MODULE VECTORS
C
      CALL MODULE_VECTORS( LQCLGA, PHIMD, XS, YS, ZS, NS, IERR)
      IF( IERR .NE. 0) GO TO 999
C
C ... IF NECESSARY FLIP SIGNS
C
      IF( SGN .LE. 0.) THEN
        CALL REFLECT_VECTORS( XS, YS, ZS, NS, IERR)
      END IF
C
  999 RETURN
      END
