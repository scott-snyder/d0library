      SUBROUTINE LMODUL( IETA, IPHI, ILAYER, LCLGA, PHIMD, SGN, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO RETURN THE POINTER AND ORIENTATION OF
C-           MODULE CONTAINING A CELL ADDRESSED BY PHYSICS VARIABLES.
C-           THIS VERSION GIVES THE POINTER TO THE NOMINAL FIRST
C-           MODULE.  IT ALSO SUPPLIES THAT NECESSARY ROTATION ANGLE
C-           TO ORIENT IT CORRECTLY.
C-
C-   Inputs  :     IETA      PHYSICS ETA NUMBER
C-                 IPHI      PHYSICS PHI NUMBER
C-                 ILAYER    PHYSICS LAYER NUMBER
C-   Outputs :     LCLGA     POINTER TO MODULE BANK (CLGA)
C-                 PHIMD     AZIMUTHAL ANGLE FOR MODULE ORIENTATION
C-                 SGN       SIGN -- IMPLY WHETHER SIGN FLIP OF MODULE
C-                             IS NECESSARY
C-                 IERR      ERROR FLAG -- 0: OK
C-   Controls: 
C-
C-   Created  20-JAN-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INTEGER IETA, IPHI, ILAYER, LCLGA, IERR, IETAA
      INTEGER LZFIND, MODREG, JERR
      REAL    SGN, PHIMD, PHI, PSI, PHIC, CYLIND
C
      LOGICAL FIRST
      SAVE FIRST
      CHARACTER*4 CHAR41
      INTEGER ICHAR41
      EQUIVALENCE (ICHAR41,CHAR41)
      DATA     CHAR41 /'TUBE'/
      DATA FIRST /.TRUE./
C
      IF( FIRST) THEN                  ! initialize PSI
        CALL CALPHI( 1, 1, PHI, PSI, JERR)
        IF( JERR .NE. 0) THEN          ! error from CALPHI 
          IERR = 9                       ! should not occur
          RETURN
        END IF
        FIRST = .FALSE.
      END IF
C
      IETAA = ABS(IETA)
      LCLGA = 0
      IERR = 1
      SGN = 1.
C
      IF( ILAYER .GE. MNLYEM .AND. ILAYER .LE. MXLYEM) THEN     ! EM
        IF ( IETAA .LE. 12) THEN       ! CC/EM
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ICCAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICCEMA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
        ELSE IF (IETAA .GE. 14 .AND. IETAA .LE. NETAL) THEN   ! EC/EM
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISEMCL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          IF(ILAYER .EQ. MNLYEM) MODREG = ICEC1A
          IF(ILAYER .EQ. MNLYEM+1) MODREG = ICEC2A
          IF(ILAYER .GE. LYEM3A .AND. ILAYER .LE. LYEM3D) MODREG =
     +      ICEC3A
          IF(ILAYER .EQ. MXLYEM) MODREG = ICEC4A
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), MODREG, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
          IF(IETA.LT.0) SGN=-1.
        END IF
      ELSE IF( ILAYER .GE. MNLYFH .AND. ILAYER .LE. MXLYFH) THEN 
        IF (IETAA .LE. 21-ILAYER) THEN      ! CC/FH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ICCAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICCFHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
        ELSE IF(IETAA .GE. MIN(ILAYER,13) .AND. IETAA .LE. ILAYER+5)
     &    THEN                              ! MFH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISECAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICMFHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
          IF(IETA.LT.0) SGN=-1.
        ELSE IF(IETAA .GE. ILAYER+6 .AND. IETAA .LE. NETAL)
     +    THEN                              ! IFH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISECAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICIFHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
          IF(IETA.LT.0) SGN=-1.
        END IF
      ELSE IF( ILAYER .GE. MNLYCH .AND. ILAYER .LE. MXLYCH) THEN 
        IF (IETAA .LE. 7 .AND. ILAYER .EQ. MNLYCH) THEN      ! CC/CH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ICCAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICCCHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
        ELSE IF(IETAA .GE. 7 .AND. IETAA .LE. ILAYER-3)
     &    THEN                              ! OCH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISECAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICOCHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
          IF(IETA.LT.0) SGN=-1.
        ELSE IF(IETAA .GE. 14 .AND. IETAA .LE. 20 .AND. ILAYER .EQ.
     &    MNLYCH) THEN                      ! MCH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISECAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICMCHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
          IF(IETA.LT.0) SGN=-1.
        ELSE IF(IETAA .GE. 21 .AND. IETAA .LE. NETAL .AND. ILAYER .EQ.
     &    MNLYCH) THEN                      ! ICH
          LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISECAL, IGREGN)
          IF(LQCREG .EQ. 0) GO TO 980
          LCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), ICICHA, IGIDEN)
          IF(LCLGA .EQ. 0) GO TO 990
          IF(IETA.LT.0) SGN=-1.
        END IF
      END IF
C
      IF (LCLGA .EQ. 0) GO TO 990
        IF(C(LCLGA+IGSHAP) .EQ. ICHAR41) THEN
          PHIMD = 0.
        ELSE
          PHIC = CYLIND(C(LCLGA+IGXCEN),2,IC(LCLGA+IGCOOR)) 
                                        ! central phi of a module
          PHIMD = IPHI*PSI - PHIC + 0.5*C(LCLGA+IGDPHI)
          PHIMD = C(LCLGA+IGDPHI) * INT(PHIMD/C(LCLGA+IGDPHI) - 0.001) 
        END IF
        IERR = 0
      GO TO 999
  980 CONTINUE     ! no region found
      IERR = 2
      GO TO 999
  990 CONTINUE     ! no module found
      IERR = 3
      GO TO 999
C----------------------------------------------------------------------
  999 RETURN
      END
