      SUBROUTINE L2_ROAD(ETA, PHI, XYZ_CLUS, WETA, WPHI, ROAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return PHImin, PHImax, ETAmin, ETAmax based
C-                         on decisions on road size and shape along z
C-                         and in r-phi
C-
C-   Inputs  : ETA, PHI - nominal electron shower position (in pad units)
C-   Outputs : ROAD(1,WIRE,LAYER) = PHImin
C              ROAD(2,WIRE,LAYER) = PHImax
C              ROAD(3,WIRE,LAYER) = ETAmin
C              ROAD(4,WIRE,LAYER) = ETAmax
C              ROAD(5,WIRE,LAYER) = Zmin
C              ROAD(6,WIRE,LAYER) = Zmax
C              ETA,PHI physical positions returned by L2_ROAD
C
C-             If beam crossing not at origin, but at some stable off-center
C-             vertex (x0,y0,z0) may  need to replace the code carrying this
C-             correction (see the entry point  COSMIC_TRGR  to see how this
C-             was handled for random  x0,y0,z0 in the COSMICS beam trigger)
C
C-   Controls:
C-
C-   Created  26-AUG-1991   Daniel R. Claes
C-            08-NOV-1991   Officialized in final TOOL format
C-            18-FEB-1992   Pass add'l road info to delay line hit-finder
C-            03-JUL-1992   Provide vertex offset thru L2TRAK_RCP and
C-                          correct road in r-phi view
C-            09-JUL-1992   Remove scaling to continuous ETA,PHI values
C-                          L2_EM will now pass the physical ETA,PHI
C-            17-AUG-1992   New L2_EM position algorithm passes EM cluster
C-                          position within the array XYZ_CLUS(3)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'
C
      REAL B, B2, B_TH, BUNCH, COSPHI, SGN, DEN, ETA, ETA_c
      REAL M, M_TH, PHI, PHI_c, PHI_0, PHIpt, PHIz
      REAL PRIME1, PRIME2, R_pr_1, R_pr_2
      REAL R, R_0, R1, R2, R_em3, R_prime
      REAL ROAD(1:6,0:6,0:3)                 ! MAX:MIN,WIRE,LAYER roads
      REAL TEST, TH_em3, THETA, WETA, WETA_c, WPHI, XYZ_CLUS(3)
      REAL X, X1, X2, Y, Y1, Y2, Z, Z1, Z_prime, Z_DIFF
C
      INTEGER ILAYER, IWIRE
C
      LOGICAL VTX_FLAG
C
      DATA BUNCH /90/                   ! half of bunch size (centimeters)
C     DATA R_em3 /100/                  ! Radius out to EM3 layer center
C                                       !  96.98cm inner surface
C                                       ! 104.02cm outer surface
C     17-AUG-92                         ! This will now be calculated from
C                                       ! XYZ_CLUS (x,y) positions passed
C
C----------------------------------------------------------------------
      REAL DLTZR1, DLTZR2, WZ
      REAL X0, Y0, Z0, ZTOL
      COMMON /L2VERTEX/ X0, Y0, Z0
      COMMON /DELAYS/ DLTZR1, DLTZR2, WZ, ZTOL
      COMMON /CDCROAD/ ETA_c, WETA_c, PHI_c, R_em3
C----------------------------------------------------------------------
C
C
      ROAD(3,0,0) = ETA - WETA
      ROAD(4,0,0) = ETA + WETA
C
  996 CONTINUE
      RETURN
C
      ENTRY CDC_ROAD(ETA, PHI, XYZ_CLUS, WETA, WPHI, ROAD)
C
      PHI_c = PHI                       ! Allow to pass by COMMON block
      ETA_c = ETA                       ! to L2_CDEL where a second pass
      WETA_c = WETA                     ! is made at a narrower road
C
      X1 = XYZ_CLUS(1)
      Y1 = XYZ_CLUS(2)
      R_em3 = SQRT(X1**2 + Y1**2)
C
      IF (X0.EQ.0 .AND. Y0.EQ.0) THEN
        VTX_FLAG = .FALSE.
      ELSE
        VTX_FLAG = .TRUE.
        M = (Y1-Y0)/(X1-X0)             ! Parameters of line extending from
        B = Y1 - M*X1                   ! vertex to EM cluster center.
C
C Need to solve the intersection of R^2 = x^2 + y^2 and the calorimeter 'track'
C                                    y  =  Mx + B
C for each layer/wire radius.  This intersection has two roots given by
C            x = (-2MB +/- SQRT(4M^2B^2 - 4(M^2+1)(B^2-R^2)) )/(2(M^2+1)
C Assume the real root has x,y close to where PHI extrapolates from the origin.
C
        R1 = M*B
        DEN = (M**2 + 1)
      ENDIF
C
      DO ILAYER = 0,3
        DO IWIRE = 0,6
C
          R = RADIUS(IWIRE,ILAYER)
C
          IF (VTX_FLAG) THEN

            R2 = SQRT((M*B)**2-(M**2+1)*(B**2-R**2))
            TEST = R*COS(PHI)
            X1 = (-R1+R2)/DEN
            X2 = (-R1-R2)/DEN
            IF (X1*X2.LT.0) THEN                        ! TEST x for correct
              IF (ABS(TEST-X1).LT.ABS(TEST-X2)) THEN    ! solution
                X = X1
              ELSE
                X = X2
              ENDIF
              Y = M*X + B
            ELSE                                        ! TEST y for correct
              Y1 = M*X1 + B                             ! solution
              Y2 = M*X2 + B
              TEST = R*SIN(PHI)
              IF (ABS(TEST-Y1).LT.ABS(TEST-Y2)) THEN
                X = X1
                Y = Y1
              ELSE
                X = X2
                Y = Y2
              ENDIF
            ENDIF
            PHI_0 = ATAN2(Y,X)            ! Phi to projected hit position
                                        ! on this LAYER,WIRE
            IF (PHI_0 .LT. 0) PHI_0 = PHI_0 + TWOPI
C
            ROAD(1,IWIRE,ILAYER) = PHI_0 - WPHI
            ROAD(2,IWIRE,ILAYER) = PHI_0 + WPHI
C
          ELSE
            ROAD(1,IWIRE,ILAYER) = PHI - WPHI
            ROAD(2,IWIRE,ILAYER) = PHI + WPHI
          ENDIF
C
          IF (ROAD(1,IWIRE,ILAYER).LT.0.00) 
     &        ROAD(1,IWIRE,ILAYER) = ROAD(1,IWIRE,ILAYER) + TWOPI
          IF (ROAD(2,IWIRE,ILAYER).GT.TWOPI) 
     &        ROAD(2,IWIRE,ILAYER) = ROAD(2,IWIRE,ILAYER) - TWOPI
C
          ROAD(3,IWIRE,ILAYER) = ETA - WETA
          ROAD(4,IWIRE,ILAYER) = ETA + WETA
C
          THETA = 2*ATAN(EXP(-1*ROAD(3,IWIRE,ILAYER)))  ! THETA min
C          Z1 = R_em3 * COS(THETA)
          Z1 = R_em3/TAN(THETA)
C          ROAD(5,IWIRE,ILAYER) = R*(Z1+BUNCH)/R_em3 - BUNCH
          ROAD(5,IWIRE,ILAYER) = R*(Z1+BUNCH)/R_em3 - BUNCH - WZ
C
C This initially wide road will be narrowed during hit-finding in L2_CDEL
C
          THETA = 2*ATAN(EXP(-1*ROAD(4,IWIRE,ILAYER)))  ! THETA max
C          Z1 = R_em3 * COS(THETA)
          Z1 = R_em3/TAN(THETA)
          ROAD(6,IWIRE,ILAYER) = R*(Z1-BUNCH)/R_em3 + BUNCH
          ROAD(6,IWIRE,ILAYER) = R*(Z1-BUNCH)/R_em3 + BUNCH + WZ
C
        ENDDO
      ENDDO
C
  998 CONTINUE
      RETURN
C
      ENTRY COSMIC_TRGR(ETA, PHI, WETA, WPHI, ROAD)
C
C Here X0, Y0, Z0 mark some point (within the CDC layers) along the
C track found by the off-line package
C
C   09-JUL-1992 in anticipation of L2_EM passing actual ETA,PHI instead of
C   psuedo tower index centers.  Remove scaling to continuous ETA,PHI values
C
C      ETA = ETA * 0.10                  ! Change to physical ETA
C      PHI = PHI * 0.098175              ! Change to physical PHI
C
      PHI_c = PHI                       ! Allow to pass by COMMON block
      ETA_c = ETA
      WETA_c = WETA
C
      THETA = 2*ATAN(EXP(-1*ETA))
      R_0 = SQRT( X0**2 + Y0**2 )
      M_TH = TAN(THETA)
      B_TH = R_0 - Z0*M_TH
C
      COSPHI = (X0*COS(PHI) + Y0*SIN(PHI))/SQRT(X0**2 + Y0**2)
      PHIpt = ATAN2(Y0,X0)              ! Angle to space point (COSMICS)
C
      M = TAN(PHI)                      ! SLOPE of true DTRAK in COSMIC data
      B = Y0 - X0 * M                   ! INTERCEPT  "    "    "   "    "
C
C      APPROACH = ABS(B/SQRT(M**2 + 1))
C
C   Check if intercepts within trigger counter volume
C
      B2 = -B/M
C
C Solve the intersection of R^2 = x^2 + y^2 and the offline track given by
C                            y  =  Mx + B
C which has two roots
C             x = (-2MB +/- SQRT($M^2B^2 - (M^2+1)(B^2-R^2)) )/(2(M^2+1)
C Assume the real root has x,y close to where PHI extrapolates from origin.
C
      R1 = M*B
      DEN = (M**2 + 1)
      DO ILAYER = 0,3
        DO IWIRE = 0,6
          R = RADIUS(IWIRE,ILAYER)
          R2 = SQRT((M*B)**2-(M**2+1)*(B**2-R**2))
          TEST = R*COS(PHI)
          X1 = (-R1+R2)/DEN
          X2 = (-R1-R2)/DEN
          IF (X1*X2.LT.0) THEN                        ! TEST x for correct
            IF (ABS(TEST-X1).LT.ABS(TEST-X2)) THEN    ! solution
              X = X1
            ELSE
              X = X2
            ENDIF
            Y = M*X + B
          ELSE                                        ! TEST y for correct
            Y1 = M*X1 + B                             ! solution
            Y2 = M*X2 + B
            TEST = R*SIN(PHI)
            IF (ABS(TEST-Y1).LT.ABS(TEST-Y2)) THEN
              X = X1
              Y = Y1
            ELSE
              X = X2
              Y = Y2
            ENDIF
          ENDIF
          PHI_0 = ATAN2(Y,X)            ! Phi to projected hit position
                                        ! on this LAYER,WIRE
          IF (PHI_0 .LT. 0) PHI_0 = PHI_0 + TWOPI
C
          ROAD(1,IWIRE,ILAYER) = PHI_0 - WPHI
          IF (ROAD(1,IWIRE,ILAYER).LT.0.00) 
     &      ROAD(1,IWIRE,ILAYER) = ROAD(1,IWIRE,ILAYER) + TWOPI
          ROAD(2,IWIRE,ILAYER) = PHI_0 + WPHI
          IF (ROAD(2,IWIRE,ILAYER).GT.TWOPI) 
     &      ROAD(2,IWIRE,ILAYER) = ROAD(2,IWIRE,ILAYER) - TWOPI
          ROAD(3,IWIRE,ILAYER) = ETA - WETA
          ROAD(4,IWIRE,ILAYER) = ETA + WETA
C
C          Z = (R - B_TH)/M_TH
C
C Calculate road in Z
C
          PRIME2 = R_0**2*COSPHI**2 - (R_0**2 - R**2)
C          IF (PRIME2.LT.0) THEN
C            PRIME2 = 0
C          ENDIF
          PRIME2 = SQRT(PRIME2)
          PRIME1 = R_0*COSPHI
C
          R_PR_1 = -PRIME1 + PRIME2
C          IF (R.GT.R_0) THEN
C            R_PR_2 = PRIME1 + PRIME2
C          ELSE
C            R_PR_2 = -PRIME1 - PRIME2
C          ENDIF
C          IF ( ABS(R_PR_2).LT.ABS(R_PR_1) ) THEN
C            R_prime = R_PR_2
C          ELSE
            R_prime = R_PR_1
C          ENDIF
C
          Z = R_prime/M_TH + Z0
C

          ROAD(5,IWIRE,ILAYER) = Z - WZ
          ROAD(6,IWIRE,ILAYER) = Z + WZ
C
        ENDDO
      ENDDO
C
C Calculate the nominal ETA at the EM3 layer to pass by COMMON to L2_CDEL
C
          PRIME2 = R_0**2*COSPHI**2 - (R_0**2 - R_em3**2)
          PRIME2 = SQRT(PRIME2)
          PRIME1 = R_0*COSPHI
          R_PR_1 = -PRIME1 + PRIME2
          R_prime = R_PR_1
          Z = R_prime/M_TH + Z0
C
          TH_em3 = ATAN(R_em3/Z)
          IF (TH_em3.LT.0)  TH_em3 = TH_em3 + PI
C          IF (TH_em3.GT.0) THEN
            ETA_c = -ALOG(TAN(TH_em3/2.))
C          ENDIF
C
  999 CONTINUE
      RETURN
C
      END
