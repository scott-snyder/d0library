      SUBROUTINE VALS_UPDATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update alignment banks.  Each layer has the following
C-               rigid transformations:
C-                    XZERO,YZERO,ZZERO = coordinate of center
C-                    ROTZ,ROTYP,ROTZP  = Euler angles
C-                    TWIST             = relative rotation of bulkheads
C-              
C-               In addition, each if the 8 types of wires in a layer have
C-               bowing parameter -- DBOW and NBOW are signed magnitudes of the
C-               wire deflections in the drift direction and normal directions
C-               respectively.
C-   Inputs  : VTWSTP parameter
C-   Outputs : VALS
C-   Controls: 
C-
C-   Created  12-FEB-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL XZERO(0:2),YZERO(0:2),ZZERO(0:2) 
      REAL ROTZ(0:2),ROTYP(0:2),ROTZP(0:2),TWIST(0:2)
      REAL DBOW(0:7,0:2),NBOW(0:7,0:2)
      INTEGER ERR,LAYER,SECTOR,WIRE,NS(0:2),LVRFT,LVALS,ITEMS
      REAL   SIGNS,SIGNW,LENGTH2,BFACT
      DOUBLE PRECISION R0,DR,STAGGER,PHIC,DELTA
      DOUBLE PRECISION NORM(3),WMID(3),WDIR(3),DDIR(3),ROT(3,3)
      DOUBLE PRECISION PI,X,Y,CS,SN
      INTEGER GZVRFT,GZVALS
      DATA NS/15,31,31/
      DATA STAGGER/0.01016/
C----------------------------------------------------------------------
      LVRFT = GZVRFT()
      DR = C(LVRFT+24)-C(LVRFT+23)
      PI = DACOS(-1.D0)
      CALL EZPICK('VTWSTP_RCP')
      CALL EZGET('XZERO',XZERO(0),ERR)  
      CALL EZGET('YZERO',YZERO(0),ERR)
      CALL EZGET('ZZERO',ZZERO(0),ERR)
      CALL EZGET('ROTZ',ROTZ(0),ERR)
      CALL EZGET('ROTYP',ROTYP(0),ERR) 
      CALL EZGET('ROTZP',ROTZP(0),ERR)
      CALL EZGET('TWIST',TWIST(0),ERR)
      CALL EZGET('DBOW',DBOW(0,0),ERR)
      CALL EZGET('NBOW',NBOW(0,0),ERR)
      CALL EZRSET
      DO LAYER = 0,2
        CALL VROTATE(ROTZ(LAYER),
     &              ROTYP(LAYER),
     &              ROTZP(LAYER),
     &              ROT)
        R0      = C(LVRFT+7+7*LAYER) + C(LVRFT+23)
        PHIC    = C(LVRFT+8+7*LAYER)*PI/180.
        DELTA   = C(LVRFT+6+7*LAYER)*2.*PI/180.
        LENGTH2 = C(LVGEH+IC(LVRFT+4+7*LAYER))**2
        DO SECTOR = 0,NS(LAYER)
          SIGNS = 1 - 2*MOD(SECTOR,2)
          BFACT = -SIGNS/LENGTH2
          LVALS = GZVALS(LAYER,SECTOR)
          ITEMS = IC(LVALS+6)
          CS = DCOS(PHIC + DELTA*SECTOR)
          SN = DSIN(PHIC + DELTA*SECTOR)
          NORM(1) = CS
          NORM(2) = SN
          NORM(3) = 0.
          DO WIRE = 0,7
C
C ****  GENERATE WIRE MIDPOINT AND DRIFT AND WIRE DIRECTIONS
C
            X = R0 + DR*WIRE + NBOW(WIRE,LAYER)
            SIGNW = 1 - 2*MOD(WIRE,2)
            Y = SIGNS*( SIGNW*STAGGER + DBOW(WIRE,LAYER))
            WMID(1) = X*CS - Y*SN
            WMID(2) = X*SN + Y*CS
            WMID(3) = 0.
            DDIR(1) = -NORM(2)
            DDIR(2) =  NORM(1)
            DDIR(3) = 0.
            WDIR(1) = 0.
            WDIR(2) = 0.
            WDIR(3) = 1.
C
C ****  TWIST:  CHANGES DRIFT AND WIRE DIRECTIONS
C
            WDIR(1) = -WMID(2)*TWIST(LAYER)
            WDIR(2) =  WMID(1)*TWIST(LAYER)
            WDIR(3) =  DSQRT(1.-WDIR(1)**2-WDIR(2)**2)
            DDIR(1) =  WDIR(2)*NORM(3) - WDIR(3)*NORM(2)
            DDIR(2) = -WDIR(1)*NORM(3) + WDIR(3)*NORM(1)
            DDIR(3) =  WDIR(1)*NORM(2) - WDIR(2)*NORM(1)
C
C ****  ROTATE: CHANGES WIRE POSITIONS AND DRIFT AND WIRE DIRECTIONS
C
            CALL VMULT(ROT,WMID)
            CALL VMULT(ROT,DDIR)
            CALL VMULT(ROT,WDIR)
C
C ****  FILL ALIGNMENT BANK -- FOR INDIVIDUAL WIRES
C ****  TRANSLATE WIRE POSITIONS
C
            C(LVALS+ITEMS*WIRE+7) = WMID(1) + XZERO(LAYER)
            C(LVALS+ITEMS*WIRE+8) = WMID(2) + YZERO(LAYER)
            C(LVALS+ITEMS*WIRE+9) = WMID(3) + ZZERO(LAYER)
            C(LVALS+ITEMS*WIRE+10)= WDIR(1)
            C(LVALS+ITEMS*WIRE+11)= WDIR(2)
            C(LVALS+ITEMS*WIRE+12)= DBOW(WIRE,LAYER)*DDIR(1)*BFACT
            C(LVALS+ITEMS*WIRE+13)= DBOW(WIRE,LAYER)*DDIR(2)*BFACT
          ENDDO
C
C ****  FILL ALIGNMENT BANK -- SECTOR WIDE DRIFT DIRECTION
C
          C(LVALS+3) = DDIR(1)
          C(LVALS+4) = DDIR(2)
        ENDDO
      ENDDO
  999 RETURN
      END
