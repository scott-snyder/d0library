      SUBROUTINE MUELOS(QQUAD,P,X,Y,Z,DIX,DIY,DIZ,DOX,DOY,DOZ,
     A ELFE,ELCAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO DETERMINE ENERGY LOSS OF MUON 
C-    in Iron and Calorimeter
C-   Inputs  : QUAD: muon track quadrant number (indicates if basement)
C              P: MUON MOMENTUM
C              X,Y,Z: location of muon track in center of iron
C             DIX,Y,Z: direction cosines inside magnet
C             DOX,Y,Z: direction cosines outside magnet
C-   Outputs : ELFE,ELCAL: energy loss (GeV) in iron and calorimeter
C-   Controls: 
C-
C-   Created  12-MAR-1989   David Hedin: use CERN85-03 as guide
C-   quick kluge especially concerning calorimeter energy loss
C    DH 4/91: ADD MOMENTUM DEPENDENCE AND BETTER CALORIMETER
C    DH 9/91: add dip in iron at 40 degrees; tune slightly e-dependence
C    DH 10/91 MODIFY QUAD USAGE
C    DH 12/91 really want most probable. cern tables give average.
C             too lazy to do right right now (have in BNL stuff) so
C             kluge up by lowering values
C    DH 2/92 lower upper limits
C    Updated 5MAR4-94 Roy Thatcher - Eliminate spurious entry in D0ENTRY
C      changed local variable ENTRY to ENTRY1
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER QUAD,IFE,ENTRY1,NINT,QQUAD
      REAL X,Y,Z,DIX,DIY,DIZ,DOX,DOY,DOZ,ELFE,ELCAL,P,PP,EDEPFE,FEL
      REAL DEFE,EFE(6),CAL(46),ABSUR,DEDXUR,ABS1,THET,EU(6),EDEPU
      REAL ABSFE,DEDXFE
      DATA DEFE/1.26/     ! ENERGY LOSS IN 1 M IRON at 1 GeV/c
      DATA EFE/1.09,1.11,1.13,1.15,1.16,1.17/
      DATA EU/1.20,1.22,1.24,1.26,1.28,1.30/
      DATA CAL/0.0,9.7,9.7,9.7,9.7,9.7,9.7,9.0,8.0,8.6,8.9,9.3,9.8,
     A9.8,9.1,8.9,9.0,9.0,9.2,8.0,7.5,7.3,7.1,7.0,6.9,6.9,6.4,6.0,
     A5.8,6.4,7.0,7.5,7.4,7.3,7.3,7.2,7.2,7.1,7.1,7.0,7.0,7.0,
     A7.0,6.9,6.9,6.9/
      DATA ABSUR,DEDXUR,ABSFE,DEDXFE/199.,1.2,132.,1.48/
      QUAD=MOD(QQUAD,100)
      PP=ABS(P)
C   DO CALORIMETER
      THET = ACOS(DIZ)*180./3.141593
      IF (THET.GT.90.) THET = 180. - THET
      IF (THET.LE.90.0.AND.THET.GE.0.0) THEN
        ENTRY1 = NINT((THET/2.0) + 1.0)
        ABS1 = CAL(ENTRY1)
      ELSE
        ABS1 = 0.0
      END IF
CC  SCALE ENERGY LOSS FROM ABSORPTION LENGTHS
CC  SCALE ENERGY DEPENDENCE TO 1 GEV
      EDEPU=1.                           ! URANIUM
      IF(PP.GE.1..AND.PP.LT.5.) THEN
        EDEPU=1.+ .20*SQRT((PP-1.)/4.)
      ELSE IF(PP.GE.5..AND.PP.LT.30.) THEN
        IFE=PP/5. 
        EDEPU=EU(IFE) + (PP/5.-IFE)*(EU(IFE+1)-EU(IFE))
      ELSE
        EDEPU=1.30 +(PP-30.)*.007
      ENDIF
        IF(EDEPU.GT.1.40) EDEPU=1.40
      EDEPFE=1.                         ! IRON
      IF(PP.GE.1..AND.PP.LT.5.) THEN
        EDEPFE=1.+ .09*SQRT((PP-1.)/4.)
      ELSE IF(PP.GE.5..AND.PP.LT.30.) THEN
        IFE=PP/5. 
        EDEPFE=EFE(IFE) + (PP/5.-IFE)*(EFE(IFE+1)-EFE(IFE))
      ELSE
        EDEPFE=1.17 +(PP-30.)*.002
      ENDIF
        IF(EDEPFE.GT.1.30) EDEPFE=1.30
CC  ASSUME CAL 90% URANIUM, 10% iron
      IF(THET.LE.45.) THEN
      ELCAL=ABS1*.001*(EDEPU*ABSUR*DEDXUR*.90+
     A                 EDEPFE*ABSFE*DEDXFE*.10)
      ELSE
      ELCAL=ABS1*.001*(EDEPU*ABSUR*DEDXUR*.95+
     A                 EDEPFE*ABSFE*DEDXFE*.05)
      ENDIF
CC  DO IRON; INCLUDE DIP AT CF-EF GAP
      FEL=1.52                   ! END IRON THICKNESS
      IF(THET.GT.47.) FEL=1.09   ! CENTRAL IRON THICKNESS
      IF(THET.LE.47..AND.THET.GT.40.) THEN ! thinner corners
        FEL=.37 +(THET-40.)*.72/7.
      ENDIF
      IF(THET.LE.40..AND.THET.GT.34.) THEN
        FEL=.37 +(40.-THET)*1.15/6.
      ENDIF
      IF(QUAD.EQ.0) THEN                   ! BASEMENT
        ELCAL=0.
        ELFE=DEFE/2./ABS(DIY) + DEFE/2./ABS(DOY)
      ELSE IF(QUAD.EQ.1.OR.QUAD.EQ.3) THEN ! SIDE CENTRAL
        ELFE=(DEFE/2./ABS(DIX) + DEFE/2./ABS(DOX))*FEL
      ELSE IF(QUAD.EQ.2.OR.QUAD.EQ.4) THEN ! TOP/BOTTOM CENTRAL
        ELFE=(DEFE/2./ABS(DIY) + DEFE/2./ABS(DOY))*FEL
      ELSE IF(QUAD.GE.5) THEN               ! ENDS
        ELFE=(DEFE/2./ABS(DIZ) + DEFE/2./ABS(DOZ))*FEL
      ENDIF
      ELFE=ELFE*EDEPFE       ! ADD ENERGY TERM
C----------------------------------------------------------------------
  999 RETURN
      END
