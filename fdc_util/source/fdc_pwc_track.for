      FUNCTION FDC_PWC_TRACK(
     &          X_PWC_0,Y_PWC_0,DX_PWC_0,DY_PWC_0,
     &          X_PWC,Y_PWC,DX_PWC,DY_PWC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the position of PWC track, if one
C-   exists. Use coordinates based on FDC. x=y=0 is center of chamber,
C-
C-   Inputs  : (PWCT bank)
C-   Outputs : X_PWC_0,Y_PWC_0,DX_PWC_0,DY_PWC_0        PWC COORDINATES
C-   Outputs : X_PWC,Y_PWC,DX_PWC,DY_PWC                FDC COORDINATES
C-   Controls: 
C-
C-   Created  19-JUL-1990   Robert E. Avery
C-   Updated   8-AUG-1991   Robert E. Avery  Change NPLANES to NPWCMX,
C-      compatibility with TB91 data.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FDC_PWC_TRACK
C  output:
      REAL    X_PWC_0,Y_PWC_0               ! POSITION OF PWC TRACK
      REAL    DX_PWC_0,DY_PWC_0             ! SLOPE OF PWC TRACK
      REAL    X_PWC,Y_PWC               ! POSITION OF PWC TRACK
      REAL    DX_PWC,DY_PWC             ! SLOPE OF PWC TRACK
C  local variables:
      INCLUDE 'D0$PARAMS:PWCPAR.DEF'
      INTEGER NTY,NTXD,NTXU,NHY,NHXD,CHY,CHXD 
      INTEGER NTR, NWTRK,NP
      INTEGER IER
      INTEGER REF_PLANE
C
      REAL    YT(5),DXT(5),UXT(4),MOM(4)
      REAL    Z_PLANE(NPWCMX) 
      REAL    X_PWC_TEMP,Y_PWC_TEMP               ! POSITION OF PWC TRACK
      REAL    DX_PWC_TEMP,DY_PWC_TEMP             ! SLOPE OF PWC TRACK
      REAL    X_FDC,Y_FDC,Z_FDC         ! POSITION OF FDC REL. TO PWC1
      REAL    X_ANGLE,Y_ANGLE           !  (Z_FDC is center of chamber)
      REAL    PHI_ANGLE
      REAL    DENOM
      REAL    ZPOS
      REAL    Z0(0:1)
      REAL    Z_OFFSET  ! offset to chamber center from I.R.
C
      LOGICAL PWCCHK
      LOGICAL FIRST 
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
C
C Get center of Phi (and FDC) in z:
C
        CALL GTFALH(0,1,0,0,7,X_FDC,Y_FDC,Z_FDC)
        Z_OFFSET =  Z_FDC/2.
        CALL GTFALH(0,1,0,0,8,X_FDC,Y_FDC,Z_FDC)
        Z_OFFSET =  Z_OFFSET + Z_FDC/2.
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZRSET
        CALL EZPICK('FDC_RCP')
        CALL EZGET('X_FDC',X_FDC,IER)
        CALL EZGET('Y_FDC',Y_FDC,IER)
        CALL EZGET('Z_FDC',Z_FDC,IER)
        CALL EZGET('X_ANGLE',X_ANGLE,IER)
        CALL EZGET('Y_ANGLE',Y_ANGLE,IER)
        CALL EZGET('PHI_ANGLE',PHI_ANGLE,IER)
        CALL EZRSET
        CALL EZPICK('PWC_RCP')
        CALL EZGET('Z_PLANE(1)',Z_PLANE(1),IER)
        CALL EZRSET
        IF ( NPLANES .EQ. 12 ) THEN
          REF_PLANE = 8
        ELSE
          REF_PLANE = 7
        ENDIF
        FIRST = .FALSE.
      END IF
      FDC_PWC_TRACK = .FALSE.
C
C  Check if event numbers match.
C
      IF ( .NOT. PWCCHK() ) THEN        
        GOTO 999
      ENDIF
C
C  Get PWC information; Require 1 good in X and Y.
C
      CALL GTPWCT(
     &          YT,DXT,UXT,
     &          NTY,NTXD,NTXU,
     &          NHY,NHXD,CHY,CHXD,
     &          NP,MOM,IER)
      IF ( ( IER .NE. 0 ) .OR.
     &     ( NTXD .NE. 1) .OR. 
     &     ( NTY .NE. 1) )  THEN
        GOTO 999
      ENDIF
C
C  Express PWC track in FDC coordinates:
C
C  Invert PWC X-Direction 
      X_PWC_0 = - DXT(1) 
      DX_PWC_0 = - DXT(2)
      Y_PWC_0 = YT(1) 
      DY_PWC_0 = YT(2)
C
C  Translate to chamber-center z (ASSUME HALF 0, SO Z INCREASING UPSTREAM):
      ZPOS = - (Z_FDC + Z_PLANE(REF_PLANE))
      DX_PWC_0 = -DX_PWC_0 
      DY_PWC_0 = -DY_PWC_0 
      X_PWC_0 = X_PWC_0 + DX_PWC_0*ZPOS 
      Y_PWC_0 = Y_PWC_0 + DY_PWC_0*ZPOS 
C
C  Translate transverse to beam:
      X_PWC_TEMP = X_PWC_0 - X_FDC
      Y_PWC_TEMP = Y_PWC_0 - Y_FDC
C
C  Rotate IN PHI :
      X_PWC =  X_PWC_TEMP*COS(PHI_ANGLE) + Y_PWC_TEMP*SIN(PHI_ANGLE)
      Y_PWC = -X_PWC_TEMP*SIN(PHI_ANGLE) + Y_PWC_TEMP*COS(PHI_ANGLE)
C
      DX_PWC =  DX_PWC_0*COS(PHI_ANGLE) + DY_PWC_0*SIN(PHI_ANGLE)
      DY_PWC = -DX_PWC_0*SIN(PHI_ANGLE) + DY_PWC_0*COS(PHI_ANGLE)
C
C  Rotate:
C   note:
C    for small dx and x_angle, this simplifies to 
C       dx_pwc = dx_pwc_0 + x_angle
      DENOM = COS(X_ANGLE) - DX_PWC * SIN(X_ANGLE)
      X_PWC = X_PWC / DENOM
      DX_PWC = (DX_PWC * COS(X_ANGLE)  + SIN(X_ANGLE)) / DENOM
C
      DENOM = COS(Y_ANGLE) - DY_PWC * SIN(Y_ANGLE)
      Y_PWC = Y_PWC / DENOM
      DY_PWC = (DY_PWC * COS(Y_ANGLE)  + SIN(Y_ANGLE)) / DENOM
C
C  Translate in z again to Z0 (again, half must be 0)
      X_PWC = X_PWC + DX_PWC * ( Z0(0) - Z_OFFSET )
      Y_PWC = Y_PWC + DY_PWC * ( Z0(0) - Z_OFFSET )
C
      FDC_PWC_TRACK = .TRUE.
  999 CONTINUE
      RETURN
      END
