      SUBROUTINE DSGSEC (LAYER,TX0,TY0,SECTOR,SIDE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given layer return the sector and side
C-                         containing the NC point of the segment
C-
C-   Inputs  : LAYER       - Layer to analyze
C-             TX0         - X coord. of segment NC point
C-             TY0         - Y coord. of segment NC point
C-   Outputs : SECTOR      - Sector containing segment NC point
C-             SIDE        - If 0 then segment NC point on + side of SW plane
C-                         - If 1 then segment NC point on - side of SW plane
C-   Controls: 
C-
C-   Created   9-JUN-1989   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'

      INTEGER SECTOR,SIDE,LAYER,LLDRFT,GZDRFT
      REAL PHOFF,DELPHI,NCPHI,TX0,TY0,RSEC

      LOGICAL FIRST

      DATA FIRST /.TRUE./

      IF (FIRST) THEN
       FIRST  = .FALSE.
       LLDRFT = GZDRFT()
       PHOFF  = C (LLDRFT+9)*PI/180. ! Half opening angle of drift cell
       DELPHI = 2.*PHOFF             ! Angle spanned by a drift cell
      END IF

      SIDE = 0
      NCPHI = ATAN2(TY0,TX0)              ! Phi of NC point in D0 frame
      IF (TY0.LT.0.) NCPHI = NCPHI+2.*PI

C- Determine sector containing segment non correlation point 
C- If layer = 0,2 then rotate NCPHI by 1/2 sector. This is to compensate
C- for the fact that the D0 frame X axis is aligned with the L0S0,L2S0
C- sense wire planes
      IF (LAYER.EQ.0.OR.LAYER.EQ.2.) THEN
       NCPHI = NCPHI+PHOFF
       IF (NCPHI.GT.2.*PI) NCPHI = NCPHI-2.*PI
      END IF

      RSEC = NCPHI/DELPHI
      SECTOR = INT (RSEC)
      IF (RSEC-FLOAT(SECTOR).LT..5) SIDE = 1      !- Side hit
      IF (SECTOR.GE.32) SECTOR=31

  999 RETURN
      END
