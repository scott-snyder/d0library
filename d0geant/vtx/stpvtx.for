      LOGICAL FUNCTION STPVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-  STPVTX handles stepping through the vertex chamber drift cells.
C-  Outputs (via GSAHIT call):
C-    HITSV(1)   x-global
C-    HITSV(2)   y-global
C-    HITSV(3)   z-global
C-    HITSV(4)   x-local  (distance from cell center along drift direction)
C-    HITSV(5)   Pulse height  (integrated charge) 
C-    or HITSV(5)   Pulse height  (integrated charge) + 10000*NMEAN (for new MC)
C-               (NMEAN is the mean number of ionization clusters per VTX cell)
C-    HITSV(6)   Distance to positive z(global) end of wire (cm)
C-    HITSV(7)   Distance to negative z(global) end of wire (cm)
C-    HITSV(8)   Track length in cell (cm)
C-    HITSV(9)   Isajet track number
C-    HITSV(10)  Proj. of track length in cell onto drift dir., abs. val. (cm)
C-    HITSV(11)  Time of flight (ns)
C-    
C-   Returned value  : TRUE 
C-   Inputs  : None
C-   Outputs : None
C-   Controls: D0LOG.INC, GCSETS
C-
C-   Created  15-NOV-1985   T. Trippe
C-  Radically revised May 28, 1986 to standardize CDC, FDC, VTX,  T. Trippe
C   G.Rahal Nov 1987 changed HITSV(9): now contains Isajet track number
C                                      instead of Geant track number
C-   Updated  17-JUL-1989   Harrison B. Prosper   
C-   Made into pbd interface function.
C-   Updated  30-JAN-1990   Peter Grudberg - fix track id again
C-   Updated   5-MAY-1992   Alexandre Zinchenko - modified to simulate
C-                          new pulse shapes (if SVTX(6)=1.) 
C-   Updated  26-JUN-1992   Alexandre Zinchenko - fix negative distance
C-                          to end of wire 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC'
C
      REAL VIN(6),VMID(6),VLOC(6),CELSIZ(4),HITSV(11)
      REAL TRKLEG,TKPROJ,PULPH
      INTEGER IXYZ,IHIT
      INTEGER  NSECON    !  Secondary track number bits = 2**11
      INTEGER NMEAN 
      DATA NSECON/2048/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER VTX
C----------------------------------------------------------------------
      STPVTX = .TRUE.
      IF ( DVTX .LT. 2 ) GOTO 999
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH('VTX ',VTX,4,4)
      ENDIF
C
      IF ( .NOT. (DVTX .GT. 1 .AND. IHSET .EQ. VTX) ) GOTO 999 
C
C  No vertex chamber hit if no charge.
      IF (CHARGE.EQ.0) GOTO 999
C
C  Incident point in the sense wire CELL
       IF (INWVOL.EQ.1) THEN
        DO 10 IXYZ=1,6
         VIN(IXYZ)=VECT(IXYZ)
  10    CONTINUE
       END IF
C
C  Exit point : we can look at the DRIFT Distance
       IF (INWVOL.NE.2) RETURN
C
C  Calculate midpoint of the track in the cell
       DO 20 IXYZ=1,3
         VMID(IXYZ)=(VIN(IXYZ)+VECT(IXYZ))/2.    ! x, y, z
         VMID(IXYZ+3)=VIN(IXYZ+3)                ! direction cosines
  20   CONTINUE
C
C  Calculate the track length
C
      TRKLEG=SQRT((VIN(1)-VECT(1))**2+
     +               (VIN(2)-VECT(2))**2+(VIN(3)-VECT(3))**2)
C
C  Convert the midpoint to local coordinates
      CALL LOCGEO(VMID,VLOC,CELSIZ,4)
C
C  In local frame, cell is trapezoid with wire lying
C  along y-axis and oriented so that the cell surfaces closest to
C  and furthest from the beam are at z=-(dz/2) and +(dz/2) respectively,
C  where dz is the cell thickness.
C  VLOC is VMID transformed to local coordinates.
C  CELSIZ gives the parameters of the trapezoid system shape 'TRD1'.
C  CELSIZ(1) and (2) give the half lengths in x at z=-(dz/2) and +(dz/2).
C  CELSIZ(3) is the half length in y (along wire).
C  CELSIZ(4) is the half length in z (cell half thickness).
C
C  Calculate the track length projected onto the drift direction.
C  Can be used to calculate the pulse width on the sense wires.
      TKPROJ=ABS(VLOC(4)*TRKLEG)
C
C  Landau distribution
      CALL VLANDA  (TRKLEG, PULPH, NMEAN) 
C
C  Fill the hit banks
      HITSV(1)=VMID(1)
      HITSV(2)=VMID(2)
      HITSV(3)=VMID(3)
      HITSV(4)=VLOC(1)       ! distance to cell center along drift direction
      HITSV(5)=PULPH
      IF(SVTX(6).EQ.1.) THEN
        IF(PULPH.GT.9999.) WRITE(*,*) ' VTX: VERY HIGH IONIZATION!' 
        IF(PULPH.GT.9999.) PULPH=9999. 
        HITSV(5)=PULPH + NMEAN*10000. 
      ENDIF
      IF (VLOC(2).LT.-CELSIZ(3)) VLOC(2) = -CELSIZ(3)
      IF (VLOC(2).GT.CELSIZ(3)) VLOC(2) = CELSIZ(3)
      HITSV(6)=CELSIZ(3)-VLOC (2)  ! distance to plus z end of sense wire
      HITSV(7)=CELSIZ(3)+VLOC (2)  ! distance to minus z end of sense wire
      HITSV(8)=TRKLEG
C
C ****  Fill track id.=2**11*secondary track # + primary track #
C
      IF ( ISTAK .LE. 15 .AND. ITRA .LE. NSECON )  THEN
        HITSV(9) = ISTAK * NSECON + ITRA
      ELSE
        HITSV(9) =   16  * NSECON + ITRA
      ENDIF
C
      HITSV(10) = TKPROJ
      HITSV(11) = TOFG*10**9
C
C  Store hit.
C  ISET, IDET and NUMBV are obtained from COMMON/GCSETS/.
C  ITRA is obtained from COMMON/GCKINE/.  Returns IHIT=current-hit-number
C  or IHIT=0 if hit not stored.
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSV,IHIT)
C
  999 RETURN
      END
