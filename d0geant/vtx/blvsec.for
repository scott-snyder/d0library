      SUBROUTINE BLVSEC( HIT, NHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the Zebra structure
C-                         VTXH -- VLAY -- VSEC from GEANT hits as stored in
C-                         JHITS.  This routine handles hits from one wire at a
C-                         time.
C-
C-   Inputs  : HIT(IWORD,IHIT) [F] = Array containing the GEANT hit data
C-               HIT(1,IHIT)  = X-GLOBAL for hit IHIT
C-               HIT(2,IHIT)  = Y-GLOBAL
C-               HIT(3,IHIT)  = Z-GLOBAL
C-               HIT(4,IHIT)  = X-LOCAL (along drift direction)
C-               HIT(5,IHIT)  = Pulse height (integrated charge)
C-               HIT(6,IHIT)  = Distance to +z (global) end of wire
C-               HIT(7,IHIT)  = Distance to -z end of wire
C-               HIT(8,IHIT)  = Track length in cell (cm)
C-               HIT(9,IHIT)  = Track id=2**11*Second. track # + Prim. track #
C-               HIT(10,IHIT) = Projection of track length onto drift direction
C-               HIT(11,IHIT) = Time of flight (ns)
C-
C-             NHIT [I]       = Number of hits for this wire
C-             LAYER, SECTOR, WIRE in VTLOCA
C-
C-   Outputs : VSEC is filled
C- Output array: VHIT(1,IHIT)  = logical address (see VCODER)
C-               VHIT(2,IHIT)  = drift distance (cm) (+ phi solution)
C-               VHIT(3,IHIT)  = drift distance (cm) (- phi solution)
C-               VHIT(4,IHIT)  = Z position (cm)
C-               VHIT(5,IHIT)  = Error on the drift distance (cm)
C-               VHIT(6,IHIT)  = error in Z from charge division
C-               VHIT(7,IHIT)  = ionization (MIPs)
C-               VHIT(8,IHIT)  = error on ionization
C-               VHIT(9,IHIT)  = drift time (ns)
C-               VHIT(10,IHIT) = status word
C-               VHIT(11,IHIT) = track id (see above)
C-
C-   Created   9-NOV-1989   Peter Grudberg
C-   Updated   1-OCT-1992   Peter M. Grudberg  Remove z-strips 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VTLOCA.INC'
C
      INTEGER NHIT, NDIM
      PARAMETER( NDIM = 11 )
      REAL HIT( NDIM, * )
C
      INTEGER NMAX
      PARAMETER ( NMAX = 50 )
      REAL VHIT(NDIM,NMAX), TIME(NMAX), PULWID(NMAX)
      INTEGER IVHIT(NDIM,NMAX), IORDER(NMAX), IORD
      EQUIVALENCE (VHIT,IVHIT)
      INTEGER IHIT, JHIT, INEXT, LOGCHA, IEND, UBIT
      INTEGER LVRFT, GZVRFT, LVTMW, GZVTMW
      REAL STAGGR, STAG, ZCOORD, DFTVEL, SAGIT
      PARAMETER ( SAGIT = 0.132 ) ! sagitta of isochrone (cm), 90% collection
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( NHIT .LE. 0 ) GO TO 999
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LVRFT = GZVRFT()
        IF ( LVRFT .LE. 0 ) THEN
          WRITE (LOUT,*) ' **** BLVSEC: bank VRFT not defined'
          CALL EXIT(1)
        ENDIF
        STAGGR = C( LVRFT + 31 )
C
C ****  Get drift velocity
C ****  By default, the drift velocity is the same for all wires
C
        LVTMW = GZVTMW( 0 )
        IF ( LVTMW .LE. 0 ) THEN
          WRITE (LOUT,*) ' **** BLVSEC: bank VTMW not defined'
          CALL EXIT(1)
        ENDIF
        DFTVEL = C ( LVTMW + 7 )
      ENDIF
C
      STAG = STAGGR * (-1)**( SECTOR + WIRE )
C
C ****  Get hit times, pulse width, and sort hit times
C ****  For now, omit time of flight
C **** TIME = (Dist. from sense wire / drift velocity) (+ time of flight)
C **** PULWID = (track projection + isochrone sagitta) / DFTVEL
C
      DO IHIT = 1, NHIT
        TIME(IHIT) = ABS(HIT(4,IHIT)-STAG) / DFTVEL ! + HIT(11,IHIT)
        PULWID(IHIT) = ( SAGIT + HIT(10,IHIT) ) / DFTVEL
      ENDDO
      CALL SORTZV(TIME, IORDER, NHIT, 1, 0, 0)
C
C ****  Loop through hits in time order creating wire hits and saving z strip
C ****  information.
C
      JHIT = 0
      DO INEXT = 1, NHIT
        IORD = IORDER(INEXT)
        ZCOORD = ( HIT(7,IORD) - HIT(6,IORD) ) / 2.
C
C ****  Form coordinate hit in array VHIT (IVHIT)
C
        IF ( JHIT .GE. NMAX ) THEN
          WRITE(LOUT,*) 
     &    ' **** BLVSEC: Too many hits on one wire; excess lost'
          GO TO 101
        ELSE
          JHIT = JHIT + 1
          CALL VCODER(LOGCHA,0,LAYER,SECTOR,WIRE,STRIP,IEND,UBIT,2)
          IVHIT(1,JHIT) = LOGCHA        ! packed channel address
          VHIT(2,JHIT) = TIME(IORD)*DFTVEL+STAG ! dft dist(if phi>wire)
          VHIT(3,JHIT) =-TIME(IORD)*DFTVEL+STAG ! dft dist(if phi<wire)
          VHIT(4,JHIT) = ZCOORD         ! charge division z (cm)
          VHIT(5,JHIT) = 0.007          ! error in drift dist. (cm)
          VHIT(6,JHIT) = 1.5            ! error in chg div z (cm)
          VHIT(7,JHIT) = HIT(5,IORD)    ! ionization (MIP)
          VHIT(8,JHIT) = SQRT(VHIT(7,JHIT))     ! error on ionization
          VHIT(9,JHIT) = TIME(IORD)     ! drift time (ns)
C
C ****  Status word: set bits 0 and 1 to indicate both ends hit
C
          IVHIT(10,JHIT) = 3            ! status word
          IVHIT(11,JHIT) = INT( HIT(9,IORD) )  ! track id
        ENDIF
      ENDDO                             ! loop over hits
  101 CONTINUE                          ! overflow exit
C
C ****  Load hits into ZEBRA structure (in VSEC):
C
      CALL LDVSEC( VHIT, JHIT )
C
  999 RETURN
      END
