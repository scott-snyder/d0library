      SUBROUTINE BLVWDA( HIT, NHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the Zebra structure
C-                         VTXH -- VLAY -- VSEC -- VWDA from GEANT hits
C-                         as stored in JHITS.  This routines handles hits from
C-                         one wire at a time.
C-
C-   Inputs  : HIT(IWORD,IHIT) [F] = Array containing the GEANT hit data
C-               HIT(1,IHIT)  = X-GLOBAL for hit IHIT
C-               HIT(2,IHIT)  = Y-GLOBAL
C-               HIT(3,IHIT)  = Z-GLOBAL
C-               HIT(4,IHIT)  = X-LOCAL (along drift direction)
C-               HIT(5,IHIT)  = Pulse height (integrated charge)
C-         or    HIT(5,IHIT)  = Pulse height (integrated charge)+10000*mean
C-                              number of ionization clusters per VTX cell 
C-                              (for new pulse shapes)
C-               HIT(6,IHIT)  = Distance to +z (global) end of wire
C-               HIT(7,IHIT)  = Distance to -z end of wire
C-               HIT(8,IHIT)  = Track length in cell (cm)
C-               HIT(9,IHIT)  = Track id=2**11*Second. track # + Prim. track #
C-               HIT(10,IHIT) = Projection of track length onto drift direction
C-               HIT(11,IHIT) = Time of flight (ns)
C-
C-             NHIT [I] = number of hits on the wire
C-             LAYER, SECTOR, WIRE in VTLOCA
C-
C-   Outputs : VWDA is filled
C- Output array: IVDAT(1,IHIT) = logical address (see VCODER)
C-               VDAT(2,IHIT)  = drift time (ns)
C-          or   VDAT(2,IHIT)  = tzero+time along wire (new pulse shapes)
C-               VDAT(3,IHIT)  = pulse area (counts)
C-          or   VDAT(3,IHIT)  = pulse area (counts)+10000*nmean (for 0 end)
C-          or   VDAT(3,IHIT)  = pulse area (counts) (for 1 end) (new PS)
C-               VDAT(4,IHIT)  = pulse width (ns)
C-          or   VDAT(4,IHIT)  = drift distance (new PS)
C-               VDAT(5,IHIT)  = peak height (counts)
C-          or   VDAT(5,IHIT)  = track length in cell (new PS)
C-               VDAT(6,IHIT)  = drift time error (ns)
C-               VDAT(7,IHIT)  = pulse area error (counts)
C-               IVDAT(8,IHIT) = status
C-               IVDAT(9,IHIT) = track id (see above)
C-
C-   Created  10-NOV-1989   Peter Grudberg
C-   Updated  25-FEB-1991   Peter Grudberg  Fix bad indexing bug 
C-   Updated   5-MAY-1992   Alexandre Zinchenko - modified to simulate
C-                          new pulse shapes
C-   Updated   1-OCT-1992   Peter M. Grudberg  Remove z-strips 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VTLOCA.INC'
      INCLUDE 'D0$INC:D0LOG.INC' 
C
      INTEGER NHIT, NDIM
      PARAMETER( NDIM = 11 )
      REAL HIT( NDIM, * )
C
      INTEGER NMAX, NWORDS
      PARAMETER ( NMAX = 50 )
      PARAMETER ( NWORDS = 9 )
      INTEGER IVDAT(NWORDS,NMAX)
      REAL VDAT(NWORDS,NMAX)
      EQUIVALENCE ( VDAT, IVDAT )
      INTEGER LVRFT, GZVRFT, LVTMW, GZVTMW
      INTEGER INEXT, IORD, UBIT
      INTEGER IHIT, IORDER(NMAX,0:1), JHIT, LOGCHA
      REAL STAGGR, STAG, TZERO, DFTVEL, THRESH
      REAL TTIME, TIME(NMAX,0:1), ZPLUS, ZMINUS, ZCOORD
      REAL VLIGHT, PULWID(NMAX), SAGIT, VPLHT, VPULSE(0:1,NMAX)
      LOGICAL FIRST
      INTEGER NMEAN 
      DATA FIRST / .TRUE. /
      PARAMETER ( VLIGHT = 29.979 )     ! cm/ns
      PARAMETER ( THRESH = 0. )
      PARAMETER ( SAGIT = 0.132 ) ! sagitta of isochrone (cm); 90% collection
C----------------------------------------------------------------------
      IF ( NHIT .LE. 0 ) GO TO 999
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LVRFT = GZVRFT()
        IF ( LVRFT .LE. 0 ) THEN
          WRITE (LOUT,*) ' **** BLVWDA: bank VRFT not defined'
          CALL EXIT(1)
        ENDIF
        STAGGR = C( LVRFT + 31 )
C
C ****  Get drift velocity and time offset
C ****  By default, the drift velocity and offset are the same for all wires.
C
        LVTMW = GZVTMW( 0 )
        IF ( LVTMW .LE. 0 ) THEN
          WRITE (LOUT,*) ' **** BLVWDA: bank VTMW not defined'
          CALL EXIT(1)
        ENDIF
        TZERO  = C( LVTMW + 6 )
        DFTVEL = C( LVTMW + 7 )
      ENDIF
C
      STAG = STAGGR * (-1)**( SECTOR + WIRE )
C
C ****  Get hit times, pulse widths, do charge division, save z-strip
C ****  information, then sort the hits on time.
C ****  For now, omit time of flight.
C **** TIME = (Dist. from sense wire/drift vel) + tzero (+time of flight)
C **** PULWID = (track projection + isochrone sagitta) / drift velocity
C
      DO IHIT = 1, NHIT
        TTIME  = ABS(HIT(4,IHIT)-STAG)/DFTVEL + TZERO !+ HIT(11,IHIT) 
        ZPLUS  = HIT(6,IHIT)
        ZMINUS = HIT(7,IHIT)
        TIME(IHIT,0) = TTIME + ZMINUS / VLIGHT  ! end 0 is - z
        TIME(IHIT,1) = TTIME + ZPLUS / VLIGHT   ! end 1 is + z
        PULWID(IHIT) = ( SAGIT + HIT(10,IHIT) ) / DFTVEL
        IF(SVTX(6).EQ.1.) THEN 
          TIME(IHIT,0) = TZERO + ZMINUS/VLIGHT 
          TIME(IHIT,1) = TZERO + ZPLUS/VLIGHT 
          PULWID(IHIT) = ABS(HIT(4,IHIT)-STAG) 
        ENDIF 
C
C ****  charge division:
C
        NMEAN = HIT(5,IHIT)/10000. + 0.5 
        VPLHT = HIT(5,IHIT)
        IF(SVTX(6).EQ.1.) VPLHT = HIT(5,IHIT) - NMEAN*10000. 
        CALL CHGDIV(VPLHT,ZMINUS,ZPLUS,VPULSE(0,IHIT),VPULSE(1,IHIT))
        IF(SVTX(6).EQ.1.) VPULSE(0,IHIT)=VPULSE(0,IHIT)+NMEAN*10000. 
C
      ENDDO
C
C ****  Sort hits from both ends:
C
      IF(SVTX(6).NE.1.) CALL SORTZV(TIME(1,0), IORDER(1,0), 
     &                              NHIT, 1, 0, 0) 
      IF(SVTX(6).NE.1.) CALL SORTZV(TIME(1,1), IORDER(1,1), 
     &                              NHIT, 1, 0, 0) 
C
C ****  Loop through hits in time order creating wire pulses
C
      DO 100 END = 0, 1
        JHIT = 0
        DO 110 INEXT = 1, NHIT
          IORD = IORDER(INEXT,END)
          IF(SVTX(6).EQ.1.) IORD = INEXT 
C
C ****  If pulse height is below threshold, ignore it
C
        IF ( VPULSE(END,IORD).LT.THRESH.AND.SVTX(6).NE.1. ) GO TO 110 
C
C ****  Form pulse in array VDAT (IVDAT)
C
          IF ( JHIT .GE. NMAX ) THEN
            WRITE (LOUT,*) 
     &      ' **** BLVWDA: Too many pulses on one wire; excess lost'
            GO TO 101
          ELSE
            JHIT = JHIT + 1
            CALL VCODER(LOGCHA,0,LAYER,SECTOR,WIRE,STRIP,END,UBIT,2)
            IVDAT(1,JHIT) = LOGCHA      ! packed channel address
            VDAT(2,JHIT)  = TIME(IORD,END)     ! drift time (ns)
            VDAT(3,JHIT)  = VPULSE(END,IORD)   ! pulse height (counts)
            VDAT(4,JHIT)  = PULWID(IORD)       ! pulse width (ns) or 
                                               ! drift distance (new PS)
            VDAT(5,JHIT)  = VPULSE(END,IORD)   ! peak height - same as
                                               ! area for now
            IF(SVTX(6).EQ.1.) VDAT(5,JHIT)=HIT(8,IORD) ! track length 
                                                       ! in cell (new PS) 
            VDAT(6,JHIT)  = 7.          ! drift time error
            VDAT(7,JHIT)  = 11.         ! pulse area error
            IVDAT(8,JHIT) = 0           ! status
            IVDAT(9,JHIT) = INT(HIT(9,IORD))    ! track id
          ENDIF
  110   CONTINUE                        ! loop over hits for one end
  101   CONTINUE                        ! overflow exit
C
C ****  Load hits into ZEBRA structure (VWDA):
C
        CALL LDVWDA( VDAT, JHIT )
  100 CONTINUE
  999 RETURN
      END
