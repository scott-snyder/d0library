      SUBROUTINE MKFTHE(IHALF,IQUAD,ISECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make the 'data' and 'hits' for one sector
C-                         of one Theta unit of the FDC
C-
C-   Inputs  : HALF    = forward/backward half         (0-1)
C-             UNIT    = front & back full theta chamber (0)
C-             QUAD    = theta quadrant (0-7)
C-             SECTOR  = theta sector   (0-5)
C-   Outputs : - Fills the Hits   ZEBRA bank FTSC if SFDC(1or3)=1.
C-             - Fills the "DATA" ZEBRA bank FTDA if SFDC(2)=1.
C-   Controls:
C-
C-   Created  26-SEP-1988   Jeffrey Bantly
C-   Updated  18-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  13-AUG-1992   Robert E. Avery  Correct orientation of SFDC. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC' 
      INCLUDE 'D0$INC:D0LOG.INC' 
      INCLUDE 'D0$INC:FDLOCA.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER IHALF,IQUAD,ISECTOR
C
      INTEGER NVDIM,NHMAX,NWPHV
      PARAMETER (NVDIM=6,NHMAX=50,NWPHV=10)
      INTEGER IWIRE,IBOX,I
      INTEGER NSECTR,NWIRE,LOCQUD
      INTEGER NHITSV
      INTEGER QUAD_FLIP(0:7)
      INTEGER NUMV(NVDIM),ITRAV(NHMAX),NUMBV(NVDIM,NHMAX)
      REAL HITSV(NWPHV,NHMAX)
      REAL TEMP
      CHARACTER*4 NAMEV(12)
      LOGICAL X_FLIP 
C
      DATA NAMEV/'FXZ0','FXZ1','FXZ2','FXZ3','FXZ4','FXZ5',
     1           'FYZ0','FYZ1','FYZ2','FYZ3','FYZ4','FYZ5'/
      DATA NSECTR,NWIRE/6,8/
      DATA QUAD_FLIP /1,0,3,2,6,5,4,7/
C-------------------------------------------------------------------------
C
      HALF=IHALF
      UNIT=0
      QUAD=IQUAD
      X_FLIP = .FALSE.
      IF ( (HALF.EQ.1) .AND. (IC(LFGEH+10).EQ.9009) ) THEN
        QUAD=QUAD_FLIP(IQUAD)
        IF (QUAD.GE.4) X_FLIP = .TRUE.
      ENDIF
      SECTOR=ISECTOR
      UB=0
      NUMV(1)=HALF+1
      NUMV(2)=UNIT+1
      LOCQUD=IQUAD
      IF(LOCQUD.GE.4) THEN
        NUMV(2)=UNIT+2
        LOCQUD=LOCQUD-4
      ENDIF
      NUMV(3)=1
      NUMV(4)=1
      NUMV(5)=1
      IF (LOCQUD.GE.2) NUMV(3)=2
      IF (LOCQUD.EQ.0.OR.LOCQUD.EQ.2) IBOX=SECTOR+1
      IF (LOCQUD.EQ.1.OR.LOCQUD.EQ.3) IBOX=SECTOR+1+NSECTR
C
C---------------------------------------------------------------------------
C
      DO 100 IWIRE=0,NWIRE-1
        WIRE=IWIRE
        NUMV(6)=IWIRE+1
        CALL GFHITS('FDC ',NAMEV(IBOX),NVDIM,NWPHV,NHMAX,  ! get hits for
     &             0,NUMV,ITRAV,NUMBV,HITSV,NHITSV)        ! this channel
        IF (NHITSV.LE.0) GO TO 100
        IF (NHITSV.GT.NHMAX) THEN
          CALL ERRMSG('Too many hits','MKFTHE',
     &                'More than 50 hits in this sector','I')
          NHITSV=NHMAX
        ENDIF
        IF ( X_FLIP ) THEN
          DO I =  1, NHITSV
            TEMP = HITSV(6,I) 
            HITSV(6,I) = HITSV(7,I) 
            HITSV(7,I) = TEMP
          ENDDO
        ENDIF
C
C----------------------------------------------------------------------
C  Subroutine FTHITS makes "hits" for one sense wire in one sector of
C  theta unit of the forward drift chamber.
C----------------------------------------------------------------------
        IF ( SFDC(2) .EQ. 1. ) THEN
          CALL FTHITS(HITSV,NHITSV)
        ENDIF
C
C----------------------------------------------------------------------------
C  Subroutine FTDATA makes channel "data" for one sense wire in one sector 
C  of one Theta unit of the Forward Drift Chamber.
C  channels are: 0 - 7 wires, 8,9 two ends of delay line
C  (delay line "data" is made during the call to FTDATA with IWIRE=0)
C----------------------------------------------------------------------------
C
        IF ( SFDC(1) .EQ. 1. .OR. SFDC(3) .EQ. 1. ) THEN
          CALL FTDATA(HITSV,NHITSV)
        ENDIF
C
  100 CONTINUE
C----------------------------------------------------------------------
C   Trim unused portion of banks
C----------------------------------------------------------------------
      CALL PUFTSC(HALF,QUAD,SECTOR)
      CALL PUFTDA(HALF,QUAD,SECTOR)
C
C----------------------------------------------------------------------
  999 RETURN
      END
