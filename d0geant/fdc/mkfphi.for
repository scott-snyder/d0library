      SUBROUTINE MKFPHI(IHALF,ISECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make 'hits' and 'data' for one sector in one
C-                         half of the Phi chambers. 
C-
C-   Inputs  : IHALF    = forward/backward half         (0-1)
C-             ISECTOR  = phi sector   (0-35)
C-   Outputs : - Fills the Hits   ZEBRA bank FPSC if SFDC(2)=1.
C-             - Fills the "DATA" ZEBRA bank FPDA if SFDC(1or3)=1.
C-   Controls:none
C-
C-   Created  26-SEP-1988   Jeffrey Bantly
C-   Updated  18-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  13-AUG-1992   Robert E. Avery  Correct orientation of SFDC. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:FDLOCA.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER IHALF,ISECTOR
C
      INTEGER NVDIM,NHMAX,NWPHV
      PARAMETER (NVDIM=6,NHMAX=50,NWPHV=10)
      INTEGER IWIRE,IBOX,I
      INTEGER NSECTR,NWIRE,TEMPSE,TEMPS2
      INTEGER NUMV(NVDIM),ITRAV(NHMAX),NUMBV(NVDIM,NHMAX)
      INTEGER NHITSV
      REAL HITSV(NWPHV,NHMAX)
      CHARACTER*4 NAMEV(6)
      LOGICAL X_FLIP 
C
      DATA NAMEV/'FPZ0','FPZ1','FPZ2','FPZ3','FPZ4','FPZ5'/
      DATA NSECTR,NWIRE/36,16/
C--------------------------------------------------------------------------
C
      HALF=IHALF
      UNIT=1
      QUAD=0
      SECTOR=ISECTOR
      IF ( (HALF.EQ.1) .AND. (IC(LFGEH+10).EQ.9009) ) THEN
C Flip X-Axis For SFDC 
        SECTOR = 17-ISECTOR
        SECTOR = MOD(SECTOR+36,36)
        X_FLIP = .TRUE.
      ELSE
        X_FLIP = .FALSE.
      ENDIF
      UB=0
      TEMPSE=INT( FLOAT(ISECTOR)/6.)
      TEMPS2=ISECTOR-TEMPSE*6
      TEMPSE = TEMPSE + 1
      TEMPS2 = TEMPS2 + 1
      NUMV(1)=HALF+1
      NUMV(2)=1
      NUMV(3)=1
      NUMV(4)=1
      NUMV(5)=TEMPS2
      IBOX=TEMPSE
      DO 100 IWIRE=0,NWIRE-1
        WIRE=IWIRE
        NUMV(6)=IWIRE+1
        CALL GFHITS('FDC ',NAMEV(IBOX),NVDIM,NWPHV,NHMAX,   ! get hits for
     &             0,NUMV,ITRAV,NUMBV,HITSV,NHITSV)         ! this wire
        IF (NHITSV.LE.0) GO TO 100
        IF (NHITSV.GT.NHMAX) THEN
          CALL ERRMSG('Too many hits','MKFPHI',
     &                'Over 50 hits in this sector','I')
          NHITSV=NHMAX
        ENDIF
        IF ( X_FLIP ) THEN
          DO I =  1, NHITSV
            HITSV(4,I) = -HITSV(4,I) 
          ENDDO
        ENDIF
C-----------------------------------------------------------------------
C  Subroutine FPHITS makes "hits" for one sense wire in one sector of
C  the phi unit of the forward drift chamber.
C-----------------------------------------------------------------------
        IF ( SFDC(2) .EQ. 1. ) THEN
          CALL FPHITS(HITSV,NHITSV)
        ENDIF
C-----------------------------------------------------------------------
C  Subroutine FPDATA makes "data" for one sense wire in one sector of
C  phi unit of the forward drift chamber.
C------------------------------------------------------------------------
        IF ( SFDC(1) .EQ. 1. .OR. SFDC(3) .EQ. 1. ) THEN
          CALL FPDATA(HITSV,NHITSV)
        ENDIF
C
  100 CONTINUE
C-----------------------------------------------------------------------
C   Trim unused portion of banks
C-----------------------------------------------------------------------
      CALL PUFPSC(HALF,SECTOR)
      CALL PUFPDA(HALF,SECTOR)
C-----------------------------------------------------------------------
  999 RETURN
      END
