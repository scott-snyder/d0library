      SUBROUTINE FILL_MULIB(LGMUH,MULB,NSMULIB,NSDATA,PMASS,ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILLS THE MULB ARRAY WITH GMUH INFO
C-
C-   Inputs  : LGMUH
C-           : MULB = ARRAY FOR MUONLIBRARY USAGE
C-             NSMULIB = STORAGE IN MULB ARRAY LEFT
C-   OUTPUT  : NSDATA = Number of data points
C-             PMASS = mass of the partical in GMUH
C-             ITRA = Track number. ITRA=-1 means no track.
C-   Controls:
C-
C-   CREATED 08-MAY-1993   Jasbir Singh and Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZGHIT.LINK'
      INCLUDE 'D0$LINKS:IZGMUH.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER NSDATA,NSMULIB,ITRA
      INTEGER LGMUH
C
      REAL VERT(3,3),MVERT
      REAL MULB(NMULIB)
      REAL HTDATA(NMULIB)
C
      INTEGER IE,IP,IL,ITAG
      INTEGER I,J,JQ,II,IJ,IX,NPLIVE
      REAL XT,PT,EM,MASS_IN,PMASS
      INTEGER VERSION,NSKIP,KSKIP,NM,NH,KHIT,JHIT,NREPEAT,IHIT
      INTEGER IREJ_MOM,LEFT
      INTEGER KDATA_MAX,NENTRY,KDATA
      REAL    EFR_KEEP,X,F,RNDM
C
      INTEGER IER
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      INTEGER SSUNIT
C----------------------------------------------------------------------
      CALL GDGMUH(LGMUH,ITRA,VERT,P,HTDATA,NSDATA)
      JHIT = 0
      IF(ITRA.LT.0)GOTO 999         ! No more tracks: return
      IF(NSDATA.LE.0)GOTO 999         ! No HITS IN MUON CHAMBER: return
      NREPEAT = IQ(LGMUH+3)
C Check the flag for bad tracks
C
      IF (P(4,2) .GT. 9990.0 ) THEN ! Skip the track
        GOTO 999
      ELSE IF (P(4,2) .LT. -9990.0) THEN  ! This was flaged incorrectly
        P(4,2) = SQRT(P(1,2)**2+P(2,2)**2+P(3,2)**2) ! recorrect the bug
      ENDIF                                          ! neglecting mass
C
C ****  Extrapolate back to notional primary vertex z-position
C
      XT=SQRT(VERT(1,2)**2 + VERT(2,2)**2)
      PT=SQRT(P(1,2)**2 + P(2,2)**2)
      IF (PT.EQ.0.)PT=0.000001
C
C ****  Determine key vector values
C
      MVERT=SQRT(VERT(1,2)**2+VERT(2,2)**2+VERT(3,2)**2)
      IF (MVERT.NE.0.)THEN
        PX=VERT(1,2)
        PY=VERT(2,2)
        PZ=VERT(3,2)
        EIN=P(4,2)
        NVTX=VERT(3,2)-XT*P(3,2)/PT
      ELSE                              ! MVERT=0 for old datafiles
        PX=P(1,1)
        PY=P(2,1)
        PZ=P(3,1)
        EIN=P(4,1)
        NVTX=VERT(1,3)
      ENDIF
C ****  Get the MUON library entry for the given KEY value
C ****  Store information in the library entry
C
      MULB(1)=VERT(1,2)                   ! x
      MULB(2)=VERT(2,2)                   ! y
      MULB(3)=VERT(3,2)                   ! z
      MASS_IN = SQRT(P(1,2)**2+P(2,2)**2+P(3,2)**2) ! momentum
      MASS_IN = SQRT((P(4,2)+MASS_IN)*ABS(P(4,2)-MASS_IN)) ! mass
      MULB(4)=P(4,2)                      !  Energy
      MULB(5)=HTDATA(1)                   ! GEANT PARTICLE TYPE
      PMASS = MASS_IN
      NM = 7
      NH = 16
      NSDATA = NSDATA - 14
      KHIT = HTDATA(15)
      DO IHIT = 1, KHIT
        NH = NH+NREPEAT
        LEFT = NSMULIB-NM
        IF(LEFT.LT.2000) THEN ! START SQUEEZING HITS IF RUNNING OUT OF SPACE
          F = RNDM(0)
          X = LEFT/NMULIB
          IF ( F.GT.X) GO TO 990
        ENDIF
        CALL UCOPY( HTDATA(NH-NREPEAT),MULB(NM),NREPEAT)
        JHIT = JHIT + 1
        NM = NM + NREPEAT
  990   CONTINUE
      END DO
      NSDATA = NM
  999 CONTINUE
      MULB(6) = JHIT
      RETURN
      END
