      SUBROUTINE PTLOCA(WIRE,LAYER,ENRG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allws the user to pick a wire in the TRD
C-              computes the wire number and layer number picked.
C-
C-   Inputs  : None
C-
C-   Outputs : LAYER,WIRE - Wire and layer number of TRD picked
C-             ENRG       - Energy of the wire picked
C-
C-   Created   5-JAN-1989   Lupe Rosas
C-   Updated  19-APR-1991   Lupe Howell   Hardcopy check
C-   Updated  16-MAY-1991   JFG Adds cathodes
C-   Updated  22-FEB-1994   A. Zylberstejn  : correct for 512 wires in layer 3 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRHITW.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
C----------------------------------------------------------------------
      INTEGER TMAX,J
      PARAMETER(TMAX=15)
C
      REAL TFADC(128,TMAX)
      REAL AK
      LOGICAL TRUTH
      REAL ENRG
      REAL OLD_ENRG
      REAL VX,VY,X,Y,Z,RADEG,PI,TEMP
      REAL R(4)     ! RAIOUS OF TRD LAYERS
      REAL DELANG(3)   ! ANGULAR 1/2 WIDTH OF CELL IN DEGREES
      REAL DIST     ! DISTANCE BETW THE CENTER AND POINT PICKED
      REAL PTDIST   ! DIST BETW TWO PONITS FUNCTION
      REAL PTANGL   ! ANG BET TWO POINTS FUNCTION
      REAL PHI      ! ANGULE BETW THE CENTER AND THE POINT PICKED
C
      INTEGER LAYER    ! LAYER WHERE THE WIRE PICKED IS
      INTEGER WIRE
      INTEGER OLD_LAYER,OLD_WIRE
      INTEGER IB1,SEGNUM,ICRT
      INTEGER ANODE
      INTEGER NTFADC,TLAY(TMAX),TWIR(TMAX)
C
      LOGICAL FLGVAL,FIRST
C
      SAVE OLD_WIRE,OLD_LAYER,OLD_ENRG
C-----------------------------------------------------------------------
      DATA PI/3.1415927/
      DATA ICRT/1/
      DATA R/17.50,28.05,38.60,49.15/
      DATA ANODE/1/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
C  define half angle covered by 1/2 cell (in degrees)
        DO J=1,3
          DELANG(J)=180./FLOAT(NWIRE_PER_LAYER(J))
        END DO
      END IF
C
C ****  Checking if a Hardcopy was requested
C ****  If it is a hardcopy use old wire, energy and layer values
C
      TRUTH = FLGVAL('TRD_ANO_CATH')
C       IF (TRUTH.EQ..TRUE.) THEN
      AK = 0.
C       ELSE IF (TRUTH.EQ..FALSE.) THEN
C        AK = 3.
C       ENDIF
      IF ( FLGVAL('HARDCOPY') ) THEN
        WIRE = OLD_WIRE
        LAYER = OLD_LAYER
        ENRG = OLD_ENRG
      ELSE
C
C ****  Get wire from the user
C
        RADEG=180./PI
        CALL JPECHO(ICRT,2,1,0.,0.)
    5   CALL JIENAB(ICRT,2,1)
        CALL PUOPEN
        CALL JLOCAT(ICRT,1,1,IB1,VX,VY)    ! Picking cell by user
        IF (IB1.LT.0) THEN
          CALL JRCLOS
          CALL STAMSG('Selection Out of Range.. try again')
          WIRE=0
          GO TO 999
C          GO TO 5  ! SELECTION OUT OF RANGE..
        ENDIF
        CALL JPECHO(ICRT,2,1,VX,VY)
        CALL JCONVW(VX,VY,X,Y,Z)
        CALL JIDISA(ICRT,2,1)
C
C ****  Tranlating picked point to TRD wire
C
        DIST=PTDIST(0.,0.,X,Y)          ! GETTING DISTANCE
        PHI=PTANGL(0.,0.,X,Y,DIST)      ! GETTING ANGLE
        PHI=PHI*RADEG
        IF((DIST.LT.R(1)).OR.(DIST.GT.R(4))) THEN
          CALL JRCLOS
          WIRE=0
          GO TO 999
C          GO TO 5        ! ILLEGAL PICK
        ELSEIF((DIST.GE.R(1)).AND.(DIST.LT.R(2))) THEN
          LAYER=1. + AK
        ELSEIF((DIST.GE.R(2)).AND.(DIST.LT.R(3))) THEN
          LAYER=2. + AK
        ELSE
          LAYER=3. + AK
        ENDIF
        PHI = PHI - OFSDPH(LAYER)          ! INCLUDE CHAMBER OFFSETS
        WIRE=INT((PHI/(DELANG(LAYER)*2.)+1.)+.5)  ! CALCULATING WIRE ROUNDING UP
C        WIRE=WIRE
C
C ****  Getting energy of wire
C
        DO 700 J=1,NBTHIT(LAYER,ANODE)
          IF(NUMTWH(J,LAYER,ANODE).EQ.WIRE) THEN
            ENRG= ENTWH(J,LAYER,ANODE)
            GO TO 702
          ENDIF
  700   CONTINUE
  702   CONTINUE
        CALL JMARK(X,Y)
        CALL JRCLOS
        OLD_WIRE = WIRE
        OLD_LAYER = LAYER
        OLD_ENRG = ENRG
      ENDIF
  999 RETURN
      END

