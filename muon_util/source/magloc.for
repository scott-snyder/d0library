      SUBROUTINE MAGLOC( VECT, NUMED, XX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Look at muon magenet geometry. Determine 
C-                         given point is in fiducial of any magnet.
C-                         And then converted to the local coordinate
C-                         of that piece of iron.
C-   Inputs  : VECT(3), X,Y,Z in global coordinate 
C-   Outputs : NUMED = 0 not in magnet
C-                     1 CF
C-                     2 EF
C-                     3 SAMUS
C-             XX  Local coordinate in iron.
C-   Controls: 
C-
C-   Created  D.Hedin  1/92  first version....simolifies things
C-   Updated  DH  4/92   add SAMUS; still simple
C-   Updated  A.Mesin  7/92  Use STP instead of being hard-wired
C-   Updated  9/92  Atsushi Taketani Modify for fast algorithm
C-   Updated  4/93  Susumu Igarashi for magnet rotation MMAH version 4
C-   Updated  5/93  Atsushi Taketani Bug fix for Samus part
C-   Updated  6/93  Susumu Igarashi Bug fix for LMMAH
C-   Updated  12/93 AT fast scanning
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL    VECT(3), XX(3)
      INTEGER NUMED
C
      INTEGER I
      INTEGER NMAG
      REAL    SPAR(6),XPAR(3),ROTM(3,3)
      INTEGER NSPAR,NBUF,IBUF
      CHARACTER*4 HSHAPE
      REAL    MSPAR(3), HSPAR(3)
      EQUIVALENCE (MSPAR(1),SPAR(1))
      EQUIVALENCE (HSPAR(1),SPAR(4))
C
      REAL    SMAX(3), SMIN(3), HMAX(3), HMIN(3)
C
      REAL    WMAX(3), WMIN(3), WSPAR(3)
      REAL    ROTMINV(3,3)
      INTEGER K1,K2
      INTEGER LMMAH,GZMMAH
      INTEGER J,FIRST,VERSION
      REAL    MAGTRA(3),MAGROT(3,3)
      REAL    VECT1(3),VECT2(3),XPAR1(3),XPAR2(3),WSPAR1(3)
C
      INTEGER NMAG_OLD, NMAGL
C
      DATA FIRST/0/
      DATA NMAG_OLD/1/
C----------------------------------------------------------------------
C
      LMMAH=GZMMAH(0)   !Get location of which field map to use
      IF (FIRST .EQ. 0) THEN
        FIRST=1
        VERSION=IC(LMMAH+1)  ! MMAH version
      ENDIF

      NUMED = 0           ! reset output
C
      DO 100 I=1,3        ! Same position for NOW
  100   XX(I) = VECT(I)
C
      DO 200 NMAGL=0,29
C  start with 
        IF ( NMAGL.EQ.0 ) THEN
          NMAG = NMAG_OLD
        ELSE
          NMAG = NMAGL
        END IF
C consider rotation of magnets
        IF(VERSION.EQ.4)THEN
          IF(NMAG.LE.20)THEN                    ! CF
            DO 201 I=1,3
  201         MAGTRA(I)=-C(LMMAH+34+I)           ! Inverse translation
            DO 202 I=1,3
              DO 202 J=1,3
  202           MAGROT(J,I)=C(LMMAH+34+I+3*J)    ! Inverse matrix
          ELSEIF(NMAG.GE.21.AND.NMAG.LE.24)THEN ! EFS
            DO 203 I=1,3
  203         MAGTRA(I)=-C(LMMAH+22+I)
            DO 204 I=1,3
              DO 204 J=1,3
  204           MAGROT(J,I)=C(LMMAH+22+I+3*J)
          ELSEIF(NMAG.GE.25)THEN                ! EFN
            DO 205 I=1,3
  205         MAGTRA(I)=-C(LMMAH+10+I)
            DO 206 I=1,3
              DO 206 J=1,3
  206           MAGROT(J,I)=C(LMMAH+10+I+3*J)
          ENDIF
        ENDIF
C
        CALL MUMAGS( NMAG,HSHAPE, NSPAR, SPAR, XPAR, ROTM, NBUF, IBUF )
        IF ( NSPAR.EQ.0 ) GOTO 210
C        DO 220 K1=1,3
C        DO 220 K2=1,3
C  220     ROTMINV(K1,K2) = ROTM(K2,K1)

        IF(VERSION.LE.3) THEN
          CALL VMATR( SPAR, ROTM, WSPAR, 3, 3 )
          DO 230 K1=1,3
  230       WSPAR(K1) = ABS(WSPAR(K1))
          CALL VADD( XPAR, WSPAR, WMAX, 3 )
          CALL VSUB( XPAR, WSPAR, WMIN, 3 )
          DO 231 I=1,3
            IF ( VECT(I).GT.WMAX(I) ) GOTO 210
            IF ( VECT(I).LT.WMIN(I) ) GOTO 210
  231     CONTINUE

        ELSEIF(VERSION.EQ.4) THEN
          CALL VADD( VECT, MAGTRA, VECT1, 3 )
          CALL VMATR( VECT1, MAGROT, VECT2, 3, 3 )
          CALL VADD( XPAR, MAGTRA, XPAR1, 3 )
          CALL VMATR( XPAR1, MAGROT, XPAR2, 3, 3 )
          CALL VMATR( SPAR, ROTM, WSPAR, 3, 3 )
          CALL VMATR( WSPAR, MAGROT, WSPAR1, 3, 3 )
          DO 242 I=1,3        
  242       XX(I) = VECT2(I)  ! Local coordinate in magnet
          DO 240 K1=1,3
  240       WSPAR1(K1) = ABS(WSPAR1(K1))
          CALL VADD( XPAR2, WSPAR1, WMAX, 3 )
          CALL VSUB( XPAR2, WSPAR1, WMIN, 3 )
          DO 241 I=1,3
            IF ( VECT2(I).GT.WMAX(I) ) GOTO 210
            IF ( VECT2(I).LT.WMIN(I) ) GOTO 210
  241     CONTINUE
        ENDIF

        IF ( NMAG.LE.20 ) THEN
          NUMED = 1
        ELSE
          NUMED = 2
        END IF
        NMAG_OLD = NMAG
        GOTO 999
  210   CONTINUE
  200 CONTINUE
C
      DO I=1,3
        XX(I) = VECT(I)
      END DO
C
      DO 500 NMAG=1,2    ! for SMAUS scan, assume only 2 slab
        CALL GTSMAG( NMAG, HSHAPE, NSPAR, SPAR, XPAR, ROTM, NBUF, IBUF )
        IF ( NSPAR.EQ.0 ) GOTO 510   ! check GTSMAG output
        CALL VADD( XPAR, MSPAR, SMAX, 3 )  ! get edge position
        CALL VSUB( XPAR, MSPAR, SMIN, 3 )
        CALL VADD( XPAR, HSPAR, HMAX, 3 )
        CALL VSUB( XPAR, HSPAR, HMIN, 3 )
        DO I=1,3                           ! check outside edge  
          IF ( VECT(I).LE.SMIN(I) ) GOTO 510
          IF ( VECT(I).GE.SMAX(I) ) GOTO 510
        END DO
        DO I=1,2                           ! check inside edge
          IF ( VECT(I).GT.HMAX(I) ) GOTO 520
          IF ( VECT(I).LT.HMIN(I) ) GOTO 520
        END DO
        GOTO 510
  520   NUMED = 3
        GOTO 999
  510   CONTINUE
  500 CONTINUE
C
  999 RETURN
      END
