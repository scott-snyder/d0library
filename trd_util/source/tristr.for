      SUBROUTINE TRISTR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Select the subset of tracks in the ISAJET banks
C-                          which should be seen in a tube defined by in
C-                          inner cylinder ( RMIN, ZMIN ), and an outer cylinder
C-                          ( RMAX, ZMAX). Create a track bank
C-                          ISDITR containing characteristics of each good
C-                          track.
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  14-APR-1988   Ghita Rahal-Callot
C-   Updated  22-FEB-1989   A. Zylberstejn  adapted to TRD
C-   Updated   2-NOV-1989   J.Fr. Glicenstein: no more outputs+
C-                          calls to TRISIS
C-   Updated  17-NOV-1989   A. Zylberstejn
C-   Updated  29-JUN-1992   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTRMAX
      PARAMETER( NTRMAX =100  )
      INTEGER  NISA,NUMISA(2,NTRMAX)
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDVOL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
C
      EXTERNAL ISCHAR
      INTEGER LOUT,ISCHAR,NERR,TRUNIT
      INTEGER LISAE, LISV1, LISV2, LISP1, LISP2, LISV2S, LISP2S
      REAL VIN(6), VOUT(6), RINS2, ROUTS2, RPAR, RVER, P,RIN,ROUT,ZOUT
      REAL ST,THETA
      INTEGER IGTRAC, IV, IT
      INTEGER I, IS, ICONF
C
      INTEGER NDAMAX
      PARAMETER ( NDAMAX = 9 )
      REAL DATA ( NDAMAX )
      REAL    DIFANG
      LOGICAL FIRST
      DATA      DIFANG / 0.060 /
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        LOUT = TRUNIT()
        FIRST=.FALSE.
        IF(LOUT.NE.0)
     +    WRITE(LOUT,*)' For TRD analysis Theta min',THMIN,' Theta max',
     &    THMAX
        NERR=0
      END IF
      RIN = RADWIN(1)
      ROUT = RADEXT(3)
      ZOUT =ZACT(1)
      RINS2  = RIN  * RIN
      ROUTS2 = ROUT * ROUT
      NISA=0
      IF ( LHEAD .LE. 0 ) THEN
        NERR=NERR+1
        IF(NERR.LE.20)
     +    CALL ERRMSG('Bank HEAD not booked','TRISTR',' ','F')
        GO TO 999
      END IF
      LISAE = LQ ( LHEAD - IZISAE )
      IF ( LISAE .LE. 0 )THEN
        NERR=NERR+1
        IF(NERR.LE.20)
     +    CALL ERRMSG('Bank ISAE not booked','TRISTR',' ','W')
        GO TO 999
      END IF
C
C ****  Look at particles created at primary vertices
C
      LISV1 = LQ ( LISAE - IZISV1 )
      IF ( LISV1 .LE. 0 ) THEN
        GO TO 998
      ENDIF
      DO 100 IV = 1, 1000
C
C ****  Loop on all the short decay vertices
C ****  Verify that the origin vertex is before the RIN
C
        RPAR = Q(LISV1+7)**2 + Q(LISV1+8)**2
        IF ( RPAR .GT. RINS2 ) GO TO 102
        LISP1 = LQ ( LISV1 - IZISP1 )
        IF ( LISP1 .LE. 0 ) THEN
          GO TO 102
        ENDIF
        CALL UCOPY ( Q(LISV1+7), VIN(1), 3 )
        DO 110 IT = 1, 1000
          ICONF = 0
C
C ****  rejects if charge 0
C
          IF ( ISCHAR(IQ(LISP1+1)) .EQ. 0 ) GO TO 112
C
C ****  Loop on all the tracks at the vertex IV
C ****  Look if IT could cross all the Central detector
C
          P = Q ( LISP1 + 5 )
          IF ( P .EQ. 0. ) GO TO 112
          CALL UCOPY ( Q (LISP1+2), VIN(4), 3 )
          VIN(4) = VIN(4) / P
          VIN(5) = VIN(5) / P
          VIN(6) = VIN(6) / P
          ST=SQRT(VIN(4)**2+VIN(5)**2)
          ST=AMIN1(ST,1.)
          THETA=ASIN(ST)*RADDEG
          IF(THETA.LT.THMIN .OR. THETA.GT.THMAX)GO TO 112
C
C ****  Look at the entrance point
C
          CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
          IF ( IGTRAC .NE. 0 ) GO TO 112
          IF ( ABS ( VOUT(3) ) .GT. ZMIN ) GO TO 112
C
C ****  look at the exit point
C
          CALL EXTCYL ( VIN, VOUT, RMAX, IGTRAC )
          IF ( IGTRAC .NE. 0 ) GO TO 112
          IF ( ABS ( VOUT(3) ) .GT. ZMAX ) GO TO 112
C
C ****  Good track : verify if it made secondary vertex
C
          LISV2 = LQ ( LISP1 - 4 )
          IF ( LISV2 .LE. 0 ) GO TO 113
C
C ****  Where is the secondary vertex ? If after (ROUT,ZOUT), good
C
          RVER = Q(LISV2+7)**2 + Q(LISV2+8)**2
          IF ( RVER .LT. ROUTS2 ) THEN
C
C ****  looks if a secondary track follows the primary one
C
            LISP2 = LQ(LISV2-IZISP2)
            IF ( LISP2 .LE. 0 ) GO TO 112
            DO 120 IS = 1 , 1000
              IF ( ISCHAR(IQ(LISP2+1)) .EQ. 0 ) GO TO 121
              P = Q(LISP2 + 5 )
              IF ( P .LE. 0. ) GO TO 121
              CALL UCOPY( Q(LISP2+2), VIN(4), 3 )
              VIN(4) = VIN(4)/P
              VIN(5) = VIN(5)/P
              VIN(6) = VIN(6)/P
              IF ( ABS(Q(LISP2+8)-Q(LISP1+8)) .GT. DIFANG) GO TO 121
              IF ( ABS(Q(LISP2+7)-Q(LISP1+7)) .GT. DIFANG) GO TO 121
              ICONF = IQ(LISP2-5)
              GO TO 113
  121         LISP2 = LQ(LISP2)
              IF ( LISP2 .LE. 0 ) GO TO 112
  120       CONTINUE
            GO TO 112
          ENDIF
  113     CONTINUE
C
C ****  This is a good track : stores its characteristics
C
          IF(TROAD)THEN
            CALL SELTRA(LISV1,LISP1)
          ELSE
            NISA = NISA + 1
            CALL TRISIS(NISA,LISV1,LISP1)
C         NUMISA(1,NISA)=LISV1
C         NUMISA(2,NISA)=LISP1
          END IF
          GO TO 112
C
C ****  Next track at the vertex IV
C
  112     CONTINUE
          LISP1 = LQ ( LISP1 )
          IF ( LISP1 .LE. 0 ) GO TO 111
  110   CONTINUE
  111   CONTINUE
C
C ****  Next vertex bank
C
  102   CONTINUE
        LISV1 = LQ ( LISV1 )
        IF ( LISV1 .LE. 0 ) GO TO 101
  100 CONTINUE
  101 CONTINUE
C
C ****  Look at particles created at secondary vertices
C
  998 CONTINUE
      LISV2 = LQ ( LISAE - IZISV2 )
      IF ( LISV2 .LE. 0 ) THEN
        GO TO 999
      ENDIF
      DO 1200 IV = 1, 1000
C
C ****  Loop on all the long lived vertices
C ****  created in GEANT
C ****  Verify that the vertex is before the RIN
C
        RPAR = Q(LISV2+7)**2 + Q(LISV2+8)**2
        IF ( RPAR .GT. RINS2 ) GO TO 1102
        LISP2 = LQ ( LISV2 - IZISP2 )
        IF ( LISP2 .LE. 0 ) THEN
          GO TO 1102
        ENDIF
        CALL UCOPY ( Q(LISV2+7), VIN(1), 3 )
        DO 1110 IT = 1, 1000
          ICONF = 0
C
C ****  rejects if charge 0
C
          IF ( ISCHAR(IQ(LISP2+1)) .EQ. 0 ) GO TO 1112
C
C ****  Loop on all the tracks at the vertex IV
C ****  Look if IT could cross all the Central detector
C
          P = Q ( LISP2 + 5 )
          IF ( P .EQ. 0. ) GO TO 1112
          CALL UCOPY ( Q (LISP2+2), VIN(4), 3 )
          VIN(4) = VIN(4) / P
          VIN(5) = VIN(5) / P
          VIN(6) = VIN(6) / P
          CALL EXTCYL ( VIN, VOUT, RMAX, IGTRAC )
          IF ( IGTRAC .NE. 0 ) GO TO 1112
C
C ****  Verify that the Z component is in the cylinder
C
          IF ( ABS ( VOUT(3) ) .GT. ZOUT ) GO TO 1112
C
C ****  Good track : verify if it made secondary vertex
C
          LISV2S = LQ ( LISP2 - 4 )
          IF ( LISV2S .LE. 0 ) GO TO 1113
C
C ****  Where is the secondary vertex ? If after (ROUT,ZOUT), good
C
          RVER = Q(LISV2S+7)**2 + Q(LISV2S+8)**2
          IF ( RVER .LT. ROUTS2 )THEN
C
C ****  looks if a secondary track follows the primary one
C
            LISP2S = LQ(LISV2S-IZISP2)
            IF ( LISP2S .LE. 0 ) GO TO 1112
            DO 1120 IS = 1 , 1000
              IF ( ISCHAR(IQ(LISP2S+1)) .EQ. 0 ) GO TO 1121
              P = Q(LISP2S + 5 )
              IF ( P .LE. 0. ) GO TO 1121
              IF ( ABS(Q(LISP2+8)-Q(LISP2S+8)) .GT. DIFANG) GO TO 1121
              IF ( ABS(Q(LISP2+7)-Q(LISP2S+7)) .GT. DIFANG) GO TO 1121
              ICONF = IQ(LISP2S-5)
              GO TO 1113
 1121         LISP2S = LQ(LISP2S)
              IF ( LISP2S .LE. 0 ) GO TO 1112
 1120       CONTINUE
            GO TO 1112
          ENDIF
 1113     CONTINUE
C
C ****  This is a good track : stores its characteristics
C
          IF(TROAD)THEN
            CALL SELTRA(LISV1,LISP1)
          ELSE
            NISA = NISA + 1
            CALL TRISIS(NISA,LISV2,LISP2)
C         NUMISA(1,NISA)=LISV2
C         NUMISA(2,NISA)=LISP2
          END IF
          GO TO 1112
C
C ****  Next track at the vertex IV
C
 1112     CONTINUE
          LISP2 = LQ ( LISP2 )
          IF ( LISP2 .LE. 0 ) GO TO 1111
 1110   CONTINUE
 1111   CONTINUE
C
C ****  Next vertex bank
C
 1102   CONTINUE
        LISV2 = LQ ( LISV2 )
        IF ( LISV2 .LE. 0 ) GO TO 1210
 1200 CONTINUE
 1210 CONTINUE
  999 RETURN
      END
