      SUBROUTINE GTISAT(LVREF,LPREF,PHIIN,THETIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Select the ISAJET track the closest in phi qnd
C-   Theta to track defined by phiin and thetin
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  14-APR-1988   Ghita Rahal-Callot
C-   Updated  22-FEB-1989   A. Zylberstejn  adapted to TRD
C-   Updated  13-MAR-1991   A. Zylberstejn  :CHANGE ARGUMENTS
C-
C----------------------------------------------------------------------
      INTEGER NTRMAX
      PARAMETER( NTRMAX =300  )
      INTEGER  NISA
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:trdvol.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
C
      EXTERNAL ISCHAR
      INTEGER LOUT,ISCHAR,NERR,TRUNIT
      INTEGER LISAE, LISV1, LISV2, LISP1, LISP2, LISV2S, LISP2S
      REAL VIN(6), VOUT(6), RINS2, ROUTS2, RPAR, RVER, P
      REAL DPHI,DPHIN,ST,THETA,PTMAX,PHIIN,THETIN
      INTEGER IGTRAC, IV, IT,LVREF,LPREF
      INTEGER I, IS, ICONF
      INTEGER NDAMAX
      REAL    DIFANG
      LOGICAL FIRST
      DATA      DIFANG / 0.060 /
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        THMIN=40.
        THMAX=150.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('THMIN',THMIN,IER)
        CALL EZGET('THMAX',THMAX,IER)
        CALL EZRSET
C ****  Define the limits of the TRD RMIN, ZMIN, RMAX, ZMAX
        RIN = RADWIN(1)
        ROUT = RADEXT(3)
        ZMIN =ZACT(1)
        ZMAX = ZMIN
        ZOUT =ZACT(1)
        RINS2  = RIN  * RIN
        ROUTS2 = ROUT * ROUT
        NERR=0
      END IF
      NISA=0
      LVREF=0
      LPREF=0
      DPHI=1000.
      IF ( LHEAD .LE. 0 ) THEN
        IF(NERR.LE.20)
     +    CALL ERRMSG('Bank HEAD not booked','GTISAT',' ','F')
        NERR=NERR+1
        GO TO 999
      END IF
      LISAE = LQ ( LHEAD - IZISAE )
      IF ( LISAE .LE. 0 )THEN
        NERR=NERR+1
        IF(NERR.LE.20)
     +    CALL ERRMSG('Bank ISAE not booked','GTISAT',' ','W')
        GO TO 999
      END IF
C
C
C ****  Look at particles created at primary vertices
C
      LISV1 = LQ ( LISAE - IZISV1 )
      IF ( LISV1 .LE. 0 ) THEN
        GO TO 998
      ENDIF
      PTMAX=0.
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
C ****  Loop on all the tracks at the vertex IV
C ****  Look if IT could cross all the Central detector
C
          P = Q ( LISP1 + 5 )
          IF ( P .EQ. 0. ) GO TO 112
          IF(P.LT.PTMAX)GO TO 112
          CALL UCOPY ( Q (LISP1+2), VIN(4), 3 )
          VIN(4) = VIN(4) / P
          VIN(5) = VIN(5) / P
          VIN(6) = VIN(6) / P
          ST=SQRT(VIN(4)**2+VIN(5)**2)
          ST=AMIN1(ST,1.)
          THETA=Q(LISP1+8)*RADDEG
C
C ****  Look at the entrance point
C
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
C              IF ( ISCHAR(IQ(LISP2+1)) .EQ. 0 ) GO TO 121
              P = Q(LISP2 + 5 )
              IF ( P .LE. 0. ) GO TO 121
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
          NISA = NISA + 1
          PT=SQRT(Q(LISP1+2)**2+Q(LISP1+3)**2)
          DPHIN=ABS(Q(LISP1+7)-PHIIN)
          IF(DPHIN.LT.DPHI)THEN
            LVREF=LISV1
            LPREF=LISP1
            DPHI=DPHIN
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
C          print*,' dans gtisat, charge', ISCHAR(IQ(LISP1+1)),' pt',
C     +          sqrt(q(lisp1+2)**2+q(lisp1+3)**2)
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
      DO 1100 IV = 1, 1000
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
C          IF ( ISCHAR(IQ(LISP2+1)) .EQ. 0 ) GO TO 1112
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
          PT=SQRT(Q(LISP2+2)**2+Q(LISP2+3)**2)
          DPHIN=ABS(Q(LISP2+7)-PHIIN)
          IF(DPHIN.LT.DPHI)THEN
            DPHI=DPHIN
            LVREF=LISV2
            LPREF=LISP2
            PTMAX=PT
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
        IF ( LISV2 .LE. 0 ) GO TO 1101
 1100 CONTINUE
 1101 CONTINUE
  999 CONTINUE
      RETURN
      END
