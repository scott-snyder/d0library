      SUBROUTINE CDISTR(ZIN, RIN, ZOUT, ROUT, NISA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Select the subset of tracks in the ISAJET banks
C-                          which should be seen in a tube defined by in
C-                          inner cylinder ( RIN, ZIN ), and an outer cylinder
C-                          ( ROUT, ZOUT). Create a track bank DITR containing
C-                          characteristics of each good track.
C-
C-   Inputs  : ZIN          : Max Z of inner cylinder
C-             RIN          : Radius of inner cylinder
C-             ZOUT         : Max Z of outer cylinder
C-             ROUT         : Radius of outer cylinder
C-   Outputs : NISA         : # of tracks
C-
C-   Created  14-APR-1988   Ghita Rahal-Callot
C-   Updated  12-JUL-1988   Ghita Rahal-Callot  add as a good track, a track
C-                          which gives a vertex in the chamber but where a
C-                          track from this vertex has the same slope as the
C-                          mother track
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  remove CDCTRL.INC
C-                                                   and GCUNIT.INC     
C-   Updated   5-JUL-1991   Qizhong Li-Demarteau  fix when no ISAJET bank exist 
C-   Updated  11-MAR-1992   Qizhong Li-Demarteau  use ERRMSG 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NISA
      REAL RIN, ZIN, ROUT, ZOUT
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
C
      EXTERNAL ISCHAR
      INTEGER ISCHAR
      INTEGER LISAE, LISV1, LISV2, LISP1, LISP2, LISV2S, LISP2S
      REAL VIN(6), VOUT(6), RINS2, ROUTS2, RPAR, RVER, P
      INTEGER IGTRAC, IV, IT
      INTEGER I, IS, ICONF
C
      INTEGER NDAMAX
      PARAMETER ( NDAMAX = 9 )
      REAL DATA ( NDAMAX )
      REAL    DIFANG
      DATA      DIFANG / 0.060 /
C----------------------------------------------------------------------
      RINS2  = RIN  * RIN
      ROUTS2 = ROUT * ROUT
      IF (LHEAD .LE. 0) THEN 
        CALL ERRMSG('DTRAKS','CDISTR','LHEAD not defined','F')
      ENDIF
      LISAE = LQ(LHEAD - IZISAE)
      IF (LISAE .LE. 0) THEN
        CALL ERRMSG('DTRAKS','CDISTR',
     &    'Unable to find bank ISAE, will not build DITR bank','W')
        GOTO 999
      ENDIF
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
C
C ****  Look at the entrance point
C
          CALL EXTCYL ( VIN, VOUT, RIN, IGTRAC )
          IF ( IGTRAC .NE. 0 ) GO TO 112
          IF ( ABS ( VOUT(3) ) .GT. ZIN ) GO TO 112
C
C ****  look at the exit point
C
          CALL EXTCYL ( VIN, VOUT, ROUT, IGTRAC )
          IF ( IGTRAC .NE. 0 ) GO TO 112
          IF ( ABS ( VOUT(3) ) .GT. ZOUT ) GO TO 112
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
          NISA = NISA + 1
          DATA ( 1 ) = Q ( LISV1 + 7 )
          DATA ( 2 ) = Q ( LISV1 + 8 )
          DATA ( 3 ) = Q ( LISV1 + 9 )
          DATA ( 4 ) = Q ( LISP1 + 7 )
          DATA ( 5 ) = Q ( LISP1 + 8 )
          DATA ( 6 ) = Q ( LISP1 + 5 )
          DATA ( 7 ) = Q ( LISP1 + 6 )
          DATA ( 8 ) = FLOAT(IQ ( LISP1 -5)+ 10000*ICONF)
          DATA ( 9 ) = - 999.
          CALL ZFDITR ( NISA, DATA )
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
          CALL EXTCYL ( VIN, VOUT, ROUT, IGTRAC )
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
          NISA = NISA + 1
          DATA ( 1 ) = Q ( LISV2 + 7 )
          DATA ( 2 ) = Q ( LISV2 + 8 )
          DATA ( 3 ) = Q ( LISV2 + 9 )
          DATA ( 4 ) = Q ( LISP2 + 7 )
          DATA ( 5 ) = Q ( LISP2 + 8 )
          DATA ( 6 ) = Q ( LISP2 + 5 )
          DATA ( 7 ) = Q ( LISP2 + 6 )
          DATA ( 8 ) = FLOAT(IQ ( LISP2 - 5 ) + 10000*ICONF)
          DATA ( 9 ) = -999.
          CALL ZFDITR ( NISA, DATA )
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
  999 RETURN
      END
