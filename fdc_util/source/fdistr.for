      SUBROUTINE FDISTR(  ZIN, RIN, ZOUT, ROUT, NISA )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Select the subset of tracks in the ISAJET banks
C-                          which should be seen in a thick disk with a
C-                          central hole defined by an inner cylinder with
C-                          RIN and an outer cylinder with ROUT.  Extending
C-                          from ZIN to ZOUT along the Z axis.  Create a
C-                          track bank ISFITR containing characteristics
C-                          of each good track.
C-
C-   Inputs  : ZIN          : Min Z of inner disk plane
C-             RIN          : Inner radius of thick disk
C-             ZOUT         : Max Z of outer disk plane
C-             ROUT         : Outer radius of thick disk
C-             NISA         : # of tracks already found
C-   Outputs : NISA         : # of tracks found, total
C-
C-   Modified 17-FEB-1989   Jeffrey Bantly from GRC CDISTR.FOR original
C-   Updated  23-JUL-1990   Jeffrey Bantly  general cleanup
C-   Updated   8-NOV-1990   Jeffrey Bantly  add next vertex check for
C-                                          each particle path
C-   Updated  27-NOV-1991   Robert E. Avery  Compute THETA and PHI from
C-                              P vector (because of old ISAJET bug). 
C-   Updated   7-FEB-1992   Robert E. Avery  Fix bug, primary tracks that 
C-                              decayed at small readius before FDC were
C-                              being included in FISA bank. 
C-   Updated  17-FEB-1992   Robert E. Avery  Major rewrite and cleanup.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NISA
      REAL RIN, ZIN, ROUT, ZOUT
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER ISCHAR
      INTEGER LISAE, GZISAE
      INTEGER IVTX 
      INTEGER IGTRAC, IGTRAC2
      INTEGER I, IS, IER
      INTEGER LISV, LISP, LISV2
      REAL    RINSQ, ROUTSQ
      REAL    RVERSQ, ZVER
      REAL    RVERSQ2, ZVER2
      REAL    VIN(6), VOUT(6)
      REAL    DATA(9)
      REAL    PHI, PT, THETA, P
C
C----------------------------------------------------------------------
      RINSQ  = RIN  * RIN
      ROUTSQ = ROUT * ROUT
C
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) GOTO 999
C
C  Look at particles created at primary and secondary vertices
C
      DO IVTX =  0, 1
C
C  Loop on all vertices
C
        LISV = LQ( LISAE - (IZISV1+IVTX) )
        DO WHILE (LISV .GT. 0 )
C
C ****  Verify that the origin vertex is before ZOUT
C
          RVERSQ = Q(LISV+7)**2 + Q(LISV+8)**2
          ZVER = Q(LISV+9)
          IF ( ABS(ZVER).GT.ABS(ZOUT) ) GOTO 100
          IF ( (ABS(ZVER).GT.ABS(ZIN))
     &      .AND. (RVERSQ .LE. RINSQ) ) GOTO 100
C
          CALL UCOPY ( Q(LISV+7), VIN(1), 3 )
C
C Loop on all the tracks at the vertex 
C
          LISP = LQ ( LISV - IZISP1 )
          DO WHILE (LISP.GT.0)
C
C Reject if charge 0
C
            IF ( ISCHAR(IQ(LISP+1)) .EQ. 0 ) GOTO 110
C
            P = Q ( LISP + 5 )
            IF ( P .EQ. 0. ) GOTO 110
            CALL UCOPY ( Q (LISP+2), VIN(4), 3 )
            VIN(4) = VIN(4) / P
            VIN(5) = VIN(5) / P
            VIN(6) = VIN(6) / P
C
C Reject backwards tracks:
C
            IF( ZIN*VIN(6) .LE. 0. ) GOTO 110
C
C Recompute Theta and Phi (beacuse of old isajet bug)
C
            PHI = ATAN2( Q(LISP+3), Q(LISP+2) )
            IF ( PHI.LT.0 ) PHI = PHI + TWOPI
            PT = SQRT( VIN(4)**2  + VIN(5)**2 )
            THETA = ATAN2( PT, VIN(6) )
C
C Check fiducual
C
C ****  look inner radius
C
            CALL EXTDSK ( VIN, VOUT, RIN, ZIN, IGTRAC )
            CALL EXTDSK ( VIN, VOUT, RIN, ZOUT, IGTRAC2 )
            IF ( (IGTRAC.NE.1) .AND. (IGTRAC2.NE.1) ) GOTO 110
C
C ****  Look at outer radius
C
            CALL EXTDSK ( VIN, VOUT, ROUT, ZIN, IGTRAC )
            CALL EXTDSK ( VIN, VOUT, ROUT, ZOUT, IGTRAC2 )
            IF ( (IGTRAC.NE.0) .AND. (IGTRAC2.NE.0) ) GOTO 110
C
C Look for secondary decay:
C
            LISV2 = LQ ( LISP - 4 )
            IF ( LISV2 .GT. 0 ) THEN
              RVERSQ2 = Q(LISV2+7)**2 + Q(LISV2+8)**2
              ZVER2 = Q(LISV2+9)
            ELSE
              RVERSQ2 = 999.
              ZVER2 = 999.
            ENDIF
            IF ( (ABS(ZVER2) .LE. ABS(ZIN)) 
     &        .OR. (RVERSQ2 .LE. RINSQ) ) THEN 
              GOTO 110
            ENDIF
C
C ****  This is a good track : stores its characteristics
C
            NISA = NISA + 1
            DATA(1) = Q(LISV+7)                         
            DATA(2) = Q(LISV+8)                         
            DATA(3) = Q(LISV+9)                         
            DATA(4) = PHI
            DATA(5) = THETA
            DATA(6) = Q(LISP+5)                         
            DATA(7) = Q(LISP+6)                         
            DATA(8) = FLOAT( IQ(LISP-5) )   
            DATA(9) = -999.                               
            CALL ZFFITR ( NISA, DATA )
C                
C ****  Next tracK at the vertex 
C                 
  110       CONTINUE
            LISP = LQ ( LISP )
          ENDDO
C
C ****  Next vertex bank
C
  100     CONTINUE
          LISV = LQ ( LISV )
        ENDDO
      ENDDO
C
C-------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
