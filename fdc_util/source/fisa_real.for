      SUBROUTINE FISA_REAL( HALF, NISA )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Select the subset of tracks in the ISAJET banks
C-                          which should be seen in the FDC, using realistic
C-                          description of FDC active volume.
C-                          Track is required to pass through at least one 
C-                          of the two theta chambers (at the sense wire 0
C-                          plane).
C-                          Create a track bank ISFITR containing 
C-                          characteristics of each good track.
C-
C-   Inputs  : HALF         : FDC half.
C-             NISA         : # of tracks already found
C-   Outputs : NISA         : # of tracks found, total
C-
C-   Updated  17-FEB-1992   Robert E. Avery  Based loosely on FDISTR
C-                             by Jeffrey Bantly from GRC CDISTR.FOR original
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NISA
      INTEGER  HALF
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER ISCHAR
      INTEGER LISAE, GZISAE
      INTEGER IVTX 
      INTEGER ICALL,IER
      INTEGER LISV, LISP, LISV2
      INTEGER H, LAYER,QUAD,SECTOR
      REAL    DIMEN(6)
      REAL    RMINSQ_THET ,RMAXSQ_THET 
      REAL    XW,YW,ZW(0:1,0:1),ZWIRE 
      REAL    DX_TRK ,DY_TRK
      REAL    XPOS,YPOS 
      REAL    RSQ_POS 
      REAL    RVERSQ, ZVER
      REAL    RVERSQ2, ZVER2  
      REAL    VIN(6)
      REAL    DATA(9)
      REAL    PHI, PT, THETA, P
      LOGICAL LAY_OK(0:1)
C
      SAVE ICALL
      DATA ICALL /0/
C----------------------------------------------------------------------
      IF ( ICALL .EQ. 0 ) THEN
        ICALL=1
C
C Get Z of SW0 for inner and outer theta chamber
C
        DO H =  0, 1
          DO LAYER =  0, 1
            CALL GTFALH( H,0,4*LAYER,0,0,
     &               XW,YW,ZW(LAYER,H))
          ENDDO
        ENDDO
C
C Get Minimum and maximum radius where point is assured to be in theta
C   (unless in a crack)
C
        CALL GTFWTX(0,4,0,DIMEN)
        XPOS = DIMEN(4)-DIMEN(1) 
        YPOS = DIMEN(2) 
        RMINSQ_THET = XPOS**2+YPOS**2
C
        CALL GTFWTX(0,4,4,DIMEN)
        XPOS = DIMEN(4)-DIMEN(1) 
        YPOS = DIMEN(2) 
        RMAXSQ_THET = XPOS**2+YPOS**2
      ENDIF
C
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) GOTO 999
C
C  Look at particles created at primary and secondary vertices
C
      DO IVTX = 0, 1
C
C  Loop on all vertices
C
        LISV = LQ( LISAE - (IZISV1+IVTX) )
        DO WHILE (LISV .GT. 0 )
C
C  Verify that the origin vertex is before Outer Theta chamber.
C
          RVERSQ = Q(LISV+7)**2 + Q(LISV+8)**2
          ZVER = Q(LISV+9)
          IF ( ABS(ZVER).GT.ABS(ZW(1,0) ) ) GOTO 100
C
          CALL UCOPY ( Q(LISV+7), VIN(1), 3 )
C
C  Loop on all the tracks at the vertex 
C
          LISP = LQ ( LISV - IZISP1 )
          DO WHILE (LISP.GT.0)
C
C  Reject if charge 0
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
            IF( VIN(6)*( (-1)**HALF ) .GE. 0. ) GOTO 110
C
C Recompute Theta and Phi (beacuse of old isajet bug)
C
            PHI = ATAN2( VIN(5),VIN(4) )
            IF ( PHI.LT.0 ) PHI = PHI + TWOPI
            PT = SQRT( VIN(4)**2  + VIN(5)**2 )
            THETA = ATAN2( PT, VIN(6) )
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
C
C Check that it goes through at least one Theta chamber:
C
            DX_TRK = TAN(THETA)* COS(PHI)
            DY_TRK = TAN(THETA)* SIN(PHI)
            DO LAYER =  0, 1
              ZWIRE = ZW(LAYER,HALF)
              XPOS = VIN(1) + DX_TRK * ( ZWIRE - ZVER ) 
              YPOS = VIN(2) + DY_TRK * ( ZWIRE - ZVER ) 
              RSQ_POS = XPOS**2+YPOS**2
              LAY_OK(LAYER) = (RSQ_POS.GT.RMINSQ_THET ) 
     &                    .AND. (RSQ_POS.LT.RMAXSQ_THET ) 
C
              IF ( .NOT.LAY_OK(LAYER)  ) THEN
                CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QUAD,SECTOR)
                IF (SECTOR .GT. -1) LAY_OK(LAYER) = .TRUE.
              ENDIF
              IF (   ( ABS(ZVER)  .GT. ABS(ZWIRE) )
     &            .OR. ( ABS(ZVER2) .LT. ABS(ZWIRE) ) ) THEN
                LAY_OK(LAYER) = .FALSE.
              ENDIF
            ENDDO
C
            IF ( (.NOT.LAY_OK(0)) .AND. (.NOT.LAY_OK(1)) )  THEN
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
