      SUBROUTINE CAEHFL_VERTEX(NVMAX,XYZV,DXYZV,NV,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Provide vertex position and its error for CAEHFL routine.
C-
C-   Inputs  : NVMAX        [I] Maximum number of vertices
C-   Outputs : XYZV(3,*)    [R] Vertex positon
C-             DXYZV(3,*)   [R] Error in vertex position
C-             NV           [I] Number of vertices found
C-             IER          [I] Error code: 0 - OK
C-   Controls: None
C-
C-   Created   7-APR-1993   Stan M. Krzywdzinski
C-     Original code in CAEHFL pertaining to vertex was copied over here.
C-   Updated  15-MAR-1994   Chip Stewart   Altered priorities 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZPLV0.LINK'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
C----------------------------------------------------------------------
      REAL XYZV(3,*),DXYZV(3,*)
      INTEGER NVMAX, NV, IER
C----------------------------------------------------------------------
      INTEGER GZISV1,GZPROC,LPLV0,NVER,IVER,ICONT(10),NL
      REAL    ZV,DZV
      REAL    VERT(14),XYBEAM(2)
      LOGICAL USE_VERT_XY,USE_RCP_XY
      LOGICAL FIRST,LMONTE,LV0
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      NV = 0
      IER = 0
      IF (NVMAX .LE. 0) THEN
        IER = 1
        GO TO 999
      ELSE
        CALL VZERO( XYZV,3*NVMAX)
        CALL VZERO(DXYZV,3*NVMAX)
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
        CALL EZGET('USE_LEVEL_ZERO_VERTEX',LV0,IER)
        CALL EZGET('USE_VERT_XY',USE_VERT_XY,IER)
        CALL EZGET('USE_RCP_XY',USE_RCP_XY,IER)
        IF(USE_VERT_XY.AND.USE_RCP_XY) CALL ERRMSG('RCP Mismatch',
     &    'CAEHFL_VERTEX',' Default is to use VERT bank XY','W')
        IF(USE_RCP_XY) CALL EZGET('RCP_BEAM_XY',XYBEAM,IER)
        CALL EZRSET
      ENDIF
C
C *** Try Vertex from tracking first
C
      CALL GTVERH(ICONT)
      NVER = ICONT(2)
      NV = NVER
      IF (NVER .GT. 0) THEN
        DO IVER=1,MIN(NVER,NVMAX)
          CALL GTVERT(IVER,VERT)
          XYZV(3,IVER) = VERT(5)
          DXYZV(3,IVER)= VERT(8)
          IF (USE_RCP_XY) THEN
            XYZV(1,IVER) = XYBEAM(1)
            XYZV(2,IVER) = XYBEAM(2)
          ELSE
            XYZV(1,IVER) = VERT(3)
            XYZV(2,IVER) = VERT(4)
            DXYZV(1,IVER)= VERT(6)
            DXYZV(2,IVER)= VERT(7)
          ENDIF
        ENDDO
      ENDIF
C
C ****  Check ISV1 vertex if no VERH; set LMONTE switch and make some noise
C
      NL = IQ(LHEAD-3)-IZISAE
      IF ((NL.LT.0).OR.(GZISV1().LE.0)) THEN
        IF ( LMONTE ) THEN
          LMONTE = .FALSE.
          CALL EZPICK('CAHITS_RCP')
          CALL EZSET('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
          CALL EZRSET
          CALL ERRMSG('No ISV1 Vertices','CAEHFL_VERTEX',
     &      ' TRYING VERT BANK','W')
        END IF
      ELSE IF(LMONTE.OR.(ICONT(1).EQ.0)) THEN
        IF(.NOT.LMONTE) THEN
          CALL ERRMSG('Using ISV1 Vertices','CAEHFL_VERTEX',
     &      ' UNABLE TO FIND VERH BANK','W')
          LMONTE = .TRUE.
          CALL EZPICK('CAHITS_RCP')
          CALL EZSET('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
          CALL EZRSET
        END IF
        CALL ZVERTX(ZV,DZV)                ! Isajet Vertex (only z)
        NV = 1
        XYZV(3,NV) = ZV
        DXYZV(3,NV)= DZV
        IF (USE_RCP_XY) THEN
          XYZV(1,IVER) = XYBEAM(1)
          XYZV(2,IVER) = XYBEAM(2)
        ELSE
          XYZV(1,IVER) = 0
          XYZV(2,IVER) = 0
          DXYZV(1,IVER)= 0
          DXYZV(2,IVER)= 0
        ENDIF
      END IF
C
      IF (LV0) THEN
        LPROC = GZPROC ()
        LV0 = .FALSE.
        IF( LPROC.LE.0) THEN
          CALL ERRMSG('No PROC','CAEHFL_VERTEX',
     &      'PLV0 missing - try VERT','W')
        ELSE
          LPLV0 = LQ(LPROC-IZPLV0)
          IF(LPLV0.LE.0) THEN
            CALL ERRMSG('No PLV0','CAEHFL_VERTEX',
     &        'PLV0 missing - try VERT','W')
          ELSE
            LV0 = .TRUE.
            NV = 1
            IF (BTEST(IQ(LPLV0+1),5)) THEN  ! GOOD SLOW Z
              XYZV(3,NV) = Q(LPLV0+3)  !slow Z
              DXYZV(3,NV) = 7.0        !Jeff Bantly's guess 10/22/92
            ELSE                       !fast Z
              XYZV(3,NV) = Q(LPLV0+2)
              DXYZV(3,NV) = 15.0
            END IF
          END IF
        END IF
      END IF
C
      IF(NV.EQ.0) THEN
        CALL ERRMSG('No Vertices','CAEHFL_VERTEX','XYZV set to 0','W')
      ENDIF
C
  999 RETURN
      END
