      SUBROUTINE ZTRKFT(LZTRK1,FITVTX,ERRVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make global fit for a central track ZTRK
C-
C-   Inputs  : LZTRK1: the ZTRK's bank address
C-             FITVTX : array containing x, y and z of the vertex
C-             ERRVTX : array containing errors on x, y and z of the vertex
C-                      If ERRVTX(1) or ERRVTX(2) > 10.0, the fit will
C-                      without vertex
C-   Outputs : ZFIT bank is created and filled
C-
C-   Created  02-MAR-1990   Qizhong Li-Demarteau
C-   Updated   2-SEP-1991   Susan K. Blessing  Make the size of the HITx
C-    and Wx arrays a parameter equal to 85 (up from 65) to allow for
C-    fitting of VTX-CDC-FDC tracks.  Change IF-THEN chain to allow 
C-    fitting of CDC-FDC tracks (previously, if there was a CDC-FDC track,
C-    only the CDC part was used in the fit) and make clearer.
C-    Change IQ(LZFIT+5) to FNHIT from IQ(LFTRK+2) since the delay lines
C-    are no longer being included in the ZTRK fit.  (Change in FTRKHT
C-    to not use the delay line information since track information is 
C-    3 dimensional.)
C-   Updated  20-SEP-1991   Qizhong Li-Demarteau  added dE/dx  
C-   Updated  29-OCT-1991   Susan K. Blessing  Declare IBSET. 
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  removed IBSET declaration
C-   Updated  11-MAR-1992   Susan K. Blessing  If ZTRK is an FDC-only track,
C-    load information from FDCT bank into ZFIT bank without redoing fit.
C-   Updated   8-JUN-1992   Qizhong Li-Demarteau  added two more words 
C-                                                for VTX's dE/dx
C-   Updated  23-AUG-1992   Qizhong Li-Demarteau  added two more words for
C-                   impact parameter in X-Y plan and distance to vertex_z
C-   Updated  22-APR-1993   Susan K. Blessing  Replace USE_VERTEX in 
C-    FITFDC_VERTEX with ZVERT.
C-   Updated  17-JUN-1993   Qizhong Li-Demarteau  added check on THETA
C-                                       to determine if VTX joins ZFIT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZFTLNK.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
C
      INTEGER NHIT
      PARAMETER (NHIT = 85)
      INTEGER ZNHIT, VNHIT, CNHIT, FNHIT, LZTRK1
      INTEGER IER
      INTEGER CNWIRE,VNWIRE
      PARAMETER(CNWIRE = 28)
      PARAMETER(VNWIRE = 24)
C
      REAL    FITVTX(3), ERRVTX(3), ERRORS
      REAL    HITX(NHIT), HITY(NHIT), HITZ(NHIT), WR(NHIT),  WZ(NHIT)
      REAL    TANPHI, TANTHE, XVERT, YVERT, ZVERT
      REAL    THETA, THETAV
C
      LOGICAL USE_VERTEX,FDC_ONLY
      LOGICAL OK
C
C----------------------------------------------------------------------
C
      IF (LZTRK1 .LE. 0) RETURN
      LZFIT = LQ(LZTRK1 - IZZFIT)
      IF (LZFIT .GT. 0) RETURN          ! ZFIT is already done
C
C ****  Initialize a temporary LINK area
C
      IF (ZFTLNK(1) .EQ. 0)
     &  CALL MZLINT(IXCOM,'/ZFTLNK/',ZFTLNK,LVTXT,LFTRK)
C
      LZTRK = LZTRK1
      LVTXT = LQ(LZTRK - 6)
      LDTRK = LQ(LZTRK - 7)
      LFTRK = LQ(LZTRK - 8)
      LZFIT = 0
      VNHIT = 0
      CNHIT = 0
      FNHIT = 0
      FDC_ONLY = .FALSE.
      CALL VZERO(HITX(1),NHIT)
      CALL VZERO(HITY(1),NHIT)
      CALL VZERO(HITZ(1),NHIT)
      CALL VZERO(WR(1),NHIT)
      CALL VZERO(WZ(1),NHIT)
C
C  If ERRVTX(1) and ERRVTX(2) is less than 10.0, the primary vertex will
C  be included in the fit too.
C
      USE_VERTEX = .FALSE.
      IF (ERRVTX(1) .LE. 10.0 .AND. ERRVTX(2) .LE. 10.0) THEN
        HITX(1) = FITVTX(1)
        HITY(1) = FITVTX(2)
        HITZ(1) = FITVTX(3)
        ERRORS = ERRVTX(1)**2 + ERRVTX(2)**2
        USE_VERTEX = .TRUE.
        IF (ERRORS .EQ. 0) THEN
          WR(1) = 10000.0
        ELSE
          WR(1) = 1.0 / ERRORS
        ENDIF
        IF (ERRVTX(3) .GT. 0 .AND. ERRVTX(3) .LT. 9999.0)
     &    WZ(1) = 1.0 / ERRVTX(3)**2
      ENDIF
C
      OK = .TRUE.
      THETA = 0.0
      IF (LDTRK .GT. 0) THEN
        THETA = Q(LDTRK+9)
      ELSE
        IF (LFTRK .GT. 0) THETA = Q(LFTRK+22)
      ENDIF
      IF (THETA .GT. 0.0 .AND. LVTXT .GT. 0) THEN
        THETAV = Q(LVTXT+9)
        CALL ZCHECK_VTXT(THETA,THETAV,OK)
        IF (.NOT. OK) GOTO 111
      ENDIF
C
C if VTX track contributed to this ZTRK, get hits information from VTXT
C
      IF (LVTXT .GT. 0) THEN
        CALL VTXTHT(LVTXT,VNHIT,HITX(2),HITY(2),HITZ(2),
     &              WR(2),WZ(2))
      ENDIF
C
C if CDC track contributed to this ZTRK, get hits information from DTRK.
C
  111 IF (LDTRK .GT. 0) THEN
        CALL DTRKHT(LDTRK,CNHIT,HITX(VNWIRE+2),HITY(VNWIRE+2),
     &              HITZ(VNWIRE+2),WR(VNWIRE+2),WZ(VNWIRE+2))
      END IF
C
C if FDC track contributed to this ZTRK, get hits information from FDCT
C
      IF (LFTRK .GT. 0) THEN
C If this is an FDC-only track, don't get hit information.
        IF (CNHIT.GT.0.OR.VNHIT.GT.0) THEN
          CALL FTRKHT(LFTRK,FNHIT,HITX(VNWIRE+CNWIRE+2),
     &              HITY(VNWIRE+CNWIRE+2),HITZ(VNWIRE+CNWIRE+2),
     &              WR(VNWIRE+CNWIRE+2),WZ(VNWIRE+CNWIRE+2))
        ELSE
          FDC_ONLY = .TRUE.
        END IF
      END IF
C
      ZNHIT = VNHIT + CNHIT + FNHIT
      IF (ZNHIT .GT. 0 .OR. FDC_ONLY ) THEN
        CALL BKZFIT(LZTRK,LZFIT)
        IF (FDC_ONLY) THEN
          IF (USE_VERTEX) THEN
            ZVERT = FITVTX(3)
          ELSE
            ZVERT = -9999.
          END IF
          CALL ZFLZFT_FDC(LZFIT,LFTRK,ZVERT,FNHIT)
        ELSE
          CALL ZFLZFT(LZFIT,HITX,HITY,HITZ,WR,WZ)
        END IF
      ENDIF
C
C   fill the rest of information for bank ZFIT
C
      IF (LZFIT .GT. 0) THEN
        IQ(LZFIT - 5) = IQ(LZTRK - 5)
        IQ(LZFIT + 1) = 0
        IF (WR(1) .NE. 0.0) IQ(LZFIT+2) = IBSET(IQ(LZFIT+2),0)
        IF (WZ(1) .NE. 0.0) IQ(LZFIT+2) = IBSET(IQ(LZFIT+2),1)
        IF (.NOT.OK .AND. LVTXT .GT. 0) THEN
          IF (LZTRK .GT. 0) IQ(LZTRK) = IBSET(IQ(LZTRK),0)
          IQ(LZFIT+2) = IBSET(IQ(LZFIT+2),2)
        ENDIF
        IF (VNHIT .GT. 0) THEN
          CALL MVBITS(IQ(LVTXT+2),0,16,IQ(LZFIT+3),0)
          CALL MVBITS(IQ(LVTXT+5),0,16,IQ(LZFIT+3),16)
          Q(LZFIT + 30) = Q(LVTXT + 20)  ! temporarily
          Q(LZFIT + 31) = Q(LVTXT + 21)  ! temporarily
        ENDIF
        IF (FNHIT .GT. 0) THEN
          IQ(LZFIT + 5) = FNHIT
          Q(LZFIT + 26) = Q(LFTRK + 20)  ! temporarily
          Q(LZFIT + 27) = Q(LFTRK + 21)  ! temporarily
        END IF
        IF (CNHIT .GT. 0) THEN
          CALL MVBITS(IQ(LDTRK+2),0,16,IQ(LZFIT+4),0)
          CALL MVBITS(IQ(LDTRK+5),0,16,IQ(LZFIT+4),16)
          Q(LZFIT + 26) = Q(LDTRK + 20)  ! temporarily
          Q(LZFIT + 27) = Q(LDTRK + 21)  ! temporarily
        ENDIF
      ENDIF
C
C  calculate the impact parameter in X-Y plane and 
C  calculate the distance between Vertex_Z and the track along Z axis
C
      XVERT = FITVTX(1)
      YVERT = FITVTX(2)
      ZVERT = FITVTX(3)
      TANPHI = TAN(Q(LZFIT + 10))
      TANTHE = TAN(Q(LZFIT + 13))
      Q(LZFIT + 32) = ABS(XVERT * TANPHI - YVERT + Q(LZFIT + 12) -
     &  Q(LZFIT + 11) * TANPHI) / SQRT(TANPHI**2 + 1)
      IF (TANTHE .NE. 0.0) THEN
        Q(LZFIT + 33) = Q(LZFIT + 15) - (Q(LZFIT + 14) / TANTHE) - ZVERT
      ELSE
        Q(LZFIT + 33) = 999.9
      ENDIF
C
      ZFTLNK(1) = 0
C
  999 RETURN
      END
