      SUBROUTINE DCDEDX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  :TRACKBANK AND IONISATION IN [MIP] FOR WIRES IN DSEC BANK 
C-   Outputs : DEDX AND ERROR INTO TRACK BANK
C-   Controls: PARAMETER FAC = FRACTION FOR TRUNCATED MEAN
C-
C-   Created  19-JUL-1988   Rod Engelmann
C-   MODIFIED 17-NOV-1989        "
C-   Updated  16-JAN-1991   Qizhong Li-Demarteau  added protection for 
C-                                                LDTRH
C-   Updated  19-MAR-1991   Qizhong Li-Demarteau  added a choice to handle
C-                                                MC data differently 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  23-OCT-1991   Qizhong Li-Demarteau  remove set bit in status 
C-                                                word for no theta tracks
C-   Updated  18-DEC-1992   Qizhong Li-Demarteau  add a normalization factor 
C-                                                for MC data 
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'

      INTEGER NBTWIR
      PARAMETER ( NBTWIR=NBSENS*4)
      INTEGER IND,NCUT,J 
      INTEGER LAYER,SECTOR,WIRE,IHIT 
      INTEGER PTDSEC(0:6),NBDSEC(0:6),NBWIR
      INTEGER LHDSEC,IPDSEC
      INTEGER LABEL,LABPTR
      INTEGER NMWIRS,LLDTRK,LLDTTH,I, IERR
      INTEGER GZDTRH
      REAL SINLAM,ION(NBTWIR),TRUNCT,DEDX,ERDEDX, TRUNMC
      REAL    NORMAL
      INTEGER INDEX(NBTWIR)
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DCDEDX',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('TRUNCT',TRUNCT,IERR)
        CALL EZGET('TRUNMC',TRUNMC,IERR)
        CALL EZGET('NORMAL_FACTOR',NORMAL,IERR)
        CALL EZRSET
        IF (IQ(LHEAD+1) .GT. 1000) TRUNCT = TRUNMC
        FIRST = .FALSE.
      ENDIF
C
      IF (LDTRH .LE. 0) THEN
        LDTRH = GZDTRH()
        IF (LDTRH .LE. 0) RETURN
      ENDIF
C
C     GO THROUGH TRACKBANKS AND HISTO DEDX 
C
      LLDTRK = LQ(LDTRH-1)        !POINTER TO 'DTRK' BANK
      IF(LLDTRK.EQ.0) GO TO 199   ! NO TRACKS
  110 LLDTTH=LQ(LLDTRK-1)         !POINTER TO 'DTTH' BANK
      NMWIRS=IQ(LLDTRK+2)         !# of sen. wires in full track
      SINLAM=SIN(Q(LLDTRK+9))     !THETA ANGLE CORRECTION OF PULSE HEIGHT       
C     IF NO R-Z TRACK IGNORE THETA AND FLAG TRACK STATUS  
      IF (SINLAM.EQ.0.) THEN
        SINLAM = 1. 
      ENDIF  
C
      CALL VZERO(ION(1),NBTWIR)
 
      DO 101 I=1,NMWIRS         !LOOP OVER PARTICIPATING SENSEWIRES        
      LABPTR=LLDTTH+(I-1)*2+1         !Pointer to wire label
      LABEL=IQ (LABPTR)               
      LAYER = IBITS(LABEL,16,2)
      SECTOR = IBITS (LABEL,11,5)
      WIRE = IBITS (LABEL,8,3)
      IHIT = IBITS (LABEL,1,7)

      CALL ZGDSEC (LAYER,SECTOR,NBWIR,PTDSEC,NBDSEC,LHDSEC)
      IF ( NBDSEC(WIRE).EQ.0 ) GO TO 101
      IPDSEC = PTDSEC(WIRE) + (IHIT-1)*LHDSEC   ! POINTER TO HIT IN DSEC
      ION(I) = Q( IPDSEC + 7 )          !ION IN [MIP]

  101 CONTINUE            

C     SORT VECTOR 'ION' FOR TRUNCATED MEAN
      CALL SORTZV(ION,INDEX,NMWIRS,1,0,0)

C     GET TRUNCT% TRUNCATED MEAN
      NCUT=TRUNCT*NMWIRS

      DEDX=0.
      ERDEDX=0.
      DO 103 I=1,NCUT
      J=INDEX(I)
      DEDX=DEDX+ION(J)
103   ERDEDX=ERDEDX+ION(J)**2
C     CORRECT FOR THETA OF TRACK
      DEDX=DEDX*SINLAM     
      ERDEDX=ERDEDX*SINLAM**2
      DEDX=DEDX/NCUT
      ERDEDX=ERDEDX/NCUT-DEDX**2
      IF(ERDEDX.GT.0.)ERDEDX=SQRT(ERDEDX)
C
      IF (IQ(LHEAD+1) .GT. 1000) THEN
        DEDX = DEDX / NORMAL
        ERDEDX = ERDEDX / NORMAL
      ENDIF
C     STORE IN DTRK
      Q(LLDTRK+20)=DEDX
      Q(LLDTRK+21)=ERDEDX
      IF (LQ (LLDTRK).EQ.0) GOTO 199    ! NO MORE TRACK
      LLDTRK=LQ (LLDTRK)                !Pointer to next bank 'DTRK' 
      GO TO 110
  199 CONTINUE
C
  999 RETURN

      END
