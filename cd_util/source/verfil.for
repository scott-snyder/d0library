      SUBROUTINE VERFIL(NV,VZ,SIGVZ)
C------------------------------------------------------------------
C
C  Fill banks of primary vertices
C
C  Modified Dec. 1989
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR,
C-                                                also added SAVE statement
C-   Updated  21-DEC-1993   Qizhong Li-Demarteau  removed filling HSTR 
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER NV,IV,LVERH,GZVERH,LVERT,NR,IER,ISETVN,ICALL
      REAL VZ(*),SIGVZ(*)
      LOGICAL EZERROR
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','VERFIL',
     &       'Unable to find bank VERTEX_RCP','W')
          GOTO 1000
        ENDIF
        CALL EZGET('NR',NR,IER)
        CALL EZRSET
        ICALL=1
      END IF
      LVERH=GZVERH()
      IF (LVERH.LE.0) CALL BKVERH(LVERH)  ! book veretx header bank
      IQ(LVERH)=ISETVN(IQ(LVERH),0)       ! set version number
      IQ(LVERH+1)=0                       ! also version number
      IF (NV.EQ.0) GO TO 1000
      DO 100 IV=1,NV
        IQ(LVERH+2)=IQ(LVERH+2)+1         ! number of vertices
C
C  Book vertex bank.
C  Reference links point to tracks on vertex. Their number is unknown
C  at this stage, use NR.
C
        CALL BKVERT(LVERT,NR)
        IQ(LVERT)=ISETVN(IQ(LVERT),0)     ! set version number
        IQ(LVERT+1)=0                     ! version number
C
C  Fill Vz and sig(Vz)
C
        IF (IV.EQ.1) THEN
          IQ(LVERT+2)=IBSET(IQ(LVERT+2),31)  ! primary vertex
        ELSE
          IQ(LVERT+2)=IBSET(IQ(LVERT+2),30)  ! additional primary vertex
        END IF
        Q(LVERT+5)=VZ(IV)
        Q(LVERT+8)=SIGVZ(IV)
C
C  Get x,y from a run dependent parameter file and store them in VERT bank
C
        CALL BEAMXY(LVERT)
  100 CONTINUE
 1000 RETURN
      END
