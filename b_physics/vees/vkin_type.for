      SUBROUTINE VKIN_TYPE(KEY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : KEY = 1 - calculate number of degrees of freedom NR
C-                 = 3 - write title for successful fit
C-                 = 4 - write title for failed fit
C-   Outputs : NTR - number of tracks
C-             NM  - number of   measured parameters
C-             NX  - number of unmeasured parameters
C-             NR  - number of degrees of freedom
C-   Controls: 
C-
C-   Created  12-mar-1991   V. Burtovoy
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      LOGICAL FIRST
      CHARACTER*8 SHARP(8),XCOD(7)
      INTEGER KEY,NX,NM,I,II,K,NR,PRUNIT,USUNIT,IPRINT,IER
      SAVE FIRST,PRUNIT
      DATA SHARP / 'NUMB','MOM','THETA','PHI','MSS','DMOM',
     +             'DTHE','DPHI'/
      DATA XCOD / 'PLFM','PLFX','XLFM','XFLX','XXXX','XXXM','PXXM'/
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        PRUNIT=USUNIT()
        CALL EZPICK('VEES_RCP')
        CALL EZGET('IPRINT',IPRINT,IER)
        CALL EZRSET
      END IF
C
      IF(KEY.EQ.1.AND.IPRINT.GT.0) WRITE(PRUNIT,2001)
 2001 FORMAT(///29X,'START VALUES OF TRACK PARAMETERS:')
      IF(KEY.EQ.3.AND.IPRINT.GT.0) WRITE(PRUNIT,2003)
 2003 FORMAT(/30X,'FINAL VALUES OF TRACK PARAMETERS:')
      IF(KEY.EQ.4.AND.IPRINT.GT.0) WRITE(PRUNIT,2010)
 2010 FORMAT(/30X,'VALUES FOUND AT LAST STEP:==FAILED==')
      IF(IPRINT.GT.0) WRITE(PRUNIT,2004)
 2004 FORMAT(1X,78(1H-))
      IF(IPRINT.GT.0) WRITE(PRUNIT,2005) SHARP
 2005 FORMAT(1X,8(A5,5X))
      IF(IPRINT.GT.0) WRITE(PRUNIT,2004)
      NX=0
      NM=0
      DO 1 I=1,NTR
      II=IND(I)
      IF(IPRINT.GT.0) 
     + WRITE(PRUNIT,2006) I,(STR(K,I),K=1,4),(ETR(K,I),K=1,3)
 2006 FORMAT(2X,I1,3F11.4,F12.4,3F9.4)
      GO TO (11,12,13,14,15,16,17),II
   11 NM=NM+3
      GO TO 1
   12 NM=NM+3
      NX=NX+1
      GO TO 1
   13 NM=NM+2
      NX=NX+1
      GO TO 1
   14 NM=NM+2
      NX=NX+2
      GO TO 1
   15 NX=NX+4
      GO TO 1
   16 NX=NX+3
      GO TO 1
   17 NM=NM+1
      NX=NX+2
    1 CONTINUE
      IF(IPRINT.GT.0) WRITE(PRUNIT,2004)
      IF(KEY.NE.1) RETURN
      NR=4-NX
      IF(IPRINT.GT.0) WRITE(PRUNIT,2009) NTR,NM,NX,NR
 2009 FORMAT(/40X,'ENTRY TO NCFIT PROGRAM:'/
     +30X,'  NUMB. OF TRACKS              =',I2/
     +30X,'  NUMB. OF MEASURED PARAMETERS =',I2/
     +30X,'  NUMB. OF UNMEASUR.PARAMETERS =',I2/
     +30X,'           ',I2,'C-FIT'/)
      RETURN
      END
