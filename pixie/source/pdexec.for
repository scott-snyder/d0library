      FUNCTION PDEXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         CDCDIS.
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-SEP-1990   PXBUILD V1.00
C-   Updated  15-JAN-1993   S. Hagopian - Check if the number of CDC 
C-       hits was greater that CDC MAX HITS (interactive parameter).
C-   Updated  27-JAN-1993   N. Oshima - Fix the 'CDC DRAW HITS' problem.   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PDEXEC
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER RUNNO,EVONUM
      INTEGER  LRUNNO,LEVONUM,CRUNNO,CEVONUM
      INTEGER KCDCH,NCDCHIT,MAXCHITS,GZCDCH
      INTEGER LDHIT,GZDHIT
      INTEGER IFDHIT,IFDHITSAV
      SAVE IFDHITSAV,LRUNNO,LEVONUM
      CHARACTER*30 HEADER
      CHARACTER*24 WARN
      CHARACTER*60 IMESS
C
      SAVE WARN
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'CDC System Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'CDCDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
C DATA STATEMENTS
      DATA LRUNNO,LEVONUM/-9,-9/
      DATA WARN/'- CDC - TOO MANY HITS'/
C----------------------------------------------------------------------
C Get current run,event number
      CRUNNO=RUNNO()
      CEVONUM=EVONUM()
C Check number of hits in CDC
      NCDCHIT=0
      KCDCH=GZCDCH(0)
C check for raw hits bank
      IF(KCDCH.GT.0)THEN
        NCDCHIT=IQ(KCDCH+1)
      ENDIF
C Check for compressed hits bank, If there are no raw hits...
      IF(NCDCHIT.LE.0)THEN
        LDHIT=GZDHIT()
        IF(LDHIT.GT.0)THEN
           NCDCHIT=IQ(LDHIT+2)
        ENDIF
      ENDIF
C get interactive parameters
      CALL EZPICK('PX_CDCDIS_RCP')
      CALL PUGETV('CDC MAX HITS',MAXCHITS)
      CALL PUGETV('CDC DRAW HITS', IFDHIT )
C Save current value of IFDHIT
      IFDHITSAV=IFDHIT
      IF(NCDCHIT.GT.MAXCHITS.AND.IFDHIT.GT.0)THEN
        IF(CRUNNO.NE.LRUNNO.OR.CEVONUM.NE.LEVONUM)THEN
          WRITE( HEADER, 1000 ) CRUNNO, CEVONUM
 1000 FORMAT(' Run',I8,' Event',I8)
          IMESS=HEADER//WARN
          CALL INTMSG(IMESS)
          LRUNNO=CRUNNO
          LEVONUM=CEVONUM
        ENDIF
C SET IFDHIT .EQ. 0 (DO NOT DRAW HITS)
        CALL PUSETV('CDC DRAW HITS',0)
      ENDIF  
      CALL EZRSET
   50 PDEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'CDC_X-Y SEGMENTS' ) THEN
          CALL PDRPHI
        ELSEIF ( COMMAND .EQ. 'CDC_1LAY_FADC' ) THEN
          CALL PDFSEC
        ELSEIF ( COMMAND .EQ. 'CDC_ONE_FADC' ) THEN
          CALL PDFADC
        ELSEIF ( COMMAND .EQ. 'CDC_3SEC SEGMENTS' ) THEN
          CALL PD3SEC
        ELSEIF ( COMMAND .EQ. 'CDC_3SEC TRACKS' ) THEN
          CALL PD3TRK
        ELSEIF ( COMMAND .EQ. 'CDC_X-Y VIEW' ) THEN
          CALL PDXYVW
        ELSEIF ( COMMAND .EQ. 'CDC_R-Z VIEW' ) THEN
          CALL PDRZVW
        ELSEIF ( COMMAND .EQ. 'CDC_XY_TRACKS' ) THEN
          CALL PDXYTK
        ELSEIF ( COMMAND .EQ. 'CDC_DRAW_PHI_ROAD' ) THEN
          CALL PDPHI_ROAD
        ELSEIF ( COMMAND .EQ. 'CDC_DRAW_THETA_ROAD' ) THEN
          CALL PDTHETA_ROAD
        ELSEIF ( COMMAND .EQ. 'CDC_3D VIEW' ) THEN
          CALL PDCDC3D
        ENDIF
      ENDDO
C
      CALL PUSETV('CDC DRAW HITS',IFDHITSAV)
  999 RETURN
      END
