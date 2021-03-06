      FUNCTION L2_FTRINI()
C---------------------------------------------------------------------
C
C  Initialization routine for FTRAKS package.
C  Read control parameters.
C
C-   Created  xx-MAY-1988   Daria Zieminska,    modified June 1989.
C-   Updated  28-FEB-1990   Jeffrey Bantly  add permanent link area 
C-   Updated   6-AUG-1991   Susan K. Blessing  Add flag for initializtion
C-    of FTRAKS.RCP
C-   Updated  16-AUG-1991   Robert E. Avery  setup ALL permanent link areas. 
C-   Updated  10-DEC-1991   Robert E. Avery  Initialize Geometry here. 
C-   Updated   4-MAR-1992   Robert E. Avery   Check existance of ftrak_rcp
C-              bank before calling INRCP.
C-   Updated  12-MAY-1992   Yi-Cheng Liu   for initializing FDC in L2
C                           ( also renamed from FTRINI to L2_FTRINI )
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER,LRCP
C
      CHARACTER*(*) RCPFIL 
      PARAMETER( RCPFIL = 'FTRAKS_RCP' )   ! Logical name of control file
C
      LOGICAL L2_FTRINI
      LOGICAL OK, FIRST,FGEOM_INIT
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      L2_FTRINI = .TRUE.
      IF (.NOT.FIRST) RETURN
      FIRST = .FALSE.
C
      CALL FDPLNK               ! Create permanent link area for hit banks
      CALL FPLTRK               ! Create permanent link area for track banks
      CALL FSPLNK               ! Create permanent link area for FDC STP 
C
      CALL EZLOC(RCPFIL,LRCP)                        
      IF(LRCP.LE.0) THEN                                   
        CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
        IF (IER .NE. 0) THEN
          CALL ERRMSG('FTRAKS RCP BAD','L2_FTRINI',
     &    'FTRAKS RCP had a bad read','W')
          L2_FTRINI = .FALSE.
          GO TO 999
        END IF
      END IF
C
      CALL FLGSET('FTRAKS_RCP',.TRUE.)
      CALL FLGERR(IER) 
      IF ( IER.NE.0 ) THEN
        CALL FLGBK('FTRAKS_RCP',1)
        CALL FLGSET('FTRAKS_RCP',.TRUE.)
      ENDIF
C
C  Get list of banks to dump
C
      CALL FTRDDF      
C
C  Get list of banks to drop
C
      CALL FTRDRP(IER) 
C
C  Initialize Geometry (read STP file, etc.)
C
      OK = FGEOM_INIT()
C
      L2_FTRINI = OK 
C
  999 RETURN
      END
