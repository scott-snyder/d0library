      FUNCTION GZTACH()
C----------------------------------------------------------------------
C-   PURPOSE AND METHODS : RETURNS THE LINK TO TACH BANK
C-   RETURNED VALUE  : LINK TO TACH,TAKING INTO ACCOUNT :
C-                       - TYPE OF JOB (GEANT OR RECO)
C-                       - TYPE OF DATA (MONTE CARLO OR REAL)
C-                       - DATE OF GEANT JOB
C-   CREATED  12-JUL-1991 16:00:53.04  Alain PLUQUET
C-   Updated  11-MAY-1992              Alain PLUQUET  Remove print
C-   Updated   9-SEP-1992   Robert E. Avery   Mask out bits of HSTR word 22
C-       so that they can be used by FDC and VTX. 
C-       Do this using Herb Greenlee's  BYTE_ORDER.PARAMS.
C-       Also, Do not use time to determine version, since time is very
C-       machine dependent,
C-   Updated   1-OCT-1992   Robert E. Avery   Fix bug in above.
C-       Only check version for MC data, not real data.
C-   Updated  15-OCT-1992   Robert E. Avery   Add RCP parameter to 
C-       allow the user to choose which STP bank version to use.
C-       STP_VERSION = 0  -> Choose default as before
C-       STP_VERSION > 0  -> Use STP_VERSION 
C-   Updated   4-DEC-1992   Robert E. Avery   Also look for vers. in EVENT_HEAD 
C-   Updated  16-APR-1993   Robert E. Avery   Fix bug, LGEAN wasn't being set
C-       correctly. This is ok as long as STP version in HEAD+13 word exists.
C-   Updated  22-SEP-1993   J.P. Cussonneau  New version for  M.C. TRD is 
C-                                           implemented
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTACH
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTACH.LINK'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$LINKS:IZTDMC.LINK'
      INCLUDE 'D0$LINKS:IZTDDA.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER LINK,VERSION,DEFAULT
      INTEGER GZTGEO,LZFIND,LGEAN,LHSTR,ND
      INTEGER GZRECO,GZHSTR
      INTEGER IER
      INTEGER LRCP
      CHARACTER*4 PATH
      LOGICAL MCDATA
      INTEGER IVERSION
      INTEGER MBYTET
      BYTE BVERSION(4)
      EQUIVALENCE(BVERSION,IVERSION)
      LOGICAL FIRST
      SAVE    FIRST,VERSION
C
      DATA    FIRST /.TRUE./
C-------------------------------------------------------------------------------
C     INITIALIZATION, SEARCH FOR TGEO
C-------------------------------------------------------------------------------
      GZTACH=0
      LTGEO=GZTGEO()
      IF (LTGEO.LE.0) THEN
        CALL ERRMSG('TRD','GZTACH','TGEO BANK DOES NOT EXIST','W')
        GOTO 999
      ENDIF
      LINK=0
      DEFAULT=LC(LTGEO-IZTACH)  
C-------------------------------------------------------------------------------
C     IF MC RECO JOB THEN SEARCH FOR THE VALID TACH BANK
C     FIND LINK TO HSTR UNDER GEAN UNDER BEGIN OF RUN HEADER 
C-------------------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF ( LHEAD.GT.0 ) THEN
        MCDATA =  IQ(LHEAD+1) .GT. 1000
      ELSE
        MCDATA =  .FALSE.
      ENDIF
      IF ( MCDATA .AND. (PATH.EQ.'RECO') ) THEN
        IF ( FIRST ) THEN
          FIRST = .FALSE.
          CALL EZLOC('TRD_RCP',LRCP)
          IF (LRCP.GT.0) THEN
            CALL EZPICK('TRD_RCP')
            CALL EZGET('STP_VERSION',VERSION,IER)
            CALL EZRSET
          ENDIF 
C
          IF ( VERSION.LE.0 ) THEN
C
C  First try word in Geant HSTR bank.
C
            LHSTR=0
            IF (LHEADR.GT.0) THEN
              LGEAN=LQ(LHEADR-IZGEAN)            
              IF (LGEAN.GT.0) THEN
                LHSTR=LQ(LGEAN-IZHSTR)
              ENDIF
            ENDIF
            IF (LHSTR.GT.0) THEN
              ND=IQ(LHSTR-1)        ! CHECK IF HSTR CONTAINS VERSION INFORMATION
              IF (ND.GE.22) THEN             
                IVERSION = IQ(LHSTR+21)         ! READ VERSION NUMBER
                VERSION = MBYTET(3,BVERSION(BYTE1),1,8) ! TRD is in first byte
                IF ( VERSION.NE.2 ) VERSION=1
              ELSE
                VERSION=1
              ENDIF
            ELSE
C
C  Else use word in EVENT_HEAD bank.
C
              IVERSION = IQ(LHEAD+13)         ! READ VERSION NUMBER
              VERSION = MBYTET(3,BVERSION(BYTE1),1,8) ! TRD is in first byte
              IF ( VERSION.NE.2 ) VERSION=1
            ENDIF
          ENDIF
        ENDIF
C
        LINK=LZFIND(IDVSTP,DEFAULT,VERSION,1)   ! FIND THE LINK FOR THIS VERSION
      ENDIF
C-------------------------------------------------------------------------------
C     ASSIGNMENT OF GZTACH   
C-------------------------------------------------------------------------------
      IF (LINK.GT.0) THEN
        GZTACH=LINK                               
C       WRITE (*,'(1X,A20,I3)') 'TRD STP FILE VERSION',IC(LINK+1)
      ELSE
        GZTACH=DEFAULT
C       WRITE (*,'(1X,A20,I3)') 'TRD STP FILE VERSION',IC(DEFAULT+1)
      ENDIF
  999 CONTINUE
      END
