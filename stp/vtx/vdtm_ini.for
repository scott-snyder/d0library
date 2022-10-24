      SUBROUTINE VDTM_INI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hang VDTM banks from VTMW banks.  The VDTM data is
C-                         read from either a binary or ASCII file.
C-
C-   Inputs  : DTM data from file
C-   Outputs : VDTM banks under VTMW
C-   Controls: in VTWSTP_RCP
C-
C-   Created   2-AUG-1992   Peter M. Grudberg
C-   Updated  25-DEC-1992   Ed Oltman  Form linear structure of VDTM banks --
C-                           first is for Luminosity = 0; use IC(LVDTM-5) to
C-                           index luminosity (units of 10E28)
C-
C-   Updated  19-APR-1994   Adam L. Lyon: Fixed to compile on IBM/AIX
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
C
      INTEGER MXITEM, MXWIRE, MAXMAP
      PARAMETER ( MXITEM = 5 )
      PARAMETER ( MXWIRE = 8 )
      PARAMETER ( MAXMAP = 3000 )
      REAL WIRE_POS(0:15), MAP(MAXMAP)
      INTEGER NUM_CATEG(0:2), CATEG(0:31,0:2), CAT
      INTEGER LUN, OUTLUN, IER, LAY, SEC, SECMAX
      INTEGER LVTMW, GZVTMW, NWORDS, NWIR, NSEC, OFFSET, LENGTH
      INTEGER ITEMS, SIZE, POINTERS(4*MXITEM*MXWIRE)
      INTEGER LVDTM, DOT, ILEN, ISETVN
      CHARACTER*7 NAME
      CHARACTER*6 SUFFIX
      CHARACTER*10 CHOPT
      CHARACTER*60 DTM_FILE, OUTFILE,OUTNAME,txt
      LOGICAL ASCII, WRITE, XCHANGE, OK
      INTEGER lum,nlum,INDX,ILUM
C----------------------------------------------------------------------
      CALL EZPICK('VTWSTP_RCP')
      CALL EZGET_iarr('NUM_CATEG',NUM_CATEG,IER)
      CALL EZGET_iarr('CATEG',CATEG,IER)
      CALL EZGET_l('ASCII_DTM', ASCII, IER)
      CALL EZGET_l('WRITE_VDTM', WRITE, IER)
      CALL EZGET_l('XCHANGE', XCHANGE, IER)
      CALL ezget_i('NUM_DTMLUM',nlum,ier)
C
      CALL GTUNIT(666,LUN,IER)
      CALL GTUNIT(666,OUTLUN,IER)
C
      IF ( XCHANGE ) THEN
        SUFFIX = 'X_ZDAT'
      ELSE
        SUFFIX = 'ZDAT'
      ENDIF
C
      DO LAY = 0, 2
C
        LVTMW = GZVTMW(LAY)
        NWORDS = IC(LVTMW + 3)
        NWIR = IC(LVTMW + 4)
        NSEC = IC(LVTMW + 5)
        OFFSET = NWIR*NWORDS*NSEC + 6
C
        SECMAX = 31
        IF ( LAY .EQ. 0 ) SECMAX = 15
        DO SEC = 0, SECMAX              ! Store Category info
          IC(LVTMW+OFFSET+SEC) = CATEG(SEC,LAY)
        ENDDO
C
        DO CAT = 0, NUM_CATEG(LAY) - 1
          WRITE(NAME,'(A,I1,A,I2.2)') 'DTM', LAY, '_', CAT
          INDX = 1
          DO lum = 1,nlum
            CALL EZGETS(NAME, lum, DTM_FILE, LENGTH, IER)
            INDX = INDX + (3+LENGTH)/4
            CALL EZGETA(NAME,INDX,INDX,0,ILUM,IER)
            WRITE(txt,'(a,a,1x,i5)') ' Booking and filling ',
     &        dtm_file(1:length),ilum
            CALL intmsg(txt)
            INDX = INDX + 1
            IF (LUM .EQ. 1) OUTNAME = DTM_FILE
C
            IF ( ASCII ) THEN
              CALL D0OPEN(LUN,DTM_FILE,'IF',OK)
            ELSE
              CALL D0OPEN(LUN,DTM_FILE,'IU',OK)
            ENDIF
            IF ( .NOT. OK ) THEN
              CALL ERRMSG('VTWSTP','VDTM_INI',
     &          'Error opening DTM file','W')
              GO TO 999
            ENDIF
            CALL VDTM_READ(LUN,ASCII,ITEMS,NWIR,SIZE,WIRE_POS,
     &        POINTERS,MAP)
            CLOSE(LUN)
            CALL BKVDTM(LAY,CAT,ilum,NWIR,ITEMS,SIZE,LVDTM)
            IC(LVDTM) = ISETVN(IC(LVDTM),1)
            IC(LVDTM-5) = ILUM
            CALL VDTMFL(LAY,CAT,WIRE_POS,POINTERS,MAP)
          ENDDO
C
C ****  Write VDTM bank to a file if requested -- the whole linear structure
C
          IF ( WRITE ) THEN
            DOT = INDEX(OUTNAME,'.')
            IF ( DOT .GT. 0 ) THEN
              OUTFILE = OUTNAME(1:DOT)//SUFFIX
            ELSE
              OUTFILE = OUTNAME//'.'//SUFFIX
            ENDIF
            IF ( XCHANGE ) THEN
              CALL D0OPEN(OUTLUN,OUTFILE,'OG',OK)
            ELSE
              CALL D0OPEN(OUTLUN,OUTFILE,'OU',OK)
            ENDIF
            CALL XZRECL(ILEN,CHOPT)
            CALL FZFILE(OUTLUN,ILEN,CHOPT)
            LVTMW = GZVTMW(LAY)
            LVDTM = LC(LVTMW-IZVDTM-CAT)
            CALL FZOUT(OUTLUN,IDVSTP,LVDTM,1,'L',0,0)
            CALL FZENDO(OUTLUN,'QTU')
            CLOSE(OUTLUN)
          ENDIF
        ENDDO
      ENDDO
C
      CALL dzsurv(' end of vddtm_ini',ixstp,lvtmh)
C
  999 CALL RLUNIT(666,LUN,IER)
      CALL RLUNIT(666,OUTLUN,IER)
      CALL EZRSET
      RETURN
      END
