C DEC/CMS REPLACEMENT HISTORY, Element CSFMAKE.FOR
C *1    15-APR-1992 17:29:46 STEWART "program to generate CSF STPFILE"
C DEC/CMS REPLACEMENT HISTORY, Element CSFMAKE.FOR
      PROGRAM CSFMAKE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the file CSF_STPFILE.DAT which contains
C-   the conversion factor from calorimeter cell ADC counts to total
C-   energy for every cell in the CC, ECN, ECS, ICD and Massless Gaps.
C-   See D0$STP$CAL:CSFMAKE.DOC for more detail.
C-
C-   Created  28-FEB-1992   Harrison B. Prosper, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER STATUS,LUN,LFILE, LCSFH, GZCSFH,GZCSFW,GZCSFC
      INTEGER IETA,IPHI,ILYR,NPHIC,IMOD,LMOD,ICSFC,ICSFW,LCSFC
      INTEGER CAL_MODULE,LCSFC1,LZFIND
      LOGICAL OK,CEXIST,HEXIST
      REAL AWC
C
      CHARACTER*(*) RCP_FILE
      PARAMETER( RCP_FILE = 'CSF_RCP' )
      CHARACTER*(*) STP_FILE
      PARAMETER( STP_FILE = 'CSF_STPFILE' )
      CHARACTER*80 REMARK,FILENAME,TITLE
      CHARACTER MODULE*4
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL INZBRA
      CALL INZSTP
      CALL INPAWC
C
C ****  Build the CSF Structure (Calorimeter Sampling Fraction banks)
C ****  READ RCP_FILE
C
      CALL TRNLNM(RCP_FILE,FILENAME,LFILE)
      REMARK = ' Reading file '//FILENAME(1:LFILE)
      CALL INTMSG(REMARK)
      CALL INRCP(RCP_FILE,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        REMARK = ' Cannot open file '//RCP_FILE
        CALL ERRMSG('BADOPEN','INRCP',REMARK,'F')
      END IF
C
      CALL CSFBUILD(RCP_FILE,STATUS)
C
      IF(STATUS.EQ.0) THEN
        CALL TRNLNM(STP_FILE,FILENAME,LFILE)
        REMARK = ' Writing file '//FILENAME(1:LFILE)
        CALL INTMSG(REMARK)
        CALL GTUNIT(34,LUN,OK)
        CALL ZZOPEN (LUN,STP_FILE,STATUS,'OUTPUT')
        LCSFH = GZCSFH ()
        CALL FZOUT  (LUN,IDVSTP,LCSFH,1,' ',1,0,0)
        CALL ZZCLOS (LUN,STATUS,'OUTPUT')
      ELSE
        REMARK = ' Cannot build CSF Structure '//RCP_FILE
        CALL ERRMSG('BAD CSFBUILD','CSFMAKE',REMARK,'F')
      ENDIF
C
C ****  HISTOGRAMS
C
      NPHIC = NPHIL
      LCSFC = GZCSFC()
      IF(LCSFC.EQ.0) NPHIC = 1
      DO ILYR = 1, NLYRL
        LCSFC1 = LZFIND(IDVSTP,LCSFC,ILYR,2)
        DO IETA = -NETAL, NETAL
          IMOD = CAL_MODULE(IETA,ILYR,MODULE) 
          DO IPHI = 1, NPHIC
            IF(CEXIST(IETA,IPHI,ILYR)) THEN
              ICSFC = IPHI+(IETA+NETAL)*NPHIL  ! 1 to 4800
              ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
              IF(LCSFC1.GT.0) THEN
                AWC = C(LCSFC1+2+ICSFC)*C(GZCSFW()+1+ICSFW)
              ELSE
                AWC = C(GZCSFW()+1+ICSFW)
              END IF
              IF(IMOD.NE.LMOD) THEN
                LMOD = IMOD 
                WRITE(TITLE,100)MODULE
  100           FORMAT(' A*W*C ',A5)
                IF(.NOT.HEXIST(IMOD))THEN
                  CALL HBOOK1(IMOD,TITLE,50,0,AWC*5.,0)
                  LCSFC = GZCSFC()
                  LCSFC1 = LZFIND(IDVSTP,LCSFC,ILYR,2)
                END IF
              END IF
              CALL HF1(IMOD,AWC,1.)
            END IF
          END DO
        END DO
      END DO
      CALL HPRINT(0)
C
      END
