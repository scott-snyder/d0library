      SUBROUTINE MUHITS(NMODT,JMODT,JERR)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Find hits in selected modules and store
C-                         in MUOH bank
C-
C-    Input  :  NMODT  - Number of modules in list
C-              JMODT(200)- Array of modules to search
C-
C-    Output :  JERR   - Error flag
C-
C-    Created :  2-SEP-93  M. Fortner
C-    Updated :  2-FEB-94  A.T. for scintillator
C-               5/95  MF Improved diagnostic errors
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMODT,JMODT(200),JERR
C
      INTEGER IERR,I,J,K,KK,IHIT,JHIT
      INTEGER NRAW,NCEL,IMUD1,LAT,IADC(8),IFLG(3,2)
      INTEGER NMODX,NPLNX,NWIRX,NMOD,NPLN,NWIR
      INTEGER IADD,NTIM,IORN,IMUH,ICEL,IPMT(2)
      INTEGER D2,D3,D4,D5
      REAL D6(2),D7(3),D8(3)
      INTEGER MAXHIT,MAXSCI,ISTA,ISEC,MAXSAM(6),KCEL,NSCT
      INTEGER NSCLUP,NSCEL,ISLAT,NPMT,ISTAT
      REAL DIST(2),XYZ(3),DXYZ(3),DIM(3),TIME(2)
      REAL HIT(6,2),POS(12),HITSAM(4)
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      DATA MAXSAM/63,95,63,63,95,63/
      DATA CALLER/'MUHITS'/
C-------------------------------------------------------------------
C       Initialize utilities
C-------------------------------------------------------------------
      JERR = 0
      CALL MUDMOD(0,NRAW,JHIT,IMUD1)
      CALL MUDHIT(0,JHIT,NCEL,LAT,IADC)
      CALL SADHIT(0,JHIT,NCEL,LAT,IADC)
      CALL MUHMOD(0,NRAW,JHIT)
C-------------------------------------------------------------------
C       Loop over modules and test for valid module
C-------------------------------------------------------------------
      DO 30 I=1,NMODT
        NMOD = JMODT(I)
        CALL MUMDAT(NMOD,NMODX,NPLNX,NWIRX)
        IF (NMODX.EQ.-1) THEN
          JERR = 1
          MESSID = 'MUHITS: illegal module number'
          WRITE(MESSAG,115) NMOD
  115     FORMAT('Module = ',I3,' ')
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          GOTO 30
        ENDIF
        CALL MUDMOD(NMOD,NRAW,JHIT,IMUD1)
        IF (NRAW.EQ.0) GOTO 30
C-------------------------------------------------------------------
C       Extend banks before adding hits
C-------------------------------------------------------------------
        IF (NMOD.LT.400) THEN
          MAXHIT = NWIRX*4 - 1
          CALL MNMDAT(NMOD,D2,MAXSCI,NSCT)
          IF (MAXSCI.GT.0) THEN
            NSCLUP = 3 - MAXSCI/NSCT
            MAXSCI = MAXSCI*4 - 1
            CALL MSCTFL(98,NMOD,D2,D3,D4,D5,D6,D7,D8)
          ENDIF
          IF (NMOD.LT.0) GOTO 30
          CALL MUOHFL(98,NMOD,D2,HIT)
        ELSE
          ISTA = NWIRX
          ISEC = NPLNX
          MAXHIT = MAXSAM(ISEC)
          CALL SAPHFL(98,NMOD,D2,D3,D4,HITSAM)
          IF (NMOD.LT.0) GOTO 30
        ENDIF
        CALL MUHTFL(0,NMOD,D2)
        KCEL = -1
C-------------------------------------------------------------------
C       Loop over raw hits and test for bad zebra pointer
C-------------------------------------------------------------------
        DO 20 J=1,NRAW
          IF (JHIT.EQ.0) THEN
            JERR = 3
            MESSID = 'MUHITS: Unexpected end of data'
            WRITE(MESSAG,135) NMOD,J,IHIT,JHIT
  135       FORMAT('Module = ',I3,' Hit = ',3I4,' ')
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
            GOTO 25
          ENDIF
          IHIT = JHIT
C-------------------------------------------------------------------
C       Process WAMUS hit
C-------------------------------------------------------------------
          IF (NMOD.LT.400) THEN
            CALL MUDHIT(IHIT,JHIT,NCEL,LAT,IADC)
            IF (NCEL.LE.KCEL.OR.NCEL.GT.MAXHIT) GOTO 22
            NWIR = NCEL/4
            IF (MOD(NWIR,2).EQ.1) GOTO 22
            NPLN = NCEL - NWIR*4
            IF (NPLN.LT.NPLNX) THEN
              CALL MUHRAW(NMOD,NCEL,LAT,IADC,IFLG,HIT)
              DO K=1,2
                IADD = IFLG(1,K)
                ICEL = NCEL + (K-1)*4
                IF (IADD.NE.0 .AND. (ICEL/4).LT.NWIRX) THEN
                  CALL MUOHFL(2,IADD,IHIT,HIT(1,K))
                  NTIM = IFLG(3,K)
                  CALL MUHPRO(NMOD,ICEL,NTIM,HIT(1,K),IORN,POS)
                  CALL MUOHFL(3,IFLG(2,K),IFLG(3,K),POS)
                  CALL MUOHFL(4,IMUH,IORN,POS(7))
                ENDIF
              ENDDO
C-------------------------------------------------------------------
C       Process scintillator hit 
C-------------------------------------------------------------------
            ELSE
              IF (NCEL.GT.MAXSCI) GOTO 22
              NSCEL = NCEL
              ISLAT = LAT
              IPMT(1) = IADC(2)
              IPMT(2) = IADC(6)
              DO K=1,NSCLUP
                CALL MNHPRO(NMOD,NSCEL,ISLAT,IPMT,ISTAT,TIME)
                CALL GTMSGE(NMOD,NSCEL,NPMT,IORN,XYZ,DIM,IERR)
                IF (IERR.NE.0) GOTO 20
                CALL MUROTG(IORN,DIM,DXYZ)
                DO KK=1,3
                  DXYZ(KK) = ABS(DXYZ(KK))
                ENDDO
                CALL MSCTFL(2,NMOD,NSCEL,ISTAT,IHIT,NPMT,TIME,XYZ,DXYZ)
                NSCEL = NSCEL + 4
                ISLAT = ISLAT/2
                IPMT(1) = IPMT(2)
                IPMT(2) = 0.
              ENDDO
            ENDIF
C-------------------------------------------------------------------
C       Process SAMUS hit
C-------------------------------------------------------------------
          ELSE
            CALL SADHIT(IHIT,JHIT,NCEL,LAT,IADC)
            IF (NCEL.LE.KCEL.OR.NCEL.GT.MAXHIT) GOTO 22
            CALL SAHPRO(ISTA,ISEC,NCEL,IADC,DIST)
            DO K=1,2
              IF (DIST(K) .GE. 0) THEN
                IADD = NMOD*256 + NCEL*2 + K
                HITSAM(1) = IADC(K)
                HITSAM(2) = 1.0
                HITSAM(3) = DIST(K)
                HITSAM(4) = 1.0
                IF (ISEC.EQ.2.OR.ISEC.EQ.5) IADD=IADD-16
                CALL SAPHFL(2,IADD,IHIT,D3,D4,HITSAM)
              END IF
            ENDDO
          ENDIF
   20   CONTINUE
        GOTO 25
C-------------------------------------------------------------------
C       Flag bad hit sequence
C-------------------------------------------------------------------
   22   JERR = 2
        MESSID = 'MUHITS: illegal cell number'
        WRITE(MESSAG,125) NMOD,NCEL
  125   FORMAT('Module = ',I3,' Cell = ',I3,' ')
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
C-------------------------------------------------------------------
C       Compress banks after module complete
C-------------------------------------------------------------------
   25   IF (NMOD.LT.400) THEN
          CALL MUOHFL(99,NMOD,D2,HIT)
          IF (MAXSCI.GT.0) CALL MSCTFL(99,NMOD,D2,D3,D4,D5,D6,D7,D8)
        ELSE
          CALL SAPHFL(99,NMOD,D2,D3,D4,HITSAM)
        ENDIF
   30 CONTINUE
C
      RETURN
      END
