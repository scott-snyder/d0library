      SUBROUTINE MSCTFL(IGO,NMOD,NCEL,IFLG,IPTR,NPMT,TIME,XYZ,DXYZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill hit information into MSCT
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   fill hit information
C-                          =3   fill track information
C-                          =98  extend bank
C-                          =99  compress bank
C-              NMOD   - module ID
C-              NCEL   - cell number/hit number
C-              IFLG   - quality flag
C-              IPTR   - pointer to MUHP/MUOT
C-              NPMT   - number of phototubes
C-              TIME(2)- times for each tube/time of flight
C-              XYZ(2) - global center of scintillator/track position
C-              DXYZ(2)- half widths of scintillator
C-
C-   Controls: MSCT zebra bank
C-
C-   Created   20-OCT-1993  M. Fortner
C-   Modified  8/94 MF protect against bad zebra bank
C-   Modified  3/95 RM store WLS time in word 13
C-   Modified  7/95 TM call to MNWLST
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
C  -- input variables.
      INTEGER IGO,NMOD,NCEL,IFLG,IPTR,NPMT,IDCELL
      REAL TIME(2),XYZ(3),DXYZ(3),WLS_TIME,SXYZ(3)
C
C  -- local variables.
      INTEGER LMSCT,IMSCT,NWD,NDUM
      INTEGER NSCN,JSCN,ISCN,I
      INTEGER NHIT,JHIT,LMUD1
      INTEGER  GZMSCT
      EXTERNAL GZMSCT
      REAL     TOF
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      INTEGER  BITMASK(2), NTOF
      DATA BITMASK/21,42/
      DATA MESSID/'MSCTFL: missing MSCT bank'/
      DATA CALLER/'MSCTFL'/
      DATA MESSAG/'  MSCT bank not found'/
C
C                Initialize and book bank if needed
C
      LMSCT = GZMSCT(0)
      IF (LMSCT.EQ.0) THEN
        IF (IGO.EQ.1) CALL BKMSCT(0,0,LMSCT)
        IF (LMSCT.EQ.0.AND.IGO.NE.98) THEN
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          RETURN
        ENDIF
        CALL MNHMOD(0,NSCN,JSCN)
      ENDIF
C
C                Extend bank
C
      IF (IGO.EQ.98) THEN
        CALL MUDMOD(NMOD,NHIT,JHIT,LMUD1)
        NWD = 2*NHIT*20
        IF (LMSCT.EQ.0) THEN
          CALL BKMSCT(0,NWD,LMSCT)
        ELSE
          CALL MZPUSH(IXCOM,LMSCT,0,NWD,'I')
        ENDIF
C
C                Compress bank
C
      ELSE IF (IGO.EQ.99) THEN
        CALL MNHMOD(0,NSCN,JSCN)
        NWD = NSCN*20 - IQ(LMSCT-1)
        CALL MZPUSH(IXCOM,LMSCT,0,NWD,'I')
C
C                Load new hit with corrected data
C
      ELSE IF (IGO.EQ.2) THEN
        CALL MNHMOD(NMOD,NSCN,JSCN)
        IMSCT = LMSCT + (JSCN+NSCN-1)*20
        IQ(IMSCT+1) = NMOD*256+NCEL
        IQ(IMSCT+2) = IFLG
        IQ(IMSCT+4) = IPTR
        IQ(IMSCT+5) = NPMT
        Q(IMSCT+7) = TIME(1)
        Q(IMSCT+8) = TIME(2)
        TOF = 0.0
        NTOF = 0
        DO I=1,2
          IF ( IAND(BITMASK(I),IFLG).EQ.0 ) THEN
            TOF = TOF + TIME(I)
            NTOF = NTOF + 1
          END IF
        END DO
        IF ( NTOF.NE.0 ) THEN
          Q(IMSCT+9) = TOF/REAL(NTOF)
        ELSE
          Q(IMSCT+9) = -10000.0
        END IF
        DO I = 1,3
          Q(IMSCT+14+I) = XYZ(I)
          Q(IMSCT+17+I) = DXYZ(I)
        ENDDO
C
        ISCN = JSCN+NSCN
        CALL MUHMFL(5,NMOD,1,ISCN,NDUM)
        CALL MUOFFL(5,NMOD,1,ISCN)
C
C                Update hit with new position and track #
C
      ELSE IF (IGO.EQ.3) THEN
        IF ( NCEL.GE.1 ) THEN
          IMSCT = LMSCT + (NCEL-1)*20
          IQ(IMSCT+3) = IPTR
          DO I = 1,3
            Q(IMSCT+9+I) = XYZ(I)
            SXYZ(I) = Q(IMSCT+14+I)
          ENDDO
C
C         Use word 13 to store wavelength shifter correction
C
          NMOD = IQ(IMSCT+1)/256
          IDCELL = MOD(IQ(IMSCT+1),256)
          CALL MNWLST(NMOD,IDCELL,XYZ,SXYZ,WLS_TIME)
          Q(IMSCT+13) = WLS_TIME
        END IF
C
      ENDIF
C
  999 RETURN
      END
