      SUBROUTINE VERMULTI(METHOD,NV,VZ,SIGVZ)
C------------------------------------------------------------------
C
C  Look for multiple primary vertices.
C
C  Input: none (use contents of histogram filled in VERTE1)
C
C  Output:  NV            number of primary vertices
C           VZ(1:NV)      z coordinates
C           SIGVZ(1:NV)   errors of z
C
C  Daria Zieminska Dec. 1989
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR,
C-                                                also added SAVE statement
C------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NV,IBIN,JBIN,JMAX,NBIN,NBIN1,NBIN2,JDOWN,IFIRST,ILAST,IEND
      INTEGER ICALL,IER,MAXPRIM,NVHI,IMEAN,METHOD
      REAL VZ(*),SIGVZ(*),CONTEN(200),DIF(200),MIN1,MIN2,MIN3,FUDGE
      REAL SIGMAZ,SUM,MINSUM,MAXSUM,MEAN,VTEMP,STEMP,ZHI,DELTAZ,ZLOW
      REAL BKG
      LOGICAL PEAK
      LOGICAL EZERROR
      SAVE ICALL
      DATA ICALL/0/
      DATA FUDGE/5./
C------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','VERMULTI',
     &       'Unable to find bank VERTEX_RCP','W')
          GOTO 1000
        ENDIF
        CALL EZGET_i('NBIN',NBIN1,IER)
        CALL EZGET_i('NBIN2',NBIN2,IER)
        CALL EZGET('ZHI',ZHI,IER)
        CALL EZGET_i('MAXPRIM',MAXPRIM,IER)
        CALL EZGET('SIGMAZ',SIGMAZ,IER)
        CALL EZGET('MIN1',MIN1,IER)
        CALL EZGET('MIN2',MIN2,IER)
        CALL EZGET('MIN3',MIN3,IER)
        CALL EZGET('MINSUM',MINSUM,IER)
        CALL EZRSET
        ICALL=1
      END IF
      IF (METHOD.EQ.1) THEN
        NBIN=NBIN1
        DELTAZ=(2.*ZHI)/FLOAT(NBIN)
      END IF
      IF (METHOD.EQ.2) THEN
        NBIN=NBIN2
        DELTAZ=(2.*ZHI)/FLOAT(NBIN)
      END IF
      CALL HUNPAK(1,CONTEN,' ',0)
      DO 500 IBIN=1,NBIN-1
        DIF(IBIN)=CONTEN(IBIN+1)-CONTEN(IBIN)
  500 CONTINUE
      NV=0
      NVHI=0
      IBIN=0
      IEND=0
      MAXSUM=0.
  100 CONTINUE
      IBIN=IBIN+1
      IF (IBIN.GE.NBIN-2) GO TO 800
      PEAK=(DIF(IBIN).GE.MIN1).OR.(DIF(IBIN+1)+DIF(IBIN).GE.MIN1.OR.
     &      DIF(IBIN)+DIF(IBIN+1)+DIF(IBIN+2).GE.MIN1)
      IF (.NOT.PEAK) GO TO 100
      IFIRST=IBIN  ! possible beginning of a peak
      PEAK=.FALSE.
      DO 200 JBIN=IBIN,NBIN-1
        IF (CONTEN(JBIN).GE.MIN2) PEAK=.TRUE. ! the peak's  hight is OK
        IF (DIF(JBIN).LE.MIN3.AND.
     &  DIF(JBIN)+DIF(JBIN+1).LT.MIN3) THEN ! top of the peak
          IF (PEAK) THEN
            IF (JBIN.LT.NBIN) THEN
              JMAX=JBIN
              GO TO 201
            ELSE
              GO TO 800
            END IF
          ELSE
            GO TO 100
          END IF
        END IF
  200 CONTINUE
  201 CONTINUE
      PEAK=.FALSE.
      DO 300 JBIN=JMAX,NBIN
        IF (DIF(JBIN).LE.-MIN1.OR.DIF(JBIN)+DIF(JBIN+1).LE.-MIN1
     &  .OR.DIF(JBIN)+DIF(JBIN+1)+DIF(JBIN+2).LE.-MIN1) THEN
          PEAK=.TRUE. ! falling side
          JDOWN=JBIN
          GO TO 301
        END IF
  300 CONTINUE
  301 CONTINUE
      IF (JDOWN.GE.NBIN-1) THEN
        ILAST=NBIN
        GO TO 401
      END IF
      DO 400 JBIN=JDOWN+1,NBIN
        IF (DIF(JBIN)+DIF(JBIN+1).GT.MIN3) THEN  ! end of the peak
          IF (DIF(JBIN).GT.MIN3) THEN
            ILAST=JBIN
          ELSE
            ILAST=JBIN+1
          END IF
          GO TO 401
        END IF
  400 CONTINUE
      ILAST=NBIN
  401 CONTINUE
      IF (PEAK) THEN
        SUM=0.
        MEAN=0.
        DO 600 JBIN=IFIRST,ILAST
          SUM=SUM+CONTEN(JBIN)
          MEAN=MEAN+CONTEN(JBIN)*JBIN
  600   CONTINUE
        IF (SUM.LT.MINSUM) GO TO 700
        BKG=(CONTEN(ILAST)+CONTEN(IFIRST))*FLOAT(ILAST+1-IFIRST)/2.
        IF (SUM-BKG.LT.10) GO TO 700
        NV=NV+1
        MEAN=MEAN/SUM
        IMEAN=MEAN
        CALL HIX(1,IMEAN,ZLOW)
        VZ(NV)=ZLOW+(MEAN-IMEAN+0.5)*DELTAZ
        SIGVZ(NV)=SIGMAZ/SQRT(SUM)
        SIGVZ(NV)=SIGVZ(NV)+FUDGE*BKG/SUM
        IF (SUM.GT.MAXSUM) THEN
          MAXSUM=SUM
          NVHI=NV
        END IF
      END IF
  700 CONTINUE
      IF (ILAST.EQ.IEND) GO TO 800
      IBIN=ILAST
      IEND=ILAST
      IF (IBIN.LT.NBIN-1.AND.NV.LT.5) GO TO 100
  800 CONTINUE
      IF (NVHI.GT.1) THEN   ! The highest peak should be first
        VTEMP=VZ(1)
        STEMP=SIGVZ(1)
        VZ(1)=VZ(NVHI)
        SIGVZ(1)=SIGVZ(NVHI)
        VZ(NVHI)=VTEMP
        SIGVZ(NVHI)=STEMP
      END IF
 1000 RETURN
      END
