      SUBROUTINE DT0ZEB
C----------------------------------------------------------------------------
C-
C-    Purposes and Methods:  Fill new constants zebra banks for deltatimes
C-
C-    Inputs: None
C-    Outputs: None
C-    Controls: None
C-
C-    Created: 2-23-92 Eric James
C-   4/92 DH various mods
C    8/92 DH skip MOBO; allow use of line-DT
C    8/92 PQ add option to adjust DTSLOPE
C    8/92 PQ add histograms of DT0 and DTSLOPE
C    9/92 PQ add options to "clean up" bad DT0 and DTSLOPE
C    1/93 PQ add options to do DT0ADJ and SLPFAC together
C----------------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LPSMUO,LPMDTH,LPMDTM,LPMDTM2,CALIB_DT
      INTEGER NMODU,MUNMOD3,I,J,BKMDTM,BKSMUO,BKMDTH
      REAL HSTATI,SSG(3),CHI,SIG,AV,CC
      INTEGER GZMDTM,GZMDTM_N,COUNT,NCHAN
      INTEGER ID,KID,MODN,P,K,R,WNUM,NP,NMB,L,IADD
      INTEGER RUN,ITIME,JTIME,JDATE,SSUNIT,IUNIT,IER,IORIENT
      INTEGER NDT1BAD, NDT2BAD, NDT1OOR, NDT2OOR, NDT1BSL, NDT2BSL
      INTEGER TOTDT1BAD, TOTDT2BAD, TOTDT1OOR
      INTEGER TOTDT2OOR, TOTDT1BSL, TOTDT2BSL
      REAL F1,F2,F3,F4,EVEN,ODD
      REAL NUM3MISS,NUMETRK,NUMOTRK,NFITMIN
      REAL OLDDT1,OLDDT2,OLDSL1,OLDSL2,DT0ADJ,SLPFAC
      REAL VECT,WLEN,VOFF,AVE
C
      DATA NFITMIN / 50. /
      IUNIT=SSUNIT()
      CALL HOUTPU(10)
C
CC    book header zebra banks for new dt0 values
C
      CALL EZGET('CALIB_DT',CALIB_DT,IER)
      IF(CALIB_DT.EQ.0) THEN
        WRITE(IUNIT,*) ' Use odd-even 3-MISS for DT0 correction'
      ELSEIF (CALIB_DT.EQ.1) THEN
        WRITE(IUNIT,*) ' Use odd-even DT-PAD for DT0 correction'
      ELSEIF (CALIB_DT.EQ.2) THEN
        WRITE(IUNIT,*) ' Use odd-even DT-PAD for DTSLOPE correction'
      ELSEIF (CALIB_DT.EQ.3) THEN
        WRITE(IUNIT,*) ' Clean up bad-flag and out-of-range DT0s'
      ELSEIF (CALIB_DT.EQ.4) THEN
        WRITE(IUNIT,*) ' Clean up out-of-range DT0s and DTSLOPEs'
      ELSEIF (CALIB_DT.EQ.5) THEN
        WRITE(IUNIT,*) ' Force all slopes to 0.040'
      ELSEIF (CALIB_DT.EQ.6) THEN
        WRITE(IUNIT,*) ' Force all slopes to 0.040 and DT0 to 1850.'
      ELSEIF (CALIB_DT.EQ.10) THEN
        WRITE(IUNIT,*) ' Use odd-even 3-MISS for DT0ADJ and do SLPFAC'
      ELSEIF (CALIB_DT.EQ.11) THEN
        WRITE(IUNIT,*) ' Use odd-even DT-PAD for DT0ADJ and for SLPFAC'
      ELSEIF (CALIB_DT.EQ.12) THEN
        WRITE(IUNIT,*) ' Use overall DT-PAD for DT0ADJ and for SLPFAC'
      ELSE
	WRITE(IUNIT,*) ' CALIB_DT code',CALIB_DT,' not recognized'
	RETURN
      ENDIF
      LPSMUO = BKSMUO('STPN',0)
      LPMDTH = BKMDTH('STPN',0)
      TOTDT1BAD = 0
      TOTDT2BAD = 0
      TOTDT1OOR = 0
      TOTDT2OOR = 0
      TOTDT1BSL = 0
      TOTDT2BSL = 0
      NMODU = MUNMOD3(0,0)
      DO I = 1,NMODU
        MODN = MUNMOD3(1,I)
        ID = MODN
	CALL MUGEOM(MODN,1,1,VECT,WLEN,VOFF,IORIENT)
	NDT1BAD = 0
	NDT2BAD = 0
	NDT1OOR = 0
	NDT2OOR = 0
	NDT1BSL = 0
	NDT2BSL = 0
C
        LPMDTM2 = GZMDTM(MODN)      ! current bank
        IF(LPMDTM2.EQ.0) THEN
          WRITE(IUNIT,*) ' NO MDTM BANK MODULE ',MODN
        ELSE
          LPMDTM = BKMDTM('STPN',MODN)
          NCHAN=IC(LPMDTM2-1)        ! NUMBER OF DATA POINTS IN BANK
          DO K=1,12                  ! COPY OLD TO START
            IC(LPMDTM+K)=IC(LPMDTM2+K)
          ENDDO
          DO K=13,NCHAN
            C(LPMDTM+K)=C(LPMDTM2+K)
          ENDDO
          CALL MUJUNK(1,RUN,ITIME,JTIME,JDATE,F1,F2,F3,F4)
          IC(LPMDTM+1) = 1
          IC(LPMDTM+6) = RUN
          IC(LPMDTM+7) = JDATE
          IC(LPMDTM+8) = JTIME
CC UPDATE WITH NEW VALUES
          NCHAN=IC(LPMDTM+10)     ! NO. CHANNELS
          DT0ADJ=-99999.
	  SLPFAC=1.
C
          NUM3MISS = HSTATI(10000+ID,3,' ',0)
          IF (NUM3MISS.GT.NFITMIN) THEN
	    CALL HFITGA(10000+ID,CC,AV,SIG,CHI,112,SSG)
	  ELSE
	    AV = 0.
	  ENDIF
C	  
          NUMETRK = HSTATI(12000+ID,3,' ',0)
          IF (NUMETRK.GT.NFITMIN) THEN
	    CALL HFITGA(12000+ID,CC,EVEN,SIG,CHI,112,SSG)
	  ELSE
	    EVEN = 0.
	  ENDIF
C	  
          NUMOTRK = HSTATI(14000+ID,3,' ',0)
          IF (NUMOTRK.GT.NFITMIN) THEN
	    CALL HFITGA(14000+ID,CC,ODD,SIG,CHI,112,SSG)
	  ELSE
	    ODD = 0.
	  ENDIF
C
C-PZQ 11-jan
	if (calib_dt.eq.12) then
          NUMETRK = HSTATI(13000+ID,3,' ',0)
          IF (NUMETRK.GT.NFITMIN) THEN
	    CALL HFITGA(13000+ID,CC,EVEN,SIG,CHI,112,SSG)
	  ELSE
	    EVEN = 0.
	  ENDIF
C	  
          NUMOTRK = HSTATI(15000+ID,3,' ',0)
          IF (NUMOTRK.GT.NFITMIN) THEN
	    CALL HFITGA(15000+ID,CC,ODD,SIG,CHI,112,SSG)
	  ELSE
	    ODD = 0.
	  ENDIF
	endif
C	  
          IF(CALIB_DT.EQ.0.or.calib_dt.eq.10) THEN   ! USE ODD/EVEN 3-MISS
              DT0ADJ=2.*AV
          ELSE                     ! use odd/even residuals to fitted line
CCC   CONVERT TO ADC COUNTS USING .039 FOR ALL MODS
              DT0ADJ=(ODD-EVEN)/2./15./.039
          ENDIF

CCC	  SLPFAC = 1. + ((EVEN + ODD)/2.) / (WLEN/2.)
	  SLPFAC = 1. + (EVEN + ODD) / WLEN

	  WRITE (IUNIT,1000) MODN,DT0ADJ,SLPFAC
 1000	  FORMAT (' MODULE',I4,' DT0ADJ',F6.1,' SLPFAC',F6.3)
C
          IF(DT0ADJ.NE.-99999.) THEN
CC  DT0ADJ is the correction factor measured using tracks. assume Dt2=Dt1
            DO J =1,NCHAN/2
              IADD=8*(J-1)

	      OLDDT1 = C(LPMDTM+21+IADD)
	      OLDDT2 = C(LPMDTM+17+IADD)
	      OLDSL1 = C(LPMDTM+22+IADD)
	      OLDSL2 = C(LPMDTM+18+IADD)
	      CALL HFILL(10001,OLDDT1,0.,1.)
	      CALL HFILL(10002,OLDDT2,0.,1.)
	      CALL HFILL(10003,OLDSL1,0.,1.)
	      CALL HFILL(10004,OLDSL2,0.,1.)
	      CALL HFILL(10005,DT0ADJ,0.,1.)
	      CALL HFILL(10006,SLPFAC,0.,1.)

	      IF (CALIB_DT.EQ.0 .OR. CALIB_DT.EQ.1) THEN
                C(LPMDTM+21+IADD)=C(LPMDTM+21+IADD)+DT0ADJ
                C(LPMDTM+17+IADD)=C(LPMDTM+17+IADD)+DT0ADJ
	      ENDIF

	      IF (CALIB_DT.EQ.2) THEN
                C(LPMDTM+22+IADD)=C(LPMDTM+22+IADD)*SLPFAC
                C(LPMDTM+18+IADD)=C(LPMDTM+18+IADD)*SLPFAC
	      ENDIF

	      IF (CALIB_DT.EQ.10 .OR. CALIB_DT.EQ.11
     x                           .OR. CALIB_DT.EQ.12) THEN
                C(LPMDTM+21+IADD)=C(LPMDTM+21+IADD)+DT0ADJ
                C(LPMDTM+17+IADD)=C(LPMDTM+17+IADD)+DT0ADJ
                C(LPMDTM+22+IADD)=C(LPMDTM+22+IADD)*SLPFAC
                C(LPMDTM+18+IADD)=C(LPMDTM+18+IADD)*SLPFAC
	      ENDIF
C
C check for "bad" values and fix them
C NOTE: reset old bank, because new bank may not be copied over
C       when running on a short job
C NOTE:  <dt01>=1844, <dt02>=1804 from calibdb, inner centrals, run 56000
C        so if reset dt01, then set dt02 = dt01 - 40.
C
	      IF (CALIB_DT.EQ.3) THEN
C-bad flag DT0_2
	        IF (C(LPMDTM+20+IADD).LT.0.) THEN
		  AVE = C(LPMDTM+13)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1810.
                  C(LPMDTM+17+IADD)=AVE
                  C(LPMDTM+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM+20+IADD)=999.
                  C(LPMDTM2+17+IADD)=AVE-40.
                  C(LPMDTM2+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM2+20+IADD)=999.
	          NDT2BAD = NDT2BAD + 1
	          TOTDT2BAD = TOTDT2BAD + 1
	        ENDIF
C-out-of-range DT0_2
	        IF (C(LPMDTM+17+IADD).LT.1550. .OR.
     X              C(LPMDTM+17+IADD).GT.2150) THEN
		  AVE = C(LPMDTM+13)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1810.
                  C(LPMDTM+17+IADD)=AVE
                  C(LPMDTM+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM2+17+IADD)=AVE
                  C(LPMDTM2+18+IADD)=C(LPMDTM+15)
	          NDT2OOR = NDT2OOR + 1
	          TOTDT2OOR = TOTDT2OOR + 1
	        ENDIF
C-bad flag DT0_1
	        IF (C(LPMDTM+24+IADD).LT.0.) THEN
		  AVE = C(LPMDTM+14)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1850.
                  C(LPMDTM+21+IADD)=AVE
                  C(LPMDTM+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM+24+IADD)=999.
                  C(LPMDTM2+21+IADD)=AVE
                  C(LPMDTM2+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM2+24+IADD)=999.
	          NDT1BAD = NDT1BAD + 1
	          TOTDT1BAD = TOTDT1BAD + 1
	        ENDIF
C-out-of-range flag DT0_1
	        IF (C(LPMDTM+21+IADD).LT.1550. .OR.
     X	            C(LPMDTM+21+IADD).GT.2150.) THEN
		  AVE = C(LPMDTM+14)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1850.
                  C(LPMDTM+21+IADD)=AVE
                  C(LPMDTM+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM2+21+IADD)=AVE
                  C(LPMDTM2+22+IADD)=C(LPMDTM+16)
	          NDT1OOR = NDT1OOR + 1
	          TOTDT1OOR = TOTDT1OOR + 1
	        ENDIF
	      ENDIF
C
	      IF (CALIB_DT.EQ.4) THEN
	        IF (C(LPMDTM+17+IADD).LT.1550. .OR.
     X              C(LPMDTM+17+IADD).GT.2150) THEN
		  AVE = C(LPMDTM+13)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1810.
                  C(LPMDTM+17+IADD)=AVE
                  C(LPMDTM+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM+19+IADD)=0.
                  C(LPMDTM+20+IADD)=0.
                  C(LPMDTM2+17+IADD)=AVE
                  C(LPMDTM2+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM2+19+IADD)=0.
                  C(LPMDTM2+20+IADD)=0.
	          NDT2OOR = NDT2OOR + 1
	          TOTDT2OOR = TOTDT2OOR + 1
	        ENDIF
	        IF (C(LPMDTM+21+IADD).LT.1550. .OR.
     X	            C(LPMDTM+21+IADD).GT.2150.) THEN
		  AVE = C(LPMDTM+14)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1850.
                  C(LPMDTM+21+IADD)=AVE
                  C(LPMDTM+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM+23+IADD)=0.
                  C(LPMDTM+24+IADD)=0.
                  C(LPMDTM2+21+IADD)=AVE
                  C(LPMDTM2+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM2+23+IADD)=0.
                  C(LPMDTM2+24+IADD)=0.
	          NDT1OOR = NDT1OOR + 1
	          TOTDT1OOR = TOTDT1OOR + 1
	        ENDIF
	        IF (C(LPMDTM+18+IADD).LT.0.035 .OR.
     X              C(LPMDTM+18+IADD).GT.0.045) THEN
                  C(LPMDTM+18+IADD)=0.040
                  C(LPMDTM+20+IADD)=999.
                  C(LPMDTM2+18+IADD)=0.040
                  C(LPMDTM2+20+IADD)=999.
	          NDT2BSL = NDT2BSL + 1
	          TOTDT2BSL = TOTDT2BSL + 1
	        ENDIF
	        IF (C(LPMDTM+22+IADD).LT.0.035 .OR.
     X	            C(LPMDTM+22+IADD).GT.0.045) THEN
                  C(LPMDTM+22+IADD)=0.040
                  C(LPMDTM+24+IADD)=999.
                  C(LPMDTM2+22+IADD)=0.040
                  C(LPMDTM2+24+IADD)=999.
	          NDT1BSL = NDT1BSL + 1
	          TOTDT1BSL = TOTDT1BSL + 1
	        ENDIF
	      ENDIF   
C
	      IF (CALIB_DT.EQ.5) THEN
	        IF (C(LPMDTM+17+IADD).LT.1550. .OR.
     X              C(LPMDTM+17+IADD).GT.2150) THEN
		  AVE = C(LPMDTM+13)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1810.
                  C(LPMDTM+17+IADD)=AVE
                  C(LPMDTM+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM+20+IADD)=999.
                  C(LPMDTM2+17+IADD)=AVE
                  C(LPMDTM2+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM2+20+IADD)=999.
	          NDT2OOR = NDT2OOR + 1
	          TOTDT2OOR = TOTDT2OOR + 1
	        ENDIF
	        IF (C(LPMDTM+21+IADD).LT.1550. .OR.
     X	            C(LPMDTM+21+IADD).GT.2150.) THEN
		  AVE = C(LPMDTM+14)
	          IF (AVE.LT.1550. .OR. AVE.GT.2150.) AVE=1850.
                  C(LPMDTM+21+IADD)=AVE
                  C(LPMDTM+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM+24+IADD)=999.
                  C(LPMDTM2+21+IADD)=AVE
                  C(LPMDTM2+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM2+24+IADD)=999.
	          NDT1OOR = NDT1OOR + 1
	          TOTDT1OOR = TOTDT1OOR + 1
	        ENDIF
	        IF (C(LPMDTM+18+IADD).LT.0.0399 .OR.
     X              C(LPMDTM+18+IADD).GT.0.0401) THEN
                  C(LPMDTM+18+IADD)=0.040
                  C(LPMDTM+20+IADD)=999.
                  C(LPMDTM2+18+IADD)=0.040
                  C(LPMDTM2+20+IADD)=999.
	          NDT2BSL = NDT2BSL + 1
	          TOTDT2BSL = TOTDT2BSL + 1
	        ENDIF
	        IF (C(LPMDTM+22+IADD).LT.0.0399 .OR.
     X	            C(LPMDTM+22+IADD).GT.0.0401) THEN
                  C(LPMDTM+22+IADD)=0.040
                  C(LPMDTM+24+IADD)=999.
                  C(LPMDTM2+22+IADD)=0.040
                  C(LPMDTM2+24+IADD)=999.
	          NDT1BSL = NDT1BSL + 1
	          TOTDT1BSL = TOTDT1BSL + 1
	        ENDIF
	      ENDIF   
C
	      IF (CALIB_DT.EQ.6) THEN
	        IF (C(LPMDTM+17+IADD).LT.1809. .OR.
     X              C(LPMDTM+17+IADD).GT.1811.) THEN
		  AVE = 1810.
                  C(LPMDTM+17+IADD)=AVE
                  C(LPMDTM+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM+20+IADD)=999.
                  C(LPMDTM2+17+IADD)=AVE
                  C(LPMDTM2+18+IADD)=C(LPMDTM+15)
                  C(LPMDTM2+20+IADD)=999.
	          NDT2OOR = NDT2OOR + 1
	          TOTDT2OOR = TOTDT2OOR + 1
	        ENDIF
	        IF (C(LPMDTM+21+IADD).LT.1849. .OR.
     X	            C(LPMDTM+21+IADD).GT.1851.) THEN
		  AVE = 1850.
                  C(LPMDTM+21+IADD)=AVE
                  C(LPMDTM+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM+24+IADD)=999.
                  C(LPMDTM2+21+IADD)=AVE
                  C(LPMDTM2+22+IADD)=C(LPMDTM+16)
                  C(LPMDTM2+24+IADD)=999.
	          NDT1OOR = NDT1OOR + 1
	          TOTDT1OOR = TOTDT1OOR + 1
	        ENDIF
	        IF (C(LPMDTM+18+IADD).LT.0.0399 .OR.
     X              C(LPMDTM+18+IADD).GT.0.0401) THEN
                  C(LPMDTM+18+IADD)=0.040
                  C(LPMDTM+20+IADD)=999.
                  C(LPMDTM2+18+IADD)=0.040
                  C(LPMDTM2+20+IADD)=999.
	          NDT2BSL = NDT2BSL + 1
	          TOTDT2BSL = TOTDT2BSL + 1
	        ENDIF
	        IF (C(LPMDTM+22+IADD).LT.0.0399 .OR.
     X	            C(LPMDTM+22+IADD).GT.0.0401) THEN
                  C(LPMDTM+22+IADD)=0.040
                  C(LPMDTM+24+IADD)=999.
                  C(LPMDTM2+22+IADD)=0.040
                  C(LPMDTM2+24+IADD)=999.
	          NDT1BSL = NDT1BSL + 1
	          TOTDT1BSL = TOTDT1BSL + 1
	        ENDIF
	      ENDIF   
C
            ENDDO
          ENDIF
        ENDIF

	IF (NDT1BAD+NDT2BAD.GT.0)
     X    WRITE (IUNIT,9000) MODN, NDT1BAD, NDT2BAD
 9000	  FORMAT (' MODULE',I4,' HAD BAD DTIMES',I3,1X,I3)
	IF (NDT1OOR+NDT2OOR.GT.0)
     X    WRITE (IUNIT,9001) MODN, NDT1OOR, NDT2OOR
 9001	  FORMAT (' MODULE',I4,' HAD O-O-R DTIMES',I3,1X,I3)
	IF (NDT1BSL+NDT2BSL.GT.0)
     X    WRITE (IUNIT,9002) MODN, NDT1BSL, NDT2BSL
 9002	  FORMAT (' MODULE',I4,' HAD O-O-R DTSLOPES',I3,1X,I3)

      ENDDO

	IF (TOTDT1BAD+TOTDT2BAD.GT.0)
     X    WRITE (IUNIT,9003) TOTDT1BAD, TOTDT2BAD
 9003	  FORMAT (' THERE WERE A TOTAL OF ',I4,1X,I4,' BAD DTIMES')
	IF (TOTDT1OOR+TOTDT2OOR.GT.0)
     X    WRITE (IUNIT,9004) TOTDT1OOR, TOTDT2OOR
 9004	  FORMAT (' THERE WERE A TOTAL OF ',I4,1X,I4,' O-O-R DTIMES')
	IF (TOTDT1BSL+TOTDT2BSL.GT.0)
     X    WRITE (IUNIT,9005) TOTDT1BSL, TOTDT2BSL
 9005	  FORMAT (' THERE WERE A TOTAL OF ',I4,1X,I4,' O-O-R SLOPES')

      CALL HOUTPU(IUNIT)
      RETURN
      END
