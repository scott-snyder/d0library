      SUBROUTINE DT0OUT
C---------------------------------------------------------------------------
C-
C-    Purposes and Methods:  create output file of new vs. old dt0's
C-
C-    Inputs: None
C-    Outputs: None
C-    Controls: None
C-
C-    Created 2-23-1992  Eric James
C-    DH 3/92 use standard summary for output
C-    EJ 4/92 add overwrite of STPC banks for smuo output
C     DH 4/92 mostly monir mods
C     DH 8/92 eliminate use of mobo
C     PQ 8/92 add overall averages to output
C     DH 9/92 FIX SCREWUP IN STORED SIGMA
C     PQ 10/92 write summary ntuple
C     PQ 2/93 FIX BUG: DO J copy through 16+NCON
C     PQ 4/93 add tracks resolution to first part of output
C     PQ 10/93 chose either 3-miss or tracks for DTRES only if 50 entries
C--------------------------------------------------------------------------
      IMPLICIT NONE
C--------------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LPSMUO,LPMDTH,LPMDTM,LPMDTM2
      INTEGER NUM3MISS,NUMTRACK,NMODU,MUNMOD3,I,J
      REAL MEAN,HSTATI,MEAN2,MEANO,SLPAV1,SLPAV2,R2
      INTEGER GZMDTM,GZMDTM_N,COUNT
      REAL DT01,DT02,DTSLP1,DTSLP2,DTRES
      REAL SIG,CC,AV,CHI,SSG(3),R1,SIGAVE,MEAN1,TAV,TSIG
      INTEGER ID,KID,MODN,L,K,NP,NCHAN,IADD
      INTEGER IUNIT,SSUNIT,NCON,NMB
      REAL F1,F2,F3,F4,SIG1,SIG2,NUM1,NUM2,NUM3
      INTEGER RUN,ITIME,JTIME,JDATE
      CHARACTER*26 TIME
      LOGICAL OK
C
      IUNIT=SSUNIT()
      CALL HOUTPU(10)
C
      NMODU = MUNMOD3(0,0)
C  get time/date and convert to string
      CALL MUJUNK(1,RUN,ITIME,JTIME,JDATE,F1,F2,F3,F4)
      CALL OFTSTR(ITIME,TIME)
      WRITE(IUNIT,*)
      WRITE(IUNIT,*) ' *****   DELTA T0 TUNING    *********'
      WRITE(IUNIT,*) ' RUN ',RUN, ' TIME ',TIME
      WRITE(IUNIT,*)
      WRITE(IUNIT,*) '*   ODD/EVEN 3-MISS SUMMARY    *'
      WRITE(IUNIT,*)
     A' MOD      EVENTS    OLD/NEW DT01   MEAN-3M(cm) SIG-3M SIG-TRK'
C
      DO I = 1,NMODU                ! LOOP OVER MODULES
        MODN = MUNMOD3(1,I)
        ID = MODN
        LPMDTM = GZMDTM_N(MODN)
        LPMDTM2 = GZMDTM(MODN)
        IF(LPMDTM.EQ.0.OR.LPMDTM2.EQ.0) THEN
          WRITE(IUNIT,*) ' No MDTM bank. Module ',MODN
        ELSE
          NCHAN=IC(LPMDTM+10)    ! NUMBER OF CHANNELS
          MEAN2=0
          MEAN1=0.
          MEANO=0.
          R2=0.
          SLPAV1=0.
          SLPAV2=0.
          R1=0.
C
C GET SUM OVER ALL MODULES (ID=11000)
C
	  CALL HOPERA(11000+ID,'+',11000,11000,1.,1.)
C
          NUM3MISS = HSTATI(10000+ID,3,' ',0)
          IF (NUM3MISS.GT.30) THEN
            CALL HFITGA(11000+ID,CC,AV,SIG,CHI,112,SSG)
            CALL HFILL(10000,SIG,0.,1.)
	  ELSE
	    AV = 0.
	    SIG = 99.
	  ENDIF
C
C ABOVE ELSE CLAUSE ADDED 16-NOV-92 PZQ
C INTENTION IS TO REPLACE OLD W/NEW EVEN IF NOT ENOUGH 3-MISS TRACKS
C RECALL IN DT0ZEB THERE ALREADY IS A CUT ON NEVENTS
C
C PZQ: OUTPUT ADDED 16-APR-93
C
	  CALL HBOOK1(17500+ID,'DT vs PADS',40,-100.,100.,0.)
	  CALL HOPERA(17000+ID,'+',19000+ID,17500+ID,1.,1.)
	  CALL HOPERA(17500+ID,'+',18000+ID,17500+ID,1.,1.)
	  NUMTRACK = HSTATI(17500+ID,3,' ',0)
	  IF (NUMTRACK.GT.30) THEN
	    CALL HFITGA(17500+ID,CC,TAV,TSIG,CHI,112,SSG)
	  ELSE
	    TAV = 0.
	    TSIG = 99.
	  ENDIF
C
C PZQ: RESOLUTION TO BE STORED IN ZEBRA BANKS NEEDS >=50 ENTRIES
C
	  IF (NUM3MISS.GE.50) THEN
	    DTRES = SIG * SQRT(2./3.)
	  ELSEIF (NUMTRACK.GE.50) THEN
	    DTRES = TSIG
	  ELSE
	    DTRES = 99.
	  ENDIF
C
            DO L=1,NCHAN/2
              IADD=8*(L-1)
              IF(C(LPMDTM2+24+IADD).GE.0.) THEN
                R1=R1+1.
                SLPAV1 = SLPAV1+C(LPMDTM+22+IADD)
                MEAN1 = MEAN1+C(LPMDTM+21+IADD)
                MEANO = MEANO+C(LPMDTM2+21+IADD)
              ENDIF
              IF(C(LPMDTM2+20+IADD).GE.0.) THEN
                R2=R2+1.
                SLPAV2 = SLPAV2+C(LPMDTM+18+IADD)
                MEAN2 = MEAN2+C(LPMDTM+17+IADD)
              ENDIF
            ENDDO
            MEAN1=MEAN1/(R1+.001)
            MEANO=MEANO/(R1+.001)
            MEAN2=MEAN2/(R2+.001)
            WRITE(IUNIT,533) MODN,NUM3MISS,MEANO,MEAN1,AV,SIG,TSIG
  533       FORMAT(I5,I8,3X,2F8.0,F8.1,5X,2F8.1)
C
C-PZQ summary ntuple
C
            DT02=C(LPMDTM2+13)
            DT01=C(LPMDTM2+14)   ! DELTA T FLIPPED ORDER 1,2
            DTSLP2=C(LPMDTM2+15)
            DTSLP1=C(LPMDTM2+16)
	    WRITE (90,9090) MODN,NUM3MISS,NUMTRACK,
     X                      SIG*SQRT(2./3.),TSIG,
     X                      DT01,DT02,DTSLP1,DTSLP2
 9090       FORMAT (I4,2I6,2F8.1,2F7.1,2F7.4)
C
          ENDIF
          NCON=IC(LPMDTM+10)*4
CCC POSSIBLY WANT TO CUT IF GOOD OR NOT
          IF(R1.GE.1) THEN
            IC(LPMDTM2+1)=101
            DO J=2,12
              IC(LPMDTM2+J)=IC(LPMDTM+J)
            ENDDO
            IC(LPMDTM2+3)=DTRES*1000. ! AVERAGE RES IN 10*MICRONS
            SLPAV1=SLPAV1/(R1+.001)
            SLPAV2=SLPAV2/(R2+.001)
            C(LPMDTM2+13)=MEAN2
            C(LPMDTM2+14)=MEAN1       ! DELTA T FLIPPED ORDER 1,2
            C(LPMDTM2+15)=SLPAV2
            C(LPMDTM2+16)=SLPAV1
            DO J=17,16+NCON
              C(LPMDTM2+J)=C(LPMDTM+J)
            ENDDO
          ENDIF
      ENDDO
C
C CALCULATE AVERAGE OVER ALL MODULES
C
      CALL HFITGA(11000,CC,AV,SIG,CHI,112,SSG)
      WRITE (IUNIT,1100) SIG*SQRT(2./3.)
 1100 FORMAT (' OVERALL 3-MISS RESOLUTION',F6.1,' (cm)')

      WRITE(IUNIT,*)
      WRITE(IUNIT,*) '*       DTIMES-PADS SUMMARY      *'
      WRITE(IUNIT,*)
     A' MODULE       EVENTS               AVERAGE             SIGMA'
C     A'MODULE DT-PAD:AVE(low   center  high-end)',
C     A'SIG(low   center  high-cm) '
      WRITE(IUNIT,*) '    ',
     x '(elec./center/far end)',
     x '(elec./center/far end)',
     x '(elec./center/far end)'
  534 FORMAT(I6,   3F7.0,2X,3F6.0,2X,3F6.0)
  535 FORMAT(I6,7X,2F7.0,8X,2F6.0,8X,2F6.0)
      DO I = 1,NMODU                ! LOOP OVER MODULES
        MODN = MUNMOD3(1,I)
	NUM1 = HSTATI(17000+MODN,3,' ',0)
	NUM2 = HSTATI(18000+MODN,3,' ',0)
	NUM3 = HSTATI(19000+MODN,3,' ',0)
        CALL HFITGA(17000+MODN,CC,AV,SIG,CHI,112,SSG)
        CALL HFITGA(18000+MODN,CC,MEAN1,SIG1,CHI,112,SSG)
        CALL HFITGA(19000+MODN,CC,MEAN2,SIG2,CHI,112,SSG)
        WRITE(IUNIT,534) MODN,NUM1,NUM2,NUM3,
     X                   AV,MEAN1,MEAN2,SIG,SIG1,SIG2
      ENDDO
C
C-CALCULATE OVERALL RESOLUTIONS IN DIFFERENT REGIONS
C
        CALL HFITGA(17000,CC,AV,SIG,CHI,112,SSG)
        CALL HFITGA(18000,CC,MEAN1,SIG1,CHI,112,SSG)
        CALL HFITGA(19000,CC,MEAN2,SIG2,CHI,112,SSG)
        WRITE(IUNIT,1700) AV,MEAN1,MEAN2,SIG,SIG1,SIG2
 1700   FORMAT('  FOR ALL MODS',15X,3F6.0,2X,3F6.0)
C
      WRITE(IUNIT,*) ' FOR EVEN WIRES'
c     A'MODULE EVEN-DT:AVE(      center  high-end)',
c     A'SIG(      center  high-cm) '
      DO I = 1,NMODU                ! LOOP OVER MODULES
        MODN = MUNMOD3(1,I)
	NUM2 = HSTATI(12000+MODN,3,' ',0)
	NUM3 = HSTATI(13000+MODN,3,' ',0)
        CALL HFITGA(12000+MODN,CC,MEAN1,SIG1,CHI,112,SSG)
        CALL HFITGA(13000+MODN,CC,MEAN2,SIG2,CHI,112,SSG)
        WRITE(IUNIT,535) MODN,NUM2,NUM3,MEAN1,MEAN2,SIG1,SIG2
      ENDDO
      WRITE(IUNIT,*) ' FOR ODD WIRES'
c     A'MODULE ODD-DT:AVE(      center  high-end)',
c     A'SIG(      center  high-cm) '
      DO I = 1,NMODU                ! LOOP OVER MODULES
        MODN = MUNMOD3(1,I)
	NUM2 = HSTATI(14000+MODN,3,' ',0)
	NUM3 = HSTATI(15000+MODN,3,' ',0)
        CALL HFITGA(14000+MODN,CC,MEAN1,SIG1,CHI,112,SSG)
        CALL HFITGA(15000+MODN,CC,MEAN2,SIG2,CHI,112,SSG)
        WRITE(IUNIT,535) MODN,NUM2,NUM3,MEAN1,MEAN2,SIG1,SIG2
      ENDDO
C
C CALCULATE OVERALL DTIMES-PADS RESOLUTION
C
      CALL HFITGA(16000,CC,AV,SIG,CHI,112,SSG)
      WRITE (IUNIT,1600) SIG
 1600 FORMAT (' OVERALL DT-PAD RESOLUTION',F6.1,' (cm)')

      CALL HOUTPU(IUNIT)
      RETURN
      END
