      SUBROUTINE TSTTYP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fills all TRD static parameters banks
C-                         with typical values
C-
C-   Inputs  : none
C-   Outputs : all TRD static param banks filled.
C-
C-   Created  23-OCT-1987   MANSOULIE
C-   Modified 11-MAR-1988       "
C-   Updated  12-MAY-1989   A. Zylberstejn   Put in realistic sector gains
C-                                           define pedestals depemding on
C-                                           the FADC module
C-   Updated   4-SEP-1989   A. Zylberstejn   Introduce the tables for physics
C-   Updated  17-JUL-1991   Alain PLUQUET    Remove TWAH TRWA TMAT
C-                                           Add values for TDDA and TDMC
C-   Updated  06-APR-1992   Alain PLUQUET    New geometry for real data
C-   Updated  28-JUN-1993   Alain PLUQUET    Add data from run 1a
C-                                           analysis version 0  (T1A0)
C-   Updated  12-OCT-1994   Alain PLUQUET   Add Laurent's canary fit
C-   Updated  12-SEP-1995   L.T.Goss added code for EPSL' (courtesy of JFL)
C-   Updated  16-FEB-1996   Y.Ducros: Tables to compute epsl like quantities
C-                                    for FAKE, Electrons,conversions
C-   Updated  26-FEB-1996   A. ZYLBERSTEJN : Add TCOR
C-   Updated  11-APR-1996   Y. Ducros: add tables to compute likelihoods
C-   Updated  16-APR-1996   A. Zylberstejn  : add Tnd1 and Tnd2 to be used in
C-                                            epsilon_t_end
C-   Updated   1-JUL-1996   A. Zylberstejn  : Increment version nb. of TDOR,
C-                          TDOI,TDFA,TDEL,TDCO
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTCOR
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTELH.LINK'
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$LINKS:IZTLIK.LINK'
      INCLUDE 'D0$LINKS:IZTRLE.LINK'
      INCLUDE 'D0$LINKS:IZTRLC.LINK'
      INCLUDE 'D0$LINKS:IZTACH.LINK'
      INCLUDE 'D0$LINKS:IZTPHY.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TCHA
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTCHA.LINK'
      INCLUDE 'D0$LINKS:IZTCA1.LINK'
      INCLUDE 'D0$LINKS:IZTCA2.LINK'
      INCLUDE 'D0$LINKS:IZTMAE.LINK'
      INCLUDE 'D0$LINKS:IZTMXE.LINK'
      INCLUDE 'D0$LINKS:IZTMAA.LINK'
      INCLUDE 'D0$LINKS:IZTMAB.LINK'
      INCLUDE 'D0$LINKS:IZTMAC.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TPRO
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTPRO.LINK'
      INCLUDE 'D0$LINKS:IZTPR1.LINK'
      INCLUDE 'D0$LINKS:IZTPR2.LINK'
      INCLUDE 'D0$LINKS:IZTPET.LINK'
      INCLUDE 'D0$LINKS:IZTPE3.LINK'
      INCLUDE 'D0$LINKS:IZTPTR.LINK'
      INCLUDE 'D0$LINKS:IZTPT3.LINK'
      INCLUDE 'D0$LINKS:IZTPLE.LINK'
      INCLUDE 'D0$LINKS:IZTPEC.LINK'
      INCLUDE 'D0$LINKS:IZTPCA.LINK'
      INCLUDE 'D0$LINKS:IZTPCB.LINK'
      INCLUDE 'D0$LINKS:IZTPCC.LINK'
      INCLUDE 'D0$LINKS:IZTDMC.LINK'
      INCLUDE 'D0$LINKS:IZTDDA.LINK'
C
      INCLUDE 'D0$LINKS:IZTPIO.LINK'
      INCLUDE 'D0$LINKS:IZTELE.LINK'
      INCLUDE 'D0$LINKS:IZTPI1.LINK'
      INCLUDE 'D0$LINKS:IZTPI2.LINK'
      INCLUDE 'D0$LINKS:IZTPDE.LINK'
      INCLUDE 'D0$LINKS:IZTEL1.LINK'
      INCLUDE 'D0$LINKS:IZTEL2.LINK'
      INCLUDE 'D0$LINKS:IZTEDE.LINK'
      INCLUDE 'D0$LINKS:IZTLTA.LINK'
C-------------------------------------------------------------------------------
C     STRUCTURE UNDER TGEN
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTDEN.LINK'
      INCLUDE 'D0$LINKS:IZTDOR.LINK'
      INCLUDE 'D0$LINKS:IZTDPI.LINK'
      INCLUDE 'D0$LINKS:IZTDFA.LINK'
      INCLUDE 'D0$LINKS:IZTDEL.LINK'
      INCLUDE 'D0$LINKS:IZTDCO.LINK'
C
      INTEGER LTRLE,LTRLC,LTACH,LTRAC,LTPIO,LTPI1,
     &  LTPI2,LTPDE,LTEL1,LTEL2,LTELE,LTLTA,LTEDE
      INTEGER LTRPD,LTELH,LTREL,LTWGH,LTRWG,LTROP
      INTEGER LTDMC,LTDDA
      INTEGER LTDEN,LTDOR,LTDPI,LTDFA,LTDEL,LTDCO
      INTEGER NUU,NVV,NWW,NUVW
      INTEGER ICHAN
      INTEGER I,J,KK,KKK,L,K
      INTEGER IW,IBK,ISECT,JW,NW
      REAL CHGN(3),SECTGN(16,3),BIN1,BIN2,BIN3,BIN4,BIN5,BIN6,
     &  MIN,MAX,ENTRIES
      CHARACTER*20  MYKEY
C-------------------------------------------------------------------------------
C Data from run 1a , analysis version 0 (bank T1A0)
C-------------------------------------------------------------------------------
      INCLUDE 'D0$INC:LTRD.INC'
      INCLUDE 'D0$LINKS:IZT1A0.LINK'
      CHARACTER*25 KEY
      INTEGER ID,DUMMY,N,LT1,CASE
      REAL XDUMMY,BIN,UNDERFLOWS,OVERFLOWS,XMIN,XMAX
      LOGICAL OK
C-------------------------------------------------------------------------------
C Data from run 1a , analysis version 0 (bank T1B0)
C-------------------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZT1B0.LINK'
C-------------------------------------------------------------------------------
C Fit of canary signal
C-------------------------------------------------------------------------------
      REAL CANARY_FIT(8000)
      INTEGER LTCY1,LNEXT
      INCLUDE 'D0$LINKS:IZTCY1.LINK'
C-------------------------------------------------------------------------------
C     RELATIVE GAINS FOR THE TRD SECTORS (11-MAY-1989)
C-------------------------------------------------------------------------------
      DATA SECTGN                            /
     +1.13,1.09,1.19,1.16,1.17,1.18,1.12,1.00,  !CHAMB 1
     +1.02,1.12,1.16,1.14,1.18,1.07,1.07,1.08,
     +0.96,0.89,0.98,0.94,0.98,1.00,0.94,0.91,  !CHAMB 2
     +0.90,0.92,0.95,0.95,1.01,0.94,0.98,0.94,
     +1.00,0.89,0.93,0.87,0.93,0.94,0.91,0.89,  !CHAMB 3
     +0.92,0.91,0.94,1.05,0.99,0.86,0.94,0.94/
C-------------------------------------------------------------------------------
C     FILL PEDESTAL BANKS
C-------------------------------------------------------------------------------
      IC(LTPDH+1)= 1
      IC(LTPDH+2)= 10000
      DO IBK=1,6
        LTRPD = LC(LTPDH-IBK)
        NW = IC(LTRPD-1)
        DO ICHAN=1,NW
          C(LTRPD+ICHAN)=20.+ICHAN/100.
        END DO
      END DO
C-------------------------------------------------------------------------------
C     FILL ELECTRONICS GAINS BANKS
C-------------------------------------------------------------------------------
      LTELH=LC(LTGAI-IZTELH)
      IC(LTELH+1)= 1
      IC(LTELH+2)= 10000
      DO IBK=1,6
        LTREL = LC(LTELH-IBK)
        NW=IC(LTREL-1)
        CALL UFILL(C,LTREL+1,LTREL+NW,120.)
      END DO
C-------------------------------------------------------------------------------
C     FILL RELATIVE WIRE GAINS BANK (TAKEN FROM CALIBRATION RUNS AT CERN)
C-------------------------------------------------------------------------------
      LTWGH=LC(LTGAI-IZTWGH)
      IC(LTWGH+1)= 1
      IC(LTWGH+2)= 10000
      DO IBK=1,3
        LTRWG = LC(LTWGH-IBK)
        NW=IC(LTRWG-1)
        DO  IW=1,NW
          JW=IW
          IF(IBK.EQ.3)JW=IW/2
          ISECT=JW/16+1
          C(LTRWG+IW)=SECTGN(ISECT,IBK)
        END DO
      END DO
C-------------------------------------------------------------------------------
C     FILL OPERATING CONDITIONS BANK
C-------------------------------------------------------------------------------
      LTROP=LC(LTGEN-IZTROP)
      IC(LTROP+1)= 1
      IC(LTROP+2)= 10000
      C(LTROP+3)=.16
      C(LTROP+4)=.01
C-------------------------------------------------------------------------------
C     FILL GEOMETRY BANK (ACTIVE MATERIAL)
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     MOST RECENT VERSION OF GEOMETRY
C-------------------------------------------------------------------------------
      LTACH=LC(LTGEO-IZTACH)
      IC(LTACH+1)= 2
      IC(LTACH+2)= 0
      C(LTACH+3)=0.
      C(LTACH+4)=0.
      C(LTACH+5)=0.
      C(LTACH+6)=0.
      C(LTACH+7)=0.
      C(LTACH+8)=0.
      LTRAC = LC(LTACH-1)
      IC(LTRAC+10)=768
      IC(LTRAC+11)=256
      IC(LTRAC+12)=256
      C(LTRAC+13)=24.4
      C(LTRAC+14)=83.3
      C(LTRAC+15)=25.9
      C(LTRAC+16)=83.3
      C(LTRAC+17)=0.
      C(LTRAC+18)=26.3
      C(LTRAC+19)=83.3
      C(LTRAC+20)=0.  ! angle of sense wire 1
      C(LTRAC+21)=26.7
      C(LTRAC+22)=83.3
      C(LTRAC+23)=.02045308
      C(LTRAC+24)=.01646813
      C(LTRAC+25)=1.396497
      LTRAC = LC(LTACH-2)
      IC(LTRAC+10)=1024
      IC(LTRAC+11)=256
      IC(LTRAC+12)=256
      C(LTRAC+13)=34.95
      C(LTRAC+14)=83.3
      C(LTRAC+15)=36.45
      C(LTRAC+16)=83.3
      C(LTRAC+17)=0.
      C(LTRAC+18)=36.85
      C(LTRAC+19)=83.3
      C(LTRAC+20)=0.  ! angle of sense wire 1
      C(LTRAC+21)=37.25
      C(LTRAC+22)=83.3
      C(LTRAC+23)=.02147573
      C(LTRAC+24)=-.01485945
      C(LTRAC+25)=-1.266217
      LTRAC = LC(LTACH-3)
      IC(LTRAC+10)=1280
      IC(LTRAC+11)=512
      IC(LTRAC+12)=256
      C(LTRAC+13)=45.5
      C(LTRAC+14)=83.3
      C(LTRAC+15)=47.0
      C(LTRAC+16)=83.3
      C(LTRAC+17)=0.
      C(LTRAC+18)=47.4
      C(LTRAC+19)=83.3
      C(LTRAC+20)=0.  ! angle of sense wire 1
      C(LTRAC+21)=47.8
      C(LTRAC+22)=83.3
      C(LTRAC+23)=.02147573
      C(LTRAC+24)=.02244478
      C(LTRAC+25)=1.897182
C-------------------------------------------------------------------------------
C     OLDEST VERSION OF GEOMETRY
C-------------------------------------------------------------------------------
      LTACH=LC(LTACH)
      IC(LTACH+1)= 1
      IC(LTACH+2)= 0
      C(LTACH+3)=0.
      C(LTACH+4)=0.
      C(LTACH+5)=0.
      C(LTACH+6)=0.
      C(LTACH+7)=0.
      C(LTACH+8)=0.
      LTRAC = LC(LTACH-1)
      IC(LTRAC+10)=768
      IC(LTRAC+11)=256
      IC(LTRAC+12)=256
      C(LTRAC+13)=24.4
      C(LTRAC+14)=83.3
      C(LTRAC+15)=25.9
      C(LTRAC+16)=83.3
      C(LTRAC+17)=0.
      C(LTRAC+18)=26.3
      C(LTRAC+19)=83.3
      C(LTRAC+20)=.01227185      !  = 2*PI/512 ! angle of sense wire 1
      C(LTRAC+21)=26.7
      C(LTRAC+22)=83.3
      C(LTRAC+23)=.02045308
      C(LTRAC+24)=.01646813
      C(LTRAC+25)=1.396497
      LTRAC = LC(LTACH-2)
      IC(LTRAC+10)=1024
      IC(LTRAC+11)=256
      IC(LTRAC+12)=256
      C(LTRAC+13)=34.95
      C(LTRAC+14)=83.3
      C(LTRAC+15)=36.45
      C(LTRAC+16)=83.3
      C(LTRAC+17)=0.
      C(LTRAC+18)=36.85
      C(LTRAC+19)=83.3
      C(LTRAC+20)=.01227185      !  = 2*PI/512  ! angle of sense wire 1
      C(LTRAC+21)=37.25
      C(LTRAC+22)=83.3
      C(LTRAC+23)=.02147573
      C(LTRAC+24)=-.01485945
      C(LTRAC+25)=-1.266217
      LTRAC = LC(LTACH-3)
      IC(LTRAC+10)=1280
      IC(LTRAC+11)=512
      IC(LTRAC+12)=256
      C(LTRAC+13)=45.5
      C(LTRAC+14)=83.3
      C(LTRAC+15)=47.0
      C(LTRAC+16)=83.3
      C(LTRAC+17)=0.
      C(LTRAC+18)=47.4
      C(LTRAC+19)=83.3
      C(LTRAC+20)=.006135925      !  = 2*PI/1024  ! angle of sense wire 1
      C(LTRAC+21)=47.8
      C(LTRAC+22)=83.3
      C(LTRAC+23)=.02147573
      C(LTRAC+24)=.02244478
      C(LTRAC+25)=1.897182
C-------------------------------------------------------------------------------
C     FILL THE STRUTURE UNDER TPHY
C-------------------------------------------------------------------------------
      LTPHY=LC(LTGEN-IZTPHY)
      CALL FITPHY(LTPHY)
      LTCHA=LC(LTPHY-IZTCHA)
      CALL FITCHA(LTCHA)
      DO I=1,2
        LTCA=LC(LTCHA-I)
        CALL FITCA(LTCA)
        LTMXE=LC(LTCA-IZTMXE)
        CALL FITMXE(LTMXE,I)
        LTMAE=LC(LTCA-IZTMAE)
        IF(LTMAE.LE.0)THEN
          IF(I.EQ.1)CALL ERRMSG('TMAE bank not filled','TSTTYP',
     &      ' ' ,'W')
        ELSE
          CALL FITMAE(LTMAE)
          DO J=1,3    !FILL THE STRUCTURE UNDER TMAE
            LTMA=LC(LTMAE-J)
            CALL FITMA(LTMA,J,I)
          END DO
        END IF
      END DO
      CALL TND1FL ! added 15-apr-96
      CALL TND2FL ! added 15-apr-96
C-------------------------------------------------------------------------------
C     FILL THE STRUCTURE UNDER TPRO
C-------------------------------------------------------------------------------
      LTPRO=LC(LTPHY-IZTPRO)
      CALL FITPRO(LTPRO)
      DO I=1,2
        LTPR=LC(LTPRO-I)
        CALL FITPR(LTPR)
        LTPET=LC(LTPR-IZTPET)
        CALL FITPET(LTPET,I)
        LTPE3=LC(LTPR-IZTPE3)
        CALL FITPE3(LTPE3,I)
        LTPTR=LC(LTPR-IZTPTR)
        CALL FITPTR(LTPTR,I)
        LTPT3=LC(LTPR-IZTPT3)
        CALL FITPT3(LTPT3,I)
        LTPLE=LC(LTPR-IZTPLE)
        CALL FITPLE(LTPLE,I)
        LTPEC=LC(LTPR-IZTPEC)
        IF(LTPEC.LE.0)THEN
          IF(I.EQ.1)CALL ERRMSG('TPEC bank not filled','TSTTYP',
     &      ' ' ,'W')
        ELSE
          CALL FITPEC(LTPEC)
          DO J=1,3    !FILL THE STRUCTURE UNDER TPEC
            LTPC=LC(LTPEC-J)
            CALL FITPC(LTPC,J,I)
          END DO
        END IF
      END DO
C-------------------------------------------------------------------------------
C     FILL THE CALENDAR BANKS TDMC AND TDDA
C     (2 VERSIONS OF TDMC, 1 VERSION OF TDDA)
C-------------------------------------------------------------------------------
      LTDMC=LC(LTGEO-IZTDMC)            !  most recent period
      LTDDA=LC(LTGEO-IZTDDA)

      IC(LTDMC+1)=1910079488            !  01-MAR-1992 00:00:00.00
      IC(LTDMC+2)=9793147
      IC(LTDMC+3)=-1495711744           !  01-JAN-3000 00:00:00.00
      IC(LTDMC+4)=83843238
      IC(LTDMC+5)=2

      IC(LTDDA+1)=0                     !  17-NOV-1858 00:00:00.00
      IC(LTDDA+2)=0
      IC(LTDDA+3)=-1495711744           !  01-JAN-3000 00:00:00.00
      IC(LTDDA+4)=83843238
      IC(LTDDA+5)=2

      LTDMC=LC(LTDMC)                   !  oldest period

      IC(LTDMC+1)=0                     !  17-NOV-1858 00:00:00.00
      IC(LTDMC+2)=0
      IC(LTDMC+3)=1910079488            !  01-MAR-1992 00:00:00.00
      IC(LTDMC+4)=9793147
      IC(LTDMC+5)=1

C-------------------------------------------------------------------------------
C Data from run 1a , analysis version 0 (bank T1A0)
C-------------------------------------------------------------------------------

    1 FORMAT (A25,I20)
    2 FORMAT (A25,F20.6)
    3 FORMAT (F10.0)
      DO CASE=1,2
        IF(CASE.EQ.1)THEN !1a
          LT1=LC(LTPHY-IZT1A0)
          CALL D0OPEN (10,'STP_TRD_1A0','I',OK)
          C(LT1+2)=320.4  ! EPICOR LAYER 0
          C(LT1+3)=492.9  ! EPICOR LAYER 1
          C(LT1+4)=402.4  ! EPICOR LAYER 2
        ELSE ! 1b
          LT1=LC(LTPHY-IZT1B0)
          CALL D0OPEN (10,'STP_TRD_1B0','I',OK)
          C(LT1+2)=340.  ! EPICOR LAYER 0
          C(LT1+3)=340.  ! EPICOR LAYER 1
          C(LT1+4)=340.  ! EPICOR LAYER 2
        END IF
C        print*,'case',case,' fill bank',lt1
C        C(LT1+1)=1.1    ! VERSION ageing no de/dx cut reco V11.19
        C(LT1+1)=1.1    ! VERSION V12.18
        C(LT1+8)=0.     ! spare
        C(LT1+9)=0.     ! spare
        C(LT1+10)=0.    ! spare
        DO I=1,4
          READ (10,1) KEY,ID
          READ (10,1) KEY,DUMMY
          READ (10,1) KEY,DUMMY
          READ (10,1) KEY,DUMMY
          READ (10,1) KEY,DUMMY
          READ (10,1) KEY,N
          READ (10,2) KEY,XMIN
          READ (10,2) KEY,XMAX
          READ (10,2) KEY,XDUMMY
          READ (10,1) KEY,DUMMY
          READ (10,2) KEY,UNDERFLOWS
          READ (10,2) KEY,OVERFLOWS
          IC(LT1+(I-1)*153+11)=ID
          IC(LT1+(I-1)*153+12)=IFIX(UNDERFLOWS)
          DO J=1,N
            READ (10,3) BIN
            IC(LT1+(I-1)*153+12+J)=IFIX(BIN)
          ENDDO
          IC(LT1+I*153+10)=IFIX(OVERFLOWS)
        ENDDO
        IC(LT1+5)=N            ! number of bin in histogram
        C(LT1+6)=XMIN          ! x min histogram (MIP)
        C(LT1+7)=XMAX          ! x max histogram (MIP)
        CLOSE (10)
      END DO
C      PRINT*,' on arrive a canary'
C-------------------------------------------------------------------------------
C Fit of canary signal
C-------------------------------------------------------------------------------
      CALL D0OPEN (10,'CANARY_FIT_1','I',OK)
    4 FORMAT (10F7.4)
      READ (10,4) CANARY_FIT
      LTCY1=LC(LTCAN-IZTCY1)
      CALL PACK_REAL_STP (CANARY_FIT,10000.,1,8000,2,LTCY1+1,LNEXT)
      CLOSE (10)
C
C-------------------------------------------------------------------------------
C Data from run 1a , likelihood analysis JFL Tables
C-------------------------------------------------------------------------------
C
      CALL D0OPEN (7,'TABLES_JFL','I',OK)
C
      LTLIK=LC(LTGEN-IZTLIK)
      LTPIO=LC(LTLIK-IZTPIO)
      LTELE=LC(LTLIK-IZTELE)
      LTPI1=LC(LTPIO-IZTPI1)
      LTPI2=LC(LTPIO-IZTPI2)
      LTPDE=LC(LTPIO-IZTPDE)
      LTELE=LC(LTLIK-IZTELE)
      LTEL1=LC(LTELE-IZTEL1)
      LTEL2=LC(LTELE-IZTEL2)
      LTEDE=LC(LTELE-IZTEDE)
      LTLTA=LC(LTLIK-IZTLTA)
C
C
C---------------------- PIONS ------------------------------
C-----------------------------------------------------------
C-------------------- BANK TPI1 ----------------------------
C-----------------------------------------------------------
C

      C(LTPI1+1)=1.     ! VERSION
      C(LTPI1+2)=320.4  ! EPICOR LAYER 0
      C(LTPI1+3)=492.9  ! EPICOR LAYER 1
      C(LTPI1+4)=402.4  ! EPICOR LAYER 2
C      CALL DZSHOW ('HELLO BUDDY ',IXSTP,LTlik,'SLV',0,0,0,-1)

C
C----------------------- layer 1 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPI1+10+(J-1)*5+1) = BIN1
        C(LTPI1+10+(J-1)*5+2) = BIN2
        C(LTPI1+10+(J-1)*5+3) = BIN3
        C(LTPI1+10+(J-1)*5+4) = BIN4
        C(LTPI1+10+(J-1)*5+5) = BIN5
      END DO

      C(LTPI1+5)=N            ! number of bin in histogram
      C(LTPI1+6)=MIN          ! x min histogram (MIP)
      C(LTPI1+7)=MAX          ! x max histogram (MIP)
      C(LTPI1+8)=ENTRIES       ! number of entries in histogram l1
C
C-------------------------- layer 2 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPI1+110+(J-1)*5+1) = BIN1
        C(LTPI1+110+(J-1)*5+2) = BIN2
        C(LTPI1+110+(J-1)*5+3) = BIN3
        C(LTPI1+110+(J-1)*5+4) = BIN4
        C(LTPI1+110+(J-1)*5+5) = BIN5
      END DO

      C(LTPI1+9)=ENTRIES       ! number of entries in histogram l2
C
C-------------------------  layer 3 --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPI1+210+(J-1)*5+1) = BIN1
        C(LTPI1+210+(J-1)*5+2) = BIN2
        C(LTPI1+210+(J-1)*5+3) = BIN3
        C(LTPI1+210+(J-1)*5+4) = BIN4
        C(LTPI1+210+(J-1)*5+5) = BIN5
      END DO

      C(LTPI1+10)=ENTRIES       ! number of entries in histogram l3
C
C-----------------------------------------------------------
C-------------------- BANK TPI2 ----------------------------
C-----------------------------------------------------------
C

      C(LTPI2+1)=1.     ! VERSION
      C(LTPI2+2)=320.4  ! EPICOR LAYER 0
      C(LTPI2+3)=492.9  ! EPICOR LAYER 1
      C(LTPI2+4)=402.4  ! EPICOR LAYER 2
      C(LTPI2+9)=0.     ! spare
      C(LTPI2+10)=0.    ! spare
C
C----------------------- layer 1 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPI2+10+(J-1)*5+1) = BIN1
        C(LTPI2+10+(J-1)*5+2) = BIN2
        C(LTPI2+10+(J-1)*5+3) = BIN3
        C(LTPI2+10+(J-1)*5+4) = BIN4
        C(LTPI2+10+(J-1)*5+5) = BIN5
      END DO

      C(LTPI2+5)=N            ! number of bin in histogram
      C(LTPI2+6)=MIN          ! x min histogram (MIP)
      C(LTPI2+7)=MAX          ! x max histogram (MIP)
      C(LTPI2+8)=ENTRIES       ! number of entries in histogram l1
C
C-------------------------- layer 2 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPI2+110+(J-1)*5+1) = BIN1
        C(LTPI2+110+(J-1)*5+2) = BIN2
        C(LTPI2+110+(J-1)*5+3) = BIN3
        C(LTPI2+110+(J-1)*5+4) = BIN4
        C(LTPI2+110+(J-1)*5+5) = BIN5
      END DO

      C(LTPI2+9)=ENTRIES       ! number of entries in histogram l2

C
C-------------------------  layer 3 --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPI2+210+(J-1)*5+1) = BIN1
        C(LTPI2+210+(J-1)*5+2) = BIN2
        C(LTPI2+210+(J-1)*5+3) = BIN3
        C(LTPI2+210+(J-1)*5+4) = BIN4
        C(LTPI2+210+(J-1)*5+5) = BIN5
      END DO

      C(LTPI2+10)=ENTRIES       ! number of entries in histogram l3
C
C-----------------------------------------------------------
C-------------------- BANK TPDE ----------------------------
C-----------------------------------------------------------
C

      C(LTPDE+1)=1.     ! VERSION
      C(LTPDE+2)=320.4  ! EPICOR LAYER 0
      C(LTPDE+3)=492.9  ! EPICOR LAYER 1
      C(LTPDE+4)=402.4  ! EPICOR LAYER 2
      C(LTPDE+9)=0.     ! spare
      C(LTPDE+10)=0.    ! spare
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTPDE+10+(J-1)*5+1) = BIN1
        C(LTPDE+10+(J-1)*5+2) = BIN2
        C(LTPDE+10+(J-1)*5+3) = BIN3
        C(LTPDE+10+(J-1)*5+4) = BIN4
        C(LTPDE+10+(J-1)*5+5) = BIN5
      END DO

      C(LTPDE+5)=N            ! number of bin in histogram
      C(LTPDE+6)=MIN          ! x min histogram (MIP)
      C(LTPDE+7)=MAX          ! x max histogram (MIP)
      C(LTPDE+8)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- ELECTRONS ----------------------------
C-----------------------------------------------------------
C-------------------- BANK TEL1 ----------------------------
C-----------------------------------------------------------
C

      C(LTEL1+1)=1.     ! VERSION
      C(LTEL1+2)=320.4  ! EPICOR LAYER 0
      C(LTEL1+3)=492.9  ! EPICOR LAYER 1
      C(LTEL1+4)=402.4  ! EPICOR LAYER 2
      C(LTEL1+9)=0.     ! spare
      C(LTEL1+10)=0.    ! spare
C
C----------------------- layer 1 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEL1+10+(J-1)*5+1) = BIN1
        C(LTEL1+10+(J-1)*5+2) = BIN2
        C(LTEL1+10+(J-1)*5+3) = BIN3
        C(LTEL1+10+(J-1)*5+4) = BIN4
        C(LTEL1+10+(J-1)*5+5) = BIN5
      END DO

      C(LTEL1+5)=N            ! number of bin in histogram
      C(LTEL1+6)=MIN          ! x min histogram (MIP)
      C(LTEL1+7)=MAX          ! x max histogram (MIP)
      C(LTEL1+8)=ENTRIES       ! number of entries in histogram l1
C
C-------------------------- layer 2 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEL1+110+(J-1)*5+1) = BIN1
        C(LTEL1+110+(J-1)*5+2) = BIN2
        C(LTEL1+110+(J-1)*5+3) = BIN3
        C(LTEL1+110+(J-1)*5+4) = BIN4
        C(LTEL1+110+(J-1)*5+5) = BIN5
      END DO

      C(LTEL1+9)=ENTRIES       ! number of entries in histogram l2
C
C-------------------------  layer 3 --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEL1+210+(J-1)*5+1) = BIN1
        C(LTEL1+210+(J-1)*5+2) = BIN2
        C(LTEL1+210+(J-1)*5+3) = BIN3
        C(LTEL1+210+(J-1)*5+4) = BIN4
        C(LTEL1+210+(J-1)*5+5) = BIN5
      END DO

      C(LTEL1+10)=ENTRIES       ! number of entries in histogram l3
C
C-----------------------------------------------------------
C-------------------- BANK TEL2 ----------------------------
C-----------------------------------------------------------
C

      C(LTEL2+1)=1.     ! VERSION
      C(LTEL2+2)=320.4  ! EPICOR LAYER 0
      C(LTEL2+3)=492.9  ! EPICOR LAYER 1
      C(LTEL2+4)=402.4  ! EPICOR LAYER 2
      C(LTEL2+9)=0.     ! spare
      C(LTEL2+10)=0.    ! spare
C
C----------------------- layer 1 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEL2+10+(J-1)*5+1) = BIN1
        C(LTEL2+10+(J-1)*5+2) = BIN2
        C(LTEL2+10+(J-1)*5+3) = BIN3
        C(LTEL2+10+(J-1)*5+4) = BIN4
        C(LTEL2+10+(J-1)*5+5) = BIN5
      END DO

      C(LTEL2+5)=N            ! number of bin in histogram
      C(LTEL2+6)=MIN          ! x min histogram (MIP)
      C(LTEL2+7)=MAX          ! x max histogram (MIP)
      C(LTEL2+8)=ENTRIES       ! number of entries in histogram l1
C
C-------------------------- layer 2 ---------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEL2+110+(J-1)*5+1) = BIN1
        C(LTEL2+110+(J-1)*5+2) = BIN2
        C(LTEL2+110+(J-1)*5+3) = BIN3
        C(LTEL2+110+(J-1)*5+4) = BIN4
        C(LTEL2+110+(J-1)*5+5) = BIN5
      END DO

      C(LTEL2+9)=ENTRIES       ! number of entries in histogram l2
C
C-------------------------  layer 3 --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEL2+210+(J-1)*5+1) = BIN1
        C(LTEL2+210+(J-1)*5+2) = BIN2
        C(LTEL2+210+(J-1)*5+3) = BIN3
        C(LTEL2+210+(J-1)*5+4) = BIN4
        C(LTEL2+210+(J-1)*5+5) = BIN5
      END DO

      C(LTEL2+10)=ENTRIES       ! number of entries in histogram l3
C
C-----------------------------------------------------------
C-------------------- BANK TEDE ----------------------------
C-----------------------------------------------------------
C

      C(LTEDE+1)=1.     ! VERSION
      C(LTEDE+2)=320.4  ! EPICOR LAYER 0
      C(LTEDE+3)=492.9  ! EPICOR LAYER 1
      C(LTEDE+4)=402.4  ! EPICOR LAYER 2
      C(LTEDE+9)=0.     ! spare
      C(LTEDE+10)=0.    ! spare
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTEDE+10+(J-1)*5+1) = BIN1
        C(LTEDE+10+(J-1)*5+2) = BIN2
        C(LTEDE+10+(J-1)*5+3) = BIN3
        C(LTEDE+10+(J-1)*5+4) = BIN4
        C(LTEDE+10+(J-1)*5+5) = BIN5
      END DO

      C(LTEDE+5)=N            ! number of bin in histogram
      C(LTEDE+6)=MIN          ! x min histogram (MIP)
      C(LTEDE+7)=MAX          ! x max histogram (MIP)
      C(LTEDE+8)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- LIKELIHOOD ---------------------------
C-----------------------------------------------------------
C-------------------- BANK TLTA ----------------------------
C-----------------------------------------------------------
C

      C(LTLTA+1)=1.     ! VERSION
      C(LTLTA+9)=0.     ! spare
      C(LTLTA+10)=0.    ! spare
C
C-------------------- CAS (1,1,1) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+10+(J-1)*5+1) = BIN1
        C(LTLTA+10+(J-1)*5+2) = BIN2
        C(LTLTA+10+(J-1)*5+3) = BIN3
        C(LTLTA+10+(J-1)*5+4) = BIN4
        C(LTLTA+10+(J-1)*5+5) = BIN5
      END DO

      C(LTLTA+2)=N            ! number of bin in histogram
      C(LTLTA+3)=MIN          ! x min histogram (MIP)
      C(LTLTA+4)=MAX          ! x max histogram (MIP)
      C(LTLTA+5)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- CAS (1,1,2) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+410+(J-1)*5+1) = BIN1
        C(LTLTA+410+(J-1)*5+2) = BIN2
        C(LTLTA+410+(J-1)*5+3) = BIN3
        C(LTLTA+410+(J-1)*5+4) = BIN4
        C(LTLTA+410+(J-1)*5+5) = BIN5
      END DO

      C(LTLTA+6)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- CAS (1,2,2) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+810+(J-1)*5+1) = BIN1
        C(LTLTA+810+(J-1)*5+2) = BIN2
        C(LTLTA+810+(J-1)*5+3) = BIN3
        C(LTLTA+810+(J-1)*5+4) = BIN4
        C(LTLTA+810+(J-1)*5+5) = BIN5
      END DO

      C(LTLTA+7)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- CAS (2,2,2) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+1210+(J-1)*5+1) = BIN1
        C(LTLTA+1210+(J-1)*5+2) = BIN2
        C(LTLTA+1210+(J-1)*5+3) = BIN3
        C(LTLTA+1210+(J-1)*5+4) = BIN4
        C(LTLTA+1210+(J-1)*5+5) = BIN5
      END DO

      C(LTLTA+8)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- LIKELIHOOD without dedx -----------------
C-----------------------------------------------------------
C-------------------- BANK TLTA (following) --------------------
C-----------------------------------------------------------
C
      C(LTLTA+1+1610)=1.     ! VERSION
      C(LTLTA+9+1610)=0.     ! spare
      C(LTLTA+10+1610)=0.    ! spare
C
C
C-------------------- CAS (1,1,1) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+10+(J-1)*5+1+1610) = BIN1
        C(LTLTA+10+(J-1)*5+2+1610) = BIN2
        C(LTLTA+10+(J-1)*5+3+1610) = BIN3
        C(LTLTA+10+(J-1)*5+4+1610) = BIN4
        C(LTLTA+10+(J-1)*5+5+1610) = BIN5
      END DO

      C(LTLTA+2+1610)=N            ! number of bin in histogram
      C(LTLTA+3+1610)=MIN          ! x min histogram (MIP)
      C(LTLTA+4+1610)=MAX          ! x max histogram (MIP)
      C(LTLTA+5+1610)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- CAS (1,1,2) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+410+(J-1)*5+1+1610) = BIN1
        C(LTLTA+410+(J-1)*5+2+1610) = BIN2
        C(LTLTA+410+(J-1)*5+3+1610) = BIN3
        C(LTLTA+410+(J-1)*5+4+1610) = BIN4
        C(LTLTA+410+(J-1)*5+5+1610) = BIN5
      END DO

      C(LTLTA+6+1610)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- CAS (1,2,2) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+810+(J-1)*5+1+1610) = BIN1
        C(LTLTA+810+(J-1)*5+2+1610) = BIN2
        C(LTLTA+810+(J-1)*5+3+1610) = BIN3
        C(LTLTA+810+(J-1)*5+4+1610) = BIN4
        C(LTLTA+810+(J-1)*5+5+1610) = BIN5
      END DO

      C(LTLTA+7+1610)=ENTRIES       ! number of entries in histogram CDC
C
C-------------------- CAS (2,2,2) --------------------------
C
      READ (7,'(A20)') MYKEY
      READ (7,'(A20)') MYKEY
      READ (7,*) N,MIN,MAX,ENTRIES

      DO J=1,N/5
        READ (7,*) BIN1,BIN2,BIN3,BIN4,BIN5
        C(LTLTA+1210+(J-1)*5+1+1610) = BIN1
        C(LTLTA+1210+(J-1)*5+2+1610) = BIN2
        C(LTLTA+1210+(J-1)*5+3+1610) = BIN3
        C(LTLTA+1210+(J-1)*5+4+1610) = BIN4
        C(LTLTA+1210+(J-1)*5+5+1610) = BIN5
      END DO

      C(LTLTA+8+1610)=ENTRIES       ! number of entries in histogram CDC

      CLOSE(7)
C      LTPHY=LC(LTGEN-IZTPHY)
C      PRINT*,' ltphy',LTPHY
C      CALL DZSHOW (' fin tsttyp ',IXSTP,LTPHY,'SLV',0,0,0,0)
C
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     DENSITY BANKS:   TDEN,TDOR,TDPI,TDFA,TDPI,TDCO
      LTDEN=LC(LTGEN-IZTDEN)
      LTDOR=LC(LTDEN-IZTDOR)
      LTDPI=LC(LTDEN-IZTDPI)
      LTDFA=LC(LTDEN-IZTDFA)
      LTDEL=LC(LTDEN-IZTDEL)
      LTDCO=LC(LTDEN-IZTDCO)
C
C ------------ CELL ORDER ----------------
C
C              BANK TDOR
C
      CALL D0OPEN (35,'CELOR_3CH_CDC','I',OK)
      IF(.NOT.OK)THEN
        CALL ERRMSG('CELOR_3CH_CDC Data missing','TSTTYP',
     &    'TDEN BANK is not filled ' ,'W')
        CLOSE (35)
        GO TO 210
      END IF
      C(LTDOR+1)=1.1                  ! VERSION NUMBER
      READ(35,1022)BIN1,BIN2,BIN3,BIN4
      C(LTDOR+2)=BIN1                ! TOTAL NB. OF CELLS
      C(LTDOR+3)=BIN2                ! NB OF CELLS (EL. LIK - PION LIK.)
      C(LTDOR+4)=BIN3                ! NB OF CELLS (EL. LIK + PION LIK.)
      C(LTDOR+5)=BIN4                ! NB OF CELLS (CONVERSION LIK.)
      READ(35,1023)BIN1,BIN2,BIN3,BIN4,BIN5,BIN6
      C(LTDOR+6)=BIN1                ! MIN. (EL. LIK - PION LIK.)
      C(LTDOR+7)=BIN2                ! MAX. (EL. LIK - PION LIK.)
      C(LTDOR+8)=BIN3                ! MIN. (EL. LIK + PION LIK.)
      C(LTDOR+9)=BIN4                ! MAX. (EL. LIK + PION LIK.)
      C(LTDOR+10)=BIN5               ! MIN. (CONV. LIK.)
      C(LTDOR+11)=BIN6               ! MAX. (CONV. LIK.)
C                                    !
      NUU=INT(C(LTDOR+3))
      NVV=INT(C(LTDOR+4))
      NWW=INT(C(LTDOR+5))
      NUVW=NUU*NVV*NWW
      KK=0
      DO J=4,NUVW,4
        READ(35,1022)BIN1,BIN2,BIN3,BIN4
        KK=KK+1
        C(LTDOR+12+4*(KK-1)+1)=BIN1
        C(LTDOR+12+4*(KK-1)+2)=BIN2
        C(LTDOR+12+4*(KK-1)+3)=BIN3
        C(LTDOR+12+4*(KK-1)+4)=BIN4
      END DO
      KKK=LTDOR+12+4*(KK-1)+4
      DO J=4,NUU,4
        READ(35,1025)BIN1,BIN2,BIN3,BIN4
        C(KKK+1)=BIN1
        C(KKK+2)=BIN2
        C(KKK+3)=BIN3
        C(KKK+4)=BIN4
        KKK=KKK+4
      END DO
      READ(35,1024)BIN1
      C(KKK+1)=BIN1
      KKK=KKK+1
      DO J=1,NUU
        DO K=4,NVV,4
          READ(35,1025)BIN1,BIN2,BIN3,BIN4
          C(KKK+1)=BIN1
          C(KKK+2)=BIN2
          C(KKK+3)=BIN3
          C(KKK+4)=BIN4
          KKK=KKK+4
        END DO
        READ(35,1024)BIN1
        C(KKK+1)=BIN1
        KKK=KKK+1
      END DO
      DO J=1,NUU
        DO K=1,NVV
          DO L=4,NWW,4
            READ(35,1025)BIN1,BIN2,BIN3,BIN4
            C(KKK+1)=BIN1
            C(KKK+2)=BIN2
            C(KKK+3)=BIN3
            C(KKK+4)=BIN4
            KKK=KKK+4
          END DO
          READ(35,1024)BIN1
          C(KKK+1)=BIN1
          KKK=KKK+1
        END DO
      END DO
      CLOSE(35)
C
C         CELL INDEX ( 3 TRD LAYERS)
C
      CALL D0OPEN (36,'CELOR_3CH','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('CELOR_3CH Data missing','TSTTYP',
     #    'TDOR BANK  is not filled ' ,'W')
        CLOSE (36)
        GO TO 210
      END IF
      READ(36,1022)BIN1,BIN2,BIN3,BIN4
      C(LTDOR+4000+2)=BIN1                ! TOTAL NB. OF CELLS
      C(LTDOR+4000+3)=BIN2                ! NB OF CELLS (EL. LIK - PION LIK.)
      C(LTDOR+4000+4)=BIN3                ! NB OF CELLS (EL. LIK + PION LIK.)
      C(LTDOR+4000+5)=BIN4                ! NB OF CELLS (CONVERSION LIK.)
      READ(36,1023)BIN1,BIN2,BIN3,BIN4,BIN5,BIN6
      C(LTDOR+4000+6)=BIN1                ! MIN. (EL. LIK - PION LIK.)
      C(LTDOR+4000+7)=BIN2                ! MAX. (EL. LIK - PION LIK.)
      C(LTDOR+4000+8)=BIN3                ! MIN. (EL. LIK + PION LIK.)
      C(LTDOR+4000+9)=BIN4                ! MAX. (EL. LIK + PION LIK.)
      C(LTDOR+4000+10)=BIN5               ! MIN. (CONV. LIK.)
      C(LTDOR+4000+11)=BIN6               ! MAX. (CONV. LIK.)
C                                    !
      NUU=INT(C(LTDOR+4000+3))
      NVV=INT(C(LTDOR+4000+4))
      NWW=INT(C(LTDOR+4000+5))
      NUVW=NUU*NVV*NWW
C
C       (NUU+NUU*NVV+2*NUU*NVV*NWW) +12 < 4000
C
      KK=0
      DO J=4,NUVW,4
        READ(36,1022)BIN1,BIN2,BIN3,BIN4
        KK=KK+1
        C(LTDOR+4000+12+4*(KK-1)+1)=BIN1
        C(LTDOR+4000+12+4*(KK-1)+2)=BIN2
        C(LTDOR+4000+12+4*(KK-1)+3)=BIN3
        C(LTDOR+4000+12+4*(KK-1)+4)=BIN4
      END DO
      KKK=LTDOR+4000+12+4*(KK-1)+4
      DO J=4,NUU,4
        READ(36,1025)BIN1,BIN2,BIN3,BIN4
        C(KKK+1)=BIN1
        C(KKK+2)=BIN2
        C(KKK+3)=BIN3
        C(KKK+4)=BIN4
        KKK=KKK+4
      END DO
      READ(36,1024)BIN1
      C(KKK+1)=BIN1
      KKK=KKK+1
      DO J=1,NUU
        DO K=4,NVV,4
          READ(36,1025)BIN1,BIN2,BIN3,BIN4
          C(KKK+1)=BIN1
          C(KKK+2)=BIN2
          C(KKK+3)=BIN3
          C(KKK+4)=BIN4
          KKK=KKK+4
        END DO
        READ(36,1024)BIN1
        C(KKK+1)=BIN1
        KKK=KKK+1
      END DO
      DO J=1,NUU
        DO K=1,NVV
          DO L=4,NWW,4
            READ(36,1025)BIN1,BIN2,BIN3,BIN4
            C(KKK+1)=BIN1
            C(KKK+2)=BIN2
            C(KKK+3)=BIN3
            C(KKK+4)=BIN4
            KKK=KKK+4
          END DO
          READ(36,1024)BIN1
          C(KKK+1)=BIN1
          KKK=KKK+1
        END DO
      END DO
      CLOSE(36)
  210 CONTINUE
C
C
C ------------ MINIMUM BIAS ----------------
C
C              BANK TDPI
C
      C(LTDPI+1)=1.1                  ! VERSION NUMBER
      C(LTDPI+2)=100.                ! NB. OF BINS IN EN. DIST. IN  TRD LAYERS
      C(LTDPI+3)=0.                  ! MINIMUM ENERGY IN TRD LAYERS
      C(LTDPI+4)=15.                 ! MAXIMUM ENERGY IN TRD LAYERS
      C(LTDPI+5)=100.                ! NUMBER OF ENERGY BINS IN DE/DX DIST.
      C(LTDPI+6)=0.                  ! MINIMUM ENERGY IN DE/DX DISTRIBUTION
      C(LTDPI+7)=4.                  ! MAXIMUM ENERGY IN DE/DX DISTRIBUTION
C                                    ! BIN 101 : OVERFLOW
      CALL D0OPEN (32,'MIN1_TRD','I',OK)
      IF(.NOT.OK)THEN
        CALL ERRMSG('MIN1_TRD Data missing','TSTTYP',
     &    'TDEN BANK is not filled ' ,'W')
        CLOSE (32)
        GO TO 200
      END IF
      CALL D0OPEN (33,'MIN2_TRD','I',OK)
      IF(.NOT.OK)THEN
        CALL ERRMSG('MIN2_TRD Data missing','TSTTYP',
     &    'TDEN BANK is not filled ' ,'W')
        CLOSE (33)
        GO TO 200
      END IF
      CALL D0OPEN (34,'MINI_CDC','I',OK)
      IF(.NOT.OK)THEN
        CALL ERRMSG('MINI_CDC Data missing','TSTTYP',
     &    'TDPI BANK is not filled ' ,'W')
        CLOSE (34)
        GO TO 200
      END IF
      DO J=1,101
        READ(32,1020)BIN1,BIN2,BIN3
        C(LTDPI+12+(J-1)*8+1)=BIN1
        C(LTDPI+12+(J-1)*8+2)=BIN2
        C(LTDPI+12+(J-1)*8+3)=BIN3
        READ(33,1020)BIN1,BIN2,BIN3
        C(LTDPI+12+(J-1)*8+4)=BIN1
        C(LTDPI+12+(J-1)*8+5)=BIN2
        C(LTDPI+12+(J-1)*8+6)=BIN3
        READ(34,1021)BIN1,BIN2
        C(LTDPI+12+(J-1)*8+7)=BIN1
        C(LTDPI+12+(J-1)*8+8)=BIN2
      END DO
      CLOSE(32)
      CLOSE(33)
      CLOSE(34)
C
C ------------ FAKE  ----------------
C
C              BANK TDFA
C
      C(LTDFA+1)=1.1                  ! VERSION NUMBER
      C(LTDFA+2)=100.                ! NB. OF BINS IN EN. DIST. IN  TRD LAYERS
      C(LTDFA+3)=0.                  ! MINIMUM ENERGY IN TRD LAYERS
      C(LTDFA+4)=15.                 ! MAXIMUM ENERGY IN TRD LAYERS
      C(LTDFA+5)=100.                ! NUMBER OF ENERGY BINS IN DE/DX DIST.
      C(LTDFA+6)=0.                  ! MINIMUM ENERGY IN DE/DX DISTRIBUTION
      C(LTDFA+7)=4.                  ! MAXIMUM ENERGY IN DE/DX DISTRIBUTION
C                                    ! BIN 101 : OVERFLOW
      CALL D0OPEN (42,'FAK1_TRD','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('FAK1_TRD Data missing','TSTTYP',
     #    'TDFA BANK  is not filled ' ,'W')
        CLOSE (42)
        GO TO 200
      END IF
      CALL D0OPEN (43,'FAK2_TRD','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('FAK2_TRD Data missing','TSTTYP',
     #    'TDFA BANK  is not filled ' ,'W')
        CLOSE (43)
        GO TO 200
      END IF
      CALL D0OPEN (44,'FAKE_CDC','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('FAKE_CDC Data missing','TSTTYP',
     #    'TDFA BANK  is not filled ' ,'W')
        CLOSE (44)
        GO TO 200
      END IF
      DO J=1,101
        READ(42,1020)BIN1,BIN2,BIN3
        C(LTDFA+12+(J-1)*8+1)=BIN1
        C(LTDFA+12+(J-1)*8+2)=BIN2
        C(LTDFA+12+(J-1)*8+3)=BIN3
        READ(43,1020)BIN1,BIN2,BIN3
        C(LTDFA+12+(J-1)*8+4)=BIN1
        C(LTDFA+12+(J-1)*8+5)=BIN2
        C(LTDFA+12+(J-1)*8+6)=BIN3
        READ(44,1021)BIN1,BIN2
        C(LTDFA+12+(J-1)*8+7)=BIN1
        C(LTDFA+12+(J-1)*8+8)=BIN2
      END DO
      CLOSE(42)
      CLOSE(43)
      CLOSE(44)
C
C ------------ ELECTRON  ----------------
C
C              BANK TDEL
C
      C(LTDEL+1)=1.1                  ! VERSION NUMBER
      C(LTDEL+2)=100.                ! NB. OF BINS IN EN. DIST. IN  TRD LAYERS
      C(LTDEL+3)=0.                  ! MINIMUM ENERGY IN TRD LAYERS
      C(LTDEL+4)=15.                 ! MAXIMUM ENERGY IN TRD LAYERS
      C(LTDEL+5)=100.                ! NUMBER OF ENERGY BINS IN DE/DX DIST.
      C(LTDEL+6)=0.                  ! MINIMUM ENERGY IN DE/DX DISTRIBUTION
      C(LTDEL+7)=4.                  ! MAXIMUM ENERGY IN DE/DX DISTRIBUTION
C                                    ! BIN 101 : OVERFLOW
      CALL D0OPEN (52,'ELE1_TRD','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('ELE1_TRD Data missing','TSTTYP',
     #    'TDEL BANK  is not filled ' ,'W')
        CLOSE (52)
        GO TO 200
      END IF
      CALL D0OPEN (53,'ELE2_TRD','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('ELE2_TRD Data missing','TSTTYP',
     #    'TDEL BANK  is not filled ' ,'W')
        CLOSE (53)
        GO TO 200
      END IF
      CALL D0OPEN (54,'ELEC_CDC','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('ELEC_CDC Data missing','TSTTYP',
     #    'TDEL BANK  is not filled ' ,'W')
        CLOSE (54)
        GO TO 200
      END IF
      DO J=1,101
        READ(52,1020)BIN1,BIN2,BIN3
        C(LTDEL+12+(J-1)*8+1)=BIN1
        C(LTDEL+12+(J-1)*8+2)=BIN2
        C(LTDEL+12+(J-1)*8+3)=BIN3
        READ(53,1020)BIN1,BIN2,BIN3
        C(LTDEL+12+(J-1)*8+4)=BIN1
        C(LTDEL+12+(J-1)*8+5)=BIN2
        C(LTDEL+12+(J-1)*8+6)=BIN3
        READ(54,1021)BIN1,BIN2
        C(LTDEL+12+(J-1)*8+7)=BIN1
        C(LTDEL+12+(J-1)*8+8)=BIN2
      END DO
      CLOSE(52)
      CLOSE(53)
      CLOSE(54)
C
C ------------ CONVERSION  ----------------
C
C              BANK TDCO
C
      C(LTDCO+1)=1.1                  ! VERSION NUMBER
      C(LTDCO+2)=100.                ! NB. OF BINS IN EN. DIST. IN  TRD LAYERS
      C(LTDCO+3)=0.                  ! MINIMUM ENERGY IN TRD LAYERS
      C(LTDCO+4)=15.                 ! MAXIMUM ENERGY IN TRD LAYERS
      C(LTDCO+5)=100.                ! NUMBER OF ENERGY BINS IN DE/DX DIST.
      C(LTDCO+6)=0.                  ! MINIMUM ENERGY IN DE/DX DISTRIBUTION
      C(LTDCO+7)=4.                  ! MAXIMUM ENERGY IN DE/DX DISTRIBUTION
C                                    ! BIN 101 : OVERFLOW
      CALL D0OPEN (62,'CON1_TRD','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('CON1_TRD Data missing','TSTTYP',
     #    'TDCO BANK  is not filled ' ,'W')
        CLOSE (62)
        GO TO 200
      END IF
      CALL D0OPEN (63,'CON2_TRD','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('CON2_TRD Data missing','TSTTYP',
     #    'TDCO BANK  is not filled ' ,'W')
        CLOSE (63)
        GO TO 200
      END IF
      CALL D0OPEN (64,'CONV_CDC','I',OK)
      IF(.NOT.OK) THEN
        CALL ERRMSG('CONV_CDC Data missing','TSTTYP',
     #    'TDCO BANK  is not filled ' ,'W')
        CLOSE (64)
        GO TO 200
      END IF
      DO J=1,101
        READ(62,1020)BIN1,BIN2,BIN3
        C(LTDCO+12+(J-1)*8+1)=BIN1
        C(LTDCO+12+(J-1)*8+2)=BIN2
        C(LTDCO+12+(J-1)*8+3)=BIN3
        READ(63,1020)BIN1,BIN2,BIN3
        C(LTDCO+12+(J-1)*8+4)=BIN1
        C(LTDCO+12+(J-1)*8+5)=BIN2
        C(LTDCO+12+(J-1)*8+6)=BIN3
        READ(64,1021)BIN1,BIN2
        C(LTDCO+12+(J-1)*8+7)=BIN1
        C(LTDCO+12+(J-1)*8+8)=BIN2
      END DO
      CLOSE(62)
      CLOSE(63)
      CLOSE(64)
  200 CONTINUE
      CALL TCORFL ! fill bank TCOR (TGEN-5)
C        CALL DZSHOW('tsttyp apres tcorfl',IXstp,Ltgen,'SLV',0,0,0,0)
C
 1020 FORMAT(2X,3(1X,F7.4))
 1021 FORMAT(2X,2(1X,F7.4))
 1022 FORMAT(2X,4(1X,F8.1))
 1023 FORMAT(2X,6(1X,F8.3))
 1024 FORMAT(2X,(1X,F8.3))
 1025 FORMAT(2X,4(1X,F8.3))
      END
