      SUBROUTINE TRISRC_FULL(ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-
C-   Purpose and Methods :Finds the number of reconstructed tracks
C-                        in a cylinder defined by RIN,ZIN,
C-                        ROUT,ZOUT for full reconstruction (itr=0)
C-
C-   Inputs  : ITR      Dummy (kept for compatibility)
C-
C-   Created  19-DEC-1990   A. Zylberstejn: Adapted form TRISRC
C-   Updated  27-JUN-1991   A. Zylberstejn: Use bank ZFIT instead of DTRK and
C-                                          VTXT
C-   Updated  22-NOV-1992   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:izzfit.LINK'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER CAS,IER,NCAS(3),nevold
      INTEGER I,IDIN,IT,IVERT,KTRAK,IDVTRK,ITR
      INTEGER IQTRAK(8),INDICE,IQVTRK(21),LZFIT,LZTRH,GZZTRH
      INTEGER IKK1,IKK2,IUCOMP,LOUT,TRUNIT
      INTEGER LOC,NGOOD,IGTRAC,ICONTE(10),IDENT,ICDI,IVTI
      INTEGER GZZTRK,GZVTXT,GZDTRK,IVT(NTOTTR),ICD(NTOTTR)
      INTEGER ND,NZ,NV,NVT,NDT,KG
      REAL CT,VIN(6),VOUT(6),PHI,ST,TETA,THETA,QVTRK(21),VERT(14)
      REAL PHILO,PHIHI,TOLPHI
      REAL QVSECT(4,24),QVLAYR(18),QTRAK(8)
      EQUIVALENCE (QVTRK(1),IQVTRK(1)), (QTRAK(1),IQTRAK(1))
      REAL PHII(100),TETAI(100),XGI,YGI,RGI,ZGI
      INTEGER FLGFDC,IDI
      LOGICAL DOPRINT,FIRST
C Use equivalence to save some memory space
      EQUIVALENCE (PHII(1),WS(1001)),(TETAI(1),WS(1101))
C      EQUIVALENCE (XGI(1),WS(1201)),(YGI(1),WS(1301))
C      EQUIVALENCE (RGI(1),WS(1401)),(ZGI(1),WS(1501))
C      EQUIVALENCE (IDI(1),IWS(1601)),(IVT(1),IWS(1701))
C      EQUIVALENCE (ICD(1),IWS(1801))
      DATA PHILO,PHIHI,TOLPHI/0.,6.2832,0.0736/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
C        lout=6
        CALL EZPICK('TRD_RCP')
        CALL EZGET('THMIN',THMIN,IER)
        CALL EZGET('THMAX',THMAX,IER)
        CALL EZRSET
        DOPRINT=SWTDBG.EQ.1
      END IF
      IF(IQ(LHEAD+9).NE.NEVOLD)THEN ! do the following only once per event
        NEVOLD=IQ(LHEAD+9)
        KG=0
        NVT=0
        NDT=0
      END IF
      ICDI=0
      IVTI=0
      IKK1      =  0
      IKK2      =  0
      NCAS(1)=0
      NCAS(2)=0
      NCAS(3)=0
      NGOOD     =  0
      PHILO=0.
      PHIHI=TWOPI
      CALL GTZTRH(ICONTE)
      NZ    =  ICONTE(2)
      NCAS(3)=NZ
      IKK1=1
      DOPRINT=DOPRINT .AND. TNEVDG.GT.0
      IF(OPT.EQ.'VTX')THEN
        CALL GTVTRH(ICONTE)
        NV    =  ICONTE(2)
        NCAS(1)=NV
        NCAS(2)=0
        NCAS(3)=0
      END IF
C      IF(OPT.EQ.'CDC' )THEN ! CDC tracks
C        CALL GTDTRH(ICONTE)
Cc        ND    =  ICONTE(2)
C        NCAS(2)=ICONTE(2)
C        NCAS(1)=0
C        NCAS(3)=0
C      END IF
      IF (DOPRINT)
     +      WRITE(LOUT,*) ' In TRISRC_FULL,ntot,ncd,nvt',NZ,ND,NV
      IF(NZ+ND+NV.LE.0)RETURN
C      DO 80 CAS=3,1,-1
C        IF(CAS.LT.3 .AND. NGOOD.LE.0)RETURN
      CAS=3
      IKK2=NCAS(CAS)
      IKK2=MIN0(IKK2,NTOTTR-1)
      IF(IKK2.EQ.0)GO TO 80
      DO 40 INDICE=IKK1,IKK2
        IDIN=0
C          IF(CAS.EQ.3)THEN
        LOC=GZZTRK(INDICE)
        LZFIT=LQ(LOC-IZZFIT)
        IF(DOPRINT)WRITE(LOUT,*)'loc,lzfit,izzfit',LOC,LZFIT,IZZFIT
        QVTRK(6)=0.
        QVTRK(9)=0.
C            CALL GTZTRK(INDICE,QTRAK)
        IQTRAK(2)=IQ(LOC+2)
        IQTRAK(3)=IQ(LOC+3)
        IQTRAK(4)=IQ(LOC+4)
        IF(IQTRAK(2)+IQTRAK(3).LE.0 .AND. IQTRAK(4).LE.0)GO TO 40
        IF(DOPRINT)WRITE(LOUT,*)' lzfit',LZFIT,' iqtrak',
     &        IQTRAK(2),IQTRAK(3),IQTRAK(4)
        IF(LZFIT.NE.0)THEN
          QVTRK(6)=Q(LZFIT+10)
          QVTRK(9)=Q(LZFIT+13)
          QVTRK(7)=Q(LZFIT+11)
          QVTRK(8)=Q(LZFIT+12)
          QVTRK(10)=Q(LZFIT+14)
          QVTRK(11)=Q(LZFIT+15)
          IF(DOPRINT)WRITE(LOUT,*)' phi theta z fit',
     &          Q(LZFIT+10),Q(LZFIT+13),' r0,z0',QVTRK(10),QVTRK(11)
        ELSE
          IF(DOPRINT)WRITE(LOUT,*)' No ZFIT'
          GO TO 999
        END IF
        CALL SBIT1(IDIN,7)
C          ELSE IF(CAS.EQ.1)THEN ! VTX tracks
C            IQTRAK(2)=INDICE
C            IQTRAK(3)=0
C            CALL SBIT1(IDIN,5)
C            CALL GTVTXT(IQTRAK(2),QVTRK,QVSECT,QVLAYR)
C          ELSE IF(CAS.EQ.2)THEN ! CDC tracks
C            IQTRAK(2)=0
C            IQTRAK(3)=INDICE
C            CALL GTDTRK(IQTRAK(3),QVTRK)
C            CALL SBIT1(IDIN,6)
C          END IF
C          IF(IQTRAK(2)+IQTRAK(3).LE.0)GO TO 40
        TETA     =  QVTRK(9)
        THETA=TETA*RADDEG
        PHI      =  QVTRK(6)
        IF (DOPRINT)THEN
          WRITE(LOUT,*)'CAS,INDICE,IQTRAK(2),IQTRAK(3),TETA,PHI',
     +                    CAS,INDICE,IQTRAK(2),IQTRAK(3),TETA,PHI
          WRITE(LOUT,*)' ldtrk,lvtxt',LQ(LOC-7),LQ(LOC-6)
          IF(LQ(LOC-6).NE.0)WRITE(LOUT,*)' teta phi vertex',
     &        Q(LQ(LOC-6)+9),Q(LQ(LOC-6)+6)
          IF(LQ(LOC-7).NE.0)WRITE(LOUT,*)' teta phi cdc ',
     &        Q(LQ(LOC-7)+9),Q(LQ(LOC-7)+6)
        END IF
C          IF(PHI.LT.PHILO .OR. PHI.GT.PHIHI)GO TO 40
        IF(THETA.LT.THMIN .OR. THETA.GT.THMAX)GO TO 40
 1004   FORMAT(' In TRISRC_FULL,cas',I2,' INDICE',I4, ' iqtrak',2I4,
     &      ' theta,phi',2G40.4)
C Reject IF VTX track already used
        IF(NVT.GE.1 .AND. IQTRAK(2).NE.0)THEN
          IF(IUCOMP(IQTRAK(2),IVT,NVT).NE.0)GO TO 40
        END IF
C Reject if CDC track already used
        IF(NDT.GE.1 .AND. IQTRAK(3).NE.0)THEN
          IF(IUCOMP(IQTRAK(3),ICD,NDT).NE.0)GO TO 40
        END IF
C Reject if track with same phi already used
        IF(KG.GE.1)THEN
          DO 6 I=1,KG
            IF(ABS(PHI-PHII(I)).GT.0.01) GO TO 6
            IF ((TNEVDG.GT.0).AND.(SWTDBG.EQ.1))THEN
              WRITE(LOUT,*)' In TRISRC,  cas,indice',CAS,INDICE,
     &            ' trace rejetee,dphi',ABS(PHI-PHII(I))
            END IF
            GO TO 40
    6     CONTINUE
        ENDIF
        KG=KG+1
        PHII(KG)      =  PHI
        VIN(1)=QVTRK(7)
        VIN(2)=QVTRK(8)
        RGI=QVTRK(10)
        ZGI=QVTRK(11)
        IDI=IDIN+INDICE*128
        CALL SBIT1(IDI,4)
        VIN(3)=QVTRK(11)
        VIN(4)   =  Q(LZFIT+20)
        VIN(5)   =  Q(LZFIT+22)
        VIN(6)   =  Q(LZFIT+24)
        IF(IQTRAK(2).NE.0)THEN
          NVT=NVT+1
          IVT(NVT)=IQTRAK(2)
        END IF
        IF(IQTRAK(3).NE.0)THEN
          NDT=NDT+1
          ICD(NDT)=IQTRAK(3)
        END IF
        IVERT    =  1
C  Check if track crosses the TRD
C ****  Look at the entrance point
        CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
        IF ( IGTRAC .NE. 0 ) GO TO 40
        IF ( ABS ( VOUT(3) ) .GT. ZMIN ) GO TO 40
C ****  look at the exit point !Suppressed to have an extented THETA range
C          CALL EXTCYL ( VIN, VOUT, RMAX, IGTRAC )
C          IF ( IGTRAC .NE. 0 ) GO TO 40
C          IF ( ABS ( VOUT(3) ) .GT. ZMAX ) GO TO 40
C
C ****  This is a good track : stores its characteristics
C
        NGOOD           =  NGOOD + 1
        IF (NGOOD.GT.NTOTTR) THEN
          WRITE (LOUT,*) '*******   WARNING  ************'
          WRITE (LOUT,*) '   TOO MANY TRACKS IN TRD !!!!'
          WRITE (LOUT,*) '   SET TO',NTOTTR,'TRACKS'
          NGOOD = NTOTTR
          GOTO 100
        ENDIF
        IDENT  = IDI
        CALL TRDFIL(VIN,PHI,IDENT)
        IF(IDENT.EQ.-1000)GO TO 100
   40 CONTINUE
   80 CONTINUE
  100 CONTINUE
      IF(NVT.GT.0)CALL VZERO(IVT,NVT)
      IF(NDT.GT.0)CALL VZERO(ICD,NDT)
  999 RETURN
 1001 FORMAT (' In TRISRC_FULL,ivti,icdi',2I3, ' philo,phihi',2G10.4)
      END
