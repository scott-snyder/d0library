      SUBROUTINE LINSEG(NLADD,LADDRS)
C------------------------------------------------------------------------
C
C  Make VTX track candidates (ladders) by linking track segments.
C  
C  Start from outside:  First layer 2-1 -- if match try 2-1-0; else try 2-0
C  Then try 1-0
C                
C  Output: NLADD               = number of ladders              
C          LADDRS(0:2,ILADD)   = segments on ladder ILADD
C          LADDRS(LAYER,ILADD) = 0 if ladder ILADD does't have a segment
C                                from this LAYER                  
C                               
C  Daria Zieminska  May 1987
C-   Updated   4-NOV-1991   Peter M. Grudberg  Add EZRSET, fix PATH 
C-   Updated  24-SEP-1992   Peter M. Grudberg  Fix PHI in 2nd call to ENDSEG 
C-   Updated  22-MAR-1993   L. Chen  Call ZTOPSY prior to sorting segments 
C-   Updated   3-SEP-1993   Ed Oltman  SIGNIFICANT MODIFICATIONS
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INTEGER NLAYER,LUSER
      INTEGER MXTRAK
      PARAMETER (NLAYER = 2)
      PARAMETER (MXTRAK = 1000)
      INTEGER NLADD,LADDRS(0:NLAYER,MXTRAK),LAYER,LAYER1
      INTEGER ISEG,ISEG1,ISEG0,NSAME1,NSAME0
      INTEGER LOC,LOC1,LOC0,LVTRH,GZVTRH
      INTEGER LSEG(0:NLAYER),NZBANK,NSEGML(0:2),LZFIND,LZFVAL
      REAL PHI,PHI1,PHI0,XG,YG,XG1,YG1,XG0,YG0
      REAL DELPHI,TOLPHI(2),TOLDPHI(2),PHE,PHE1,PHE0
      LOGICAL SAME,VRZCHK
      REAL    DPHISEG
      INTEGER IER,ICALL 
      REAL TWOPI
      DATA ICALL/0/
C--------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        TWOPI = 2.*ACOS(-1.)
        CALL EZPICK('VTRAKS_RCP')
C      TOLPHI = tolerance for delta_phi for two segments.
C                             ! (1) for adjacent layers, (2) for 2 - 0
C      TOLDPHI = tolerance in (PHI1-PHI)**2/ERR1**2 + (PHI2-PHI)**2/ERR2 where 
C                PHI is phi angle connecting two segment's COGs, PHI1,ERR1 and 
C                PHI2,ERR2 are the phi angles and errors from the two segments.
C
        CALL EZGET('TOLPHI',TOLPHI,IER)
        CALL EZGET('TOLDPHI',TOLDPHI,IER)
        CALL EZRSET
        ICALL=1
      END IF
      LUSER=LQ(LHEAD-IZUSER) 
      LVTRH=GZVTRH()
      NLADD=0 
      DO LAYER=0,NLAYER
        LSEG(LAYER)=LQ(LVTRH-3-LAYER)
        NSEGML(LAYER)=NZBANK(0,LSEG(LAYER))
        IF (NSEGML(LAYER).GT.0) THEN
          CALL ZTOPSY(0,LSEG(LAYER))
          CALL ZSORT(0,LSEG(LAYER),20)
          LSEG(LAYER)=LQ(LVTRH-3-LAYER)
        ENDIF
      ENDDO
      DO LAYER = 2,1,-1
C
C ****  Loop over Outer layer sectors
C
        DO ISEG=1,NSEGML(LAYER) 
          LOC=LZFIND(0,LSEG(LAYER),ISEG,-5)
          NSAME1=0
          PHI=Q(LOC+20)
          XG=Q(LOC+21)
          YG=Q(LOC+22)
          PHE = Q(LOC+27)
          LAYER1=LAYER-1
          LOC1=LZFVAL(0,LSEG(LAYER1),PHI,TOLPHI(1),20)
C
C ****  Loop over Next-to-outer layer sectors 
C
          DO WHILE (LOC1 .GT. 0)
            PHI1=Q(LOC1+20)
            DELPHI=ABS(MOD(PHI1-PHI,TWOPI))
            IF (DELPHI.GT.TOLPHI(1)) GO TO 300
            SAME = VRZCHK(LOC,LOC1)
            IF (SAME) THEN 
              XG1=Q(LOC1+21)
              YG1=Q(LOC1+22)
              PHE1 = Q(LOC1+27)
              SAME= DPHISEG(XG ,YG ,PHI ,PHE,
     &                      XG1,YG1,PHI1,PHE1).LT.TOLDPHI(1)
              IF (SAME) THEN
                NSAME1=NSAME1+1
                ISEG1=IQ(LOC1-5)
                IF (NLADD.EQ.MXTRAK) GO TO 1000
                NLADD=NLADD+1
                LADDRS(LAYER,NLADD)=ISEG
                LADDRS(LAYER1,NLADD)=ISEG1
                NSAME0=0
                IF (LAYER1.EQ.1) THEN
                  LOC0=LZFVAL(0,LSEG(0),PHI1,TOLPHI(1),20)
                  DO WHILE (LOC0 .GT. 0)
                    PHI0=Q(LOC0+20)
                    DELPHI=ABS(MOD(PHI1-PHI0,TWOPI))
                    IF (DELPHI.GT.TOLPHI(1)) GO TO 400
                    SAME = VRZCHK(LOC1,LOC0)
                    IF (SAME) THEN 
                      XG0=Q(LOC0+21)
                      YG0=Q(LOC0+22)
                      PHE0 = Q(LOC0+27)
                      SAME= DPHISEG(XG1,YG1,PHI1,PHE1,
     &                              XG0,YG0,PHI0,PHE0).LT.TOLDPHI(1)
                      IF (SAME) THEN
                        NSAME0=NSAME0+1
                        ISEG0=IQ(LOC0-5)
                        IF (NLADD.EQ.MXTRAK) GO TO 1000
                        NLADD=NLADD+1
                        LADDRS(2,NLADD)=ISEG
                        LADDRS(1,NLADD)=ISEG1
                        LADDRS(0,NLADD)=ISEG0
                        IF (NLADD.EQ.MXTRAK) GO TO 1000
                        NLADD = NLADD+1
                        LADDRS(2,NLADD)=ISEG
                        LADDRS(0,NLADD)=ISEG0
                      END IF
                    END IF
                    LOC0=LQ(LOC0)
                  ENDDO             ! End of layer 0 loop for 2-1-0 ladder
  400             CONTINUE          ! 
                  IF (NSAME0 .EQ. 0) THEN
                    LOC0=LZFVAL(0,LSEG(0),PHI,TOLPHI(1),20)
                    DO WHILE (LOC0 .GT. 0)
                      PHI0=Q(LOC0+20)
                      DELPHI=ABS(MOD(PHI1-PHI0,TWOPI))
                      IF (DELPHI.GT.TOLPHI(1)) GO TO 500
                      SAME = VRZCHK(LOC,LOC0)
                      IF (SAME) THEN 
                        XG0=Q(LOC0+21)
                        YG0=Q(LOC0+22)
                        PHE0 = Q(LOC0+27)
                        SAME=DPHISEG(XG ,YG ,PHI ,PHE,
     &                               XG0,YG0,PHI0,PHE0).LT.TOLDPHI(1)
                        IF (SAME) THEN
                          NSAME0=NSAME0+1
                          ISEG0=IQ(LOC0-5)
                          IF (NLADD.EQ.MXTRAK) GO TO 1000
                          NLADD=NLADD+1
                          LADDRS(2,NLADD)=ISEG
                          LADDRS(1,NLADD)=ISEG1
                          LADDRS(0,NLADD)=ISEG0
                          IF (NLADD.EQ.MXTRAK) GO TO 1000
                          NLADD = NLADD+1
                          LADDRS(2,NLADD)=ISEG
                          LADDRS(0,NLADD)=ISEG0
                        END IF
                      END IF
                      LOC0=LQ(LOC0)
                    ENDDO             ! End of layer 0 loop for 2-1-0 ladder
  500               CONTINUE          ! 
                  ENDIF
                END IF              ! Done with layer 0 segments for 2-1-0
              END IF                ! END OF 2-1 OR 1-0 R-PHI MATCH
            END IF                  ! END OF 2-1 OR 1-0 R-Z MATCH
            LOC1=LQ(LOC1)
          ENDDO                     ! END OF LOOP OVER 1 OR 0
  300     CONTINUE                  ! Done with Next-to-outer layer segments
          IF (LAYER.EQ.2.AND.NSAME1.EQ.0) THEN    ! match layers 2 and 0
            LOC0=LZFVAL(0,LSEG(0),PHI,TOLPHI(2),20)
            DO WHILE (LOC0 .GT. 0) 
              NSAME0=0
              PHI0=Q(LOC0+20)
              DELPHI=ABS(MOD(PHI0-PHI,TWOPI))
              IF (DELPHI.GT.TOLPHI(2)) GO TO 900
              SAME = VRZCHK(LOC,LOC0)
              IF (SAME) THEN 
                XG0=Q(LOC0+21)
                YG0=Q(LOC0+22)
                PHE0 = Q(LOC0+27)
                SAME=DPHISEG(XG ,YG ,PHI ,PHE,
     &                       XG0,YG0,PHI0,PHE0).LT.TOLDPHI(2)
                IF (SAME) THEN
                  NSAME0=NSAME0+1
                  ISEG0=IQ(LOC0-5)
                  IF (NLADD.EQ.MXTRAK) GO TO 1000
                  NLADD=NLADD+1
                  LADDRS(2,NLADD)=ISEG
                  LADDRS(1,NLADD)=0
                  LADDRS(0,NLADD)=ISEG0
                END IF
              END IF
              LOC0=LQ(LOC0)
            ENDDO                   ! END OF LAYER 0 SEGMENT LOOP
  900     CONTINUE                  ! DELTA-PHI FAILURE 
          END IF                    ! END OF LAYER 2-0 MATCHING
        ENDDO
      ENDDO
 1000 IF (NLADD .EQ. MXTRAK) 
     &  CALL ERRMSG('VTX -- missing tracks',
     &              'LINSEG',
     &              'Number of VTX ladders at maximum','W')
      END
