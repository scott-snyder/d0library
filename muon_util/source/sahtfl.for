      SUBROUTINE SAHTFL(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine fills bank SAMH
C-
C-   Inputs  :
C-   Outputs : none.
C-   Controls: OK = .FALSE. if no SAMUS hits are found,
C-                  .TRUE. otherwise.
C-
C-   Created  12-NOV-1992   Alexander Efimov
C-   Updated  23-NOV-1992   Alexander Efimov
C-   Updated  06-DEC-1992   Alexei Volkov
C-   modified 11-FEB-1993   Changed an intmsg to errmsg
C-   Modified 06-MAY-1993   Denisov - Changed all an intmsg to errmsg
C-   Updated  27-OCT-1993   Vladimir Podstavkov - switch to MF routines
C-                          to get SAMUS info.
C-   Updated  12-JAN-1994   Alexander Efimov  - use GZSAHS routine.
C-   Updated  20-FEB-1994   Alexander Efimov
C-   Updated  13-JAN-1995   Andrei Mayorov  rewritten to fill bank from SAHP
C-                          KEYT removed from input par. list: SADIST controls
C-                          the presence of DBL3
C-   Updated  24-MAR-1995   A. Mayorov check if ITUBE> number of tube in section
C-   Updated  28-AUG-1995   Andrei Mayorov   check if SAHT is booked and book if
C-                                           not (it happens in MUFIX)
C-   Updated  29-AUG-1995   Andrei Mayorov   check the presence of SAHT before
C-                                           adjusting it's size
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL OK
      INTEGER NWSAPH,ND
      PARAMETER (NWSAPH=19)
      PARAMETER (ND=13)
      INTEGER NSC2NPL(6)
      DATA NSC2NPL /1, 3, 2, 1, 3, 2/
      INTEGER LSAPH,GZSAPH,LSAHS,GZSAHS,LHIT,LSSEC,GZSSEC,LSSTG,LSAHT,
     &  LMUHT,GZMUHT
      EXTERNAL GZSAPH,GZSAHS,GZSSEC,GZMUHT
      INTEGER NHITS,IHIT,IND,LMAX,OFFSET,LTG,J,NW
      INTEGER IMOD,ITUBE,ISEC,IST,NPL,IMODX,NFSAHT
      CHARACTER*23 ADDRESS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        CALL MZFORM ('SAHT', '3I 1F 9I/3I 1F 9I', NFSAHT)
        FIRST=.FALSE.
      END IF
      OK=.FALSE.
      LSAPH =GZSAPH()
      IF(LSAPH.EQ.0) RETURN
      LSAHS=GZSAHS()
      IF(LSAHS.EQ.0) RETURN
      LMUHT=GZMUHT()
      NHITS=IQ(LMUHT+5)
      DO IHIT=1,NHITS
        LHIT=LSAPH+(IHIT-1)*NWSAPH
        IMOD=IQ(LHIT+1)/256
        ITUBE=IQ(LHIT+1)-IMOD*256
        CALL MUMDAT(IMOD,IMODX,ISEC,IST)
        LSSEC=GZSSEC(IST,ISEC)
        IF(IC(LSSEC+4).GE.ITUBE) THEN
          NPL = NSC2NPL(ISEC)
          IND = 3*(IST-1) + NPL
          LSAHT=LQ(LSAHS-IND)
          IF(LSAHT.EQ.0) THEN
            CALL MZBOOK (IXMAIN, LSAHT, LSAHS, -IND, 'SAHT', 0,
     &        0,6*ND, NFSAHT, 0)
            LSAPH =GZSAPH()
            LHIT=LSAPH+(IHIT-1)*NWSAPH
          END IF
          LMAX=IQ(LSAHT-1)
          OFFSET=ND*IQ(LSAHS+IND)
          IF(OFFSET.GE.LMAX) THEN
            CALL MZPUSH(IXCOM,LSAHT,0,6*ND,'I')
            LSAPH =GZSAPH()
            LHIT=LSAPH+(IHIT-1)*NWSAPH
            LSAHS=GZSAHS()
          END IF
          LSAHT=LSAHT+OFFSET
          IQ(LSAHS+IND)=IQ(LSAHS+IND)+1
          IQ(LSAHT+1)=0
          CALL SBYT (IST,  IQ(LSAHT+2),  1,  5)
          CALL SBYT (ISEC,  IQ(LSAHT+2),  6,  5)
          CALL SBYT (NPL,   IQ(LSAHT+2), 11,  5)
          CALL SBYT (ITUBE, IQ(LSAHT+2), 17, 16)
          LSSEC = GZSSEC (IST, ISEC)
          LSSTG = LC(LSSEC-1)
          LTG = IC(LSSTG+ITUBE)
          IQ(LSAHT+3) = LSSTG + IABS(LTG)
          Q (LSAHT+4) = Q(LHIT+18)
          DO J = 1, 9
            IQ(LSAHT+4+J) = 0
          END DO
          IF (LTG .LT. 0) THEN
            IF (OFFSET+ND .LT. LMAX) THEN
              LSAHT = LSAHT + ND
              IQ(LSAHT+1) = 0
              CALL SBYT (IST,  IQ(LSAHT+2),  1,  5)
              CALL SBYT (ISEC,  IQ(LSAHT+2),  6,  5)
              CALL SBYT (NPL,   IQ(LSAHT+2), 11,  5)
              CALL SBYT (ITUBE, IQ(LSAHT+2), 17, 16)
              IQ(LSAHT+3) = LSSTG - LTG + 7
              Q (LSAHT+4) = Q(LHIT+18)
              DO J = 1, 9
                IQ(LSAHT+4+J) = 0
              END DO
              IQ(LSAHS+IND) = IQ(LSAHS+IND) + 1
            END IF
          END IF
        ELSE
          WRITE(ADDRESS,'(a4,i1,1x,a4,i1,1x,a5,i4)')
     &       'st.=',IST,'sec=',ISEC,'tube=',ITUBE
          CALL ERRMSG('Wrong tube address','SAHTFL',ADDRESS,'W')
        END IF
      END DO
      OK=.TRUE.
      DO IND=1,18 ! reduce to actual size
        LSAHT=LQ(LSAHS-IND)
        IF(LSAHT.GT.0) THEN
          NW=IQ(LSAHS+IND)*ND-IQ(LSAHT-1)
          IF (NW.LT.0) THEN
            CALL MZPUSH(IXCOM,LSAHT,0,NW,'I')
          ELSE
            CALL ERRMSG('Wrong tube address','SAHTFL','corrupted SAHT',
     &        'W')
          END IF
        END IF
      END DO
  999 RETURN
      END
