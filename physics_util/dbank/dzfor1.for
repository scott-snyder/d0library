      SUBROUTINE DZFOR1(CHFORM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CHFORM
C-
C-   Inputs  : 
C-   Outputs : CHFORM
C-   Controls: 
C-
C-   Created  12-APR-1989   Rajendran Raja, from Zebra routine DZFORP
C-
C----------------------------------------------------------------------
      PARAMETER      (IQDROP=25, IQMARK=26, IQCRIT=27, IQSYSX=28)   
      COMMON /QUEST/ IQUEST(100)    
      COMMON /ZEBQ/  IQFENC(4), LQ(100) 
                              DIMENSION    IQ(92),        Q(92) 
                              EQUIVALENCE (IQ(1),LQ(9)), (Q(1),IQ(1))   
      INCLUDE 'D0$INC:MZCA.INC'
      INCLUDE 'D0$INC:MZCB.INC'
      INCLUDE 'D0$INC:MZCC.INC'
      COMMON /MZIOC/ NWFOAV,NWFOTT,NWFODN,NWFORE,IFOCON(3)  
     +,              MFOSAV(2),  JFOEND,JFOREP,JFOCUR,MFO(200)  
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE 
      COMMON /ZUNITZ/IQDLUN,IQFLUN,IQHLUN,  NQUSED  
      PARAMETER(NOFMTQ=0)   
      PARAMETER(MSYSFQ=5,MSYSDQ=7,MSYSKQ=8,MSYSPQ=9)    
      COMMON /DZC1/  IFLOPT(26),    
     +               JDROP,LN,LS,LX,IO,NL,NS,ND,    
     +               IBASE,LBASE,NDW,JDFD,JD,JTYP   
      PARAMETER (NSTMXQ=16,NDVMXQ=20,LNULL=0,NOFLIQ=8,NCHEKQ=-7)    
      PARAMETER (MCQSIQ=8,MCQLSQ=2,MCQLGQ=15,MCQLTQ=19,MCQLIQ=87)   
      CHARACTER CQSTAK*13,CQINFO*40 
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )    
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)  
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)    
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO 
      EQUIVALENCE (CQLINE,CQMAP)    
      CHARACTER   CDUMMQ*(*)    
      PARAMETER ( CDUMMQ = ' ' )    
      PARAMETER(IFOUNQ=0,IFOBIQ=1,IFOINQ=2,IFOFLQ=3,IFODOQ=4)   
      PARAMETER(IFOHOQ=5,IFOSEQ=7,IFONAQ=9,IFOLIQ=11,JFOSEQ=16) 
      CHARACTER   CHROUT*(*),CHSTAK*6,KFOTYP(0:11)*1    
      CHARACTER*(*) CHFORM
      INTEGER IPOS1,IPOS2
C
      PARAMETER (CHROUT = 'DZFORP') 
      DATA KFOTYP /'U','B','I','F','D','H','*','S','*','N','*','L'/ 
      CHSTAK          = CQSTAK(MCQSIQ:) 
      CQSTAK(MCQSIQ:) = CHROUT  
      IPOS = 38
      IPOS1 = IPOS
      DO 100 JFOCUR = 1,JFOEND,2    
          IF (JFOCUR.EQ.JFOREP+1) THEN  
              WRITE(CQLINE(IPOS:IPOS+1),'(''/ '')') 
              IPOS = IPOS + 2   
          ENDIF 
          ITYPE  = MFO(JFOCUR)  
          IF (ITYPE.EQ.IFOSEQ) THEN 
              WRITE(CQLINE(IPOS:IPOS+1),'(''*'',A1)') KFOTYP(ITYPE) 
              IPOS = IPOS + 3   
                                                           GO TO 100    
          ENDIF 
          NWSEC  = MFO(JFOCUR+1)    
          ITYPE  = MIN(ITYPE,8) 
          IF (NWSEC.LT.0) THEN  
              WRITE(CQLINE(IPOS:IPOS+1),'(''-'',A1)') KFOTYP(ITYPE) 
              IPOS = IPOS + 3   
          ELSEIF (NWSEC.EQ.0) THEN  
              WRITE(CQLINE(IPOS:IPOS+1),'(''*'',A1)') KFOTYP(ITYPE) 
              IPOS = IPOS + 3   
          ELSE  
              DO 10 I=1,100 
                  IF(NWSEC.EQ.0)                           GO TO 20 
                      IQUEST(I)=MOD(NWSEC,10)   
                      NWSEC    = NWSEC/10   
   10         CONTINUE  
   20         DO 30 J=1,I-1 
                  WRITE(CQLINE(IPOS:IPOS),'(I1)') IQUEST(I-J)   
                  IPOS = IPOS + 1   
   30         CONTINUE  
              WRITE(CQLINE(IPOS:IPOS),'(A1)') KFOTYP(ITYPE) 
              IPOS = IPOS + 2   
          ENDIF 
          IF (IPOS.GT.100) THEN 
C              CALL DZTEXT(0,CDUMMQ,1)   
              IPOS = 23 
              CQLINE = ' '  
          ENDIF 
  100 CONTINUE  
C      IF (IPOS.GT.23) CALL DZTEXT(0,CDUMMQ,1)   
      IPOS2 = IPOS-1
      CHFORM = CQLINE(IPOS1:IPOS2)
  999 CQSTAK(MCQSIQ:) = CHSTAK  
      END   
