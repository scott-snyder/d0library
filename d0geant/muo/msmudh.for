      SUBROUTINE MSMUDH(NWDS,ICRATE,NMDS,MODN,MODH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill a create header record in MUD1 bank.
C-       
C-      This is called by S/R DIGMU.   The MUD1 bank has been created
C-      in S/R MSMUD1 which is called by S/R DIGMU befor this routine
C-      called.
C-
C-   Inputs  : 
C-      NWDS     I   number of words already stored in MUD1 bank.
C-      ICRATE   I   crate number
C-      NMDS     I   number of module with hits in this crat, ICRATE.
C-      MODN(i)  I   module number for i-th module.
C-      MODH(i)  I   number of hits in i-th module.
C-
C-   Outputs : 
C-      create header words in MUD1 bank.
C-
C-   Controls: (none)
C-
C-   Created   7-DEC-1988   Shuichi Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C                              
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'     ! to get IDEBUG.
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'     ! to get LOUT.
      INCLUDE 'D0$INC:D0LOG.INC/LIST'      ! to get DDIG.
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
      INCLUDE 'D0$LINKS:IZMUD1.LINK/LIST'  ! link pointer to MUD1.
C
      INTEGER NWDS,ICRATE,NMDS,MODN(*),MODH(*)
C
C  -- local variables...
      INTEGER LMUD1,L,NEVNT,I,ND
C
C     -- get bank pointer...
      LMUD1=LQ(LHEAD-IZMUD1)
C     -- check if number of data words is enough to store...
      IF(IQ(LMUD1-1).LT.NWDS+NMDS+6) THEN
C        -- need more words,  push the bank...
         CALL MZPUSH(IXCOM,LMUD1,0,NMDS+6,' ')
      ENDIF
C        -- fill data...
      L=LMUD1+NWDS
      IQ(L+1)=6+NMDS                ! number of crate header words-1
      IQ(L+2)=NEVNT+2**16-1         ! event no. + FFFF
      IQ(L+3)=ICRATE                ! crate ID.
      IQ(L+4)=0                     ! data type.
      IQ(L+5)=0                     ! delay/gain for pulsing data
      ND=0
      L=L+5
      IF(NMDS.GT.0) THEN
         DO 100 I=1,NMDS
            L=L+1
            IQ(L)=MODN(I)*2**16+MODH(I)*9
            ND=ND+MODH(I)*9           
  100    CONTINUE
      ENDIF
      L=L+1
      IQ(L)=ND             ! number of data words
      L=L+1
      IQ(L)=0              ! trigger type.
C     -- update number of words already stored...
      NWDS=NWDS+NMDS+7
C
  999 RETURN
      END
