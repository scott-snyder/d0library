      SUBROUTINE ISFILL
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Fill MC specific values...
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-Feb-1994   John D. Hobbs
C-   Modified 22-Aug-1994   Fixup for easier reading.
C-   Modified 21-Nov-1994   Bug fix.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$INC:MC_TREE.INC'
      INTEGER  LPAR,LISAQ,LISAL,LISAJ,LPAR1,LPAR2,LINK(NMCDAT),NTOTAL
      INTEGER  ID,ID1,ID2,ITEST,I,J,IDPAR,LISAE,IDEB
      INTEGER  GZISAQ,GZISAL,GZISAJ,GZISAE
      EXTERNAL GZISAQ,GZISAL,GZISAJ,GZISAE
C
      SAVE IDEB
      DATA IDEB/1/
C-----------------------------------------------------------------------
C
      NMC=0
      ID1=0
      ID2=0
      CALL VZERO(IMCDAT,NMCENT*NMCDAT)
      CALL VZERO(LINK,NMCDAT)
C
C  Look for primary partons in ISAJ...
C
      LISAJ=GZISAJ()
      DO WHILE( LISAJ.GT.0 ) 
        IF( NMC.GE.NMCDAT ) GOTO 999
        IF( LQ(LISAJ-1).LE.0 ) THEN
          NMC=NMC+1
          CALL UCOPY(IQ(LISAJ+1),IMCDAT(JMC_ID,NMC),9)
          LINK(NMC)=LISAJ
        ENDIF
 10     CONTINUE
        LISAJ=LQ(LISAJ)
      ENDDO
C
C  Now go to ISAQ and find all primary particles there...
C
      LISAQ=GZISAQ()
      DO WHILE( LISAQ.GT.0 ) 
        IF( NMC.GE.NMCDAT ) GOTO 999
        IF( LQ(LISAQ-1).LE.0 ) THEN
          NMC=NMC+1
          CALL UCOPY(IQ(LISAQ+1),IMCDAT(JMC_ID,NMC),9)
          LINK(NMC)=LISAQ
        ENDIF
        LISAQ=LQ(LISAQ)
      ENDDO
C
C  Build rest of tree in situ
C
      ITEST=1
      DO WHILE( ITEST.LE.NMC .AND. NMC.LT.NMCDAT) 
*JDH        TEST=ITEST+1
        LISAJ=GZISAJ()
        DO WHILE( LISAJ.GT.0 .AND. NMC.LT.NMCDAT ) 
          LPAR=LQ(LISAJ-1)
          IF( LPAR.EQ.LINK(ITEST) ) THEN
            NMC=NMC+1
            LINK(NMC)=LISAJ
            IMCDAT(JMC_PAR,NMC)=ITEST
            CALL UCOPY(IQ(LISAJ+1),IMCDAT(JMC_ID,NMC),9)
            IF( IMCDAT(JMC_ID0,ITEST).EQ.0 ) IMCDAT(JMC_ID0,ITEST)=NMC
            IMCDAT(JMC_ND,ITEST)=IMCDAT(JMC_ND,ITEST)+1
          ENDIF
          LISAJ=LQ(LISAJ)
        ENDDO
        LISAQ=GZISAQ()
        DO WHILE( LISAQ.GT.0 .AND. NMC.LT.NMCDAT ) 
          LPAR=LQ(LISAQ-1)
          IF( LPAR.EQ.LINK(ITEST) ) THEN
            NMC=NMC+1
            LINK(NMC)=LISAQ
            IMCDAT(JMC_PAR,NMC)=ITEST
            CALL UCOPY(IQ(LISAQ+1),IMCDAT(JMC_ID,NMC),9)
            IF( IMCDAT(JMC_ID0,ITEST).EQ.0 ) IMCDAT(JMC_ID0,ITEST)=NMC
            IMCDAT(JMC_ND,ITEST)=IMCDAT(JMC_ND,ITEST)+1
          ENDIF
          LISAQ=LQ(LISAQ)
        ENDDO
        LISAL=GZISAL()
        DO WHILE( LISAL.GT.0 .AND. NMC.LT.NMCDAT ) 
          LPAR=LQ(LISAL-2)
          IF( LPAR.GT.0 ) THEN
            IDPAR=IQ(LPAR+1)
            ID=IQ(LISAL+1)
            IF( ID.NE.IDPAR .AND. LPAR.EQ.LINK(ITEST) ) THEN
              NMC=NMC+1
              LINK(NMC)=LISAL
              IMCDAT(JMC_PAR,NMC)=ITEST
              CALL UCOPY(IQ(LISAL+1),IMCDAT(JMC_ID,NMC),9)
              IF( IMCDAT(JMC_ID0,ITEST).EQ.0 ) IMCDAT(JMC_ID0,ITEST)=NMC
              IMCDAT(JMC_ND,ITEST)=IMCDAT(JMC_ND,ITEST)+1
            ENDIF
          ENDIF
          LISAL=LQ(LISAL)
        ENDDO
        ITEST=ITEST+1
      ENDDO
C
  999 CONTINUE
      IF( IDEB.GT.10 ) THEN
        WRITE(*,7001) NMC
        DO I=1,NMC
          WRITE(*,7002 ) I,(IMCDAT(J,I),J=1,4),(RMCDAT(J,I),j=8,12)
        ENDDO
      ENDIF
      RETURN
 7001 FORMAT(' MC Tree dump.  ',I3,' entries.',/,
     >       '    Index   Prim   NSec  ISec1 PID',
     >       '       E     Mass    Phi   Theta   Eta')
 7002 FORMAT(' ',I6,1X,I6,4X,I4,2X,I4,I6,5X,5(F6.2,1X))
      END
