      SUBROUTINE PDG2IS(INPUT,OUTPUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : The routine converts the "standard" Particle Data
C-                         Group's Monte Carlo numbering scheme( PDB p113,
C-                         1988 ) into the ISAJET numbering scheme.  REMEMBER:
C-                         ISAJET doesnt distingush between particles with the
C-                         same quark content and spin ( ie between UPSILON(3S)
C-                         and the UPSILON(4S) ) so be sure to also check the
C-                         mass on the output ID!!!
C-
C-   Inputs  :
C-            INPUT = PDG particle ID
C-   Outputs :
C-            OUTPUT = ISAJET particle ID
C-   Controls:
C-
C-   Created  17-DEC-1989   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IQUARK(6),LEPTON(6),BOSON(20),L,JSPIN,QRKCRG(6),QPROD
      INTEGER QMESON
      DATA IQUARK/2,1,3,4,5,6/
      DATA QRKCRG/2,-1,-1,2,-1,2/
      DATA LEPTON/12,11,14,13,16,15/
      DATA BOSON/9,10,90,80,0,0,0,0,0,0,0,0,0,0,82,83,84,86,0,0/
      INTEGER IABSIN,INPUT,OUTPUT,NEXT,I5,I4,I3,I2,I1,ICHRG,IQ(3)
C----------------------------------------------------------------------
      IABSIN=IABS(INPUT)
      ICHRG=INPUT/IABSIN
C
C ****  IS THE INPUT ORDINARY MATTER?
C
      IF ( IABSIN.LE.40 ) THEN
        IF ( IABSIN.LE.6 ) THEN
C
C ****  ITS A QUARK
C
          OUTPUT=IQUARK(IABSIN)*ICHRG
        ELSEIF ( IABSIN.LE.15 ) THEN
C
C ****  ITS A LEPTON
C
          OUTPUT=LEPTON(IABSIN-10)*ICHRG
        ELSE
C
C ****  ITS A BOSON
C
          OUTPUT=BOSON(IABSIN-20)*ICHRG
        ENDIF
      ELSE
C
C ****  ITS EITHER A MESON, BARYON, OR DIQUARK
C
        I5=IABSIN/10000
        NEXT=IABSIN-I5*10000
        I4=NEXT/1000
        NEXT=NEXT-I4*1000
        I3=NEXT/100
        NEXT=NEXT-I3*100
        I2=NEXT/10
        NEXT=NEXT-I2*10
        JSPIN=NEXT-1
C
C ****  IS IT A MESON OR BARYON/DIQUARK?
C
        IF ( I3.NE.0.AND.I4.EQ.0 ) THEN
          IF ( I2.EQ.I3 ) THEN
            OUTPUT=I3*100+I2*10+JSPIN/2
          ELSE
            IQ(1)=IQUARK(I2)
            IQ(2)=IQUARK(I3)
            CALL INTSOR(IQ,2)
            OUTPUT=IQ(1)*100+IQ(2)*10+JSPIN/2
            QMESON=-QRKCRG(IQ(1))+QRKCRG(IQ(2))
            QPROD=QMESON*ICHRG
            IF ( QMESON.EQ.0 ) THEN
              IF ( QRKCRG(IQ(1))*ICHRG.GT.0 ) THEN
                OUTPUT=JISIGN(OUTPUT,ICHRG)
              ELSE
                OUTPUT=-JISIGN(OUTPUT,ICHRG)
              ENDIF
            ELSE
              IF ( QPROD.LT.0 ) THEN
                OUTPUT=JISIGN(OUTPUT,QRKCRG(IQ(1)))
              ELSE
                OUTPUT=-JISIGN(OUTPUT,QRKCRG(IQ(1)))
              ENDIF
            ENDIF
          ENDIF
C
C ****  ITS A MESON, CHECK TO SEE IF ITS SPECIAL CASE
C
          IF ( I3.EQ.3.AND.I2.EQ.1.AND.JSPIN.LE.0 ) OUTPUT=20
          IF ( I3.EQ.1.AND.I2.EQ.3.AND.JSPIN.LE.0 ) OUTPUT=-20
        ELSEIF ( I2.EQ.0 ) THEN
C
C ****  ITS A DIQUARK
C
          IQ(1)=IQUARK(I3)
          IQ(2)=IQUARK(I4)
          CALL INTSOR(IQ,2)
          OUTPUT=IQ(1)*1000+IQ(2)*100
          QMESON=QRKCRG(IQ(1))+QRKCRG(IQ(2))
          QPROD=QMESON*ICHRG
          IF ( QMESON.EQ.0 ) THEN
            IF ( QRKCRG(IQ(1))*ICHRG.GT.0 ) THEN
              OUTPUT=JISIGN(OUTPUT,ICHRG)
            ELSE
              OUTPUT=-JISIGN(OUTPUT,ICHRG)
            ENDIF
          ELSE
            IF ( QPROD.LT.0 ) THEN
              OUTPUT=-JISIGN(OUTPUT,QRKCRG(IQ(1)))
            ELSE
              OUTPUT=JISIGN(OUTPUT,QRKCRG(IQ(1)))
            ENDIF
          ENDIF
        ELSE
C
C ****  ITS A BARYON
C
          L=0
          IF(JSPIN.GT.1) L=1
          IQ(1)=IQUARK(I2)
          IQ(2)=IQUARK(I3)
          IQ(3)=IQUARK(I4)
          CALL INTSOR(IQ,3)
          OUTPUT=IQ(1)*1000+IQ(2)*100+IQ(3)*10+L
          QMESON=QRKCRG(IQ(1))+QRKCRG(IQ(2))+QRKCRG(IQ(3))
          QPROD=QMESON*ICHRG
          IF ( QMESON.EQ.0 ) THEN
            IF ( QRKCRG(IQ(1))*ICHRG.GT.0 ) THEN
              OUTPUT=JISIGN(OUTPUT,ICHRG)
            ELSE
              OUTPUT=JISIGN(OUTPUT,ICHRG)
            ENDIF
          ELSE
            IF ( QPROD.LT.0 ) THEN
              OUTPUT=-JISIGN(OUTPUT,QRKCRG(IQ(1)))
            ELSE
              OUTPUT=JISIGN(OUTPUT,QRKCRG(IQ(1)))
            ENDIF
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
