      SUBROUTINE L2_FDC_COSMICS(FDC_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fast determination of existence of track
C-                         for FDC in Level-2 ( called by the main
C-                         filter routine for Cosmic Commissioning,
C-                         COSMIC_L2_FDC )
C-
C-   Inputs  : none
C-   Outputs : FDC_FLAG = Set False if Event should be rejected.
C-   Controls: none
C-
C-   CREATED :  03-JUL-1991   YI-CHENG LIU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FDC_FLAG,STATUS
C      LOGICAL FLGVAL
      LOGICAL CHKOVL,FIRST,FLAG_INIT,FILTER(0:2),FINAL(0:1)
C
C
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER NPULSE,TPULSE,NWIR
      INTEGER NHIT(0:9)      ! Number of hits on each Thata wire ( 8,9: DL's )
      INTEGER NTHETA(0:1),NPHI(0:1)
      INTEGER TSCH(0:1)      ! Total number of sectors hit per half
      INTEGER HALF1,UNIT1,QUAD1,SECT1,HALF2,UNIT2,QUAD2,SECT2
      INTEGER HALF_MID,UNIT_MID,QUAD_MID,SECT_MID
      INTEGER NSECT,SOVRLP(50,4),NOVLP,TASK,iok
C
C ***  To make up for cross-Phi-sectors tracks ***
C
      INTEGER NUM_HALF_PHI(0:1)
C
      REAL HITLST(5,0:2,0:15)
C
      DATA FIRST /.TRUE./
      DATA HALF2 /2/, HALF_MID /2/
C
C
      NOVLP = 0
      FDC_FLAG = .FALSE.
C
 777  FORMAT(4X,A7,2X,I6,2X,A6)
C
C------ Unpack Crate banks --------------------------------------------
C
      CALL DCRUNP_FDC(STATUS)               ! Unpack data
      IF (.NOT.STATUS) GO TO 999

C------ Loop over all units,quadrants,sectors,wires and determine which  
C------ sector has been hit. 
C
      DO 10 HALF = 0,1
        NTHETA(HALF) = 0
        NPHI(HALF) = 0
        NUM_HALF_PHI(HALF) = 0
        DO 20 UNIT = 0,1
        IF (UNIT.EQ.0) THEN                 
C      
C------ for theta chamber ---------------------------------------------
          
          DO 30 QUAD = 0,7                  
            DO 40 SECT = 0,5
              NPULSE = 0                    ! Number of hits in a wire
              TPULSE = 0                    ! Total number of hits in a sector
              NWIR = 0                      ! Number of wires hit
              DO 50 WIRE = 0,9
                CALL DFSTRK_FDC(HALF,UNIT,QUAD,SECT,WIRE,NPULSE,HITLST)
C                NHIT(WIRE) = NPULSE         ! Store number of hits in that 
C                                             wire, to be used by FDLC_L2
                IF (NPULSE.GT.0) THEN
                  TPULSE = TPULSE + NPULSE
                  NWIR = NWIR + 1
                ENDIF
   50         CONTINUE                      ! End of Wire Loop
C              
              IF (TPULSE.GE.6.AND.NWIR.GE.5) THEN
                NTHETA(HALF) = NTHETA(HALF) + 1
C
                IF (FIRST) THEN
                  FIRST = .FALSE.
                  HALF1 = HALF
                  UNIT1 = UNIT
                  QUAD1 = QUAD
                  SECT1 = SECT
                ELSE
                  HALF2 = HALF
                  UNIT2 = UNIT
                  QUAD2 = QUAD
                  SECT2 = SECT
                ENDIF
C
C-     determine whether two sectors overlap    
C
                IF (HALF2.EQ.2) GOTO 40
                IF (.NOT.(HALF_MID.EQ.2)) THEN
                  CALL FOVRLP(HALF_MID,UNIT_MID,QUAD_MID,SECT_MID,
     &                  HALF2,UNIT2,QUAD2,SECT2,1,NSECT,SOVRLP,CHKOVL)
                  IF (CHKOVL) THEN
                    NOVLP = NOVLP + 1
                  ENDIF
                ENDIF
                
                CALL FOVRLP(HALF1,UNIT1,QUAD1,SECT1,HALF2,UNIT2,QUAD2,
     &                      SECT2,1,NSECT,SOVRLP,CHKOVL)
                
                HALF_MID = HALF2
                UNIT_MID = UNIT2
                QUAD_MID = QUAD2
                SECT_MID = SECT2

                IF (CHKOVL) THEN
                  NOVLP = NOVLP + 1
                ENDIF
       
              ENDIF
   40       CONTINUE                        ! End of Sector Loop
   30     CONTINUE                          ! End of Quadrant Loop
C
        ELSE
C
C------ for phi chamber ----------------------------------------------
C
          DO 60 SECT = 0,35
            NPULSE = 0                      ! Number of hits in a wire   
            TPULSE = 0                      ! Total number of hits in a sector
            NWIR = 0                        ! Number of wires hit             
            DO 70 WIRE = 0,15
              CALL DFSTRK_FDC(HALF,UNIT,QUAD,SECT,WIRE,NPULSE,HITLST)
              IF (NPULSE.GT.0) THEN
                TPULSE = TPULSE + NPULSE
                NWIR = NWIR + 1
              ENDIF
   70       CONTINUE                        ! End of Wire Loop
            IF (TPULSE.GE.11.AND.NWIR.GE.10) THEN
              NPHI(HALF) = NPHI(HALF) + 1
C---------------------------------------------------------------------

              IF (FIRST) THEN  
                FIRST = .FALSE.
                HALF1 = HALF   
                UNIT1 = UNIT   
                QUAD1 = QUAD   
                SECT1 = SECT   
              ELSE                           
                HALF2 = HALF
                QUAD2 = QUAD
                UNIT2 = UNIT
                SECT2 = SECT
              ENDIF 
          
              IF (HALF2.EQ.2) GOTO 60
              IF (.NOT.(HALF_MID.EQ.2)) THEN                        
                CALL FOVRLP(HALF_MID,UNIT_MID,QUAD_MID,SECT_MID,    
     &                HALF2,UNIT2,QUAD2,SECT2,1,NSECT,SOVRLP,CHKOVL)
                IF (CHKOVL) THEN                                    
                  NOVLP = NOVLP + 1                                 
                ENDIF                                               
              ENDIF                                                 
              
              CALL FOVRLP(HALF1,UNIT1,QUAD1,SECT1,HALF2,UNIT2,QUAD2,
     $                    SECT2,1,NSECT,SOVRLP,CHKOVL)

              HALF_MID = HALF2
              UNIT_MID = UNIT2
              QUAD_MID = QUAD2
              SECT_MID = SECT2

              IF (CHKOVL) THEN
                NOVLP = NOVLP + 1
              ENDIF
            ELSE IF (TPULSE.LT.11.AND.TPULSE.GE.3) THEN
              NUM_HALF_PHI(HALF) = NUM_HALF_PHI(HALF) + 1
            ENDIF  
   60     CONTINUE                          ! End of Sector Loop

        ENDIF

   20   CONTINUE                            ! End of Unit Loop

        TSCH(HALF) = NTHETA(HALF) + NPHI(HALF)
C
   10 CONTINUE
C
      FINAL(0) = ((NTHETA(0).GE.2.AND.NTHETA(0).LE.3.AND.
     &  NUM_HALF_PHI(0).EQ.0.AND.NPHI(0).EQ.0).AND.(TSCH(1).EQ.0))
      FINAL(1) = ((NTHETA(1).GE.2.AND.NTHETA(1).LE.3.AND.
     &  NUM_HALF_PHI(1).EQ.0.AND.NPHI(1).EQ.0).AND.(TSCH(0).EQ.0))
C
      FILTER(0) = ((NOVLP.GE.1).AND.(TSCH(0).GE.2.OR.TSCH(1).GE.2))
      FILTER(1) = ((TSCH(0).LE.11).AND.(TSCH(1).LE.11))
      FILTER(2) = .NOT.(FINAL(0).OR.FINAL(1))
C
C------ Final decision ---------------------------------------------
C
      IF (FILTER(0).AND.FILTER(1).AND.FILTER(2)) THEN
        FDC_FLAG = .TRUE.
C        OPEN(50,file='pass.txt',status='new',iostat=iok)
C        WRITE(50,777) 'Event #',IQ(LHEAD+9),'PASSED' 
        CLOSE(50)
      ENDIF
C
  999 RETURN
      END



