C----------------------------------------------------------------------
C-
C-   Updated  28-SEP-1990   Jan S. Hoftun
C-   Updated   2-OCT-1990   Jan Hoftun  (fix STAMAX etc)
C-
C----------------------------------------------------------------------
C
C      Definitions for all L2CONTROL type programs
C
      INTEGER MAXPOSSIBLE
      PARAMETER (MAXPOSSIBLE=50)
      INTEGER MAXCHAR
      PARAMETER (MAXCHAR=MAXPOSSIBLE*4)
      INTEGER MAXBIT
      PARAMETER (MAXBIT=MAXPOSSIBLE*8)          
      INTEGER MAXL1
      PARAMETER (MAXL1=64)                      
                                                
      INTEGER MAXTOOL_SCRIPT
      PARAMETER (MAXTOOL_SCRIPT=MAXTOOL)             
      INTEGER MAXTOOL_COUNT
      PARAMETER (MAXTOOL_COUNT=MAXTOOL)              
                                                     
                                                     
      INTEGER STAMAX
      PARAMETER (STAMAX=4)
      INTEGER FILE_MAX
      PARAMETER (FILE_MAX=6)
      INTEGER RUNRECORD
      PARAMETER (RUNRECORD=23040) 
C                        128*4*2+2*MAXTOOL_SCRIPT*128+128+EXTRA)  
C                                                          
      LOGICAL*1 RUNOK,RUNPAU,BEGOK,SPLMOD,TIMMOD,DUMP_ON,COOR_OK
      INTEGER RUNNUM,MAXL2
      INTEGER RUNINP,RUNOLD
      INTEGER INFIL,INFIL1,INFIL2,TYPMAX,TOOLMAX,INFIL3
      CHARACTER*6 SUPNAM,SURVNAM,L2NAME(MAXPOSSIBLE)
      CHARACTER*64 COMMNT
      CHARACTER*(MAXCHAR) OLDMASK
      CHARACTER*32 OLDDP
      CHARACTER*(MAXBIT) TOTBIT
      CHARACTER*(128*4) OLD_L2NOFILT
      CHARACTER*(MAXL1*4*4) OLD_FILT_BIT_SET
      CHARACTER*(2*MAXTOOL_SCRIPT*128) OLD_FILTER_SCRIPT
      CHARACTER*128 OLD_FILTER_FORCE
      CHARACTER*(32*8*4) OLD_SEQ_CRATES
      CHARACTER*(MAXTOOL) OLD_NEW_PARAMS
      CHARACTER*32 FILE_CONTROL(FILE_MAX)
      CHARACTER*16 SUPER_TALK_OBJECT/'SUPER_IN'/
      CHARACTER*16 SUPER_CTRL_OBJECT/'SUPER_IN'/
      CHARACTER*16 SURV_TALK_OBJECT/'SURV_IN'/
      CHARACTER*64 RUNNUM_NAME,RUNOLD_NAME
      INTEGER IN_L2NOFILT(0:127),IN_FILT_BIT_SET(4,0:(MAXL1-1)),
     &        IN_NODE_MASK(MAXPOSSIBLE),IN_SEQ_CRATES(8,0:31)
      BYTE IN_DPMASK(0:31),IN_FILTER_SCRIPT(2,MAXTOOL_SCRIPT,0:127),
     &  IN_NEW_PARAMS(MAXTOOL)
      LOGICAL*1 IN_FILTER_FORCE(0:127)
      EQUIVALENCE (OLD_L2NOFILT,IN_L2NOFILT),
     *            (OLD_FILT_BIT_SET,IN_FILT_BIT_SET),
     *            (OLD_FILTER_SCRIPT,IN_FILTER_SCRIPT),
     *            (OLD_FILTER_FORCE,IN_FILTER_FORCE),
     *            (OLDDP,IN_DPMASK),
     *            (OLDMASK,IN_NODE_MASK),
     *            (OLD_SEQ_CRATES,IN_SEQ_CRATES),
     *            (OLD_NEW_PARAMS,IN_NEW_PARAMS)
      RECORD/L2_TYPE/L2_TYPE_DEF(MAXTYP)
      RECORD/L2_TOOL/L2_TOOL_DEF(MAXTOOL)
      INTEGER L2_TYPARR(MAXPOSSIBLE)
      LOGICAL NOT_READOK/.TRUE./
      INTEGER MAXMIC
      CHARACTER*6 MICNAM(MAXPOSSIBLE)
      INTEGER*2 NAMLEN(MAXPOSSIBLE)
      BYTE STATE(MAXPOSSIBLE),NTYPE(MAXPOSSIBLE),
     &     DP_BIT_BY_CABLE_AND_NODE(0:7,MAXPOSSIBLE),
     &     MPM_TYPE(MAXPOSSIBLE)
      CHARACTER*27 COMMENT(MAXPOSSIBLE)
      CHARACTER*12 STATE_TYPE_DEF(0:STAMAX-1)
      LOGICAL WRITE_STATE,WRITE_TYPE,WRITE_TOOL,WRITE_HARDW,
     &        LOAD_TYPE(MAXTYP),LOAD_NODES(MAXPOSSIBLE),LOAD_FLAG,
     &        MORE_DISPLAY,INTERRUPT_SET
      STRUCTURE/L2_SUPER_INFO/
      UNION
        MAP
          INTEGER TOT_EVENTS(6),TRIG_COUNT(0:31),FIB_COUNTS(5),ITIME(2)
        END MAP
        MAP
          CHARACTER*(4*6+32*4+5*4+8) L2_SUPER_TOTAL
        END MAP
      END UNION
      END STRUCTURE
      STRUCTURE/L2_SEQR_INFO/
      UNION
        MAP
          INTEGER FIB_COUNTS(5),SEQ_COUNTS(8,0:7)
        END MAP
        MAP
          CHARACTER*(4*5+8*8*4) L2_SEQR_TOTAL
        END MAP
      END UNION
      END STRUCTURE
      STRUCTURE/L2_NODES_INFO/
      UNION
        MAP
          INTEGER FILTER_COUNT(3,0:127),TOTAL_COUNT(3)
        END MAP
        MAP
          CHARACTER*(FILT_REP_COUNT) L2_NODES_TOTAL 
        END MAP
      END UNION
      END STRUCTURE
      STRUCTURE/L2_NODE_TIMER/
      UNION
        MAP
          INTEGER REP_TIME(2)
          RECORD/TIMER_RECORD/L2_TIMER_RECORD
        END MAP
        MAP
          CHARACTER*(TIME_REP_COUNT) L2_TIMER_TOTAL 
        END MAP
      END UNION
      END STRUCTURE
      RECORD/L2_SUPER_INFO/L2_SUPER_COUNT
      RECORD/L2_SEQR_INFO/L2_SEQR_COUNT
      RECORD/L2_NODES_INFO/L2_NODES_COUNT(MAXPOSSIBLE)
      RECORD/L2_NODE_TIMER/L2_NODE_TIMERS(MAXPOSSIBLE)
      RECORD/HARDWARE_TYPE/HARDWARE_SETUP
      BYTE IN_FILTER_ORDER(128)
      CHARACTER*128 OLD_FILTER_ORDER
      EQUIVALENCE (OLD_FILTER_ORDER,IN_FILTER_ORDER)
      INTEGER IN_FILTER_MAX
      CHARACTER*4 OLD_FILTER_MAX
      EQUIVALENCE (OLD_FILTER_MAX,IN_FILTER_MAX)
      LOGICAL RUN_FILTER_ON,SIGNAL_HARDCOPY,SIGNAL_BEGIN_RUN
      REAL UPDATE_SEC
      COMMON /L2CONTROL_COM/RUNOK,RUNPAU,RUNNUM,SUPNAM,COMMNT,
     *       BEGOK,MAXL2,L2NAME,OLDMASK,OLDDP,HARDWARE_SETUP,TOTBIT,
     *       OLD_L2NOFILT,OLD_FILT_BIT_SET,OLD_FILTER_SCRIPT,
     *       OLD_FILTER_FORCE,SPLMOD,TIMMOD,DUMP_ON,FILE_CONTROL,
     *       L2_TYPE_DEF,L2_TOOL_DEF,TYPMAX,TOOLMAX,RUNINP,RUNOLD,
     *       INFIL,INFIL1,INFIL2,INFIL3,COOR_OK,L2_TYPARR,NOT_READOK,
     *       L2_SUPER_COUNT,L2_NODES_COUNT,L2_SEQR_COUNT,
     &       MAXMIC,NAMLEN,MICNAM,STATE,NTYPE,COMMENT,
     &       DP_BIT_BY_CABLE_AND_NODE,STATE_TYPE_DEF,
     &       WRITE_STATE,WRITE_TYPE,WRITE_TOOL,WRITE_HARDW,
     &       LOAD_TYPE,LOAD_NODES,LOAD_FLAG,SUPER_TALK_OBJECT,
     &       RUNNUM_NAME,RUNOLD_NAME,MORE_DISPLAY,OLD_SEQ_CRATES,
     &       OLD_FILTER_MAX,OLD_FILTER_ORDER,OLD_NEW_PARAMS,MPM_TYPE,
     &       RUN_FILTER_ON,SUPER_CTRL_OBJECT,SURVNAM,SURV_TALK_OBJECT,
     &       INTERRUPT_SET,SIGNAL_HARDCOPY,SIGNAL_BEGIN_RUN,UPDATE_SEC,
     &       L2_NODE_TIMERS
      INTEGER MAXSTATE
      PARAMETER (MAXSTATE=13)
      CHARACTER*12 L2_STATES(0:MAXSTATE)/'WAIT/DATA','WAIT/DONE',
     &  'FILTERING','FRAMEWORK','WAIT/SEND','FLUSHED','WAIT/MP',
     &  'DATA/CHECK','CHECKSUMMING','INITIALIZING','READY', 'WAIT/ZBD',
     &  'NO-L2-MAIN','NETWRK ERROR'/