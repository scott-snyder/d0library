C
C	Parameter Definitions for the Data Acquisition
C	System Buffer Manager.
C
C	Headers
C	=======
C
      Parameter BFM_TABLE_WORD = 0	
      Parameter BFM_UNAPSE_FP  = 2	
      Parameter BFM_UNAPSE_BP  = 3	
C
      Parameter BFM_TGSPSE_FP  = 4      
      Parameter BFM_TGSPSE_BP  = 5	
      Parameter BFM_EVBPSE_FP  = 6	
      Parameter BFM_EVBPSE_BP  = 7      
      Parameter BFM_LV3PSE_FP  = 8	
      Parameter BFM_LV3PSE_BP  = 9	
      Parameter BFM_CONPSE_FP  = 10	
      Parameter BFM_CONPSE_BP  = 11     
C
      Parameter BFM_UNACON_FP  = 16	
      Parameter BFM_UNACON_BP  = 17	
      Parameter BFM_UNAREQ_FP  = 18	
      Parameter BFM_UNAREQ_BP  = 19	
      Parameter BFM_UNAEVT_FP  = 20	
      Parameter BFM_UNAEVT_BP  = 21	
      Parameter BFM_UNAMES_FP  = 22	
      Parameter BFM_UNAMES_BP  = 23	
      Parameter BFM_ACTMES_FP  = 24	
      Parameter BFM_ACTMES_BP  = 25	
      Parameter BFM_UNAEBM_FP  = 26	
      Parameter BFM_UNAEBM_BP  = 27	
      Parameter BFM_ACTEBM_FP  = 28	
      Parameter BFM_ACTEBM_BP  = 29	
C
      Parameter BFM_NUMB_PSECT = 32	
      Parameter BFM_NUMB_CONCT = 33	
      Parameter BFM_NUMB_REQCT = 34	
      Parameter BFM_NUMB_EVTCT = 35	
      Parameter BFM_NUMB_MESCT = 36	
      Parameter BFM_NUMB_EVBCT = 37	
C
      Parameter BFM_HEAD_WORD  = 40	
      Parameter BFM_PSECT_WORD = 41	
      Parameter BFM_CONCT_WORD = 42	
      Parameter BFM_EVTCT_WORD = 43	
      Parameter BFM_REQCT_WORD = 44	
      Parameter BFM_MESCT_WORD = 45	
      Parameter BFM_EVBCT_WORD = 46	
C
      Parameter BFM_MODE       = 48	
      Parameter BFM_ENUM       = 49	
      Parameter BFM_MESID      = 50	
C
      Parameter BFM_HEAD_LENG  = 56     
C
      Parameter BFM_LOG_NAME   = 'Buffer_Manager'
C
      Parameter BFM_MODE_ACTIVE= 0	
      Parameter BFM_MODE_TRACE = 1	
C
C	Pipeline Stages
C	===============
C
      Parameter BFM_TRIG_SUP 	= 0	
      Parameter BFM_EVENT_BUILD	= 1	
      Parameter BFM_LEVEL_3	= 2	
      Parameter BFM_CONSUMER	= 3	
C
C	Pipeline Stage Names
C	====================
C
      Parameter BFM_TGS_NAME 	= 4HTGS:
      Parameter BFM_EVB_NAME	= 4HEVB:
      Parameter BFM_LV3_NAME	= 4HLV3:
      Parameter BFM_CON_NAME	= 4HCON:
C
