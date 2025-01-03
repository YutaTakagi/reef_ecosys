
!!!=== Copyright (c) 2017-2025 Takashi NAKAMURA  =====
!!!  edited by Masa Muto 2023

!--------------------------------------------------------------------------------
!
!              Output module
!
!--------------------------------------------------------------------------------

#include "cppdefs.h"

MODULE mod_output

contains

! **********************************************************************
!  Open files
! **********************************************************************
  SUBROUTINE files_open
  
    implicit none

    character(256) :: OUTFILE_prefix

    namelist/output/OUTFILE_prefix
  
    read (5, nml=output)
    rewind(5)
  
#if defined CHAMBER_SITE4
    open(10,file=trim(OUTFILE_prefix)//'_site04-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'_site04-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'_site04-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'_site04-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'_site04-crl2_ave.csv')
    open(31,file=trim(OUTFILE_prefix)//'_site04-zoo1_his.csv')
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'_site04-ecosys_his.csv')
# endif
#elif defined CHAMBER_SITE5
    open(10,file=trim(OUTFILE_prefix)//'_site05-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'_site05-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'_site05-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'_site05-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'_site05-crl2_ave.csv')
    open(31,file=trim(OUTFILE_prefix)//'_site05-zoo1_his.csv')
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'_site05-ecosys_his.csv')
# endif
#elif defined CHAMBER_SITE6
    open(10,file=trim(OUTFILE_prefix)//'_site06-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'_site06-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'_site06-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'_site06-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'_site06-crl2_ave.csv')
    open(31,file=trim(OUTFILE_prefix)//'_site06-zoo1_his.csv')
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'_site06-ecosys_his.csv')
# endif
#elif defined CHAMBER_SITE7
    open(10,file=trim(OUTFILE_prefix)//'_site07-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'_site07-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'_site07-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'_site07-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'_site07-crl2_ave.csv')
    open(31,file=trim(OUTFILE_prefix)//'_site07-zoo1_his.csv')
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'_site07-ecosys_his.csv')
# endif
#elif defined CHAMBER_SITE9
    open(10,file=trim(OUTFILE_prefix)//'_site09-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'_site09-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'_site09-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'_site09-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'_site09-crl2_ave.csv')
    open(31,file=trim(OUTFILE_prefix)//'_site09-zoo1_his.csv')
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'_site09-ecosys_his.csv')
# endif
#elif defined CHAMBER_SITE10
    open(10,file=trim(OUTFILE_prefix)//'_site10-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'_site10-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'_site10-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'_site10-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'_site10-crl2_ave.csv')
    open(31,file=trim(OUTFILE_prefix)//'_site10-zoo1_his.csv')
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'_site10-ecosys_his.csv')
# endif


#else
    open(10,file=trim(OUTFILE_prefix)//'-env_his.csv')
# if defined CORAL_TESTMODE
    open(11,file=trim(OUTFILE_prefix)//'-crl1_his.csv')
    open(12,file=trim(OUTFILE_prefix)//'-crl2_his.csv')
    open(21,file=trim(OUTFILE_prefix)//'-crl1_ave.csv')
    open(22,file=trim(OUTFILE_prefix)//'-crl2_ave.csv')
#  if defined CORAL_ZOOXANTHELLAE
    open(31,file=trim(OUTFILE_prefix)//'-zoo1_his.csv')
    open(32,file=trim(OUTFILE_prefix)//'-zoo2_his.csv')
    open(41,file=trim(OUTFILE_prefix)//'-zphot1_his.csv')
    open(42,file=trim(OUTFILE_prefix)//'-zphot2_his.csv')
#  endif
# endif
# if defined ECOSYS_TESTMODE
    open(40,file=trim(OUTFILE_prefix)//'-ecosys_his.csv')
# endif
# if defined REEF_FLOW_TESTMODE
    open(45,file=trim(OUTFILE_prefix)//'-flow_his.csv')
# endif

#endif
      
#if defined SEDIMENT_TESTMODE
    open(50,file=trim(OUTFILE_prefix)//'-sed_last_data.csv')!!!!!!!!!!!!!!!!!!!for debug
    open(62,file=trim(OUTFILE_prefix)//'-sedTmp_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    open(80,file=trim(OUTFILE_prefix)//'-sedSal_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(56,file=trim(OUTFILE_prefix)//'-sedDIC_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    open(57,file=trim(OUTFILE_prefix)//'-sedTA_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    open(58,file=trim(OUTFILE_prefix)//'-sedDO_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    ! open(59,file=trim(OUTFILE_prefix)//'-sedpH_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(60,file=trim(OUTFILE_prefix)//'-sedWarg_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    open(61,file=trim(OUTFILE_prefix)//'-sedNH4_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(63,file=trim(OUTFILE_prefix)//'-sedNO3_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(64,file=trim(OUTFILE_prefix)//'-sedPO4_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(65,file=trim(OUTFILE_prefix)//'-sedLDOC_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(66,file=trim(OUTFILE_prefix)//'-sedLPOC_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    ! open(67,file=trim(OUTFILE_prefix)//'-sedDON_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(68,file=trim(OUTFILE_prefix)//'-sedPON_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(69,file=trim(OUTFILE_prefix)//'-sedDOP_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(70,file=trim(OUTFILE_prefix)//'-sedPOP_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(71,file=trim(OUTFILE_prefix)//'-sedPg_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(72,file=trim(OUTFILE_prefix)//'-sedRdoc_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(73,file=trim(OUTFILE_prefix)//'-sedRpoc_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(74,file=trim(OUTFILE_prefix)//'-sedGn_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(75,file=trim(OUTFILE_prefix)//'-sedNit1_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(76,file=trim(OUTFILE_prefix)//'-sedNit2_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(78,file=trim(OUTFILE_prefix)//'-sedDNd_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    ! open(79,file=trim(OUTFILE_prefix)//'-sedDNp_his.csv')!!!!!!!!!!!!!!!!!!!for debug

    !! MM edit 2023
!    open(81,file=trim(OUTFILE_prefix)//'-sedN2_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(82,file=trim(OUTFILE_prefix)//'-sedRDOC_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(83,file=trim(OUTFILE_prefix)//'-sedRPOC_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(84,file=trim(OUTFILE_prefix)//'-sedMn2_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(85,file=trim(OUTFILE_prefix)//'-sedMnO2_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(86,file=trim(OUTFILE_prefix)//'-sedFe2_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(87,file=trim(OUTFILE_prefix)//'-sedFeS_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(88,file=trim(OUTFILE_prefix)//'-sedFeS2_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(89,file=trim(OUTFILE_prefix)//'-sedFeOOH_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(90,file=trim(OUTFILE_prefix)//'-sedFeOOH_PO4_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(91,file=trim(OUTFILE_prefix)//'-sedH2S_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    ! open(92,file=trim(OUTFILE_prefix)//'-sedHS_his.csv')!!!!!!!!!!!!!!!!!!!for debug
    open(93,file=trim(OUTFILE_prefix)//'-sedS0_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    open(94,file=trim(OUTFILE_prefix)//'-sedSO4_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
    ! open(95,file=trim(OUTFILE_prefix)//'-sedS2_his.csv')!!!!!!!!!!!!!!!!!!!for debug

    open(96,file=trim(OUTFILE_prefix)//'-calcheck0.csv',status='replace')!!!!!!!!!for debug
    open(97,file=trim(OUTFILE_prefix)//'-calcheck1.csv',status='replace')!!!!!!!!!for debug
!    open(98,file=trim(OUTFILE_prefix)//'-redoxlevel.csv',status='replace')!!!!!!!!!for debug
    open(99,file=trim(OUTFILE_prefix)//'-depth.csv',status='replace')!!!!!!!!!for debug
!    open(100,file=trim(OUTFILE_prefix)//'-sedPOCn_his.csv',status='replace')!!!!!!!!!!!!!!!!!!!for debug
!    open(101,file=trim(OUTFILE_prefix)//'-v1.csv',status='replace')!!!!!!!!!for debug
!    open(102,file=trim(OUTFILE_prefix)//'-v2.csv',status='replace')!!!!!!!!!for debug
!    open(103,file=trim(OUTFILE_prefix)//'-v3.csv',status='replace')!!!!!!!!!for debug
!    open(104,file=trim(OUTFILE_prefix)//'-v4.csv',status='replace')!!!!!!!!!for debug
!    open(105,file=trim(OUTFILE_prefix)//'-v5.csv',status='replace')!!!!!!!!!for debug
!    open(106,file=trim(OUTFILE_prefix)//'-vom.csv',status='replace')!!!!!!!!!for debug
    open(107,file=trim(OUTFILE_prefix)//'-R_O2.csv',status='replace')!!!!!!!!!for debug
    open(108,file=trim(OUTFILE_prefix)//'-flux_DO.csv',status='replace')!!!!!!!!!for debug
#endif
    RETURN

END SUBROUTINE files_open

! **********************************************************************
!  Close files
! **********************************************************************
  SUBROUTINE files_close
  
    implicit none
    
    close(10)
#if defined CORAL_TESTMODE
    close(11)
    close(12)
    close(21)
    close(22)
# if defined CORAL_ZOOXANTHELLAE
    close(31)
    close(32)
    close(41)
    close(42)
# endif
#endif
#if defined ECOSYS_TESTMODE
    close(40)
#endif
#if defined REEF_FLOW_TESTMODE
    close(45)
#endif
#if defined SEDIMENT_TESTMODE
    close(50)
    ! close(56)
    ! close(57)
    close(58)
    ! close(59)
    ! close(60)
    close(61)
    ! close(62)
    close(63)
    close(64)
    close(65)
    close(66)
    ! close(67)
    ! close(68)
    ! close(69)
    ! close(70)
    ! close(71)
    ! close(72)
    ! close(73)
    ! close(74)
    ! close(75)
    ! close(76)
    ! close(78)
    ! close(79)
    close(80)
    close(81)
    close(82)
    close(83)
    close(84)
    close(85)
    close(86)
    close(87)
    close(88)
    close(89)
    close(90)
    close(91)
    ! close(92)
    close(93)
    close(94)
    ! close(95)
    close(96)
    close(97)
    close(98)
    close(99)
    close(100)
    close(101)
    close(102)
    close(103)
    close(104)
    close(105)
    close(106)
    close(107)
    close(108)

#endif

    RETURN

END SUBROUTINE files_close

! **********************************************************************
!  Write lavel of environmental data
! **********************************************************************

  SUBROUTINE write_env_lavel(fid)
  
    USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
    write(fid,'(*(g0:,", "))') 'time', 'PFDsurf'                           &
        ,'Temp','Salt','TA','DIC','DO'                                     &
        ,'DOC1','DOC2','POC1','POC2','POC3','Phyt1'                        &
        ,'Phyt2','Phyt3', 'Phyt4'                                          &
        ,'TotalPhy', 'Zoop','PIC'                                          &
        ,'NO3','NH4','PO4'                                                 &
        ,'DON1','DON2','PON1','PON2','PON3'                                &
        ,'DOP1','DOP2','POP1','POP2','POP3'                                &
#if defined CARBON_ISOTOPE
        ,'d13C_DIC'                                                        &
#endif
#if defined COT_STARFISH
        ,'COT','COTl'                                                      &
#endif
#if defined REEF_FLOW
        ,'Q_rc', 'Q_ch','el_reef'                                          &
#endif
        ,'dz', 'el'
      

    RETURN

  END SUBROUTINE write_env_lavel

! **********************************************************************
!  Write lavel of reef_flow data
! **********************************************************************

  SUBROUTINE write_flow_lavel(fid)
  
    USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
    write(fid,'(*(g0:,", "))') 'time'   &
    , 'Hs_o', 'Tp', 'd_o', 'd_i'        & 
    , 'el_o', 'reef_el'                 &
    , 'Qrc', 'Qch'

    RETURN

  END SUBROUTINE write_flow_lavel

! **********************************************************************
!  Write timeseries environmental data
! **********************************************************************

  SUBROUTINE write_env_data(fid)
  
    USE mod_param
#ifdef CORAL_POLYP
    USE mod_coral
#endif
#ifdef SEAGRASS
    USE mod_seagrass
#endif
#ifdef MACROALGAE
    USE mod_macroalgae
#endif
#ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
#endif
#if defined CARBON_ISOTOPE
    USE mod_geochem
#endif
#if defined REEF_FLOW
      USE mod_reef_flow
#endif
      
    implicit none
      
    integer, intent(in) :: fid
#if defined CARBON_ISOTOPE
    real(8) :: d13C_DIC

    d13C_DIC=d13C_fromR13C(C(1,1,1,iT13C)/C(1,1,1,iTIC_))
#endif
        
    write(fid,'(*(g0:,", "))') time, PFDsurf                           &
           , C(1,1,1,iTemp), C(1,1,1,iSalt)                            &
           , C(1,1,1,iTAlk), C(1,1,1,iTIC_), C(1,1,1,iOxyg)            &
           , C(1,1,1,iDOC(1)), C(1,1,1,iDOC(2))                        &
           , C(1,1,1,iPOC(1)), C(1,1,1,iPOC(2)), C(1,1,1,iPOC(3))      &
           , C(1,1,1,iPhyt(1)), C(1,1,1,iPhyt(2)), C(1,1,1,iPhyt(3))   &
           , C(1,1,1,iPhyt(4))                                         &
           , C(1,1,1,iPhyt(1))+C(1,1,1,iPhyt(2))+C(1,1,1,iPhyt(3))+C(1,1,1,iPhyt(4)) &
           , C(1,1,1,iZoop(1)), C(1,1,1,iPIC(1))                       &
           , C(1,1,1,iNO3_), C(1,1,1,iNH4_), C(1,1,1,iPO4_)            &
           , C(1,1,1,iDON(1)), C(1,1,1,iDON(2))                        &
           , C(1,1,1,iPON(1)), C(1,1,1,iPON(2)), C(1,1,1,iPON(3))      &
           , C(1,1,1,iDOP(1)), C(1,1,1,iDOP(2))                        &
           , C(1,1,1,iPOP(1)), C(1,1,1,iPOP(2)), C(1,1,1,iPOP(3))      &
#if defined CARBON_ISOTOPE
           , d13C_DIC                                                      &
#endif
#if defined COT_STARFISH
           , C(1,1,1,iCOTe), C(1,1,1,iCOTl)                            &
#endif
#if defined REEF_FLOW
           , REEF(1)%Qrc(1,1), REEF(1)%Qch(1,1), REEF(1)%el (1,1)          &
#endif
           , dz(1,1,1), tide 


    RETURN

  END SUBROUTINE write_env_data
    
#if defined CORAL_TESTMODE
! **********************************************************************
!  Write lavel of coral internal conditions
! **********************************************************************

  SUBROUTINE write_crl_his_lavel(fid)
  
    USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
    write(fid,'(*(g0:,", "))') 'time', 'PFD'                  &
       ,'Pg', 'R', 'Pn', 'G','QC'                            &
       ,'TAcal',  'TAcoe',  'TAamb'                          &
       ,'DICcal', 'DICcoe', 'DICamb'                         &
       ,'DOcoe', 'DOamb'                                     &
       ,'pHcal','pHcoe','pHamb','Wacal','Waamb'              &
       ,'fCO2cal','fCO2coe','fCO2amb'                        &
       ,'CO2aqcal','HCO3cal','CO3cal'                        &
       ,'CO2aqcoe','HCO3coe','CO3coe'                        &
# if defined CORAL_CARBON_ISOTOPE
       ,'d13C_DICamb','d13C_DICcoe','d13C_QC','d13C_DICcal'  &
       ,'d13C_arg','d13C_argxGn'                             &
       ,'d13C_CO2aqcal','d13C_HCO3cal','d13C_CO3cal'         &
       ,'d13C_CO2aqcoe','d13C_HCO3coe','d13C_CO3coe'         &
       ,'c13CO2aqcal','cH13CO3cal','c13CO3cal'               &
       ,'c13CO2aqcoe','cH13CO3coe','c13CO3coe'               &
# endif
# if defined CORAL_ZOOXANTHELLAE
       ,'ROS','Rz', 'F_Csec', 'F_ROS','F_detox','F_dam'      &
# endif
# if defined CORAL_BORON_ISOTOPE
       ,'d11Barg'                                            &
# endif
       ,'DOCuptake'                                          &
# if defined CORAL_INGESTION
       ,'ZOOuptake'                                          &
# endif
# if defined CORAL_SIZE_DYNAMICS
       ,'growth','mort','Damage','F_Cgrowth'                 &
# endif
       ,'E_ca','F_Cgrowth','Si_DIC','Si_DIC'

    RETURN

  END SUBROUTINE write_crl_his_lavel
    
    
  SUBROUTINE write_crl_ave_lavel(fid)
  
    USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
      write(fid,'(*(g0:,", "))') 'day'  &
       ,'S_PFD_dt'                     &   ! Photon flux density (mol m-2 d-1)
# if defined CORAL_CARBON_ISOTOPE
!       ,'S_d13CargxGn_dt'              &
       ,'d13Carg'                      &   ! d13C
       ,'S_d13C_QC_dt'                 &   ! 1 day avaraged value of d13C_QC
# endif
# if defined CORAL_BORON_ISOTOPE
       ,'d11Barg'                      & 
# endif
       ,'S_Gn_dt'                      &   ! Calcification rate (umol cm-2 d-1)
       ,'S_Pg_dt'                      &   ! Gross photosynthesis rate (umol cm-2 d-1)
       ,'S_R_dt'                       &   ! Respiration rate (umol cm-2 d-1)
       ,'S_QC_dt'                      &   ! 1 day avaraged value of QC
       ,'S_Pn_dt'                          ! Net photosynthesis rate (umol cm-2 d-1)

    RETURN

END SUBROUTINE write_crl_ave_lavel

# if defined CORAL_ZOOXANTHELLAE
  SUBROUTINE write_zox_his_lavel(fid)
  
    USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
    write(fid,'(*(g0:,", "))')                &
       'time', 'PFD'                          &
       ,'Pg','R','Pn','dens'                  &
       ,'QC', 'Chl'                           &
       ,'Cg_bio','Cn_bio','Repro','Morta','F_zROS','F_Csec' &
       ,'F_Zelm','Repro*dens','Morta*dens'    &
#  if defined CORAL_NUTRIENTS
       ,'QN','QP'                             &
       ,'N_repro','P_repro'                   &
       ,'NO3'                                 &
       ,'NH4','F_ONOO'                        &
       ,'NO3_trans','NH4_trans'               &
       ,'NO3_reduc'                           &
       ,'N_assim','N_dissim'                  &
       ,'PO4','PO4_trans','P_assim'           &
#  endif
       ,'ROS','ROS_pmol_cell-1'               &
       ,'Chl_ug_cm-2'

    RETURN

  END SUBROUTINE write_zox_his_lavel
    
  SUBROUTINE write_zphot_his_lavel(fid)
    
    implicit none
    
    integer, intent(in) :: fid
    
    write(fid,'(*(g0:,", "))')                                  &
        'time','PFD','Tamb'                                     &
       ,'QAo','QAr','QAi'                                       &
       ,'QAt','QAa','Fv/Fm','Y(II)','J_ep_in'                   &
       ,'Ji2a','Ja2i','kr','s_ab','F_zSOx','SOx'                &
       ,'Pg','Pgcal','Pgmax','F_zDO'                            &
       ,'Chl','F_QAd','F_QAp','NADPH','RuBP','F_zROS','ROS'
     
    RETURN

  END SUBROUTINE write_zphot_his_lavel

# endif
#endif
#if defined ECOSYS_TESTMODE
! **********************************************************************
!  Write lavel of coral internal conditions
! **********************************************************************

  SUBROUTINE write_ecosys_his_lavel(fid)
  
    USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
    write(fid,'(*(g0:,", "))') 'time', 'PFDbott'              &
# ifdef CORAL_POLYP
        ,'coral1_Pg', 'coral1_R', 'coral1_Pn', 'coral1_G'    &
        ,'coral2_Pg', 'coral2_R', 'coral2_Pn', 'coral2_G'    &
# endif
# ifdef SEAGRASS
        ,'sgrass_Pg', 'sgrass_R', 'sgrass_Pn'                &
# endif
# ifdef MACROALGAE
        ,'algae_Pg' , 'algae_R' , 'algae_Pn'                 &
# endif
# ifdef SEDIMENT_ECOSYS
!        ,'sedeco_Pg', 'sedeco_R', 'sedeco_Pn', 'sedeco_G'    &
# endif
        ,'dDIC_dt','dTA_dt','dDO_dt'                         &
        ,'dDOC_dt','dPOC_dt'                                 &
# if defined CARBON_ISOTOPE
        ,'dDI13C_dt'                                         &
# endif
        ,'dNO3_dt','dNH4_dt'                                 &
        ,'dPO4_dt'                                           &
        ,'dDON_dt','dPON_dt','dDOP_dt','dPOP_dt'             &
        ,'pH', 'fCO2', 'Warg', 'Wcal'                        &
        ,'U10', 'CO2flux' , 'O2flux'

    RETURN

  END SUBROUTINE write_ecosys_his_lavel
#endif
#if defined SEDIMENT_TESTMODE
! **********************************************************************
!  Write lavel of coral internal conditions
! **********************************************************************
  SUBROUTINE write_sedeco_lavel(fid)
  
    ! USE mod_param
    
    implicit none
    
    integer, intent(in) :: fid
    
    ! write(fid,'(*(g0:,", "))') 'time', 'Temp', 'Salt' , 'O2'                         &
    !     ,'Dw_O2','Ds_O2','Fd_O2','Fb_O2','Fc_O2','FT_O2','v1','R_O2'

    ! write(fid,'(*(g0:,", "))') 'time', 'Temp', 'Salt' , 'DOCf'                         &
    !     ,'Dw_DOCf','Ds_DOCf','Fd_DOCf','Fb_DOCf','Fc_DOCf','FT_DOCf','vdomf','R_DOCf'

    write(fid,'(*(g0:,", "))') 'time', 'Temp', 'vOM','v1', 'v2', 'v3', 'v4', 'v5'

    RETURN

  END SUBROUTINE write_sedeco_lavel
#endif
! **********************************************************************
!  Write last sediment data
! **********************************************************************
SUBROUTINE write_sedeco_last_label(fid)
  
    USE mod_reef_ecosys_param
#ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
#endif
      
    implicit none
      
    integer, intent(in) :: fid

    write(fid,'(*(g0:,", "))') 'zr'   &
      , 'Tmp'                         &
      , 'Sal'                         & 
      , 'TA '                         & 
      , 'O2 '                         & 
      , 'DIC'                         & 
      , 'LDOC', 'RDOC'                &
      , 'LPOC', 'RPOC'                &
      , 'NO3'                         &
      , 'NH4'                         &
      , 'PO4'                         &
      , 'LDON', 'RDON'                &
      , 'LDOP', 'RDOP'                &
      , 'LPON', 'RPON'                &
      , 'LPOP', 'RPOP'                &
      , 'Mn2 '                        &
      , 'MnO2'                        &
      , 'Fe2 '                        &
      , 'FeS2'                        &
      , 'FeOOH'                       &
      , 'FeOOH_PO4'                   &
      , 'H2S'                         &
      , 'SO4'                         &
      , 'S0 '   
    
    RETURN

  END SUBROUTINE write_sedeco_last_label


  SUBROUTINE write_sedeco_last_data(fid)
  
    USE mod_reef_ecosys_param
#ifdef SEDIMENT_ECOSYS
    USE mod_sedecosys
#endif
      
    implicit none
      
    integer, intent(in) :: fid
    real(8) :: zr(0:Nsed)
    integer :: i

    zr(0) = -1.0d0
    zr(1:Nsed) = SEDECO(1)%zr(:)
    
    DO i=0,Nsed
      write(fid,'(*(g0:,", "))') zr(i)   &
      , SEDECO(1)%Tmp(i,1,1)         &
      , SEDECO(1)%Sal(i,1,1)         & 
      , SEDECO(1)%TA (i,1,1)         & 
      , SEDECO(1)%O2 (i,1,1)         & 
      , SEDECO(1)%DIC(:,i,1,1)       & 
      , SEDECO(1)%DOC(:,:,i,1,1)     &
      , SEDECO(1)%POC(:,:,i,1,1)     &
      , SEDECO(1)%NO3(:,i,1,1)       &
      , SEDECO(1)%NH4(:,i,1,1)       &
      , SEDECO(1)%PO4(:,i,1,1)       &
      , SEDECO(1)%DON(:,:,i,1,1)     &
      , SEDECO(1)%DOP(:,:,i,1,1)     &
      , SEDECO(1)%PON(:,:,i,1,1)     &
      , SEDECO(1)%POP(:,:,i,1,1)     &
      , SEDECO(1)%Mn2 (i,1,1)        &
      , SEDECO(1)%MnO2(i,1,1)        &
      , SEDECO(1)%Fe2 (i,1,1)        &
      , SEDECO(1)%FeS2(:,i,1,1)      &
      , SEDECO(1)%FeOOH(i,1,1)       &
      , SEDECO(1)%FeOOH_PO4(:,i,1,1) &
      , SEDECO(1)%H2S(:,i,1,1)       &
      , SEDECO(1)%SO4(:,i,1,1)       &
      , SEDECO(1)%S0 (:,i,1,1)   
    END DO
    
    RETURN

  END SUBROUTINE write_sedeco_last_data

END MODULE mod_output

