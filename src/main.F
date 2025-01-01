
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====

#include "cppdefs.h"


PROGRAM main
! **********************************************************************
! *                                                                    *
! *   Test program of mod_reef_ecosys                                  *
! *                                                                    *
! **********************************************************************
!
  USE mod_param
#if defined FOODWEB
  USE mod_foodweb
#endif
  USE mod_reef_ecosys
#if defined USE_HEAT
  USE mod_heat
#endif
  USE mod_input
  USE mod_output
  USE mod_geochem
#if defined REEF_FLOW
  USE mod_reef_flow
#endif
#if defined SEDIMENT_ECOSYS
  USE mod_sedecosys
#endif

  implicit none
  
  integer, parameter :: Im = 1
  integer, parameter :: Jm = 1
  integer, parameter :: N  = 2

  real(8), parameter :: dt = 1.0d0
  integer, parameter :: isplitc  = 20
  integer, parameter :: isplitsed  = 20
  integer, parameter :: Ngrids = 1

  integer :: i,j,k,id, Nid
  integer :: nheat
  
  integer :: ipcl =1       ! Step of the protocol for setting 5 (Incubation chamber condition simulated Nakamura & Nakamori (2009) experiments)
  integer :: inight = 1 ! Timer for setting 6
  integer :: iflag = 1 ! Timer for setting 6
  integer :: iclose = 0 ! Timer for setting 6
  
#if defined CARBON_ISOTOPE
  real(8) :: R13C
#endif
!  For Output      
  real(8), parameter :: OUTPUT_INTERVAL = 60.0d0 !1d0/60d0 !60.0d0 !1.0d0     ! Output interval (min)
  real(8) :: dsec = 0.d0 !sec
  
  real(8) :: h1,h2
  real(8) :: fvol_rc, fvol_ch
  real(8) :: dC_dt_sgd

  integer :: in_file(12)
  real(8) :: Tmax, time0
  integer :: nSetting
#ifdef SEDIMENT_ECOSYS
!  integer :: Nsed
#endif
  logical :: start_of_new_run = .true.

  real(8) :: DIC(N_Csp,N)        

  namelist/input_info/in_file
  namelist/ecosys_config/Tmax, time0
  namelist/ecosys_config/nSetting

#ifdef SEDIMENT_ECOSYS
!  namelist/sedecosys_config/Nsed
#endif

  read (5, nml=input_info)
  rewind(5)
  read (5, nml=ecosys_config)
  rewind(5)
#ifdef SEDIMENT_ECOSYS
!  read (5, nml=sedecosys_config)
!  rewind(5)
#endif
  
!----- Open output files -------------------------

  CALL files_open
      
!----- Set initial conditions -------------------------

  CALL initialize_params(1, Im, 1, Jm, N, Nid)

!----- Import data -----------------------------------

  CALL read_data(in_file)

  if (nSetting .eq. 5) then
    CALL read_chambercondition
  endif

!----- Set initial conditions -------------------------

#if defined USE_HEAT
  CALL initialize_heat(1, Ngrids, 1, Im, 1, Jm)
#endif
#if defined REEF_FLOW
  CALL initialize_reef_flow(1, Ngrids, 1, Im, 1, Jm)
#endif
  CALL allocate_reef_ecosys(1, Ngrids, 1, Im, 1, Jm)
  CALL initialize_reef_ecosys(1, 1, Im, 1, Jm      &
      , start_of_new_run    &   ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run
#ifdef SEAGRASS
      , XI_size, ETA_size   &   ! grid size XI-direction (meters); grid size ETA-direction (meters)
      , p_sgrass            &   ! seagrass coverage (habitat area in grid / grid area)
#endif
)
!  call Coral_iniSizeDis
!  call Coral_Size2Cover

  time=time0
    
!----- Write data labels -------------------------
  CALL write_env_lavel(10)        
#if defined CORAL_TESTMODE
  CALL write_crl_his_lavel(11)
  CALL write_crl_his_lavel(12)
  CALL write_crl_ave_lavel(21)
  CALL write_crl_ave_lavel(22)
# if defined CORAL_ZOOXANTHELLAE
  CALL write_zox_his_lavel(31)
  CALL write_zox_his_lavel(32)
  CALL write_zphot_his_lavel(41)
  CALL write_zphot_his_lavel(42)
# endif
#endif
#if defined ECOSYS_TESTMODE
  CALL write_ecosys_his_lavel(40)
#endif
#if defined REEF_FLOW_TESTMODE
  CALL write_flow_lavel(45)
#endif


!----- Main loop -------------------------------------------

  DO

!------ Set environmental parameters ----------------------------

    CALL setdata(nSetting, in_file)
        
#if defined REEF_FLOW
    if (nSetting .eq. 4) then
        

!----- Reef hydrodynamics model ----------------------------------------

      h1 = REEF(1)%el(1,1)+REEF(1)%Dir(1,1)
    
      CALL reef_flow         &
!      input parameters
              (1, 1, 1        &   ! ng: nested grid number; i,j: position
              ,time           &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
              ,dt             &   ! Time step (sec)
              ,Hs             &   ! Significant wave hight at offshore (m)
              ,Tp             &   ! Significant Wave period (s)
              ,tide           &   ! Sea surface elevation at offshore (m)
               )
        
      h2 = (REEF(1)%el(1,1)+REEF(1)%Dir(1,1))
      dz(1,1,:) =h2/N
    end if
#endif
        
#if defined USE_HEAT

!----- Heat & mass balance model ----------------------------------------

    CALL heat_mass_balance    &
!      input parameters
              (1,1,1          &   ! ng: nested grid number; i,j: position
              ,N              &   ! Number of vertical grid (following ROMS vertical grid)
              ,dt             &   ! Time step (sec)
              ,dz(1,1,:)      &   ! dz(N): vertical grid size (m)
              ,swrad          &   ! Surface shortwave radiation (W m-2)
              ,Tair           &   ! air temperature (oC)
              ,Pair           &   ! atm pressure (hPa)
              ,Rh             &   ! vapor pressur (hPa)
              ,U10            &   ! wind speed (m s-1)
              ,rain           &   ! Precipitation volume flux (m s-1)
#ifdef LONGWAVE_IN
              ,dlwrad         &   ! Downward longwave radiation (W m-2)
#endif
              ,cloud          &   ! cloud cover (0-1)

              ,C(1,1,:,iTemp)     &   ! Tmp(N): Temperature (oC)
              ,C(1,1,:,iSalt)     &   ! Sal(N): Salinity (PSU)

!          output parameters
              ,dC_dt(1,1,:,iTemp)   &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
               )
#endif

!----- Ecosystem model ----------------------------------------
!    DIC(iCt,:) = C(1,1,:,iTIC_)

#if defined SEDECO_SGD && defined SGD_ON
    sgd_flux  = 0d0 !1.0d-2   ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
    sgd_Tmp   = 20.0d0  ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    sgd_Sal   = 5.0d0  ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    sgd_DIC(:)= 3500.0d0  ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
    sgd_TA    = 3500.0d0  ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
    sgd_DOx   = 10.0d0  ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
    sgd_NO3(:)= 100.0d0  ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    sgd_NH4(:)= 1.0d0  ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    sgd_PO4(:)= 1.0d0  ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
#endif
#if defined SEDECO_BURIAL
    Fdep_sed = -3.0d-8  ! 2.77d-8 g/cm2/s = 1.0 g/m2/h; Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
#endif

    CALL reef_ecosys          &
!      input parameters
              (1, 1, 1        &   ! ng: nested grid number; i,j: position
              ,N              &   ! Number of vertical grid (following ROMS vertical grid)
              ,isplitc        &   ! Internal loop counts of coral polyp model
              ,isplitsed      &   ! Internal loop counts of sediment ecosystem model
              ,time           &   ! Date (days since x or elapsed days) yt_edit best if in phase with clock to maintain consistency on restart
              ,dt             &   ! Time step (sec)
              ,dz(1,1,:)      &   ! dz(N): vertical grid size (m)
              ,PFDsurf        &   ! Sea surface photon flux density (umol m-2 s-1)
              ,tau            &   ! bottom shear stress (N m-2)
              ,pCO2air        &   ! Air CO2 pertial pressure (uatm)
              ,U10            &   ! wind speed (m s-1)
#ifdef CORAL_POLYP
              ,p_coral(:,1,1) &   ! Coral coverage (0-1)
#endif
#ifdef SEAGRASS
              ,p_sgrass(1,1)  &   ! seagrass coverage (0-1)
#endif
#ifdef MACROALGAE
              ,p_algae(1,1)   &   ! algal coverage (0-1)
#endif
#ifdef SEDIMENT_ECOSYS
              ,p_sand(1,1)    &   ! sediment coverage (0-1)
#endif

              ,C(1,1,:,iTemp)     &   ! Tmp(N): Temperature (oC)
              ,C(1,1,:,iSalt)     &   ! Sal(N): Salinity (PSU)
!              ,DIC(iCt,:)         &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
              ,C(1,1,:,iTIC_)     &   ! DIC(N): Total dissolved inorganic carbon (DIC: umol kg-1)
              ,C(1,1,:,iTAlk)     &   ! TA (N): Total alkalinity (TA: umol kg-1)
              ,C(1,1,:,iOxyg)     &   ! DOx(N): Dissolved oxygen (umol L-1)
              ,C(1,1,:,iDOC(:))       &   ! DOC(N): Dissolved organic carbon (DOC: umol L-1)
              ,C(1,1,:,iPOC(:))       &   ! POC(N): Particulate organic carbon (POC: umol L-1)
              ,C(1,1,:,iPhyt(:))      &   ! PHY(N): phytoplankton (umol C L-1)
              ,C(1,1,:,iZoop(:))      &   ! ZOO(N): zooplankton (umol C L-1)
              ,C(1,1,:,iPIC(:))       &   ! PIC(N): Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
              ,C(1,1,:,iNO3_)     &   ! NO3(N): NO3 (umol L-1)
              ,C(1,1,:,iNH4_)     &   ! NH4(N): NH4 (umol L-1)
              ,C(1,1,:,iPO4_)     &   ! PO4(N): PO4 (umol L-1)
              ,C(1,1,:,iDON(:))     &   ! DON(N): Dissolved organic nitrogen (DON: umol L-1)
              ,C(1,1,:,iPON(:))     &   ! PON(N): Particulate organic nitrogen (PON: umol L-1)
              ,C(1,1,:,iDOP(:))     &   ! DOP(N): Dissolved organic phosporius (DOP: umol L-1)
              ,C(1,1,:,iPOP(:))     &   ! POP(N): Particulate organic phosporius (POP: umol L-1)
#if defined COT_STARFISH         
              ,C(1,1,:,iCOTe)     &   ! COTe(N): COT starfish egg (umol L-1)
              ,C(1,1,:,iCOTl)     &   ! COTl(N): COT starfish larvae (umol L-1)
#endif
#if defined SEDECO_SGD && defined SGD_ON
              , sgd_flux          &   ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
              , sgd_Tmp           &   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
              , sgd_Sal           &   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
              , sgd_DIC(:)        &   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (DIC: umol kg-1)
              , sgd_TA            &   ! SGD concentration coming in through bottom: TA : Total alkalinity (TA: umol kg-1)
              , sgd_DOx           &   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
              , sgd_NO3(:)        &   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
              , sgd_NH4(:)        &   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
              , sgd_PO4(:)        &   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
#endif
#if defined SEDECO_BURIAL
              , Fdep_sed          &    ! Sedimentation rate (g cm-2 s-1) (Positive: sedimentation; Negative: erosion)
#endif
!      output parameters
              ,dC_dt(1,1,:,iTemp)   &   ! Tmp(N): Temperature (oC)
              ,dC_dt(1,1,:,iSalt)   &   ! Sal(N): Salinity (PSU)
              ,dC_dt(1,1,:,iTIC_)   &   ! dDIC_dt(N): dDIC/dt (umol kg-1 s-1)
              ,dC_dt(1,1,:,iTAlk)   &   ! dTA_dt (N): dTA/dt (umol kg-1 s-1)
              ,dC_dt(1,1,:,iOxyg)   &   ! dDOx_dt(N): dDO/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iDOC(1):iDOC(Ndom))      &   ! dDOC_dt(N): dDOC/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iPOC(1):iPOC(Npom))      &   ! dPOC_dt(N): dPOC/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iPhyt(1):iPhyt(Nphy))   &   ! dPHY_dt(N): dPHY/dt (umol C L-1 s-1)
              ,dC_dt(1,1,:,iZoop(1):iZoop(Nzoo))   &   ! dZOO_dt(N): dZOO/dt (umol C L-1 s-1)
              ,dC_dt(1,1,:,iPIC(1):iPIC(Npim))      &   ! dPIC_dt(N): dPIC/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iNO3_)   &   ! dNO3_dt(N): dNO3/dt (umol L-1 s-1)
!              ,dC_dt(1,1,:,iNO2_)   &   ! dNO2_dt(N): dNO2/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iNH4_)   &   ! dNH4_dt(N): dNH4/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iPO4_)   &   ! dPO4_dt(N): dPO4/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iDON(1):iDON(Ndom))      &   ! dDON_dt(N): dDON/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iPON(1):iPON(Npom))      &   ! dPON_dt(N): dPON/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iDOP(1):iDOP(Ndom))      &   ! dDOP_dt(N): dDOP/dt (umol L-1 s-1)
              ,dC_dt(1,1,:,iPOP(1):iPOP(Npom))      &   ! dPOP_dt(N): dPOP/dt (umol L-1 s-1)
#if defined COT_STARFISH         
              ,dC_dt(1,1,:,iCOTe)   &   ! dCOTe/dt(N): (umol L-1 s-1)
              ,dC_dt(1,1,:,iCOTl)   &   ! dCOTl/dt(N): (umol L-1 s-1)
#endif
              ,pH(1,1,:)      &   ! pH
              ,Warg(1,1,:)    &   ! aragonite saturation state
              ,Wcal(1,1,:)    &   ! calcite saturation state
              ,ssfCO2         &   ! sea surface fCO2 (uatm)
              ,ssCO2flux      &   ! sea surface CO2 flux (mmol m-2 s-1)
              ,ssO2flux       &   ! sea surface O2 flux (mmol m-2 s-1)
              ,PFDbott        &   ! Bottom photon flux density (umol m-2 s-1)
               )                    
!

    do k=1,N
        

!---------- for Stable condition ---------------------------------
              
      if (nSetting .eq. 1) then
          
        ! nothing to calculate

!---------- for Closed Chamber condition -------------------------

      else if (nSetting .eq. 2) then
        
!        C(1,1,k,iTemp)=C(1,1,k,iTemp)+0.
!        C(1,1,k,iSalt)=C(1,1,k,iSalt)+0.
!        C(1,1,k,iSedi)=C(1,1,k,iSedi)+0.
        
        do id=1,Nid

          C(1,1,k,id)=C(1,1,k,id) + dC_dt(1,1,k,id)*dt
#if defined SEDECO_SGD && defined SGD_ON
          ! Volume flux by SGD and overflow is assumed 
          dC_dt_sgd = -0.01d0*sgd_flux *C(1,1,k,id)/dz(1,1,1)
          C(1,1,k,id)=C(1,1,k,id) + dC_dt_sgd*dt
#endif
        end do

!---------- Constant Flow condition ------------------------------------

      else if (nSetting .eq. 3) then









!---------- Reef condition ------------------------------------

      else if (nSetting .eq. 4) then
#if defined REEF_FLOW
        fvol_rc = REEF(1)%Qrc(1,1)*REEF(1)%Wrc(1,1)
        fvol_ch = REEF(1)%Qch(1,1)*REEF(1)%Wch(1,1)
        do id=1,Nid
          C(1,1,k,id) = C(1,1,k,id)*h1/h2                          &
                        +(0.5d0*(ABS(fvol_rc)+fvol_rc)* Co(1,1,id)   &  !  (t unit) m s-1
                         -0.5d0*(ABS(fvol_rc)-fvol_rc)* C(1,1,k,id)  &  !  (t unit) m s-1
                         +0.5d0*(ABS(fvol_ch)+fvol_ch)* Co(1,1,id)   &  !  (t unit) m s-1
                         -0.5d0*(ABS(fvol_ch)-fvol_ch)* C(1,1,k,id)  &  !  (t unit) m s-1
                         )/h2/REEF(1)%Air(1,1)*dt
          C(1,1,k,id)=C(1,1,k,id) + dC_dt(1,1,k,id)*dt
        end do
#endif

!---------- Incubation chamber condition ------------------------------------

      else if (nSetting .eq. 5) then
        if( ipcl == 1) then
          C(1,1,k,iTIC_) = DIC_data(1)
          C(1,1,k,iTAlk) = TA_data(1)
          C(1,1,k,iOxyg) = DO_data(1)
!          C(1,1,k,iOxyg) = O2satu(C(1,1,k,iTemp)+273.15d0, C(1,1,k,iSalt))
#if defined CARBON_ISOTOPE
          R13C=R13C_fromd13C(0.7d0)
          C(1,1,k,iT13C) =R13C*C(1,1,k,iTIC_) !DI13C (umol kg-1) 
#endif
          ipcl = 2
        end if
      
        if (time >= 4.0d0 ) then
          do id=4,Nid
            C(1,1,k,id)=C(1,1,k,id) + dC_dt(1,1,k,id)*dt
          end do
          if (WQ_time(ipcl)-15.0d0/60.0d0 < time*24.0d0 .and. time*24.0d0 < WQ_time(ipcl)  ) then
            C(1,1,k,iTIC_) = DIC_data(ipcl)
            C(1,1,k,iTAlk) = TA_data(ipcl) 
            C(1,1,k,iOxyg) = DO_data(ipcl)
!            C(1,1,k,iOxyg) = O2satu(C(1,1,k,iTemp)+273.15d0, C(1,1,k,iSalt))
#if defined CARBON_ISOTOPE
            R13C=R13C_fromd13C(0.7d0)
            C(1,1,k,iT13C) =R13C*C(1,1,k,iTIC_) !DI13C (umol kg-1) 
#endif
          else if (time*24.0d0 >= WQ_time(ipcl)  ) then
            ipcl = ipcl +1
            if(ipcl>N_WQ) ipcl = N_WQ
          end if
        end if

!---------- Flume simulation ------------------------------------

      else if (nSetting .eq. 6) then
      
        if (aint(time)+6.0d0/24.0d0 <= time .and.  &
            time <= aint(time)+18.0d0/24.0d0     ) then
          if (inight == 1 ) then
            iflag = 1
            inight = 0
          end if
        else
          if (inight == 0 ) then
            iflag = 1
            inight = 1
          end if
        end if
        
        if (7.0d0+ 6.0d0/24.0d0 < time ) then
          if (iclose == 0 ) then
            iflag = 1
            iclose = 1
          end if
        end if
        if (7.0d0+12.0d0/24.0d0 < time ) then
          if (iclose == 1 ) then
            iflag = 1
            iclose = 2
          end if
        end if
        if (7.0d0+18.0d0/24.0d0 < time ) then
          if (iclose == 2 ) then
            iflag = 1
            iclose = 3
          end if
        end if
        if (8.0d0+ 0.0d0/24.0d0 < time ) then
          if (iclose == 3 ) then
            iflag = 1
            iclose = 4
          end if
        end if
        if (8.0d0+ 6.0d0/24.0d0 < time ) then
          if (iclose == 4 ) then
            iflag = 1
            iclose = 0
          end if
        end if
        
        if( inight == 1 .and. iflag == 1 ) then  ! Night treatment ~pH -0.1 (18:00-06:00)
#if defined FLUME_AMBIENT
          C(1,1,k,iTIC_) = 2090.0d0
#elif defined FLUME_HPCO2
          C(1,1,k,iTIC_) = 2269.0d0
#endif
          C(1,1,k,iTAlk) = 2340.0d0
          C(1,1,k,iOxyg) = O2satu(C(1,1,k,iTemp)+273.15d0, C(1,1,k,iSalt))
#if defined CARBON_ISOTOPE
          R13C=R13C_fromd13C(0.7d0)
          C(1,1,k,iT13C) =R13C*C(1,1,k,iTIC_) !DI13C (umol kg-1) 
#endif
          iflag = 0
        end if
        
        if( inight == 0 .and. iflag == 1 ) then
#if defined FLUME_AMBIENT
          C(1,1,k,iTIC_) = 2033.0d0
#elif defined FLUME_HPCO2
          C(1,1,k,iTIC_) = 2243.0d0
#endif
          C(1,1,k,iTAlk) = 2340.0d0
          C(1,1,k,iOxyg) = O2satu(C(1,1,k,iTemp)+273.15d0, C(1,1,k,iSalt))
#if defined CARBON_ISOTOPE
          R13C=R13C_fromd13C(0.7d0)
          C(1,1,k,iT13C) =R13C*C(1,1,k,iTIC_) !DI13C (umol kg-1) 
#endif
          iflag = 0
        end if
        
        
        if( iclose >= 1 ) then  ! Closed
!          tau = 1024*0.14*0.02**2. *0.5  !densSW*Cd*Ub**2  (2 cm s-1)
!          tau = 1024*0.14*0.05**2. *0.5  !densSW*Cd*Ub**2  (5 cm s-1)
          tau = 1024*0.14*0.10**2. *0.5  !densSW*Cd*Ub**2  (10 cm s-1)
          do id=4,Nid
            C(1,1,k,id)=C(1,1,k,id) + dC_dt(1,1,k,id)*dt
          end do
        end if

      end if
          
!------------------------------------------------------------------------------

!          depsed(i,j)=0.
!          radi(i,j,k)=-swrad

          
    enddo


!    p_coral(1,i,j)=p_coral(1,i,j)                         &
!          +(g_coral(1,i,j)-m_coral(1,i,j))*p_coral(1,i,j)
!
!    if (p_coral(1,i,j) .lt. 1.e-5) then
!      p_coral(1,i,j)=0.
!    end if

!------- Print section --------------------------------------

    if( time*86400.0d0 >= dsec ) THEN
      if(dsec == 0.0d0) then
        dsec=time*86400.0d0+OUTPUT_INTERVAL*60.0d0
      else
        dsec=dsec+OUTPUT_INTERVAL*60.0d0
      endif
      
      write(*,*) 'Time (day): ', time  ! Output for standard out
      CALL write_env_data(10)

    endif

!------- Check end time -----------------------------------
    if( time >= time0 + Tmax ) Exit

!------- Update time --------------------------------------

    time=time+dt/86400.

  END DO
      
!----- End loop --------------------------------------


!----- Close output files --------------------------------------

  CALL files_close

END PROGRAM main
!----------------------------------------------------------------------!

!     End of main program

!-----------------------------------------------------------------------

