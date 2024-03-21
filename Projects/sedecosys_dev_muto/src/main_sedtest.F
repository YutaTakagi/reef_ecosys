
!! -----------------------------------------------!!
!!            SEDECOSYS main script               !!
!!           Made by Masa Muto 2023               !!
!!      This is for SEDIMENT_dev model test       !!
!! ---------------------------------------------- !!

#include "cppdefs.h"

!----------------------------------------------------------------------!
PROGRAM main
!----------------------------------------------------------------------!

! **********************************************************************
! *                                                                    *
! *   Test program of mod_sedecosys_MM                                 *
! *                                                                    *
! **********************************************************************
!

  USE mod_output
  ! USE mod_param
  USE mod_geochem
  use mod_sedecosys


  implicit none
!----------------------------------------------------------------------!
  integer, parameter :: Im = 1
  integer, parameter :: Jm = 1
  integer, parameter :: N  = 1

  real(8), parameter :: dt = 5.0d0 !(sec)
  integer, parameter :: isplitc  = 20
  integer, parameter :: isplitsed  = 1
  integer, parameter :: Ngrids = 1
  integer, parameter :: ng = 1
  integer, parameter :: i = 1
  integer, parameter :: j = 1
  integer :: k, istep
  real(8),parameter :: pi = 3.141519

!  For Output      
  real(8), parameter :: OUTPUT_INTERVAL = 1440d0     ! Output interval (min)
  real(8), save :: time = 0.0d0 !sec
  real(8), save :: dsec = 0.0d0 !sec
  ! real(8) :: dsec = 0.d0 !sec

  ! integer :: in_file(12)
  real(8) :: Tmax, time0
  real(8) :: ymax
  real(8) :: Tmp

  ! real(8),parameter :: dt = 1.0d0
  integer, parameter :: Ndom = 2
  integer, parameter :: Npom = 2

  real(8) :: T0, S0, O2_0, CO2_0, N2_0, TAlk0, TIC_0, Tmpmax, Tmpmin
  real(8) :: DOC1_0, DOC2_0, POC1_0, POC2_0 !, DON1_0, PON1_0, DOP1_0, POP1_0
  real(8) :: NO3_0, NH4_0, PO4_0
  real(8) :: Mn2_0, Fe2_0, H2S_0, SO4_0
  real(8) :: FdepPOC, FdepPOCmax, FdepPOCmin


  real(8) :: Flux_Tmp, Flux_Sal
  real(8) :: Flux_DIC, Flux_TA,  Flux_DO, Flux_CO2sed, Flux_N2
#if defined ORGANIC_MATTER
  real(8) :: Flux_DOC(Ndom), Flux_POC(Npom), Fdep_POC_0
#endif
# if defined NUTRIENTS           
  real(8) :: Flux_NO3, Flux_NH4, Flux_PO4  !, Flux_NO2
! # if defined ORGANIC_MATTER
!   real(8) :: Flux_DON(Ndom), Flux_PON(Npom)    
!   real(8) :: Flux_DOP(Ndom), Flux_POP(Npom)    
! # endif
#endif
# if defined SULFATE
  real(8) :: Flux_Mn2, Flux_MnO2 
  real(8) :: Flux_Fe2, Flux_FeS, Flux_FeS2, Flux_FeOOH, Flux_FeOOH_PO4
  real(8) :: Flux_H2S, Flux_S0, Flux_SO4 ! ,Flux_HS, Flux_S2
  real(8) :: Fdep_MnO2_0, Fdep_FeOOH_0
# endif


  !! import initial setting from sedecosys.in
  ! namelist/input_info/in_file
  namelist/ecosys_config/Tmax, time0, ymax
  namelist/initial/T0, S0, O2_0, CO2_0, N2_0, TAlk0, TIC_0, Tmpmax, Tmpmin
# if defined ORGANIC_MATTER
  ! namelist/initial/DOC1_0, DOC2_0, POC1_0, POC2_0, Fdep_POC_0!, DON1_0, PON1_0, DOP1_0, POP1_0
  namelist/initial/POC1_0, Fdep_POC_0, PON1_0, POP1_0
# endif
# if defined NUTRIENTS
  namelist/initial/NO3_0, NH4_0, PO4_0
# endif
# if defined SULFATE
  namelist/initial/Mn2_0, Fe2_0, H2S_0, SO4_0, Fdep_MnO2_0, Fdep_FeOOH_0, FdepPOCmax, FdepPOCmin
# endif

  ! read (5, nml=input_info)
  ! rewind(5)
  read (5, nml=ecosys_config)
  rewind(5)
  read (5, nml=initial)
  rewind(5)
  
!----------------------------------------------------------------------!

  !! Flux initial setting
  Flux_DIC = 0.0d0
  Flux_TA  = 0.0d0
  Flux_DO  = 0.0d0
  Flux_CO2sed = 0.0d0
  Flux_N2 = 0.0d0
#if defined ORGANIC_MATTER
  Flux_DOC(:) = 0.0d0
  Flux_POC(:) = 0.0d0
#endif
#if defined NUTRIENTS            
  Flux_NO3 = 0.0d0
  !    Flux_NO2 = 0.0d0
  Flux_NH4 = 0.0d0
  Flux_PO4 = 0.0d0
! # if defined ORGANIC_MATTER
!   Flux_DON(:) = 0.0d0
!   Flux_PON(:) = 0.0d0
!   Flux_DOP(:) = 0.0d0
!   Flux_POP(:) = 0.0d0
! # endif
#endif
# if defined SULFATE
  Flux_Mn2 = 0.0d0
  Flux_MnO2 = 0.0d0
  Flux_Fe2 = 0.0d0
  Flux_FeS = 0.0d0
  Flux_FeS2 = 0.0d0
  Flux_FeOOH = 0.0d0
  Flux_FeOOH_PO4 = 0.0d0
  Flux_H2S = 0.0d0
  ! Flux_HS = 0.0d0
  Flux_S0 = 0.0d0
  Flux_SO4 = 0.0d0
  ! Flux_S2 = 0.0d0
# endif 


!----------------------------------------------------------------------!

  CALL files_open
  CALL write_lavel(96)
  CALL write_lavel(97)

!----------------------------------------------------------------------!

  CALL initialize_sedecosys(ng, Ngrids, 1, Im, 1, Jm)

!----------------------------------------------------------------------!

  time=time0
  dsec=time0*86400.0d0

  istep=0

!----------------------------------------------------------------------!
!----- Main loop -------------------------------------------

  do istep=1, int(24.*60.*60./dt * Tmax)  !+1


#if defined SEDIMENT_TESTMODE
    ! Tmp = ((Tmpmax-Tmpmin)/2d0) * sin(istep*2d0*pi/(365d0*24d0*60d0*60d0)) + (Tmpmax+Tmpmin)/2d0;
    Tmp = T0
    ! S0 = 
    ! FdepPOC = ((FdepPOCmax-FdepPOCmin)/2d0) * sin(istep*2d0*pi/(365d0*24d0*60d0*60d0)) + (FdepPOCmax+FdepPOCmin)/2d0;
    FdepPOC = Fdep_POC_0
#endif
  ! DO m=1,isplitsed   !!! Loop for coral polyp model: dtc <= 0.05 sec

    CALL sedecosys     &
    ! input parameters
      ( ng, i, j       &   ! ng: nested grid number; i,j: position
      , dt             &   ! Time step (sec)
      , ymax           &   ! calculation year
      ! , istep          &   ! calculation timestep
      ! , PFDbott        &   ! Photon flux density (umol m-2 s-1)
      , Tmp             &   ! Tmp: Temperature (oC)
      , S0             &   ! Sal: Salinity (PSU)

      , O2_0           &   ! DOx: Dissolved oxygen (umol L-1)
      ! , CO2_0          &   ! CO2: Dissolved CO2    (µmol L-1)
      ,TIC_0           &   ! DIC: Dissolved organic matter (µmol kg-1)
      , N2_0           &   ! N2: Dissolved Nitrogen(µmol L-1)
#  if defined ORGANIC_MATTER
      ! , DOC1_0         &   ! DOC: Dissolved organic carbon (DOC: umol L-1)
      ! , DOC2_0         &   ! DOC: Dissolved organic carbon (DOC: umol L-1)
      , POC1_0         &   ! POC: Particulate organic carbon (DOC: umol L-1)
      ! , POC2_0         &   ! POC: Particulate organic carbon (DOC: umol L-1)
#  endif 
#  if defined NUTRIENTS            
      , NO3_0         &   ! NO3: NO3 (umol L-1)
      , NH4_0         &   ! NH4: NH4 (umol L-1)
      , PO4_0         &   ! PO4: PO4 (umol L-1)
! #   if defined ORGANIC_MATTER
!       , DON1_0         &   ! DON: Dissolved organic nitrogen (DON: umol L-1)
      , PON1_0         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
!       , DOP1_0         &   ! DOP: Dissolved organic phosporius (DOP: umol L-1)
      , POP1_0         &   ! POP: Particulate organic phosporius (POP: umol L-1)
! #   endif
#  endif
# if defined SULFATE
      , Mn2_0            &  ! Mn2: Manganese ion    (µmol L-1)
      , Fe2_0            &  ! Fe2: iron(II)         (µmol L-1)
      , H2S_0            &  ! H2S: hydrogen sulfide (µmol L-1)
      , SO4_0           &  ! SO4: sulfate          (µmol L-1)
      , Fdep_MnO2_0 /86400d0    &
      , Fdep_FeOOH_0 /86400d0  &
# endif
#  if defined ORGANIC_MATTER
      ! , 2250d0/86400d0  &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
      , FdepPOC /86400d0      & ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   if defined NUTRIENTS
      , 0d0  &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
      , 0d0  &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#   endif
#  endif
  !!   output parameters
      , Flux_Tmp       &   ! Temperature flux (K cm-2 s-1)  * direction of water column to sediment is positive
      , Flux_Sal       &   ! Salinity  flux (cm-2 s-1)  * direction of water column to sediment is positive
      , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
      , Flux_CO2sed    &   ! CO2 flux
      , Flux_N2        &   ! N2 flux
#  if defined ORGANIC_MATTER
      , Flux_DOC(1)       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_DOC(2)       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_POC(1)       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_POC(2)       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
#  if defined NUTRIENTS
      , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
  !          , Flux_NO2       &   ! NO2 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
      , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
      , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
! #   if defined ORGANIC_MATTER
!       , Flux_DON(1)       &   ! DON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
!       , Flux_PON(1)       &   ! PON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
!       , Flux_DOP(1)       &   ! DOP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
!       , Flux_POP(1)       &   ! POP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
! #   endif
#  endif
# if defined SULFATE
      , Flux_Mn2       &   ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_MnO2      &   ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive

      , Flux_Fe2       &   ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_FeS       &   ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_FeS2      &   ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_FeOOH     &   ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_FeOOH_PO4 &   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive

      , Flux_H2S       &   ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_S0        &   ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
      , Flux_SO4       &   ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
      )

      !! -------------------------- !!
      time = time +dt  ! sec
      if(time.ge.dsec) then
        dsec=dsec+OUTPUT_INTERVAL*60.
      endif

      ! if(time .ge. dsec/86400.) then
      !   dsec=dsec+OUTPUT_INTERVAL*60.
      
        ! write(*,*) 'Time (day): ', time/86400d0  !, 'POC Flux:' , Flux_POC(1), Flux_POC(2)! Output for standard out
        ! CALL write_env_data(10)
      ! endif
      !! -------------------------- !!

  end do
  ! 1 nmol cm-2 s-1 = 0.01 mmol m-2 s-1, 1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
  ! cff: convaert [nmol cm-2 s-1] to [umol L-1 s-1] 


  
!----------------------------------------------------------------------!

  CALL files_close

!----------------------------------------------------------------------!
END PROGRAM main
!----------------------------------------------------------------------!


