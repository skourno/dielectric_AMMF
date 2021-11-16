Module diel_models 
Implicit None

contains

!------------------------------------------------------------
! The HS model calls the unmodified version of the AMMF HS
! dielectric theory
!
! rho   : density, reduced with sigma^3
! mu2   : dipole moment squared, reduced with 1 / kT sigma^3
! diel  : output dielectric constant value
! Gkirk : The G1 angular correlation coefficient
! scale : optional scaling of y*g1 in the kirkwood formula
!------------------------------------------------------------	
subroutine compute_dielectric_dHS(rho, mu2, diel, Gkirk, scale)
	Use nrtype
	Use dielectric, Only : G1, dielectric_const

	Implicit None
	Real(DP)    :: rho, mu2, scale
	Real(DP)    :: Gkirk, diel
	Intent(IN)  :: rho, mu2
	Intent(OUT) :: diel, Gkirk
	OPTIONAL    :: scale

	if (.NOT. PRESENT(scale)) scale = ONE

        Gkirk =               G1(rho, mu2, ONE)
        diel  = dielectric_const(rho, mu2, ONE, scale)
end subroutine compute_dielectric_dHS


!------------------------------------------------------------
! To model the Stockmayer fluid we use a mapping proposed by
! Theiss, Gross to find a corresponding state where the 
! dielectric of the dHS corresponds to that of the Stockmayer 
! (SM) fluid.
!
! rho   : density, reduced with sigma^3
! mu2   : dipole moment squared, reduced with 1 / epsilon sigma^3
! Temp  : temperature, reduced using k / epsilon
! diel  : output dielectric constant value
! Gkirk : The G1 angular correlation coefficient
! scale : optional scaling of y*g1 in the kirkwood formula
!
!------------------------------------------------------------
subroutine compute_dielecric_SM(rho, mu2, temp, diel, Gkirk, sigEff, scale)
	Use nrtype
	Use dielectric, Only : G1, dielectric_const
	Use Laguerre,   Only : Laguerre_fast
	Implicit None
	Real(DP)    :: rho, mu2, temp, scale
	Real(DP)    :: Gkirk, diel
	Real(DP)    :: mu2_kT_eff, sigEff, sigScaling, rho_eff
	Intent(IN)  :: rho, mu2, temp
	Intent(OUT) :: diel, Gkirk, sigEff
	OPTIONAL    :: scale

	if (.NOT. PRESENT(scale)) scale = ONE



	call Laguerre_fast(Temp,2*SIX,SIX,sigEff)
	
	sigScaling = sigEff*sigEff*sigEff
	mu2_kT_eff = (mu2 / temp) !* (1/sigScaling) 
	rho_eff    = rho !* sigScaling

        Gkirk  =               G1(rho_eff, mu2_kT_eff, sigEff)
        diel   = dielectric_const(rho_eff, mu2_kT_eff, sigEff, scale)	
end subroutine compute_dielecric_SM

End Module diel_models
