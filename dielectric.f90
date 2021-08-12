Module dielectric 
	Use nrtype
	Use phys_consts
	Implicit None

contains

	!-------------------------------------------------------------------------------------
	!-------------------------------------------------------------------------------------
	Function G1(rho, mu, sigBH)
		Use g_mvt,      Only : g_atContact
		Use Laguerre,   Only : Laguerre_fast

		Implicit None

		Real(DP)     :: rho, mu, sigBH
		Real(DP)     :: G1

		Real(DP)     :: g_eff
		Real(DP)     :: r
		Real(DP)     :: m23, eta, mu23, sigBH6
		Integer(I4B) :: IFl

		Intent(IN)   :: rho, mu, sigBH


		IFl    =  2     ! denoting a Sutherland potential
		r      =  NINE  ! range of the Sutherland potential is 9. Corresponds to a dd interaction cubed

		eta    =  rho * (PI/SIX) * sigBH*sigBH*sigBH
		g_eff  =  g_atContact(eta, IFl, r)

		mu23   =  mu*mu
		mu23   =  mu23*mu23*mu23 ! dimensionless mu squared to the third
		sigBH6 =  sigBH * sigBH * sigBH * sigBH * sigBH * sigBH
   
	 	G1     =  (EIGHT/75.D0) * rho * (PI/SIX) * g_eff * mu23 * (1/sigBH6) * 1.8
	 	!print*, k_B, temp, ONE, (ONE/(k_B*temp))
	 	!print*, aux, mu**2, sigma**3, mu**2 /sigma**3, (ONE/k_B*temp), (ONE/perm0x4pi) 

	 	if (G1 < ZERO) STOP "ERROR - Function G1 : This value cant be less than zero"
	End Function G1


	!-------------------------------------------------------------------------------------
	!-------------------------------------------------------------------------------------
	Function dielectric_const(rho, mu, sigBH, scale)
		Implicit None
		Real(DP)     :: rho, mu, sigBH, scale
		Real(DP)     :: dielectric_const
		Real(DP)     :: g_kirkwood, y, aux
		Real(DP)     :: d_tmp, eta
		Intent(IN)   :: rho, mu, sigBH
		OPTIONAL     :: scale

		if (.NOT. PRESENT(scale)) scale = 1.0

		g_kirkwood       = 1 + G1(rho, mu, sigBH)
		y                = FOUR*PI * rho * mu*mu * scale 
		aux              = ONE + y*g_kirkwood
		dielectric_const = QUARTER *(aux + SQRT(aux*aux+8))
		d_tmp            = QUARTER *(aux - SQRT(aux*aux+8))

                !write( *,111) (sqrt(mu*mu*1.45)), g_kirkwood-1, y/9.D0, aux, dielectric_const
	    !111 FORMAT ( F10.2, 2X, E10.4, 2X, F9.3, 2X, F14.6, 2X, F10.4)

		if (dielectric_const < ONE) then 
			print*, 'ERROR - Function dielectric_const : This value cant be less than one', dielectric_const
			STOP
		endif
	End Function dielectric_const
End Module dielectric
