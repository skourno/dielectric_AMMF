Module Laguerre
  Use nrtype

contains

subroutine Laguerre_Fast(Temp,xl_n,xl_m,sigmaBH)

      Implicit none

      Integer(I4B)            :: NP=5
      Integer(I4B)            :: i, j, n

      Real(DP)                :: Temp,xl_n,xl_m,sigmaBH 
      Real(DP)                :: u(5),w(5),x(5),func(5)
      Real(DP)                :: CMie, theta
      Intent(IN)              :: Temp,xl_n,xl_m
      Intent(OUT)             :: sigmaBH


      u(1) =  0.26356031971814109102031d0
      u(2) =  1.41340305910651679221800d0
      u(3) =  3.59642577104072208122300d0
      u(4) =  7.08581000585883755692200d0
      u(5) = 12.64080084427578265943000d0

      w(1) =  0.5217556105828086524759d0
      w(2) =  0.3986668110831759274500d0
      w(3) =  7.5942449681707595390000d-2
      w(4) =  3.6117586799220484545000d-3
      w(5) =  2.3369972385776227891000d-5

      CMie  =  (xl_n/(xl_n-xl_m))*(xl_n/xl_m)**(xl_m/(xl_n-xl_m))
      theta =  CMie / Temp

      ! roots and weights of the laguerre polynomial of order n


      ! calculation of sigmaBH using the gauss laguerre formula
      sigmaBH  =   ZERO

      do i=1,5
         x(i)     =  (theta / (u(i)+theta))**(ONE/xl_n)
         func(i)  =   x(i)*exp(theta*(ONE/x(i)**xl_m-ONE))/(u(i)+theta)

         sigmaBH  = sigmaBH + w(i)*func(i)/xl_n

      end do

      sigmaBH = ONE - sigmaBH
end subroutine Laguerre_Fast
End Module Laguerre