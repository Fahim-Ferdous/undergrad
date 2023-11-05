program euler_modified

   implicit none
   real :: t(100), y(100), e(100)
   real :: a = 0.0, b = 2.0, y0 = 0.5
   real :: h, k1, k2, k3, k4
   real :: f, act
   integer :: i, n = 10

   h = (b - a)/n
   y(1) = y0
   e(1) = 0.0

   do i = 1, n + 1
      t(i) = a + (i - 1)*h
   end do

   do i = 1, n
      k1 = h*f(t(i), y(i))
      k2 = h*f(t(i) + h/2, y(i) + k1/2)
      k3 = h*f(t(i) + h/2, y(i) + k2/2)
      k4 = h*f(t(i) + h, y(i) + k3)

      y(i + 1) = y(i) + (k1 + 2*(k2 + k3) + k4)/6
      e(i + 1) = abs(y(i + 1) - act(t(i + 1)))
   end do

   do i = 1, n + 1
      write (*, *) t(i), y(i), act(t(i)), e(i)
   end do
end program euler_modified

function f(t, y)

   implicit none
   real, intent(in) :: t, y
   real :: f

   f = y - t**2 + 1
end function f

function act(t)

   implicit none
   real, intent(in) :: t
   real :: act

   act = (t + 1)**2 - 0.5*exp(t)
end function act
