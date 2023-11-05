program ranj_koota_2nd_order

   implicit none
   real :: t(100), x(100), y(100), e(100)
   real :: a = 0.0
   real :: h, xk1, xk2, xk3, xk4, yk1, yk2, yk3, yk4
   real :: f, g, act
   integer :: i, n = 20

   h = 0.1
   x(1) = 1
   y(1) = 0
   e(1) = 0.0

   do i = 1, n + 1
      t(i) = a + (i - 1)*h
   end do

   do i = 1, n
      xk1 = h*g(t(i), x(i), y(i))
      yk1 = h*f(t(i), x(i), y(i))

      xk2 = h*g(t(i) + h/2, x(i) + xk1/2, y(i) + yk1/2)
      yk2 = h*f(t(i) + h/2, x(i) + xk1/2, y(i) + yk1/2)

      xk3 = h*g(t(i) + h/2, x(i) + xk2/2, y(i) + yk2/2)
      yk3 = h*f(t(i) + h/2, x(i) + xk2/2, y(i) + yk2/2)

      xk4 = h*g(t(i) + h, x(i) + xk3, y(i) + yk3)
      yk4 = h*f(t(i) + h, x(i) + xk3, y(i) + yk3)

      x(i + 1) = x(i) + (xk1 + 2*(xk2 + xk3) + xk4)/6
      y(i + 1) = y(i) + (yk1 + 2*(yk2 + yk3) + yk4)/6

      e(i + 1) = abs(x(i + 1) - act(t(i + 1)))
   end do

   write (*, *) "Colourful Doggy"
   write (*, "(1a4,2a17,1a19,1a17)") "t", "x", "y", "act", "err"
   do i = 1, n + 1
      write (*, *) t(i), x(i), y(i), act(t(i)), e(i)
   end do
end program ranj_koota_2nd_order

function g(t, x, y)

   implicit none
   real, intent(in) :: t, x, y
   real :: g

   g = y
end function g

function f(t, x, y)

   implicit none
   real, intent(in) :: t, x, y
   real :: f

   f = -25*x
end function f

function act(t)

   implicit none
   real, intent(in) :: t
   real :: act

   act = cos(5*t)
end function act
