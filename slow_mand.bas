'Mandelbrot set

Option EXPLICIT

Const W = 320
Const H = 320
Const X_MIN = -2.0
Const X_MAX = 1.0
Const Y_MIN = -1.5
Const Y_MAX = 1.5

Const MAX_ITER = 16

Dim zx As FLOAT, zy As FLOAT
Dim cx As FLOAT, cy As FLOAT
Dim zx_next As FLOAT
Dim iter As INTEGER
Dim t As INTEGER
Dim x As INTEGER, y As INTEGER
Dim g As INTEGER

For y = 0 To H - 1
 cy = Y_MIN + (Y_MAX - Y_MIN) * y / H
 For x = 0 To W - 1
  cx = X_MIN + (X_MAX - X_MIN) * x / W
  zx = 0
  zy = 0
  iter = 0
  t = zx * zx + zy * zy

  Do While t <= 4 And iter < MAX_ITER
   zx_next = zx * zx - zy * zy + cx
   zy = 2 * zx * zy + cy
   zx = zx_next
   iter = iter + 1

   If zx*zx>4 Or zy*zy>4 Then Exit Do
  Loop

  g = Int(255 * iter / MAX_ITER)
  Color RGB(g, g, g)
  Pixel x, y

 Next x
Next y

Save Image "out.bmp", 0, 0, 319, 319
End