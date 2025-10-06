' Mandelbrot renderer with menu (PicoMite MMBasic) — interruptible render + per-line blit
OPTION EXPLICIT
OPTION CONTINUATION LINES ON

CSUB mandelbrot integer, integer, integer, integer, integer, integer
 00000000
 4FF0E92D E9DDB08B E9D66514 2C014600
 0600F176 682EDB7E 2E01686D 9D029502
 F1759601 DB750500 6B00E9D1 68526815
 92059504 685B681A 93099208 03C4EB00
 21009303 460B4689 460A468C FB024688
 FBA2F50C FB034703 443D5508 F708FB02
 2802FBA2 FC0CFB03 0847EB08 3703FBA3
 074CEB07 EB681AD3 0F9B0807 0388EA43
 EA4F199A 9B047454 04C5EA44 78A8EB4B
 9C0518E3 7E02FBA2 7C65EB44 F408FB02
 EB0E0FBF EA4F0E44 EA4774AE F111078E
 46710E01 FA0CFB03 FBA39406 EB045403
 EA4F044A EA457595 F1490584 17A40A00
 94079500 4501E9DD BF084555 46D1428C
 9C00D00C E9DD193D EB477406 27000404
 F04F42AF EB750501 DAA80404 E9C09B08
 199EEA00 F1009B09 EB430008 9B030B0B
 D1964283 21002000 E8BDB00B 00008FF0
End CSUB

Const SCALE = 1073741824   ' 1 << 30  (Q2.30)
Const SCREEN_W = 320
Const SCREEN_H = 320
Const MENU_H = 16
Const BLINDS = 16                   ' number of interleaved regions for rendering
Const MANDEL_W = SCREEN_W
Const MANDEL_H = SCREEN_H - MENU_H
Const MANDEL_TOP = MENU_H
Const BLOCK_W = 320
Const X_MIN_DEF = -2.0
Const X_MAX_DEF = 1.0
Const Y_MIN_DEF = -1.5
Const Y_MAX_DEF = 1.5
Const MAX_ITER = 512
Const STACK_MAX = 16
Const MENU_BG = RGB(40, 40, 40)
Const MENU_FG = RGB(255, 255, 255)
Const CURSOR_CLR = RGB(255, 255, 0)
Const OVERLAY_MAX = MANDEL_W * 4


Dim iterColor%(MAX_ITER)   ' iteration->RGB color lookup
Dim dyFix%                 ' Q28 step per row
Dim xMinFix%, yMinFix%     ' Q28 viewport minima in fixed-point
Dim linebuf%(MANDEL_W - 1)
Dim colors%(255)
Dim block%(BLOCK_W - 1)
Dim startX%
Dim startY%
Dim delta%
Dim x%
Dim y%
Dim bCount%
Dim i%
Dim iter%
Dim dx As Float
Dim dy As Float
Dim g As Integer
Dim rc%
Dim gc%
Dim bc%
Dim xMin As Float
Dim xMax As Float
Dim yMin As Float
Dim yMax As Float
Dim currDX As Float
Dim currDY As Float
Dim xHist(STACK_MAX - 1) As Float
Dim xHistMax(STACK_MAX - 1) As Float
Dim yHist(STACK_MAX - 1) As Float
Dim yHistMax(STACK_MAX - 1) As Float
Dim stackPtr As Integer
Dim running As Integer
Dim keyCode As Integer
Dim overlayX%(OVERLAY_MAX - 1)
Dim overlayY%(OVERLAY_MAX - 1)
Dim overlayClr%(OVERLAY_MAX - 1)
Dim overlayCnt As Integer
Dim pendingKey%        ' -1 means none pending
Dim fbReady%           ' framebuffer created flag
Dim PALETTE_NAME$

PALETTE_NAME$ = "rainbow"   ' default palette

pendingKey% = -1
fbReady% = 0

' ---------- Framebuffer helpers (fixed to use codes "F"/"N") ----------
Sub EnsureFramebuffer()
  If fbReady% = 0 Then
    FRAMEBUFFER CREATE           ' creates framebuffer "F" (and matches current mode)
    fbReady% = 1
  EndIf
End Sub

Sub FBWriteOn()
  FRAMEBUFFER WRITE "F"          ' draw into framebuffer
End Sub

Sub FBWriteOff()
  FRAMEBUFFER WRITE "N"          ' draw to the physical display
End Sub

Sub BlitLine(y As Integer)
  ' Copy one scanline from framebuffer F -> display N
  ' args: from, to, xin, yin, xout, yout, w, h
  BLIT FRAMEBUFFER F, N, 0, y, 0, MANDEL_TOP + y, MANDEL_W, 1
End Sub
Function Frac2(a As Float) As Float
  Frac2 = a - 2 * Int(a / 2)
End Function

Function HslRGB(h As Float, sPct As Float, lPct As Float) As Integer
  Local s As Float, l As Float, c As Float, hp As Float, x As Float, m As Float
  Local r1 As Float, g1 As Float, b1 As Float
  Local R% As Integer, G% As Integer, B% As Integer
  Local hi% As Integer

  ' --- normalize inputs ---
  If h < 0 Then
    h = 0
  ElseIf h >= 360 Then
    h = h - 360 * Int(h \ 360)
  EndIf

  If sPct < 0 Then
    sPct = 0
  ElseIf sPct > 100 Then
    sPct = 100
  EndIf

  If lPct < 0 Then
    lPct = 0
  ElseIf lPct > 100 Then
    lPct = 100
  EndIf

  s = sPct / 100.0
  l = lPct / 100.0
  c = (1 - Abs(2 * l - 1)) * s
  hp = h / 60.0
  x = c * (1 - Abs(Frac2(hp) - 1))
  m = l - c / 2

  ' --- hue sector 0..5 ---
  hi% = Int(hp)
  If hi% = 0 Then
    r1 = c : g1 = x : b1 = 0
  ElseIf hi% = 1 Then
    r1 = x : g1 = c : b1 = 0
  ElseIf hi% = 2 Then
    r1 = 0 : g1 = c : b1 = x
  ElseIf hi% = 3 Then
    r1 = 0 : g1 = x : b1 = c
  ElseIf hi% = 4 Then
    r1 = x : g1 = 0 : b1 = c
  Else
    r1 = c : g1 = 0 : b1 = x
  EndIf

  ' --- add m, scale, clamp to 0..255 ---
  R% = Int((r1 + m) * 255 + 0.5)
  If R% < 0 Then
    R% = 0
  ElseIf R% > 255 Then
    R% = 255
  EndIf

  G% = Int((g1 + m) * 255 + 0.5)
  If G% < 0 Then
    G% = 0
  ElseIf G% > 255 Then
    G% = 255
  EndIf

  B% = Int((b1 + m) * 255 + 0.5)
  If B% < 0 Then
    B% = 0
  ElseIf B% > 255 Then
    B% = 255
  EndIf

  HslRGB = RGB(R%, G%, B%)
End Function


' Non-blocking key read: returns -1 if no key waiting
Function ConsumeKeyIfAny() As Integer
  Local k$
  k$ = INKEY$
  If k$ = "" Then
    ConsumeKeyIfAny = -1
  Else
    ConsumeKeyIfAny = ASC(k$)
  EndIf
End Function

' Blocking key read
Function GetKey() As Integer
  Local k$
  Do
    k$ = INKEY$
  Loop Until k$ <> ""
  GetKey = ASC(k$)
End Function

Function Fix28(v As Float) As Integer
    If v >= 0 Then
        Fix28 = Int(v * SCALE + 0.5)
    Else
        Fix28 = -Int(-v * SCALE + 0.5)
    EndIf
End Function

Function ClampInt(v As Integer, lo As Integer, hi As Integer) As Integer
    If v < lo Then
        ClampInt = lo
    ElseIf v > hi Then
        ClampInt = hi
    Else
        ClampInt = v
    EndIf
End Function

Sub PushViewport()
    If stackPtr < STACK_MAX Then
        xHist(stackPtr) = xMin
        xHistMax(stackPtr) = xMax
        yHist(stackPtr) = yMin
        yHistMax(stackPtr) = yMax
        stackPtr = stackPtr + 1
    EndIf
End Sub

Function PopViewport() As Integer
    If stackPtr > 0 Then
        stackPtr = stackPtr - 1
        xMin = xHist(stackPtr)
        xMax = xHistMax(stackPtr)
        yMin = yHist(stackPtr)
        yMax = yHistMax(stackPtr)
        PopViewport = 1
    Else
        PopViewport = 0
    EndIf
End Function

Sub ResetViewport()
    xMin = X_MIN_DEF
    xMax = X_MAX_DEF
    yMin = Y_MIN_DEF
    yMax = Y_MAX_DEF
    stackPtr = 0
End Sub

Sub DrawMenu()
    ' BOX uses width/height (W,H), not x2/y2
    Box 0, 0, SCREEN_W, MENU_H, 1, MENU_BG, MENU_BG
    ' TEXT needs a string; leave optional args blank, set colours
    Text 6, 2, "Z)oom  O)ut  R)eset  P)alette  Esc", , , , MENU_FG, MENU_BG
End Sub

Sub DrawZoomBox(boxX As Integer, boxY As Integer, boxSize As Integer)
    Local idx As Integer
    Local xPix As Integer
    Local yPix As Integer

    idx = 0
    For xPix = boxX To boxX + boxSize - 1
        overlayX%(idx) = xPix
        overlayY%(idx) = boxY
        overlayClr%(idx) = Pixel(xPix, boxY)
        Pixel xPix, boxY, CURSOR_CLR
        idx = idx + 1
    Next xPix
    For xPix = boxX To boxX + boxSize - 1
        overlayX%(idx) = xPix
        overlayY%(idx) = boxY + boxSize - 1
        overlayClr%(idx) = Pixel(xPix, boxY + boxSize - 1)
        Pixel xPix, boxY + boxSize - 1, CURSOR_CLR
        idx = idx + 1
    Next xPix
    For yPix = boxY + 1 To boxY + boxSize - 2
        overlayX%(idx) = boxX
        overlayY%(idx) = yPix
        overlayClr%(idx) = Pixel(boxX, yPix)
        Pixel boxX, yPix, CURSOR_CLR
        idx = idx + 1
        overlayX%(idx) = boxX + boxSize - 1
        overlayY%(idx) = yPix
        overlayClr%(idx) = Pixel(boxX + boxSize - 1, yPix)
        Pixel boxX + boxSize - 1, yPix, CURSOR_CLR
        idx = idx + 1
    Next yPix
    overlayCnt = idx
End Sub

Sub UndrawZoomBox()
    Local idx As Integer
    For idx = 0 To overlayCnt - 1
        Pixel overlayX%(idx), overlayY%(idx), overlayClr%(idx)
    Next idx
    overlayCnt = 0
End Sub

' Window-blind render: interleave rows across BLINDS regions
Sub RenderViewport()
    Local rX As Integer
    Local region As Integer
    Local lineInRegion As Integer
    Local y As Integer
    Local startR As Integer
    Local endR As Integer
    Local linesSinceCheck As Integer
    Local linesDrawn As Integer

    EnsureFramebuffer
    FBWriteOn     ' draw Mandelbrot rows into FB#1

    ' Precompute steps
    If MANDEL_W > 1 Then
        dx = (xMax - xMin) / (MANDEL_W - 1)
    Else
        dx = 0
    EndIf
    If MANDEL_H > 1 Then
        dy = (yMax - yMin) / (MANDEL_H - 1)
    Else
        dy = 0
    EndIf
    currDX = dx : currDY = dy

    delta%   = Fix28(dx) : If delta% = 0 Then delta% = 1
    dyFix%   = Fix28(dy)
    xMinFix% = Fix28(xMin)
    yMinFix% = Fix28(yMax) ' not used; keep for symmetry
    yMinFix% = Fix28(yMin) ' effective value

    linesSinceCheck = 0
    linesDrawn = 0

    ' Maximum number of lines any region can have
    Local maxLines As Integer
    maxLines = ((BLINDS - 1) * MANDEL_H) \ BLINDS   ' floor((BLINDS-1)/BLINDS * H)
    If ((MANDEL_H) Mod BLINDS) <> 0 Then
        maxLines = ((MANDEL_H + BLINDS - 1) \ BLINDS) ' ceil(H/BLINDS)
    EndIf

    For lineInRegion = 0 To maxLines - 1
        For region = 0 To BLINDS - 1
            ' Compute this region's start/end lines
            startR = (region * MANDEL_H) \ BLINDS
            endR   = ((region + 1) * MANDEL_H) \ BLINDS - 1
            y = startR + lineInRegion
            If y > endR Then
                ' this region has no line at this index; skip
            Else
                ' Interrupt check about every 10 lines drawn
                linesSinceCheck = linesSinceCheck + 1
                If linesSinceCheck >= 10 Then
                    pendingKey% = ConsumeKeyIfAny()
                    If pendingKey% <> -1 Then
                        FBWriteOff
                        Exit Sub
                    EndIf
                    linesSinceCheck = 0
                EndIf

                ' Render scanline y into the framebuffer
                startY% = Fix28(yMin + dy * y)   ' or yMinFix% + dyFix% * y
                ' Using the integer form avoids float work per line:
                startY% = yMinFix% + dyFix% * y

                For rX = 0 To MANDEL_W - 1 Step BLOCK_W
                    bCount% = MANDEL_W - rX : If bCount% > BLOCK_W Then bCount% = BLOCK_W
                    startX% = xMinFix% + delta% * rX
                    mandelbrot block%(0), startX%, startY%, delta%, bCount%, MAX_ITER
                    For i% = 0 To bCount% - 1
                        iter% = block%(i%) : If iter% > MAX_ITER Then iter% = MAX_ITER
                        Pixel rX + i%, y, iterColor%(iter%)   ' draw into FB row y (off-screen)
                    Next i%
                Next rX

                ' Push this completed scanline to the visible screen at MANDEL_TOP + y
                FBWriteOff
                BLIT FRAMEBUFFER F, N, 0, y, 0, MANDEL_TOP + y, MANDEL_W, 1
                FBWriteOn

                linesDrawn = linesDrawn + 1
            EndIf
        Next region
    Next lineInRegion

    FBWriteOff
End Sub
Sub DoZoom()
    Local boxSize As Integer
    Local moveStep As Integer
    Local boxX As Integer
    Local boxY As Integer
    Local done As Integer
    Local key As Integer
    Local pixLeft As Integer
    Local pixTop As Integer
    Local pixRight As Integer
    Local pixBottom As Integer
    Local newXMin As Float
    Local newXMax As Float
    Local newYMin As Float
    Local newYMax As Float

    boxSize = MANDEL_W \ 3
    If boxSize < 12 Then boxSize = 12
    moveStep = ClampInt(boxSize \ 6, 1, boxSize)
    boxX = (MANDEL_W - boxSize) \ 2
    boxY = MANDEL_TOP + (MANDEL_H - boxSize) \ 2

    DrawZoomBox boxX, boxY, boxSize
    done = 0
    Do While done = 0
        key = GetKey()
        Select Case key
            Case 128 ' Up
                UndrawZoomBox
                boxY = ClampInt(boxY - moveStep, MANDEL_TOP, MANDEL_TOP + MANDEL_H - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 129 ' Down
                UndrawZoomBox
                boxY = ClampInt(boxY + moveStep, MANDEL_TOP, MANDEL_TOP + MANDEL_H - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 130 ' Left
                UndrawZoomBox
                boxX = ClampInt(boxX - moveStep, 0, MANDEL_W - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 131 ' Right
                UndrawZoomBox
                boxX = ClampInt(boxX + moveStep, 0, MANDEL_W - boxSize)
                DrawZoomBox boxX, boxY, boxSize
            Case 13  ' Enter: zoom
                UndrawZoomBox
                PushViewport
                pixLeft = boxX
                pixRight = boxX + boxSize - 1
                pixTop = boxY - MANDEL_TOP
                pixBottom = pixTop + boxSize - 1
                newXMin = xMin + currDX * pixLeft
                newXMax = xMin + currDX * pixRight
                newYMin = yMin + currDY * pixTop
                newYMax = yMin + currDY * pixBottom
                xMin = newXMin : xMax = newXMax
                yMin = newYMin : yMax = newYMax
                done = 1
            Case 27  ' Esc: cancel
                UndrawZoomBox
                done = 1
        End Select
    Loop
    CLS
    DrawMenu
    RenderViewport
End Sub



' ------- JS-style palettes (rainbow, blue, blue2) -------
Sub BuildPalette(name$)
  Local i%                   ' iteration index
  Local hue As Float
  Local sat As Float
  Local lit As Float
  Local nm$ : nm$ = LCase$(name$)

  ' Fill iter colors for all escape bins 0..MAX_ITER-1
  For i% = 0 To MAX_ITER - 1
    If nm$ = "rainbow" Then
      ' h = (360 * 2 * i / MAX_ITER) % 360, s=90, l=50
      hue = (720.0 * i% / MAX_ITER)
      hue = hue - 360.0 * Int(hue \ 360.0)
      sat = 90 : lit = 50
      iterColor%(i%) = HslRGB(hue, sat, lit)

    ElseIf nm$ = "blue" Then
      ' h = (((120 * 2 * i) / MAX_ITER) % 120) + 180, s=90, l=50
      hue = (240.0 * i% / MAX_ITER)
      hue = hue - 120.0 * Int(hue \ 120.0)
      hue = hue + 180.0   ' 180..300 range
      sat = 90 : lit = 50
      iterColor%(i%) = HslRGB(hue, sat, lit)

    Else ' "blue2"
      ' h = 220, s=90, l = ((75 * 2 * i) / MAX_ITER) % 75
      hue = 220
      sat = 90
      lit = (150.0 * i% / MAX_ITER)
      lit = lit - 75.0 * Int(lit \ 75.0)   ' 0..75 repeating
      iterColor%(i%) = HslRGB(hue, sat, lit)
    EndIf
  Next i%

  ' Interior colour (did not escape by MAX_ITER)
  iterColor%(MAX_ITER) = RGB(0, 0, 0)
End Sub


CLS
BuildPalette PALETTE_NAME$
ResetViewport
DrawMenu
RenderViewport

running = 1
Do While running = 1
    ' If a key arrived mid-render, use it first; otherwise block for a key.
    If pendingKey% <> -1 Then
        keyCode = pendingKey%
        pendingKey% = -1
    Else
        keyCode = GetKey()
    EndIf

    Select Case keyCode
        Case 90, 122, 128, 129, 130, 131         ' Z/z, and arrows
            DoZoom
        Case 66, 98           ' B/b
        Case 79, 111          ' O/o (zoom out/back)
            If PopViewport() Then
                CLS
                DrawMenu
                RenderViewport
            EndIf
        Case 82, 114          ' R/r
            ResetViewport
            CLS
            DrawMenu
            RenderViewport
        Case 27               ' Esc
            running = 0
        Case 80, 112          ' P/p Palette cycle (debug)
            ' cycle through palettes for testing
            If LCase$(PALETTE_NAME$) = "blue" Then
                BuildPalette "blue2"
                PALETTE_NAME$ = "blue2"
            ElseIf LCase$(PALETTE_NAME$) = "blue2" Then
                BuildPalette "rainbow"
                PALETTE_NAME$ = "rainbow"
            Else
                BuildPalette "blue"
                PALETTE_NAME$ = "blue"
            EndIf
            CLS
            DrawMenu
            RenderViewport
    End Select
Loop

' SAVE IMAGE uses StartX, StartY, width, height
Save Image "out.bmp", 0, 0, SCREEN_W, SCREEN_H
