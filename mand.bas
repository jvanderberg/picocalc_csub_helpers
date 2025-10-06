' Mandelbrot renderer with menu (PicoMite MMBasic) — interruptible render + per-line blit
OPTION EXPLICIT
OPTION CONTINUATION LINES ON

CSUB mandelbrot integer, integer, integer, integer, integer, integer
 00000000
 4FF0E92D E9DDB08D E9D66516 2C014600
 0600F176 682EDB7D 2E01686D 9D039503
 F1759602 DB740500 9B00E9D1 68526815
 92089507 685B681A 930A9209 03C4EB00
 21009306 460C468A 460F460E 468C460A
 468E460B 18DB9101 FC0CFB03 0E0EEB4E
 CC0EFB02 2302FBA3 44639D07 EA420F12
 19521203 EB459D08 9B017C23 EB671B34
 EB140503 EB4B0309 FB030E05 FBA3F40E
 FB026703 EB07F80C FBA20744 31014502
 0548EB05 EA4F9104 F14A7414 EA440100
 91051405 9501172D 1A04E9DD 5802E9DD
 BF0845D0 EA4F428D EA467616 EA4F1607
 D00D7727 46A81935 EB479D01 950B0505
 F8DF4645 45458038 F1759D0B DBB20500
 30089B04 3C08F840 F8409B05 9B093C04
 0309EB13 9B0A4699 0B0BEB43 42839B06
 2000D197 B00D2100 8FF0E8BD 40000001
End CSUB

Const SCALE = 268435456            ' 1 << 28 fixed point
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
Const MAX_ITER = 256
Const STACK_MAX = 16
Const MENU_BG = RGB(40, 40, 40)
Const MENU_FG = RGB(255, 255, 255)
Const CURSOR_CLR = RGB(255, 255, 0)
Const OVERLAY_MAX = MANDEL_W * 4

Const PALETTE_CURVE$ = "cosine"   ' "cosine" or "gamma"
Const PALETTE_GAMMA  = 0.75       ' <1 expands high iters; try 0.7..0.9
Const CYCLE_COLORS = 64         ' size of the repeating palette
Const PALETTE_PHASE = 0         ' 0..63, optional hue rotation

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

PALETTE_NAME$ = "turbo"   ' "viridis", "inferno", or "turbo"

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
' ----------------------------------------------------------------------
Function IterToT(i As Integer) As Float
  Local u As Float, t As Float
  If MAX_ITER <= 1 Then
    IterToT = 0
    Exit Function
  EndIf

  ' Normalize 1..MAX_ITER-1 -> 0..1
  u = (i - 1) / (MAX_ITER - 1.0)

  ' Clamp using multi-line If/ElseIf
  If u < 0 Then
    u = 0
  ElseIf u > 1 Then
    u = 1
  EndIf

  If LCase$(PALETTE_CURVE$) = "cosine" Then
    t = (1 - Cos(u * 3.14159265)) / 2
    IterToT = t ^ PALETTE_GAMMA
  Else
    IterToT = u ^ PALETTE_GAMMA
  EndIf
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
    Text 6, 2, "Z=Zoom  O=Back  R=Reset  Esc=Quit", , , , MENU_FG, MENU_BG
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



Sub BuildPaletteFromStops(name$)
  ' fixed-size stop arrays (8 stops)
  Local sT(7) As Float
  Local sR%(7), sG%(7), sB%(7)
  Local i%, j% 
  Local t As Float, segT As Float
  Local r As Float, g As Float, b As Float
  Local tt As Float
  ' ---- define color stops ----
  Select Case LCase$(name$)
    Case "viridis"
      sT(0)=0.00: sR%(0)= 68: sG%(0)=  1: sB%(0)= 84
      sT(1)=0.15: sR%(1)= 71: sG%(1)= 44: sB%(1)=122
      sT(2)=0.30: sR%(2)= 59: sG%(2)= 81: sB%(2)=138
      sT(3)=0.45: sR%(3)= 44: sG%(3)=113: sB%(3)=142
      sT(4)=0.60: sR%(4)= 33: sG%(4)=144: sB%(4)=141
      sT(5)=0.75: sR%(5)= 73: sG%(5)=176: sB%(5)=110
      sT(6)=0.90: sR%(6)=159: sG%(6)=190: sB%(6)= 87
      sT(7)=1.00: sR%(7)=253: sG%(7)=231: sB%(7)= 37
    Case "inferno"
      sT(0)=0.00: sR%(0)=  0: sG%(0)=  0: sB%(0)=  4
      sT(1)=0.15: sR%(1)= 31: sG%(1)= 12: sB%(1)= 72
      sT(2)=0.30: sR%(2)= 85: sG%(2)= 15: sB%(2)=109
      sT(3)=0.45: sR%(3)=136: sG%(3)= 34: sB%(3)=106
      sT(4)=0.60: sR%(4)=191: sG%(4)= 72: sB%(4)= 83
      sT(5)=0.75: sR%(5)=234: sG%(5)=132: sB%(5)= 52
      sT(6)=0.90: sR%(6)=252: sG%(6)=194: sB%(6)= 68
      sT(7)=1.00: sR%(7)=252: sG%(7)=255: sB%(7)=164
    Case Else ' "turbo"
      sT(0)=0.00: sR%(0)= 48: sG%(0)= 18: sB%(0)= 59
      sT(1)=0.15: sR%(1)=  0: sG%(1)= 67: sB%(1)=166
      sT(2)=0.30: sR%(2)=  3: sG%(2)=141: sB%(2)=195
      sT(3)=0.45: sR%(3)= 46: sG%(3)=194: sB%(3)=126
      sT(4)=0.60: sR%(4)=193: sG%(4)=221: sB%(4)= 61
      sT(5)=0.75: sR%(5)=255: sG%(5)=170: sB%(5)=  0
      sT(6)=0.90: sR%(6)=231: sG%(6)= 77: sB%(6)=  0
      sT(7)=1.00: sR%(7)=125: sG%(7)=  0: sB%(7)=  0
  End Select

 ' ---- full-range iter->colour using non-linear spread ----
  Local cycle%(CYCLE_COLORS - 1)
  Local k%
  Local idx%, src%

  ' Build a 64-swatch cycle from the same palette stops
  For k% = 0 To CYCLE_COLORS - 1
    ' sample evenly across 0..1 (centered in each bin to avoid duplicate endpoints)
    tt = (k% + 0.5) / CYCLE_COLORS
    ' find segment for tt
    For j% = 0 To 6
      If tt <= sT(j%+1) Then Exit For
    Next j%
    If sT(j%+1) > sT(j%) Then
      segT = (tt - sT(j%)) / (sT(j%+1) - sT(j%))
    Else
      segT = 0
    EndIf
    r = sR%(j%) + (sR%(j%+1) - sR%(j%)) * segT
    g = sG%(j%) + (sG%(j%+1) - sG%(j%)) * segT
    b = sB%(j%) + (sB%(j%+1) - sB%(j%)) * segT
    cycle%(k%) = RGB(Int(r), Int(g), Int(b))
  Next k%

  ' Map iterations cyclically:
  ' 1..64  -> cycle 0..63
  ' 65..128-> cycle 0..63, etc.
  For i% = 0 To MAX_ITER - 1
    If i% <= 0 Then
      idx% = 0
    Else
      idx% = (i% - 1) Mod CYCLE_COLORS
    EndIf
    iterColor%(i%) = cycle%(idx%)
  Next i%

  ' Distinct interior (did not escape by MAX_ITER)
  iterColor%(MAX_ITER) = RGB(0,0,0)

End Sub


CLS
BuildPaletteFromStops PALETTE_NAME$
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
            If LCase$(PALETTE_NAME$) = "turbo" Then
                BuildPaletteFromStops "viridis"
                PALETTE_NAME$ = "viridis"
            ElseIf LCase$(PALETTE_NAME$) = "viridis" Then
                BuildPaletteFromStops "inferno"
                PALETTE_NAME$ = "inferno"
            Else
                BuildPaletteFromStops "turbo"
                PALETTE_NAME$ = "turbo"
            EndIf
            CLS
            DrawMenu
            RenderViewport
    End Select
Loop

' SAVE IMAGE uses StartX, StartY, width, height
Save Image "out.bmp", 0, 0, SCREEN_W, SCREEN_H
